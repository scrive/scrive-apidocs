{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocSeal
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  not portable
--
-- All that is needed to seal a document
-----------------------------------------------------------------------------
module Doc.DocSeal(sealDocument) where

import Control.Concurrent
import Control.Monad.Reader
import Data.Maybe
import Data.Bits
import Data.List
import Data.Word
import Debug.Trace
import Doc.DocState
import Doc.DocStorage
import Happstack.State (update, query)
import MinutesTime
import Misc
import System.Directory
import System.Exit
import Kontra
import qualified Amazon as AWS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified SealSpec as Seal
import qualified TrustWeaver as TW
import qualified AppLogger as Log
import System.IO.Temp
import System.IO
                         
personFromSignatoryDetails :: SignatoryDetails -> Seal.Person
personFromSignatoryDetails details =
    Seal.Person { Seal.fullname = BS.toString $ signatoryname details 
                , Seal.company = BS.toString $ signatorycompany details
                , Seal.email = BS.toString $ signatoryemail details
                -- | FIXME: this should be split to company/personal number
                , Seal.personalnumber = BS.toString $ signatorypersonalnumber details
                , Seal.companynumber = BS.toString $ signatorycompanynumber details
                , Seal.fullnameverified = False
                , Seal.companyverified = False
                , Seal.numberverified = False
                , Seal.emailverified = True
                }

personsFromDocument :: Document -> [(Seal.Person, SignInfo, SignInfo, Bool, Maybe SignatureProvider)]
personsFromDocument document = 
    let
        links = documentsignatorylinks document
        authorid = unAuthor $ documentauthor document
        x (SignatoryLink{ signatorydetails
                        , maybesigninfo = Just signinfo
                        , maybeseeninfo
                        , maybesignatory
                        , signatorysignatureinfo
                        })
             -- FIXME: this one should really have seentime always...
             = ((personFromSignatoryDetails signatorydetails)
                { Seal.emailverified = True
                , Seal.fullnameverified = fullnameverified
                , Seal.companyverified = False
                , Seal.numberverified = numberverified}
              , maybe signinfo id maybeseeninfo
              , signinfo
              , maybe False ((==) authorid) maybesignatory
              , maybe Nothing (Just . signatureinfoprovider) signatorysignatureinfo)
                  where fullnameverified = maybe False (\x -> signaturefstnameverified x
                                                        && signaturelstnameverified x)
                                                signatorysignatureinfo
                        numberverified = maybe False signaturepersnumverified signatorysignatureinfo

        x link = trace (show link) $ error "SignatoryLink does not have all the necessary data"
    in map x links

fieldsFromPlacement :: String -> FieldPlacement -> Seal.Field
fieldsFromPlacement value placement  =
    let toPtt x = (x * 72 `div` 190) - 5 -- scalling and some replacing
        w = placementpagewidth placement
        h = placementpageheight placement 
    in    
    Seal.Field { Seal.value = value
               , Seal.x =  toPtt $ (placementx placement * w) `div` 943
               , Seal.y =  toPtt $ h - ((placementy placement * h) `div` 1335)
               , Seal.page = placementpage placement
               , Seal.w =  w
               , Seal.h = h
               }
               
fieldsFromDefinition :: FieldDefinition -> [Seal.Field]
fieldsFromDefinition def =
    map (fieldsFromPlacement (BS.toString (fieldvalue def))) (fieldplacements def)

fieldsFromSignatory::SignatoryDetails -> [Seal.Field]
fieldsFromSignatory sig = 
    (map (fieldsFromPlacement (BS.toString (signatoryfstname sig))) (signatoryfstnameplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatorysndname sig))) (signatorysndnameplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatoryemail sig))) (signatoryemailplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatorycompany sig))) (signatorycompanyplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatorypersonalnumber sig))) (signatorypersonalnumberplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatorycompanynumber sig))) (signatorycompanynumberplacements sig))
    ++
    (foldl (++) [] (map fieldsFromDefinition (signatoryotherfields sig)))    

-- oh boy, this is really network byte order!
formatIP :: Word32 -> String
formatIP 0 = ""
-- formatIP 0x7f000001 = ""
formatIP x = " (IP: " ++ show ((x `shiftR` 0) .&. 255) ++
                   "." ++ show ((x `shiftR` 8) .&. 255) ++
                   "." ++ show ((x `shiftR` 16) .&. 255) ++
                   "." ++ show ((x `shiftR` 24) .&. 255) ++ ")"
                   
formatProvider :: SignatureProvider -> String
formatProvider BankIDProvider = "BankID"
formatProvider NordeaProvider = "Nordea e-legitimation"
formatProvider TeliaProvider  = "Telia e-legitimation"
formatProvider _ = "e-legitimation"

sealSpecFromDocument :: String -> Document -> User ->  String -> String -> Seal.SealSpec
sealSpecFromDocument hostpart document author inputpath outputpath =
  let docid = unDocumentID (documentid document)
      authorHasSigned = (any ((maybe False (== (userid author))) . maybesignatory) (documentsignatorylinks document))
      signatoriesdetails = map signatorydetails $ documentsignatorylinks document
      authordetails = (signatoryDetailsFromUser author) 
                      { signatoryfstnameplacements = authorfstnameplacements document
                      , signatorysndnameplacements = authorsndnameplacements document
                      , signatorypersonalnumberplacements = authorpersonalnumberplacements document
                      , signatorycompanynumberplacements = authorcompanynumberplacements document
                      , signatoryemailplacements = authoremailplacements document
                      , signatorycompanyplacements = authorcompanyplacements document
                      , signatoryotherfields = authorotherfields document
                      }
      signatories = personsFromDocument document
      secretaries = if authorHasSigned then [] else [personFromSignatoryDetails authordetails]

      persons = map (\(a,_,_,_,_) -> a) signatories
      paddeddocid = pad0 20 (show docid)

      initials = concatComma (map initialsOfPerson persons)
      initialsOfPerson (Seal.Person {Seal.fullname}) = map head (words fullname)
      authorfullname = signatoryname authordetails
      -- 2. "Name of invited" granskar dokumentet online
      makeHistoryEntryFromSignatory (Seal.Person {Seal.fullname},seen, signed, False, mprovider) = 
          [   Seal.HistEntry
            { Seal.histdate = show (signtime seen)
            , Seal.histcomment = fullname ++ " granskar dokumentet online" ++ formatIP (signipnumber seen)
            } 
            , Seal.HistEntry
            { Seal.histdate = show (signtime signed)
            , Seal.histcomment = if isNothing mprovider
                                    then fullname ++ " undertecknar dokumentet online med e-post" ++ formatIP (signipnumber signed)
                                    else fullname ++ " undertecknar dokumentet online med " ++ formatProvider (fromJust mprovider) ++ formatIP (signipnumber signed)
            }
          ]
      makeHistoryEntryFromSignatory (Seal.Person {Seal.fullname},seen, signed, True, mprovider) = 
          [   Seal.HistEntry
            { Seal.histdate = show (signtime signed)
            , Seal.histcomment = if isNothing mprovider
                                    then fullname ++ " undertecknar dokumentet online med e-post" ++ formatIP (signipnumber signed)
                                    else fullname ++ " undertecknar dokumentet online med " ++ formatProvider (fromJust mprovider) ++ formatIP (signipnumber signed)
            }
          ]
      invitationSentEntry = case documentinvitetime document of
                                Nothing -> []
                                Just (SignInfo time ipnumber) -> 
                                    [ Seal.HistEntry
                                      { Seal.histdate = show time
                                      , Seal.histcomment = 
                                          if length signatories>1
                                          then BS.toString authorfullname ++ 
                                                   " skickar en inbjudan att underteckna till parterna" ++ 
                                                   formatIP ipnumber
                                          else BS.toString authorfullname ++ 
                                                   " skickar en inbjudan att underteckna till parten" ++ 
                                                   formatIP ipnumber
                                      }]

      maxsigntime = maximum (map (signtime . (\(_,_,c,_,_) -> c)) signatories)
      concatComma = concat . intersperse ", "
      
      lastHistEntry = Seal.HistEntry
                      { Seal.histdate = show maxsigntime
                      , Seal.histcomment = "Samtliga parter har undertecknat dokumentet och avtalet är nu juridiskt bindande."
                      }

      -- here we use Data.List.sort that is *stable*, so it puts
      -- signatories actions before what happened with a document
      histDateCompare a b = compare (Seal.histdate a) (Seal.histdate b)
      history = sortBy histDateCompare $ (concatMap makeHistoryEntryFromSignatory signatories) ++
                invitationSentEntry ++
                [lastHistEntry]
      
      -- document fields
      fields = if authorHasSigned
               then concatMap fieldsFromSignatory signatoriesdetails
               else concatMap fieldsFromSignatory $ authordetails : signatoriesdetails
                    
      config = Seal.SealSpec 
            { Seal.input          = inputpath
            , Seal.output         = outputpath
            , Seal.documentNumber = paddeddocid
            , Seal.persons        = persons
            , Seal.secretaries    = secretaries
            , Seal.history        = history
            , Seal.initials       = initials
            , Seal.hostpart       = hostpart
            , Seal.fields         = fields
            }
      in config

sealDocument :: Context 
             -> User
             -> Document
             -> IO (Either String Document)
sealDocument ctx@Context{ctxdocstore, ctxs3action, ctxtwconf,ctxhostpart}
             author
             document = do
                 let files = documentfiles document
                 mapM_ (sealDocumentFile ctx author document) files
                 Just newdocument <- query $ GetDocumentByDocumentID (documentid document)
                 _ <- liftIO $ forkIO $ mapM_ (AWS.uploadFile ctxdocstore ctxs3action) (documentsealedfiles newdocument)
                 return $ Right newdocument

                 

{- Someday:

 We have lost the ability to upload document to TrustWeaver. Resurrect
 it someday, when somebody asks.

   case signeddocstorage (usersettings author) of
       Nothing -> return ()
       Just twsettings -> 
           do
               _ <- forkIO $ uploadDocumentFilesToTrustWeaver ctxtwconf (BS.toString $ storagetwname twsettings) documentid
               return ()
 -}


sealDocumentFile :: Context 
                 -> User
                 -> Document
                 -> File
                 -> IO (Either String Document)
sealDocumentFile ctx@Context{ctxdocstore, ctxs3action, ctxtwconf, ctxhostpart}
                 author
                 document@Document{documentid,documenttitle}
                 file@File {fileid,filename} =
  withSystemTempDirectory ("seal-" ++ show documentid ++ "-" ++ show fileid ++ "-") $ \tmppath -> do
  let tmpin = tmppath ++ "/input.pdf"
  let tmpout = tmppath ++ "/output.pdf"
  content <- getFileContents ctx file
  BS.writeFile tmpin content
  let config = sealSpecFromDocument ctxhostpart document author tmpin tmpout
  (code,stdout,stderr) <- readProcessWithExitCode' "dist/build/pdfseal/pdfseal" [] (BSL.fromString (show config))

  case code of
      ExitSuccess -> 
          do
              newfilepdf1 <- BS.readFile tmpout
              newfilepdf <- 
                  case TW.signConf ctxtwconf of
                      Nothing -> return newfilepdf1
                      Just x -> do
                          x <- TW.signDocument ctxtwconf newfilepdf1
                          case x of
                              Left errmsg -> 
                                  do
                                      let msg = "Cannot TrustWeaver sign doc #" ++ show documentid ++ " file #" ++ show fileid ++ ": " ++ errmsg
                                      Log.error $ msg 
                                      Log.trustWeaver $ msg 
                                      return newfilepdf1
                              Right result -> 
                                  do
                                      let msg = "TrustWeaver signed doc #" ++ show documentid ++ " file #" ++ show fileid ++ ": " ++ BS.toString documenttitle
                                      Log.trustWeaver msg
                                      return result

              update $ AttachSealedFile documentid filename newfilepdf
      ExitFailure _ -> 
          do
              -- error handling
              systmp <- getTemporaryDirectory
              (path,handle) <- openTempFile systmp ("seal-failed-" ++ show documentid ++ "-" ++ show fileid ++ "-.pdf")
              let msg = "Cannot seal document #" ++ show documentid ++ " bacause of file #" ++ show fileid
              Log.error $ msg ++ ": " ++ path
              Log.error $ BSL.toString stderr
              Log.error $ "Sealing configuration: " ++ show config
              BS.hPutStr handle content
              hClose handle
              _ <- update $ ErrorDocument documentid $ "Could not seal document because of file #" ++ show fileid
              return $ Left msg
