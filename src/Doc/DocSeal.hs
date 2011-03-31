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
import Happstack.State (update)
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
                         
personFromSignatoryDetails :: SignatoryDetails -> Seal.Person
personFromSignatoryDetails details =
    Seal.Person { Seal.fullname = BS.toString $ signatoryname details 
                , Seal.company = BS.toString $ signatorycompany details
                , Seal.email = BS.toString $ signatoryemail details
                , Seal.number = BS.toString $ signatorynumber details
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
    (map (fieldsFromPlacement (BS.toString (signatorynumber sig))) (signatorynumberplacements sig))
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
                      , signatorynumberplacements = authornumberplacements document
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
      invitationSentEntry = Seal.HistEntry
            { Seal.histdate = show (documentinvitetime document)
            , Seal.histcomment = 
                if length signatories>1
                   then BS.toString authorfullname ++ " skickar en inbjudan att underteckna till parterna"
                   else BS.toString authorfullname ++ " skickar en inbjudan att underteckna till parten"
            }

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
                [invitationSentEntry] ++
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
             -> MVar (Map.Map FileID JpegPages)
             -> String
             -> MinutesTime
             -> User
             -> Document
             -> IO (Either String Document)
sealDocument ctx@Context{ctxdocstore, ctxs3action, ctxtwconf}
             _ 
             hostpart
             _
             author
             document = do
  let (file@File {fileid,filename}) = 
           safehead "sealDocument" $ documentfiles document
  let docid = documentid document

  tmppath <- getTemporaryDirectory
  let tmpin = tmppath ++ "/in_" ++ show docid ++ ".pdf"
  let tmpout = tmppath ++ "/out_" ++ show docid ++ ".pdf"
  contents <- getFileContents ctx file
  BS.writeFile tmpin contents
  let config = sealSpecFromDocument hostpart document author tmpin tmpout
  (code,stdout,stderr) <- readProcessWithExitCode' "dist/build/pdfseal/pdfseal" [] (BSL.fromString (show config))
  Log.debug $ "seal exit code " ++ show code
  Log.debug $ "seal stdout: " ++ BSL.toString stdout
  Log.debug $ "seal stderr: " ++ BSL.toString stderr

  if code == ExitSuccess 
     then do

       newfilepdf1 <- BS.readFile tmpout
       removeFile tmpout

       newfilepdf <- 
        if TW.signcert ctxtwconf == ""
         then return newfilepdf1
         else do
           x <- TW.signDocument ctxtwconf newfilepdf1
           case x of
                   Left errmsg -> do
                     -- FIXME: handle the error in a better way here
                     putStrLn errmsg
                     return newfilepdf1
                   Right result -> return result

       mnewdocument <- update $ AttachSealedFile docid filename newfilepdf
       case mnewdocument of
         Right newdocument -> do
          _ <-liftIO $ forkIO $ mapM_ (AWS.uploadFile ctxdocstore ctxs3action) (documentsealedfiles newdocument)
          case signeddocstorage (usersettings author) of
           Nothing -> return ()
           Just twsettings -> do
                        _ <- forkIO $ uploadDocumentFilesToTrustWeaver ctxtwconf (BS.toString $ storagetwname twsettings) (documentid newdocument)
                        return () 
          return $ Right newdocument
         Left msg -> error msg
     else do
        -- error handling
        Log.error $ "seal: cannot seal document " ++ show docid ++ ", fileid " ++ show fileid
        _ <- update $ ErrorDocument docid "could not seal document"
        return $ Left "could not seal document"
