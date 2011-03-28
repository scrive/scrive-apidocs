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
                }

personsFromDocument :: Document -> [(Seal.Person, (MinutesTime,Word32), (MinutesTime,Word32))]
personsFromDocument document = 
    let
        links = documentsignatorylinks document
        unSignInfo (SignInfo { signtime, signipnumber }) = (signtime,signipnumber)
        x (SignatoryLink{ signatorydetails
                        , maybesigninfo = Just (si@SignInfo { signtime, signipnumber })
                        , maybeseeninfo
                        })
             -- FIXME: this one should really have seentime always...
             = (personFromSignatoryDetails signatorydetails, unSignInfo $ maybe si id maybeseeninfo, (signtime,signipnumber))
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

sealSpecFromDocument :: String -> Document -> User ->  String -> String -> Seal.SealSpec
sealSpecFromDocument hostpart document author inputpath outputpath =
  let docid = unDocumentID (documentid document)
      authorHasSigned = (any ((maybe False (== (userid author))) . maybesignatory) (documentsignatorylinks document))
      signatoriesdetails = map signatorydetails $ documentsignatorylinks document
      authordetails = (signatoryDetailsFromUser author) 
                      {
                        signatoryfstnameplacements = authorfstnameplacements document
                      , signatorysndnameplacements = authorsndnameplacements document
                      , signatorynumberplacements = authornumberplacements document
                      , signatoryemailplacements = authoremailplacements document
                      , signatorycompanyplacements = authorcompanyplacements document
                      , signatoryotherfields = authorotherfields document
                      }
      signatories = personsFromDocument document
      secretaries = if authorHasSigned then [] else [personFromSignatoryDetails authordetails]

      -- oh boy, this is really network byte order!
      formatIP :: Word32 -> String
      formatIP 0 = ""
      -- formatIP 0x7f000001 = ""
      formatIP x = " (IP: " ++ show ((x `shiftR` 0) .&. 255) ++
                   "." ++ show ((x `shiftR` 8) .&. 255) ++
                   "." ++ show ((x `shiftR` 16) .&. 255) ++
                   "." ++ show ((x `shiftR` 24) .&. 255) ++ ")"
      persons = (map fst3 signatories)
      -- persons = authorperson : (map fst3 signatories)
      paddeddocid = reverse $ take 20 $ (reverse (show docid) ++ repeat '0')
      initials = concatComma (map initialsOfPerson persons)
      initialsOfPerson (Seal.Person {Seal.fullname}) = map head (words fullname)
      authorfullname = signatoryname authordetails
      -- 2. "Name of invited" granskar dokumentet online
      makeHistoryEntryFromSignatory (Seal.Person {Seal.fullname},(seentime2,seenipnumber2),(signtime2,signipnumber2)) = 
          [   Seal.HistEntry
            { Seal.histdate = show seentime2
            , Seal.histcomment = fullname ++ " granskar dokumentet online" ++ formatIP seenipnumber2
            } 
            , Seal.HistEntry
            { Seal.histdate = show signtime2
            , Seal.histcomment = fullname ++ " undertecknar dokumentet online" ++ formatIP signipnumber2
            }
      
          ]
      makeHistoryEntryFromEvent (DocumentHistoryInvitationSent time ipnumber _) =
          [ Seal.HistEntry
            { Seal.histdate = show time 
            , Seal.histcomment = 
                if length signatories>1
                   then BS.toString authorfullname ++ " skickar en inbjudan att underteckna till parterna" ++ formatIP ipnumber
                   else BS.toString authorfullname ++ " skickar en inbjudan att underteckna till parten" ++ formatIP ipnumber
            }
          ]
      makeHistoryEntryFromEvent _ = []         
      makeHistoryEntry = either makeHistoryEntryFromEvent makeHistoryEntryFromSignatory  
      maxsigntime = maximum (map (fst . thd3) signatories)
      concatComma = concat . intersperse ", "
      -- Hack to switch the order of events, so we put invitation send after author signing
      makeHistory (invSend@(Left (DocumentHistoryInvitationSent time _ _)):(pSign@(Right (_,_,(signtime2,_)))):rest) = 
          if (signtime2 == time)
           then (makeHistoryEntry pSign) ++ (makeHistoryEntry invSend) ++ (makeHistory rest)
           else (makeHistoryEntry invSend) ++ (makeHistoryEntry pSign) ++ (makeHistory rest)
      makeHistory (e:es) = (makeHistoryEntry e) ++ (makeHistory es)
      makeHistory [] = []
      
      lastHistEntry = Seal.HistEntry
                      { Seal.histdate = show maxsigntime
                      , Seal.histcomment = "Samtliga parter har undertecknat dokumentet och avtalet är nu juridiskt bindande."
                      }

      history = (makeHistory $ ((map Left (documenthistory document)) ++ (map Right signatories))) ++ [lastHistEntry]
      
      -- document fields
      fields = if authorHasSigned
               then (concat (map fieldsFromSignatory signatoriesdetails))
               else (concat (map fieldsFromSignatory $ authordetails : signatoriesdetails))
                    
      config = Seal.SealSpec 
            { Seal.input = inputpath
            , Seal.output = outputpath
            , Seal.documentNumber = paddeddocid
            , Seal.persons = persons
            , Seal.secretaries = secretaries
            , Seal.history = history
            , Seal.initials = initials
            , Seal.hostpart = hostpart
            , Seal.fields = fields
            }
      in config

sealDocument :: Context 
             -> MVar (Map.Map FileID JpegPages)
             -> String
             -> MinutesTime
             -> User
             -> Document
             -> IO (Either String Document)
sealDocument ctx@Context{ctxs3action,ctxtwconf}
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
          _ <-liftIO $ forkIO $ mapM_ (AWS.uploadFile ctxs3action) (documentsealedfiles newdocument)
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
