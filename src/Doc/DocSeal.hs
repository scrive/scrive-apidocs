{-# LANGUAGE CPP #-}

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

import Control.Monad.Reader
import Data.Maybe
import Data.List
import Data.Ord
import Debug.Trace
import Doc.DocProcess
import Doc.DocStateData
import Doc.Model
import Doc.DocStorage
import Doc.DocView
import Doc.DocUtils
import Misc
import System.Directory
import System.Exit
import Kontra
import Templates.Trans
import Templates.Templates
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.ByteString.Base64 as B64
import qualified SealSpec as Seal
import qualified TrustWeaver as TW
import qualified Log
import System.IO.Temp
import System.IO hiding (stderr)
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import File.Model
import DB.Classes
import Control.Applicative
import EvidenceLog.Model
import Control.Concurrent
personFromSignatoryDetails :: SignatoryDetails -> Seal.Person
personFromSignatoryDetails details =
    Seal.Person { Seal.fullname = (getFullName details) ++
                                  if not (null $ getPersonalNumber details)
                                     then " (" ++ (getPersonalNumber details) ++ ")"
                                     else ""
                , Seal.company = getCompanyName details
                , Seal.email = getEmail details
                , Seal.personalnumber = getPersonalNumber details
                , Seal.companynumber = getCompanyNumber details
                , Seal.fullnameverified = False
                , Seal.companyverified = False
                , Seal.numberverified = False
                , Seal.emailverified = True
                }

personFields :: MonadIO m => (Seal.Person, SignInfo, SignInfo, Bool, Maybe SignatureProvider,String) -> Fields m
personFields (person, signinfo,_seeninfo, _ , mprovider, _initials) = do
   field "personname" $ Seal.fullname person
   field "signip" $  formatIP (signipnumber signinfo)
   field "seenip" $  formatIP (signipnumber signinfo)
   field "provider" $ isJust mprovider
   field "bankid" $ mprovider == Just BankIDProvider
   field "nordea" $ mprovider == Just NordeaProvider
   field "telia"  $ mprovider == Just TeliaProvider


personsFromDocument :: Document -> [(Seal.Person, SignInfo, SignInfo, Bool, Maybe SignatureProvider, String)]
personsFromDocument document =
    let
        links = filter isSignatory $ documentsignatorylinks document
        x (sl@SignatoryLink { signatorydetails
                            , maybesigninfo = Just signinfo
                            , maybeseeninfo
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
              , isAuthor sl
              , maybe Nothing (Just . signatureinfoprovider) signatorysignatureinfo
              , map head $ words $ getFullName signatorydetails
              )
                  where fullnameverified = maybe False (\s -> signaturefstnameverified s
                                                        && signaturelstnameverified s)
                                                signatorysignatureinfo
                        numberverified = maybe False signaturepersnumverified signatorysignatureinfo

        x link = trace (show link) $ error "SignatoryLink does not have all the necessary data"
    in map x links

fieldsFromSignatory :: SignatoryDetails -> [Seal.Field]
fieldsFromSignatory SignatoryDetails{signatoryfields} =
  concatMap makeSealField  signatoryfields
  where
    makeSealField :: SignatoryField -> [Seal.Field]
    makeSealField sf = case  sfType sf of
                         SignatureFT -> map (fieldJPEGFromPlacement (sfValue sf)) (sfPlacements sf) 
                         _ -> map (fieldFromPlacement (sfValue sf)) (sfPlacements sf) 
    fieldFromPlacement sf placement = Seal.Field {
        Seal.value = sf
      , Seal.x = placementx placement
      , Seal.y = placementy placement
      , Seal.page = placementpage placement
      , Seal.w = placementpagewidth placement
      , Seal.h = placementpageheight placement
     }
    fieldJPEGFromPlacement sf placement = Seal.FieldJPG
      { valueBase64      =  dropWhile (\c -> c == ';' || c == ',') sf
      , Seal.x = placementx placement
      , Seal.y = placementy placement + 17 -- Fix for signature box header from UI
      , Seal.page = placementpage placement
      , Seal.w = placementpagewidth placement
      , Seal.h = placementpageheight placement
      , Seal.image_w       = 250
      , Seal.image_h       = 100
      , Seal.internal_image_w = 250
      , Seal.internal_image_h = 100
      }

sealSpecFromDocument :: TemplatesMonad m => String -> Document -> [DocumentEvidenceEvent] -> String -> String -> m Seal.SealSpec
sealSpecFromDocument hostpart document elog inputpath outputpath =
  let docid = documentid document
      Just authorsiglink = getAuthorSigLink document
      authorHasSigned = isSignatory authorsiglink && isJust (maybesigninfo authorsiglink)
      signatoriesdetails = [signatorydetails sl | sl <- documentsignatorylinks document
                                                , SignatoryPartner `elem` signatoryroles sl]
      authordetails = signatorydetails authorsiglink
      signatories = personsFromDocument document
      secretaries = if authorHasSigned then [] else [personFromSignatoryDetails authordetails]

      persons = map (\(a,_,_,_,_,_) -> a) signatories
      initialsx = map (\(_,_,_,_,_,a) -> a) signatories
      paddeddocid = pad0 20 (show docid)

      initials = concatComma initialsx
      makeHistoryEntryFromSignatory personInfo@(_ ,seen, signed, isauthor, _, _)  = do
          seenDesc <- renderLocalTemplateForProcess document processseenhistentry $ do
                        personFields personInfo
                        documentInfoFields document
          let seenEvent = Seal.HistEntry
                            { Seal.histdate = show (signtime seen)
                            , Seal.histcomment = pureString seenDesc}
          signDesc <- renderLocalTemplateForProcess document processsignhistentry $ do
                        personFields personInfo
                        documentInfoFields document
          let signEvent = Seal.HistEntry
                            { Seal.histdate = show (signtime signed)
                            , Seal.histcomment = pureString signDesc}
          return $ if (isauthor)
                    then [signEvent]
                    else [seenEvent,signEvent]
      invitationSentEntry = case documentinvitetime document of
                                Nothing -> return []
                                Just (SignInfo time ipnumber) -> do
                                   desc <-  renderLocalTemplateForProcess document processinvitationsententry $ do
                                       documentInfoFields document
                                       documentAuthorInfo document
                                       field "oneSignatory"  (length signatories>1)
                                       field "personname" $ listToMaybe $ map getFullName  signatoriesdetails
                                       field "ip" $ formatIP ipnumber
                                   return  [ Seal.HistEntry
                                      { Seal.histdate = show time
                                      , Seal.histcomment = pureString desc
                                      }]

      maxsigntime = maximum (map (signtime . (\(_,_,c,_,_,_) -> c)) signatories)
      concatComma = concat . intersperse ", "

      lastHistEntry = do
                       desc <- renderLocalTemplateForProcess document processlasthisentry (documentInfoFields document)
                       return $ if (Just True == getValueForProcess document processsealincludesmaxtime)
                                then [Seal.HistEntry
                                { Seal.histdate = show maxsigntime
                                , Seal.histcomment = pureString desc}]
                                else []

      -- document fields
      fields = concatMap (fieldsFromSignatory . signatorydetails) (documentsignatorylinks document)

  in do
      -- Log.debug "Creating seal spec from file."
      events <- fmap concat $ sequence $
                    (map makeHistoryEntryFromSignatory signatories) ++
                    [invitationSentEntry] ++
                    [lastHistEntry]
      -- Log.debug ("events created: " ++ show events)
      -- here we use Data.List.sort that is *stable*, so it puts
      -- signatories actions before what happened with a document
      let history = sortBy (comparing Seal.histdate) events
      -- Log.debug ("about to render staticTexts")
      staticTexts <- renderLocalTemplateForProcess document processsealingtext $ do
                        documentInfoFields document
                        field "hostpart" hostpart
      -- Log.debug ("finished staticTexts: " ++ show staticTexts)
      let readtexts :: Seal.SealingTexts = read staticTexts -- this should never fail since we control templates
      -- Log.debug ("read texts: " ++ show readtexts)

      -- Creating HTML Evidence Log
      htmllogs <- htmlDocFromEvidenceLog (documenttitle document) elog
      let evidenceattachment = Seal.SealAttachment { Seal.fileName = "evidencelog.html"
                                                   , Seal.fileBase64Content = BS.toString $ B64.encode $ BS.fromString htmllogs }

      return $ Seal.SealSpec
            { Seal.input          = inputpath
            , Seal.output         = outputpath
            , Seal.documentNumber = paddeddocid
            , Seal.persons        = persons
            , Seal.secretaries    = secretaries
            , Seal.history        = history
            , Seal.initials       = initials
            , Seal.hostpart       = hostpart
            , Seal.fields         = fields
            , Seal.staticTexts    = readtexts
            , Seal.attachments    = [evidenceattachment]
            }


sealDocument :: Context
             -> Document
             -> DB (Either String Document)
sealDocument ctx document = do
  files <- documentfilesM document
  Log.debug $ "Sealing document"
  mapM_ (sealDocumentFile ctx document) files
  Log.debug $ "Sealing should be done now"
  Just newdocument <- dbQuery $ GetDocumentByDocumentID (documentid document)
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
                 -> Document
                 -> File
                 -> DB (Either String Document)
sealDocumentFile ctx@Context{ctxtwconf, ctxhostpart, ctxlocale, ctxglobaltemplates}
                 document@Document{documentid, documenttitle}
                 file@File{fileid, filename} = do
  tmppath <- liftIO $ do
    tmpdir <- getTemporaryDirectory
    createTempDirectory tmpdir $ "seal-" ++ show documentid ++ "-" ++ show fileid ++ "-"
  result <- tryDB $ do
    elog <- dbQuery $ GetEvidenceLog documentid
    Log.debug ("sealing: " ++ show fileid)
    let tmpin = tmppath ++ "/input.pdf"
    let tmpout = tmppath ++ "/output.pdf"
    content <- liftIO $ getFileContents ctx file
    liftIO $ BS.writeFile tmpin content
    config <- runTemplatesT (ctxlocale, ctxglobaltemplates) $ sealSpecFromDocument ctxhostpart document elog tmpin tmpout
    Log.debug $ "Config " ++ show config
    (code,_stdout,stderr) <- liftIO $ readProcessWithExitCode' "dist/build/pdfseal/pdfseal" [] (BSL.fromString (show config))
    liftIO $ threadDelay 500000

    Log.debug $ "Sealing completed with " ++ show code
    case code of
      ExitSuccess -> do
        newfilepdf1 <- liftIO $ BS.readFile tmpout
        newfilepdf <- case TW.signConf ctxtwconf of
          Nothing -> do
            Log.debug $ "TrustWeaver configuration empty, not doing TrustWeaver signing"
            return newfilepdf1
          Just _ -> do
            Log.debug $ "About to TrustWeaver sign doc #" ++ show documentid ++ " file #" ++ show fileid
            x <- liftIO $ TW.signDocument ctxtwconf newfilepdf1
            case x of
              Left errmsg -> do
                let msg = "Cannot TrustWeaver sign doc #" ++ show documentid ++ " file #" ++ show fileid ++ ": " ++ errmsg
                Log.error $ msg
                Log.trustWeaver $ msg
                return newfilepdf1
              Right result -> do
                let msg = "TrustWeaver signed doc #" ++ show documentid ++ " file #" ++ show fileid ++ ": " ++ documenttitle
                Log.trustWeaver msg
                return result
        Log.debug $ "Adding new sealed file to DB"
        File{fileid = sealedfileid} <- dbUpdate $ NewFile filename newfilepdf
        Log.debug $ "Finished adding sealed file to DB with fileid " ++ show sealedfileid ++ "; now adding to document"
        res <- dbUpdate $ AttachSealedFile documentid sealedfileid (SystemActor (ctxtime ctx))
        Log.debug $ "Should be attached to document; is it? " ++ show ((elem sealedfileid . documentsealedfiles) <$> res)
        return res
      ExitFailure _ -> do
        -- error handling
        msg <- liftIO $ do
          systmp <- getTemporaryDirectory
          (path,handle) <- openTempFile systmp ("seal-failed-" ++ show documentid ++ "-" ++ show fileid ++ "-.pdf")
          let msg = "Cannot seal document #" ++ show documentid ++ " because of file #" ++ show fileid
          Log.error $ msg ++ ": " ++ path
          Log.error $ BSL.toString stderr
          Log.error $ "Sealing configuration: " ++ show config
          BS.hPutStr handle content
          hClose handle
          return msg
        _ <- dbUpdate $ ErrorDocument documentid ("Could not seal document because of file #" ++ show fileid) (SystemActor (ctxtime ctx))
        return $ Left msg
  liftIO $ removeDirectoryRecursive tmppath
  case result of
    Right res -> return res
    Left (e::E.SomeException) -> E.throw e
