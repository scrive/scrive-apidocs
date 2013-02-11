{-# LANGUAGE ExtendedDefaultRules #-}
{- |
   DocControl represents the controler (in MVC) of the document.
 -}
module Doc.DocControl(
    -- Exported utils or test functions
      sendReminderEmail
    -- Top level handlers
    , handleNewDocument
    , showCreateFromTemplate 
    , handleDownloadFile
    , handleSignShow
    , handleSignShowSaveMagicHash
    , splitUpDocumentWorker
    , signDocument
    , signDocumentIphoneCase
    , rejectDocument
    , rejectDocumentIphoneCase
    , handleAcceptAccountFromSign
    , handleSigAttach
    , handleDeleteSigAttach
    , handleEvidenceAttachment
    , handleIssueShowGet
    , handleIssueShowPost
    , handleIssueAuthorGoToSignview
    , handleSetAttachments
    , handleParseCSV
    , prepareEmailPreview
    , handleResend
    , handleChangeSignatoryEmail
    , handleRestart
    , showPage
    , showPreview
    , showPreviewForSignatory
    , handleFilePages
    , handleShowVerificationPage
    , handleVerify
    , handleMarkAsSaved
) where

import AppView
import Attachment.AttachmentID (AttachmentID)
import DB
import DB.TimeZoneName (TimeZoneName, mkTimeZoneName)
import DBError
import Doc.Action
import Doc.CSVUtils
import Doc.Model
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocStateUpdate
import Doc.Rendering
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import Doc.SignatoryLinkID
import Doc.DocumentID
import qualified Doc.EvidenceAttachments as EvidenceAttachments
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import qualified Doc.Screenshot as Screenshot
import Doc.Tokens.Model
import Crypto.RNG
import Attachment.Model
import InputValidation
import File.Model
import File.Storage
import Kontra
import KontraLink
import MagicHash
import Happstack.Fields
import Utils.Monad
import Utils.Prelude
import Utils.Read
import Utils.String
import Utils.Tuples
import Redirect
import User.Model
import Util.HasSomeUserInfo
import qualified Log
import Templates.Templates
import Util.CSVUtil
import Util.FlashUtil
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Util.MonadUtils
import Stats.Control
import User.Utils
import Control.Applicative
import Control.Concurrent
import qualified Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Base
import Data.Either
import Data.List
import Data.Maybe
import Happstack.Server.Types
import Happstack.Server hiding (simpleHTTP)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified Data.ByteString.UTF8 as BS hiding (length, take)
import qualified Data.Map as Map
import System.FilePath
import Text.JSON hiding (Result)
import Text.JSON.Gen hiding (value)
import Text.JSON.String (runGetJSON)
import qualified Text.JSON.Gen as J
import Text.JSON.FromJSValue
import Doc.DocDraft() -- Import instances only
import qualified User.Action
import qualified ELegitimation.Control as BankID
import Util.Actor
import qualified Templates.Fields as F
import qualified MemCache as MemCache
import qualified GuardTime as GuardTime
import System.IO.Temp
import System.Directory
import MinutesTime
import Analytics.Include
import Data.String.Utils (replace)

handleNewDocument :: Kontrakcja m => m KontraLink
handleNewDocument = do
  ctx <- getContext
  if (isJust $ ctxmaybeuser ctx)
     then withUserPost $ do
        user <- guardJustM $ ctxmaybeuser <$> getContext
        title <- renderTemplate_ "newDocumentTitle"
        actor <- guardJustM $ mkAuthorActor <$> getContext
        Just doc <- dbUpdate $ NewDocument user (replace "  " " " $ title ++ " " ++ formatMinutesTimeSimple (ctxtime ctx)) (Signable Contract) 1 actor
        _ <- dbUpdate $ SetDocumentUnsavedDraft [documentid doc] True
        return $ LinkIssueDoc (documentid doc)
     else return $ LinkLogin (ctxlang ctx) LoginTry
{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}


showCreateFromTemplate :: Kontrakcja m => m (Either KontraLink String)
showCreateFromTemplate = withUserGet $ pageCreateFromTemplate

{- |

    Handles an account setup from within the sign view.
-}
handleAcceptAccountFromSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
handleAcceptAccountFromSign documentid
                            signatorylinkid = do
  magichash <- guardJustM $ dbQuery $ GetDocumentSessionToken signatorylinkid
  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid magichash
  when (documentdeliverymethod document == PadDelivery) internalError
  signatorylink <- guardJust $ getSigLinkFor document signatorylinkid
  muser <- User.Action.handleAccountSetupFromSign document signatorylink
  case muser of
    Just user -> runJSONGenT $ do
      J.value "userid" (show $ userid user)
    Nothing -> runJSONGenT $ do
      return ()

{- |
   Control the signing of a document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
   Method: POST
 -}
signDocumentIphoneCase :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
signDocumentIphoneCase did sid _ = signDocument did sid

signDocument :: Kontrakcja m
             => DocumentID      -- ^ The DocumentID of the document to sign
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL
             -> m KontraLink
signDocument documentid
             signatorylinkid = do
  -- This method requires fixes. Right now we emulate author signing
  -- by digging out his magichash and using that in the same form as
  -- signatory signing. This is wrong.
  --
  -- Instead we want to change it to the following schema:
  --
  -- Any operation done on SignatoryLink can be done under one of circumstances:
  --
  -- . signatorylinkid is same as requested and magic hash matches the
  --   one stored in session
  -- . signatorylinkid is same as requested and maybesignatory is same
  --   as logged in user
  --
  -- The above requires changes to logic in all invoked procedures.

  mmagichash <- dbQuery $ GetDocumentSessionToken signatorylinkid
  magichash <- case mmagichash of
    Just m -> return m
    Nothing -> do
      -- we are author of this document probably
      doc <- guardRightM $ getDocByDocID documentid
      (authorlink :: SignatoryLink) <- guardJust $ getSigLinkFor doc signatorylinkid
      ctx <- getContext
      when (not (isAuthor authorlink)) $ do
        Log.debug $ "Signatory id " ++ show signatorylinkid ++ " is not author of document " ++ show documentid ++ " and cannot sign because there is no magichash in session"
        internalError

      when (fmap userid (ctxmaybeuser ctx) /= maybesignatory authorlink) $ do
        Log.debug $ "Signatory user id " ++ show (maybesignatory authorlink)
                 ++ " is not same as logged in user id " ++ show (fmap userid (ctxmaybeuser ctx))
        internalError

      return (signatorymagichash authorlink)

  fieldsJSON <- guardRightM $ liftM (runGetJSON readJSArray) $ getField' "fields"
  screenshots <- readScreenshots
  fields <- guardJustM $ withJSValue fieldsJSON $ fromJSValueCustomMany $ do
      ft <- fromJSValueM
      value <- fromJSValueField "value"
      return $ pairMaybe ft value
  mprovider <- readField "eleg"
  edoc <- case mprovider of
           Nothing -> Right <$> signDocumentWithEmailOrPad documentid signatorylinkid magichash fields screenshots
           Just provider -> do
             transactionid <- getDataFnM $ look "transactionid"
             esigninfo <- case provider of
               MobileBankIDProvider -> do
                 BankID.verifySignatureAndGetSignInfoMobile documentid signatorylinkid magichash fields transactionid
               _ -> do
                 signature <- getDataFnM $ look "signature"
                 BankID.verifySignatureAndGetSignInfo documentid signatorylinkid magichash fields provider signature transactionid
             case esigninfo of
               BankID.Problem msg -> return $ Left msg
               BankID.Mismatch msg sfn sln spn -> do
                 document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid magichash
                 handleMismatch document signatorylinkid msg sfn sln spn
                 return $ Left msg
               BankID.Sign sinfo -> Right <$> signDocumentWithEleg documentid signatorylinkid magichash fields sinfo screenshots
  case edoc of
    Right (Right (doc, olddoc)) -> do
      postDocumentPendingChange doc olddoc "web"
      udoc <- guardJustM $ dbQuery $ GetDocumentByDocumentID documentid
      handleAfterSigning udoc signatorylinkid
      siglink <- guardJust $ getSigLinkFor doc signatorylinkid
      case (signatorylinksignredirecturl siglink) of
           Nothing -> return LoopBack
           Just "" -> return LoopBack
           Just s  -> return $ LinkExternal s
    Right (Left (DBActionNotAvailable message)) -> do
      -- The DBActionNotAvailable messages are not intended for end users
      --addFlash (OperationFailed, message)
      Log.debug $ "When signing document: " ++ message
      return LoopBack
    Right (Left (DBDatabaseNotAvailable message)) -> do
      addFlash (OperationFailed, message)
      return LoopBack
    Left msg -> do
      addFlash  (OperationFailed, msg)
      return LoopBack
    _ -> internalError

handleMismatch :: Kontrakcja m => Document -> SignatoryLinkID -> String -> String -> String -> String -> m ()
handleMismatch doc sid msg sfn sln spn = do
        ctx <- getContext
        let Just sl = getSigLinkFor doc sid
        Log.eleg $ "Information from eleg did not match information stored for signatory in document." ++ show msg
        Just newdoc <- runMaybeT $ do
          True <- dbUpdate $ CancelDocument (documentid doc) (ELegDataMismatch msg sid sfn sln spn)
           (signatoryActor (ctxtime ctx)
           (ctxipnumber ctx)
           (maybesignatory sl)
           (getEmail sl)
           sid)
          Just newdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
          return newdoc
        postDocumentCanceledChange newdoc "web+eleg"

{- |
    Call after signing in order to save the document for any user, and
    put up the appropriate modal.
-}
handleAfterSigning :: Kontrakcja m => Document -> SignatoryLinkID -> m ()
handleAfterSigning document@Document{documentid} signatorylinkid = do
  ctx <- getContext
  signatorylink <- guardJust $ getSigLinkFor document signatorylinkid
  maybeuser <- dbQuery $ GetUserByEmail (Email $ getEmail signatorylink)
  case maybeuser of
    Just user | isJust $ userhasacceptedtermsofservice user-> do
      let actor = signatoryActor (ctxtime ctx) (ctxipnumber ctx)  (maybesignatory signatorylink) (getEmail signatorylink) (signatorylinkid)
      _ <- dbUpdate $ SaveDocumentForUser documentid user signatorylinkid actor
      return ()
    _ -> return ()


rejectDocumentIphoneCase :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
rejectDocumentIphoneCase did sid _ = rejectDocument did sid

{- |
   Control rejecting the document
   URL: /s/{docid}/{signatorylinkid1}
 -}
rejectDocument :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
rejectDocument documentid siglinkid = do
  magichash <- guardJustM $ dbQuery $ GetDocumentSessionToken siglinkid
  customtext <- getCustomTextField "customtext"

  edocs <- rejectDocumentWithChecks documentid siglinkid magichash customtext

  case edocs of
    Left (DBActionNotAvailable message) -> do
      addFlash (OperationFailed, message)
      getHomeOrDesignViewLink
    Left (DBDatabaseNotAvailable message) -> do
      addFlash (OperationFailed, message)
      getHomeOrDesignViewLink
    Left _ -> internalError
    Right document -> do
      postDocumentRejectedChange document siglinkid "web"
      addFlashM $ modalRejectedView document
      return $ LoopBack

-- |
-- Show the document to be signed.
--
-- We put links of the form:
--
--   /s/[documentid]/[signatorylinkid]/[magichash]
--
-- in emails. The magichash should be stored in session, redirect
-- should happen immediatelly, every following action should use
-- magichash stored.
--
-- Note: JavaScript should never be allowed to see magichash in any
-- form. Therefore we do immediate redirect without any content.
--
-- Warning: iPhones have this problem: they randomly disable cookies
-- in Safari so cookies cannot be stored. This breaks all session
-- related machinery. Everybody is suffering from this. For now we
-- handle this as special case, but this is not secure and should just
-- be removed. To iPhone users with disabled cookies: tell them to
-- call Apple service and enable cookies (again) on their phone.
handleSignShowSaveMagicHash :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
handleSignShowSaveMagicHash did sid mh = do
  dbUpdate $ AddDocumentSessionToken sid mh

  -- Getting some evidence
  ctx <- getContext
  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash did sid mh
  invitedlink <- guardJust $ getSigLinkFor document sid
  dbUpdate $ AddSignatoryLinkVisitedEvidence did invitedlink (signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory invitedlink) (getEmail invitedlink) sid)

  -- Redirect to propper page
  return $ LinkSignDocNoMagicHash did sid

handleSignShow :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
handleSignShow documentid
                signatorylinkid = do
  Context { ctxtime
          , ctxipnumber } <- getContext

  mmagichash <- dbQuery $ GetDocumentSessionToken signatorylinkid

  case mmagichash of
    Just magichash -> do
      document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid magichash
      invitedlink <- guardJust $ getSigLinkFor document signatorylinkid
      switchLangWhenNeeded  (Just invitedlink) document
      _ <- dbUpdate $ MarkDocumentSeen documentid signatorylinkid magichash
           (signatoryActor ctxtime ctxipnumber (maybesignatory invitedlink) (getEmail invitedlink) signatorylinkid)
      document' <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid magichash
      _ <- addSignStatLinkEvent document' invitedlink

      ctx <- getContext
      ad <- getAnalyticsData
      content <- pageDocumentSignView ctx document' invitedlink ad
      simpleHtmlResonseClrFlash content
    Nothing -> do
      -- There is not magic hash in session. It may mean that the
      -- session expired and we deleted the credentials already or it
      -- may mean that cookies are disabled. Lets try to find out if
      -- there are any cookies, if there are none we show a page how
      -- to enable cookies on iPhone that seems to be the only
      -- offender.
      cookies <- rqCookies <$> askRq
      if null cookies
         then sendRedirect LinkEnableCookies
         else internalError

{- |
   Redirect author of document to go to signview
   URL: /d/signview/{documentid}
   Method: POST
 -}
handleIssueAuthorGoToSignview :: Kontrakcja m => DocumentID -> m KontraLink
handleIssueAuthorGoToSignview docid = do
  doc <- guardRightM $ getDocByDocIDForAuthor docid
  user <- guardJustM $ ctxmaybeuser <$> getContext
  case (isAuthor <$> getMaybeSignatoryLink (doc,user)) of
    Just True -> return $ LinkSignDoc doc $ fromJust $ getMaybeSignatoryLink (doc,user)
    _ -> return LoopBack

handleEvidenceAttachment :: Kontrakcja m => DocumentID -> String -> m Response
handleEvidenceAttachment docid file = do
  doc <- guardRightM $ getDocByDocID docid
  es <- EvidenceAttachments.fetch doc
  e <- guardJust $ listToMaybe $ filter ((==(BS.fromString file)) . EvidenceAttachments.name) es
  let mimetype = fromMaybe "text/html" (EvidenceAttachments.mimetype e)
  return $ toResponseBS mimetype (EvidenceAttachments.content e)

{- |
   Handles the request to show a document to a logged in user.
   URL: /d/{documentid}
   Method: GET
 -}
handleIssueShowGet :: Kontrakcja m => DocumentID -> m (Either KontraLink (Either Response String))
handleIssueShowGet docid = checkUserTOSGet $ do
  document <- guardRightM $ getDocByDocID docid
  muser <- ctxmaybeuser <$> getContext

  let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
  when (isAuthor (document, muser) && isCanceled document && isJust mMismatchMessage) $
    addFlash (OperationFailed, fromJust mMismatchMessage)

  authorsiglink <- guardJust $ getAuthorSigLink document

  let ispreparation = documentstatus document == Preparation
      isauthor = (userid <$> muser) == maybesignatory authorsiglink
  mauthoruser <- maybe (return Nothing) (dbQuery . GetUserByIDIncludeDeleted) (maybesignatory authorsiglink)

  let isincompany = ((usercompany =<< muser) == (usercompany =<< mauthoruser))
      isauthororincompany = isauthor || isincompany
      msiglink = find (isSigLinkFor muser) $ documentsignatorylinks document
  ad <- getAnalyticsData

  ctx <- getContext
  case (ispreparation, msiglink) of
    (True,  _)                       -> do
       Right <$> pageDocumentDesign document
    (False, _) | isauthororincompany -> do
       Right <$> pageDocumentView document msiglink (isincompany)
    (False, Just siglink)            -> do
       Left  <$> (simpleHtmlResonseClrFlash =<< pageDocumentSignView ctx document siglink ad)
    _                                -> do
       Log.error $ "internalError in handleIssueShowGet for document #" ++ show (documentid document)
       internalError

{- |
   Modify a document. Typically called with the "Underteckna" or "Save" button
   If document is in preparation, we move it to pending
   If document is in AwaitingAuthor, we move it to Closed
   Otherwise, we do mzero (NOTE: this is not the correct action to take)
   User must be logged in.
   Document must exist
   User must be author
   URL: /d/{documentid}
   Method: POST
 -}
handleIssueShowPost :: Kontrakcja m => DocumentID -> m (Either KontraLink JSValue)
handleIssueShowPost docid = do
  document <- guardRightM $ getDocByDocID docid
  Context { ctxmaybeuser = muser } <- getContext
  timezone <- getDataFnM (look "timezone") >>= (runDBEnv . mkTimeZoneName)
  unless (isAuthor (document, muser)) internalError -- still need this because others can read document
  sign              <- isFieldSet "sign"
  send              <- isFieldSet "send"
  -- Behold!
  case documentstatus document of
    Preparation | sign              -> Right <$> (linkAsJSON $ handleIssueSign document timezone)
    Preparation | send              -> Right <$> (linkAsJSON $ handleIssueSend document timezone)
    _ | canAuthorSignNow document   -> Left  <$> handleIssueSignByAuthor document
    _ -> return $ Left LinkArchive
 where
     linkAsJSON :: (Kontrakcja m) => m KontraLink -> m JSValue
     linkAsJSON lg = do
         l <- lg
         runJSONGenT $ J.value "link" (show l)

readScreenshots :: Kontrakcja m => m SignatoryScreenshots.T
readScreenshots = do
    json <- either (fail . show) return =<<
            (runGetJSON readJSObject <$>
            (maybe (fail "readScreenshots: missing field \"screenshots\"") return =<< getField "screenshots"))
    maybe (fail "readScreenshots: invalid JSON") return $ withJSValue json $ do
      first   <- (decodeScreenshot =<<) `fmap` fromJSValueField "first"
      signing <- (decodeScreenshot =<<) `fmap` fromJSValueField "signing"
      return SignatoryScreenshots.empty{ SignatoryScreenshots.first = first
                                       , SignatoryScreenshots.signing = signing
                                       }
   where decodeScreenshot (time, s) = do
          (mt, i) <- RFC2397.decode $ BS.fromString s
          unless (mt `elem` ["image/jpeg", "image/png"]) Nothing
          return (fromSeconds time, Screenshot.T{ Screenshot.mimetype = BS.toString mt
                                                , Screenshot.image = Binary i
                                                })

handleIssueSign :: Kontrakcja m => Document -> TimeZoneName -> m KontraLink
handleIssueSign document timezone = do
    Log.debug "handleIssueSign"
    actor <- guardJustM $ mkAuthorActor <$> getContext
    screenshots <- readScreenshots
    mdocs <- splitUpDocument document
    case mdocs of
      Right docs -> do
        mndocs <- mapM (forIndividual actor screenshots) docs
        case partitionEithers mndocs of
          ([], [d]) -> do
            when_ (sendMailsDuringSigning d) $ do
                addFlashM $ modalSendConfirmationView d True
            return $ LinkIssueDoc (documentid d)
          ([], ds) -> do
              addFlashM $ flashMessageCSVSent $ length ds
              Log.debug (show $ map documenttype ds)
              return $ LinkArchive
          (ls, _) -> do
            Log.debug $ "handleIssueSign had lefts: " ++ intercalate ";" ls
            addFlash (OperationFailed, intercalate ";" ls)
            return $ LinkIssueDoc (documentid document)
      Left link -> return link
    where
      forIndividual :: Kontrakcja m => Actor -> SignatoryScreenshots.T -> Document -> m (Either String Document)
      forIndividual actor screenshots doc = do
        Log.debug $ "handleIssueSign for forIndividual " ++ show (documentid doc)
        mprovider <- readField "eleg"
        mndoc <- case mprovider of
                   Nothing ->  Right <$> authorSignDocument actor (documentid doc) Nothing timezone screenshots
                   Just provider -> do
                     transactionid <- getDataFnM $ look "transactionid"
                     esigninfo <- case provider of
                       MobileBankIDProvider -> do
                         BankID.verifySignatureAndGetSignInfoMobileForAuthor (documentid doc) transactionid
                       _ -> do
                         signature <- getDataFnM $ look "signature"
                         BankID.verifySignatureAndGetSignInfoForAuthor (documentid doc) provider signature transactionid
                     case esigninfo of
                       BankID.Problem msg -> return $ Left msg
                       BankID.Mismatch msg _ _ _ -> do
                         Log.debug $ "got this message: " ++ msg
                         return $ Left msg
                       BankID.Sign sinfo -> Right <$>  authorSignDocument actor (documentid doc) (Just sinfo) timezone screenshots
        case mndoc of
          Right (Right newdocument) -> do
            postDocumentPreparationChange newdocument "web"
            newdocument' <- guardJustM $ dbQuery $ GetDocumentByDocumentID (documentid newdocument)
            postDocumentPendingChange newdocument' newdocument' "web" -- We call it on same document since there was no change
            return $ Right newdocument'
          Right (Left (DBActionNotAvailable message)) -> return $ Left message
          Right (Left (DBDatabaseNotAvailable message)) -> return $ Left message
          Right (Left _) -> return $ Left "Server error. Please try again."
          Left s -> return $ Left s


handleIssueSend :: Kontrakcja m => Document -> TimeZoneName -> m KontraLink
handleIssueSend document timezone = do
    Log.debug "handleIssueSend"
    ctx <- getContext
    user  <- guardJust $ ctxmaybeuser ctx
    actor <- guardJustM $ mkAuthorActor <$> getContext
    mdocs <- splitUpDocument document
    case mdocs of
      Right docs -> do
        mndocs <- mapM (forIndividual user actor) docs
        case partitionEithers mndocs of
          ([], [d]) -> do
            when_ (sendMailsDuringSigning d) $ do
                addFlashM $ modalSendConfirmationView d False
            return $ LinkIssueDoc (documentid d)
          ([], ds) -> do
              addFlashM $ flashMessageCSVSent $ length ds
              Log.debug (show $ map documenttype ds)
              return $ LinkArchive
          (ls, _) -> do
            Log.debug $ "handleIssueSend had lefts: " ++ intercalate ";" (map show ls)
            internalError
      Left link -> return link
    where
      forIndividual user actor doc = do
        Log.debug $ "handleIssueSend for forIndividual " ++ show (documentid doc)
        mndoc <- authorSendDocument user actor (documentid doc) timezone
        Log.debug $ "Document send by author " ++ show (documentid doc)
        case mndoc of
          Right newdocument -> postDocumentPreparationChange newdocument "web"
          Left _ -> return ()
        return mndoc

{- |
    If the document has a multiple part this will pump csv values through it to create multiple docs, and then
    save the original as a template if it isn't already.  This will make sure to clean the csv data.  It just returns
    a list containing the original doc on it's own, if the doc hasn't got a multiple part.

    I feel like this is quite dangerous to do all at once, maybe need a transaction?!
-}
splitUpDocument :: Kontrakcja m => Document -> m (Either KontraLink [Document])
splitUpDocument doc = do
  case (msum (signatorylinkcsvupload <$> documentsignatorylinks doc), getCSVCustomFields doc) of
    (Just _, Left msg) -> do
      Log.debug $ "splitUpDocument: got csvupload, but getCSVCustomFields returned issues: " ++ show msg
      internalError
    (Nothing, _) -> do
      Log.debug $ "splitUpDocument called on document without csvupload, that is ok"
      return $ Right [doc]
    (Just csvupload, Right csvcustomfields) -> do
      Log.debug $ "splitUpDocument called on document with csvupload and we managed to split fields properly"
      case (cleanCSVContents (doc `allowsAuthMethod` ELegAuthentication) (length csvcustomfields) $ csvcontents csvupload) of
        (_prob:_, _) -> do
          addFlashM flashMessageInvalidCSV
          Log.debug $ "splitUpDocument: back to document"
          return $ Left $ LinkDesignDoc $ (documentid doc)
        ([], CleanCSVData{csvbody}) -> do
          actor <- guardJustM $ mkAuthorActor <$> getContext
          Right <$> splitUpDocumentWorker doc actor csvbody

splitUpDocumentWorker :: (MonadDB m, TemplatesMonad m, CryptoRNG m, MonadBase IO m)
                      => Document -> Actor -> [[String]] -> m [Document]
splitUpDocumentWorker doc actor csvbody = do
  let preparedData = map (\xs -> let pxs = xs ++ repeat "" in ((pxs!!0),(pxs!!1),(pxs!!2),(pxs!!3),(pxs!!4),(pxs!!5),(drop 6 xs))) csvbody
  dbUpdate $ DocumentFromSignatoryDataV (documentid doc) preparedData actor


handleIssueSignByAuthor :: Kontrakcja m => Document -> m KontraLink
handleIssueSignByAuthor doc = do
     mprovider <- readField "eleg"
     screenshots <- readScreenshots
     mndoc <- case mprovider of
                   Nothing ->  Right <$> authorSignDocumentFinal (documentid doc) Nothing screenshots
                   Just provider -> do
                      signature     <- getDataFnM $ look "signature"
                      transactionid <- getDataFnM $ look "transactionid"
                      esinfo <- BankID.verifySignatureAndGetSignInfoForAuthor (documentid doc) provider signature transactionid
                      case esinfo of
                        BankID.Problem msg -> return $ Left msg
                        BankID.Mismatch msg _ _ _ -> return $ Left msg
                        BankID.Sign sinfo -> Right <$>  authorSignDocumentFinal (documentid doc) (Just sinfo) screenshots

     case mndoc of
         Right (Right ndoc) -> do
             postDocumentPendingChange ndoc doc "web"
             addFlashM flashAuthorSigned
             return $ LinkIssueDoc (documentid doc)
         _ -> return LoopBack

{- We return pending message if file is still pending, else we return JSON with number of pages-}
handleFilePages :: Kontrakcja m => FileID -> m Response
handleFilePages fid = do
  checkFileAccess fid
  jpages <- maybeScheduleRendering fid

  case jpages of
    JpegPagesPending -> do
      -- Here we steal Twitter's Enhance Your Calm status code.  Out
      -- mechanism upwards the stack will know to retry to ask us
      -- again before giving up.
      rsp <- simpleJsonResponse $ runJSONGen $ J.value "wait" "Rendering in progress"
      return (rsp { rsCode = 420 })
    JpegPagesError _ -> do
      rsp <- simpleJsonResponse $ runJSONGen $ J.value "error" "Rendering failed"
      return (rsp { rsCode = 500 })
    JpegPages pages  -> do
      simpleJsonResponse $ runJSONGen $ J.objects "pages" $ for pages $ \(_,width,height) -> do
        J.value "width"  width
        J.value "height" height

{- |
   Get some html to display the images of the files
   URL: /pages/{fileid}
   Method: GET
   FIXME: Should probably check for permissions to view
 -}
showPage :: Kontrakcja m => FileID -> Int -> m Response
showPage fileid pageno = do
  checkFileAccess fileid
  showPage' fileid pageno

-- | Preview when authorized user is logged in (without magic hash)
showPreview:: Kontrakcja m => DocumentID -> FileID -> m (Either KontraLink Response)
showPreview docid fileid = do
   _ <- guardRightM $ getDocByDocID docid
   if (fileid == (unsafeFileID 0))
    then do
        emptyPreview <- liftIO $ BS.readFile "public/img/empty-preview.jpg"
        let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [emptyPreview]) Nothing
        return $ Right $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
    else do
        checkFileAccessWith fileid Nothing Nothing (Just docid) Nothing
        iprev <- preview fileid 0
        case iprev of
          Just res -> return $ Right res
          Nothing ->   return $ Left $ LinkDocumentPreview docid Nothing fileid

-- | Preview from mail client with magic hash
showPreviewForSignatory:: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> FileID -> m (Either KontraLink Response)
showPreviewForSignatory docid siglinkid sigmagichash fileid = do
    checkFileAccessWith fileid (Just siglinkid) (Just sigmagichash) (Just docid) Nothing
    doc <- guardJustM $ dbQuery $ GetDocumentByDocumentID docid
    iprev <- preview fileid 0
    case iprev of
         Just res -> return $ Right res
         Nothing ->   return $ Left $ LinkDocumentPreview docid (getMaybeSignatoryLink (doc,siglinkid)) fileid

preview :: Kontrakcja m => FileID -> Int -> m (Maybe Response)
preview fid value
  | value > 10 = return Nothing
  | otherwise  =   do
        Context{ctxnormalizeddocuments} <- getContext
        jpages <- MemCache.get fid ctxnormalizeddocuments
        case jpages of
            Just (JpegPages pages) -> do
                let (contents,_,_) =  pages !! 0
                scaledContent <- liftIO $ scaleForPreview contents
                let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [scaledContent]) Nothing
                return $ Just $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
            other -> do
                when_ (other == Nothing) $ maybeScheduleRendering fid
                liftIO $ threadDelay 500000
                preview fid (value+1)


showPage' :: Kontrakcja m => FileID -> Int -> m Response
showPage' fileid pageno = do
  Context{ctxnormalizeddocuments} <- getContext
  jpages <- MemCache.get fileid ctxnormalizeddocuments
  case jpages of
    Just (JpegPages pages) -> do
      let (contents,_,_) =  pages !! (pageno - 1)
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      Log.debug $ "JPEG page found and returned for file " ++ show fileid ++ " and page " ++ show pageno
      return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
    _ -> do
      Log.debug $ "JPEG page not found in cache, responding 404 for file " ++ show fileid ++ " and page " ++ show pageno
      notFound (toResponse "temporarily unavailable (document has files pending for process)")

handleRestart :: Kontrakcja m => DocumentID -> m KontraLink
handleRestart docid = withUserPost $ do
  doc <- guardRightM $ getDocByDocID docid
  doc2 <- guardRightM $ restartDocument doc
  addFlashM $ flashDocumentRestarted doc2
  return $ LinkIssueDoc (documentid doc2)

handleResend :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleResend docid signlinkid  = withUserPost $ do
  ctx <- getContext
  doc <- guardRightM $ getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid
  signlink <- guardJust $ getSigLinkFor doc signlinkid
  customMessage <- getCustomTextField "customtext"
  actor <- guardJustM $ fmap mkAuthorActor getContext
  _ <- sendReminderEmail customMessage ctx actor doc signlink
  addFlashM $ flashRemindMailSent signlink
  return (LinkIssueDoc docid)

--This only works for undelivered mails. We shoulkd check if current user is author
handleChangeSignatoryEmail :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleChangeSignatoryEmail docid slid = withUserPost $ do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Just email -> do
      edoc <- getDocByDocIDForAuthor docid
      case edoc of
        Left _ -> return LoopBack
        Right _ -> do
          muser <- dbQuery $ GetUserByEmail (Email email)
          actor <- guardJustM $ mkAuthorActor <$> getContext
          mnewdoc <- runMaybeT $ do
            True <- dbUpdate $ ChangeSignatoryEmailWhenUndelivered docid slid muser email actor
            Just newdoc <- dbQuery $ GetDocumentByDocumentID docid
            return newdoc
          case mnewdoc of
            Just newdoc -> do
              -- get (updated) siglink from updated document
              sl <- guardJust (getSigLinkFor newdoc slid)
              ctx <- getContext
              _ <- sendInvitationEmail1 ctx newdoc sl
              return $ LoopBack
            _ -> return LoopBack
    _ -> return LoopBack

checkFileAccess :: Kontrakcja m => FileID -> m ()
checkFileAccess fid = do

  -- If we have documentid then we look for logged in user and
  -- signatorylinkid and magichash (in cookie). Then we check if file is
  -- reachable as document file, document sealed file, document author
  -- attachment or document signatory attachment.
  --
  -- If we have attachmentid then we look for logged in user and see
  -- if user owns the file or file is shared in user's company.
  --
  -- URLs look like:
  -- /filepages/#fileid/This%20is%file.pdf?documentid=34134124
  -- /filepages/#fileid/This%20is%file.pdf?documentid=34134124&signatorylinkid=412413
  -- /filepages/#fileid/This%20is%file.pdf?attachmentid=34134124
  --
  -- Warning take into account when somebody has saved document into
  -- hers account but we still refer using signatorylinkid.

  msid <- readField "signatorylinkid"
  mdid <- readField "documentid"
  mattid <- readField "attachmentid"

  -- If refering to something by SignatoryLinkID check out if in the
  -- session we have a properly stored access magic hash.
  mmh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msid
  checkFileAccessWith fid msid mmh mdid mattid

checkFileAccessWith :: Kontrakcja m =>
  FileID -> Maybe SignatoryLinkID -> Maybe MagicHash -> Maybe DocumentID -> Maybe AttachmentID -> m ()
checkFileAccessWith fid msid mmh mdid mattid =
  case (msid, mmh, mdid, mattid) of
    (Just sid, Just mh, Just did,_) -> do
       doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash did sid mh
       let allfiles = maybeToList (documentfile doc) ++ maybeToList (documentsealedfile doc) ++
                      (authorattachmentfile <$> documentauthorattachments doc) ++
                      (catMaybes $ map signatoryattachmentfile $ concatMap signatoryattachments $ documentsignatorylinks doc)
       when (all (/= fid) allfiles) $
            internalError
    (_,_,Just did,_) -> do
       doc <- guardRightM $ getDocByDocID did
       let allfiles = maybeToList (documentfile doc) ++ maybeToList (documentsealedfile doc)  ++
                      (authorattachmentfile <$> documentauthorattachments doc) ++
                      (catMaybes $ map signatoryattachmentfile $ concatMap signatoryattachments $ documentsignatorylinks doc)
       when (all (/= fid) allfiles) $
            internalError
    (_,_,_,Just attid) -> do
       ctx <- getContext
       case ctxmaybeuser ctx of
         Nothing -> internalError
         Just user -> do
           atts <- dbQuery $ GetAttachments [ AttachmentsSharedInUsersCompany (userid user)
                                            , AttachmentsOfAuthorDeleteValue (userid user) True
                                            , AttachmentsOfAuthorDeleteValue (userid user) False
                                            ]
                                            [ AttachmentFilterByID [attid]
                                            , AttachmentFilterByFileID [fid]
                                            ]
                                            []
                                            (0,1)
           when (length atts /= 1) $
                internalError
    _ -> internalError

-- | This handler downloads a file by file id. As specified in
-- handlePageOfDocument rules of access need to be obeyd. This handler
-- download file as is. 
handleDownloadFile :: Kontrakcja m => FileID -> String -> m Response
handleDownloadFile fid _nameForBrowser = do
  checkFileAccess fid
  content <- getFileIDContents fid
  respondWithPDF content
  where
    respondWithPDF contents = do
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
          res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
      return res2

handleDeleteSigAttach :: Kontrakcja m => DocumentID -> SignatoryLinkID ->  m KontraLink
handleDeleteSigAttach docid siglinkid = do
  mh <- guardJustM $ dbQuery $ GetDocumentSessionToken siglinkid
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  siglink <- guardJust $ getSigLinkFor doc siglinkid
  fid <- read <$> getCriticalField asValidID "deletesigattachment"
  Context{ctxtime, ctxipnumber} <- getContext
  let email = getEmail siglink
  Log.debug $ "delete Sig attachment " ++ (show fid) ++ "  " ++ email
  _ <- dbUpdate $ DeleteSigAttachment docid siglinkid fid
       (signatoryActor ctxtime ctxipnumber (maybesignatory siglink) email siglinkid)
  return $ LinkSignDoc doc siglink

handleSigAttach :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleSigAttach docid siglinkid = do
  mh <- guardJustM $ dbQuery $ GetDocumentSessionToken siglinkid
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  siglink <- guardJust $ getSigLinkFor doc siglinkid
  attachname <- getCriticalField asValidFieldValue "attachname"
  let email = getEmail siglink
  _ <- guardJust $  find (\sa -> signatoryattachmentname sa == attachname) (signatoryattachments siglink)
  (Input contentspec _ _) <- getDataFnM (lookInput "sigattach")
  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  content <- guardRightM $ liftIO $ preCheckPDF (concatChunks content1)
  file <- dbUpdate $ NewFile attachname content
  let actor = signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory siglink) email siglinkid
  d <- guardJustM . runMaybeT $ do
    True <- dbUpdate $ SaveSigAttachment docid siglinkid attachname (fileid file) actor
    Just newdoc <- dbQuery $ GetDocumentByDocumentID docid
    return newdoc
  return $ LinkSignDoc d siglink

prepareEmailPreview :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
prepareEmailPreview docid slid = do
    mailtype <- getField' "mailtype"
    ctx <- getContext
    content <- flip E.catch (\(E.SomeException _) -> return "") $ case mailtype of
         "remind" -> do
             Right doc <- getDocByDocID docid
             Just sl <- return $ getSigLinkFor doc slid
             mailDocumentRemindContent  Nothing ctx doc sl
         "reject" -> do
             Just mh <- dbQuery $ GetDocumentSessionToken slid
             Just doc <- dbQuery $ GetDocumentByDocumentID docid
             Just sl <- return $ getSigLinkFor doc (slid,mh)
             mailDocumentRejectedContent Nothing ctx  doc sl
         "invite" -> do
             Right doc <- getDocByDocID docid
             mailInvitationContent False ctx Sign doc Nothing
         _ -> fail "prepareEmailPreview"
    runJSONGenT $ J.value "content" content

handleSetAttachments :: Kontrakcja m => DocumentID -> m JSValue
handleSetAttachments did = do
    doc <- guardRightM $ getDocByDocIDForAuthor did
    attachments <- getAttachments 0
    Log.debug $ "Setting attachments to " ++ show attachments
    actor <- guardJustM $ mkAuthorActor <$> getContext
    forM_ (documentauthorattachments doc) $ \att -> dbUpdate $ RemoveDocumentAttachment did (authorattachmentfile att) actor
    forM_ (nub attachments) $ \att -> dbUpdate $ AddDocumentAttachment did att actor -- usage of nub is ok, as we never expect this list to be big
    runJSONGenT $ return ()
   where
        getAttachments :: Kontrakcja m => Int -> m [FileID]
        getAttachments i = do
            mf <- tryGetFile i
            case mf of
                 Just f -> (f:) <$> getAttachments (i+1)
                 Nothing -> return []
        tryGetFile ::  Kontrakcja m => Int -> m (Maybe FileID)
        tryGetFile i = do
            inp <- getDataFn' (lookInput $ "attachment_" ++ show i)
            case inp of
                 Just (Input (Left filepath) (Just filename) _contentType) -> do
                     content <- liftIO $ BSL.readFile filepath
                     file <- dbUpdate $ NewFile filename (Binary $ BS.concat (BSL.toChunks content))
                     return (Just (fileid file))
                 Just (Input  (Right c)  _ _)  -> do
                      case maybeRead (BSL.toString c) of
                          Just fid -> (fmap fileid) <$> (dbQuery $ GetFileByFileID fid)
                          Nothing -> return $ Nothing
                 _ -> return Nothing

handleParseCSV :: Kontrakcja m => m JSValue
handleParseCSV = do
  ctx <- getContext
  _ <- guardJust $ ctxmaybeuser ctx
  Log.debug "Csv parsing"
  customfieldscount <- guardJustM $ maybeReadIntM $ getField "customfieldscount"
  input <- getDataFn' (lookInput "csv")
  eleg <- isFieldSet "eleg"
  res <- case input of
        Just(Input contentspec (Just filename) _ ) -> do
          content <- case contentspec of
                       Left filepath -> liftIO $ BSL.readFile filepath
                       Right content -> return content
          let _title = BS.fromString (takeBaseName filename)
          case parseCSV content of
                 Left _ -> oneProblemJSON $ renderTemplate_ "flashMessageFailedToParseCSV"
                 Right contents
                   | length contents > 1000 -> oneProblemJSON $ renderTemplate "flashMessageCSVHasTooManyRows" $ F.value "maxrows" (1000::Int)
                   | otherwise -> do
                       let (problems, csvdata) = cleanCSVContents eleg customfieldscount contents
                       runJSONGenT $ do
                         J.objects "problems" $ for problems $ \p -> do
                           J.valueM "description" $ csvProblemToDescription p
                           when (isJust $ problemRow p) $
                             J.value "row" $ fromJust $ problemRow p
                           when (isJust $ problemCell p) $
                             J.value "cell" $ fromJust $ problemCell p
                         J.value "rows" $ csvbody csvdata

        _ -> do
            oneProblemJSON $ renderTemplate_ "flashMessageFailedToParseCSV"
  return res
  where
      oneProblemJSON :: Kontrakcja m => m String -> m JSValue
      oneProblemJSON desc = runJSONGenT $ do
        J.object "problems" $ do
          J.valueM "description" desc
        J.value "rows" ([]::[String])

-- | Switch to document language. Checks first if there is not logged in user. If so it will switch only if this is a different signatory.
switchLangWhenNeeded :: (Kontrakcja m) => Maybe SignatoryLink -> Document -> m ()
switchLangWhenNeeded mslid doc = do
  cu <- ctxmaybeuser <$> getContext
  when (isNothing cu || ((isJust mslid) && not (isSigLinkFor cu mslid))) $ switchLang (getLang doc)
-- GuardTime verification page. This can't be external since its a page in our system.

handleShowVerificationPage :: Kontrakcja m =>  m String
handleShowVerificationPage = gtVerificationPage

handleVerify :: Kontrakcja m => m JSValue
handleVerify = do
      fileinput <- getDataFn' (lookInput "file")
      filepath <- case fileinput of
            Just (Input (Left filepath) _ _) -> return filepath
            Just (Input (Right content) _ _) -> liftIO $ do
                    systmp <- getTemporaryDirectory
                    (pth,handle) <- openTempFile systmp ("vpath.pdf")
                    BSL.hPutStr handle content
                    return pth
            _ -> internalError
      liftIO $ toJSValue <$> GuardTime.verify filepath

handleMarkAsSaved :: Kontrakcja m => DocumentID -> m JSValue
handleMarkAsSaved docid = do
  doc <- guardRightM $ getDocByDocID docid
  when_ (isPreparation doc) $ dbUpdate $ SetDocumentUnsavedDraft [documentid doc] False
  runJSONGenT $ return ()
      
