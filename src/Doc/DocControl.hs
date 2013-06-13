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
    , handleSignShow
    , handleSignShowSaveMagicHash
    , handleAcceptAccountFromSign
    , handleSigAttach
    , handleDeleteSigAttach
    , handleEvidenceAttachment
    , handleIssueShowGet
    , handleIssueAuthorGoToSignview
    , handleSetAttachments
    , handleParseCSV
    , prepareEmailPreview
    , handleResend
    , handleChangeSignatoryEmail
    , handleChangeSignatoryPhone
    , handleRestart
    , handleProlong
    , showPage
    , showPreview
    , showPreviewForSignatory
    , handleFilePages
    , handleShowVerificationPage
    , handleVerify
    , handleMarkAsSaved
    , handleAfterSigning
) where

import AppView
import Attachment.AttachmentID (AttachmentID)
import DB
import Doc.Action
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
import Doc.Tokens.Model
import Attachment.Model
import InputValidation
import File.Model
import Kontra
import KontraLink
import MagicHash
import Happstack.Fields
import Utils.Monad
import Utils.Prelude
import Utils.Read
import Utils.String
import Redirect
import User.Model
import Util.HasSomeUserInfo
import qualified Log
import Text.StringTemplates.Templates
import Util.CSVUtil
import Util.FlashUtil
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Doc.API.Callback.Model (triggerAPICallbackIfThereIsOne)
import Util.MonadUtils
import User.Utils
import Control.Applicative
import Control.Concurrent
import qualified Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Happstack.Server.Types
import Happstack.Server hiding (simpleHTTP)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length, take)
import qualified Data.Map as Map
import System.FilePath
import Text.JSON hiding (Result)
import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J
import Doc.DocDraft() -- Import instances only
import qualified User.Action
import Util.Actor
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
        Just doc <- dbUpdate $ NewDocument user (replace "  " " " $ title ++ " " ++ formatMinutesTimeSimple (ctxtime ctx)) (Signable Contract) 0 actor
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
  signatorylink <- guardJust $ getSigLinkFor document signatorylinkid
  muser <- User.Action.handleAccountSetupFromSign document signatorylink
  case muser of
    Just user -> runJSONGenT $ do
      J.value "userid" (show $ userid user)
    Nothing -> runJSONGenT $ do
      return ()

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

  let mMismatchMessage = msum (map signatorylinkelegdatamismatchmessage (documentsignatorylinks document))
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

handleProlong :: Kontrakcja m => DocumentID -> m KontraLink
handleProlong docid = withUserPost $ do
  doc <- guardRightM $ getDocByDocID docid
  guardRightM $ prolongDocument doc
  triggerAPICallbackIfThereIsOne doc
  addFlashM $ flashDocumentProlonged doc
  return $ LinkIssueDoc (documentid doc)

handleResend :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleResend docid signlinkid  = withUserPost $ do
  ctx <- getContext
  doc <- guardRightM $ getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid
  signlink <- guardJust $ getSigLinkFor doc signlinkid
  customMessage <- getOptionalField  asValidInviteText "customtext"
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
          dbUpdate $ ChangeSignatoryEmailWhenUndelivered docid slid muser email actor
          mnewdoc <- dbQuery $ GetDocumentByDocumentID docid

          case mnewdoc of
            Just newdoc -> do
              -- get (updated) siglink from updated document
              sl <- guardJust (getSigLinkFor newdoc slid)
              ctx <- getContext
              _ <- sendInvitationEmail1 ctx newdoc sl
              return $ LoopBack
            _ -> return LoopBack
    _ -> return LoopBack

--This only works for undelivered mails. We shoulkd check if current user is author
handleChangeSignatoryPhone :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleChangeSignatoryPhone docid slid = withUserPost $ do
  mphone <- getOptionalField asValidPhone "phone"
  case mphone of
    Just phone -> do
      edoc <- getDocByDocIDForAuthor docid
      case edoc of
        Left _ -> return LoopBack
        Right _ -> do
          actor <- guardJustM $ mkAuthorActor <$> getContext
          dbUpdate $ ChangeSignatoryPhoneWhenUndelivered docid slid phone actor
          mnewdoc <- dbQuery $ GetDocumentByDocumentID docid

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
       when (not $ fileInDocument doc fid) $ internalError
    (_,_,Just did,_) -> do
       doc <- guardRightM $ getDocByDocID did
       when (not $ fileInDocument doc fid) $ internalError
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
  dbUpdate $ SaveSigAttachment docid siglinkid attachname (fileid file) actor
  newdoc <- guardJustM $ dbQuery $ GetDocumentByDocumentID docid
  return $ LinkSignDoc newdoc siglink

prepareEmailPreview :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
prepareEmailPreview docid slid = do
    mailtype <- getField' "mailtype"
    ctx <- getContext
    content <- flip E.catch (\(E.SomeException _) -> return "") $ case mailtype of
         "remind" -> do
             Right doc <- getDocByDocID docid
             Just sl <- return $ getSigLinkFor doc slid
             mailDocumentRemindContent  Nothing ctx doc sl True
         "reject" -> do
             Just mh <- dbQuery $ GetDocumentSessionToken slid
             Just doc <- dbQuery $ GetDocumentByDocumentID docid
             Just sl <- return $ getSigLinkFor doc (slid,mh)
             x :: String <- mailDocumentRejectedContent Nothing ctx  doc sl True
             return x
         "invite" -> do
             Right doc <- getDocByDocID docid
             mailInvitationContent False ctx Sign doc Nothing True
         _ -> fail "prepareEmailPreview"
    runJSONGenT $ J.value "content" content

handleSetAttachments :: Kontrakcja m => DocumentID -> m JSValue
handleSetAttachments did = do
    doc <- guardRightM $ getDocByDocIDForAuthor did
    attachments <- getAttachments doc 0
    Log.debug $ "Setting attachments to " ++ show attachments
    actor <- guardJustM $ mkAuthorActor <$> getContext
    forM_ (documentauthorattachments doc) $ \att -> dbUpdate $ RemoveDocumentAttachment did (authorattachmentfile att) actor
    forM_ (nub attachments) $ \att -> dbUpdate $ AddDocumentAttachment did att actor -- usage of nub is ok, as we never expect this list to be big
    runJSONGenT $ return ()
   where
        getAttachments :: Kontrakcja m => Document -> Int -> m [FileID]
        getAttachments doc i = do
            mf <- tryGetFile doc i
            case mf of
                 Just f -> (f:) <$> getAttachments doc (i+1)
                 Nothing -> return []
        tryGetFile ::  Kontrakcja m => Document -> Int -> m (Maybe FileID)
        tryGetFile doc i = do
            inp <- getDataFn' (lookInput $ "attachment_" ++ show i)
            case inp of
                 Just (Input (Left filepath) (Just filename) _contentType) -> do
                     content <- liftIO $ BSL.readFile filepath
                     cres <- liftIO $ preCheckPDF (concatChunks content)
                     case cres of
                       Left _ -> do
                         Log.debug $ "Document #" ++ show did ++ ". File for attachment " ++ show filepath ++ " is broken PDF. Skipping."
                         internalError
                       Right content' -> do
                         file <- dbUpdate $ NewFile filename content'
                         return (Just (fileid file))
                 Just (Input  (Right c)  _ _)  -> do
                      case maybeRead (BSL.toString c) of
                          Just fid -> do
                            access <- hasAccess doc fid
                            if access
                              then (fmap fileid) <$> (dbQuery $ GetFileByFileID fid)
                              else internalError
                          Nothing -> internalError
                 _ -> return Nothing
        hasAccess ::  Kontrakcja m => Document -> FileID -> m Bool
        hasAccess doc fid = do
          user <- fromJust <$> ctxmaybeuser <$> getContext
          if (fid `elem` (authorattachmentfile <$> documentauthorattachments doc))
           then return True
           else do
            atts <- dbQuery $ GetAttachments [  AttachmentsSharedInUsersCompany (userid user)
                                              , AttachmentsOfAuthorDeleteValue (userid user) True
                                              , AttachmentsOfAuthorDeleteValue (userid user) False
                                             ]
                                            [ AttachmentFilterByFileID [fid]]
                                            []
                                            (0,1)
            return $ not $ null atts



handleParseCSV :: Kontrakcja m => m JSValue
handleParseCSV = do
  ctx <- getContext
  _ <- guardJust $ ctxmaybeuser ctx
  input <- getDataFn' (lookInput "csv")
  res <- case input of
        Just(Input contentspec (Just filename) _ ) -> do
          content <- case contentspec of
                       Left filepath -> liftIO $ BSL.readFile filepath
                       Right content -> return content
          let _title = BS.fromString (takeBaseName filename)
          case parseCSV content of
                 Right (h:r) -> runJSONGenT $ do
                         J.value "header" $ h
                         J.value "rows" $ r
                 _ -> runJSONGenT $ J.value "parseError" True
        _ -> runJSONGenT $ J.value "parseError" True
  return res

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
      ctx <- getContext
      liftIO $ toJSValue <$> GuardTime.verify (ctxgtconf ctx) filepath

handleMarkAsSaved :: Kontrakcja m => DocumentID -> m JSValue
handleMarkAsSaved docid = do
  doc <- guardRightM $ getDocByDocID docid
  when_ (isPreparation doc) $ dbUpdate $ SetDocumentUnsavedDraft [documentid doc] False
  runJSONGenT $ return ()

