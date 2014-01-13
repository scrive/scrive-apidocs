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
    , handleEvidenceAttachment
    , handleIssueShowGet
    , handleIssueAuthorGoToSignview
    , prepareEmailPreview
    , handleResend
    , handleChangeSignatoryEmail
    , handleChangeSignatoryPhone
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
import Doc.DocMails
import Doc.Model
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentMonad (DocumentMonad, withDocumentM, withDocument, theDocument, theDocumentID)
import Doc.Rendering
import Doc.DocView
import Doc.DocViewMail
import Doc.SignatoryLinkID
import Doc.DocumentID
import Doc.JpegPages
import qualified Doc.EvidenceAttachments as EvidenceAttachments
import Doc.Tokens.Model
import Attachment.Model
import InputValidation
import Instances ()
import File.Model
import Kontra
import KontraLink
import MagicHash
import Happstack.Fields
import Utils.Monad
import Redirect
import User.Model
import Util.HasSomeUserInfo
import qualified Log
import Text.StringTemplates.Templates
import Util.FlashUtil
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Util.MonadUtils
import User.Utils
import Control.Applicative
import Control.Concurrent
import Control.Conditional (unlessM, whenM)
import qualified Control.Exception.Lifted as E
import Control.Logic ((||^))
import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Happstack.Server.Types
import Happstack.Server hiding (simpleHTTP)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length, take)
import qualified Data.Map as Map
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
import Codec.Compression.Zlib

handleNewDocument :: Kontrakcja m => m KontraLink
handleNewDocument = do
  ctx <- getContext
  if (isJust $ ctxmaybeuser ctx)
     then withUserPost $ do
        user <- guardJustM $ ctxmaybeuser <$> getContext
        title <- renderTemplate_ "newDocumentTitle"
        actor <- guardJustM $ mkAuthorActor <$> getContext
        Just doc <- dbUpdate $ NewDocument user (replace "  " " " $ title ++ " " ++ formatMinutesTimeSimple (ctxtime ctx)) Signable 0 actor
        withDocument doc $ dbUpdate $ SetDocumentUnsavedDraft True
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
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash documentid signatorylinkid magichash) `withDocumentM` do
    signatorylink <- guardJust . getSigLinkFor signatorylinkid =<< theDocument
    muser <- User.Action.handleAccountSetupFromSign signatorylink
    case muser of
      Just user -> runJSONGenT $ do
        J.value "userid" (show $ userid user)
      Nothing -> runJSONGenT $ do
        return ()

{- |
    Call after signing in order to save the document for any user, and
    put up the appropriate modal.
-}
handleAfterSigning :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m ()
handleAfterSigning signatorylinkid = do
  signatorylink <- guardJust . getSigLinkFor signatorylinkid =<< theDocument
  maybeuser <- dbQuery $ GetUserByEmail (Email $ getEmail signatorylink)
  case maybeuser of
    Just user | isJust $ userhasacceptedtermsofservice user-> do
      _ <- dbUpdate $ SaveDocumentForUser user signatorylinkid
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
  -- Getting some evidence
  ctx <- getContext
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh) `withDocumentM` do
    dbUpdate $ AddDocumentSessionToken sid mh

    invitedlink <- guardJust . getSigLinkFor sid =<< theDocument
    dbUpdate . AddSignatoryLinkVisitedEvidence (signatoryActor ctx invitedlink) =<< theDocumentID

    -- Redirect to propper page
    return $ LinkSignDocNoMagicHash did sid

handleSignShow :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
handleSignShow documentid
                signatorylinkid = do
  ctx <- getContext

  mmagichash <- dbQuery $ GetDocumentSessionToken signatorylinkid

  case mmagichash of
    Just magichash -> 
      dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash documentid signatorylinkid magichash) `withDocumentM` do
        invitedlink <- guardJust . getSigLinkFor signatorylinkid =<< theDocument
        switchLangWhenNeeded  (Just invitedlink) =<< theDocument
        unlessM ((isTemplate ||^ isPreparation ||^ isClosed) <$> theDocument) $
          dbUpdate $ MarkDocumentSeen signatorylinkid magichash
             (signatoryActor ctx invitedlink)

        ad <- getAnalyticsData
        content <- theDocument >>= \d -> pageDocumentSignView ctx d invitedlink ad
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
  guardLoggedIn
  doc <- getDocByDocIDForAuthor docid
  user <- guardJustM $ ctxmaybeuser <$> getContext
  case (isAuthor <$> getMaybeSignatoryLink (doc,user)) of
    Just True -> return $ LinkSignDoc doc $ fromJust $ getMaybeSignatoryLink (doc,user)
    _ -> return LoopBack

handleEvidenceAttachment :: Kontrakcja m => DocumentID -> String -> m Response
handleEvidenceAttachment docid file = do
  guardLoggedIn
  doc <- getDocByDocID docid
  es <- EvidenceAttachments.fetch doc
  e <- guardJust $ listToMaybe $ filter ((==(BS.fromString file)) . EvidenceAttachments.name) es
  let mimetype = fromMaybe "text/html" (EvidenceAttachments.mimetype e)
  -- Evidence attachments embedded in PDFs are compressed using RFC #1950. This is NOT the same
  -- as what browsers are supporting (under names gzip and deflate), and we need to decompress server side.
  return $ (toResponseBS mimetype (decompress $ EvidenceAttachments.content e))

{- |
   Handles the request to show a document to a logged in user.
   URL: /d/{documentid}
   Method: GET
 -}
handleIssueShowGet :: Kontrakcja m => DocumentID -> m (Either KontraLink (Either Response String))
handleIssueShowGet docid = checkUserTOSGet $ do
  document <- getDocByDocID docid
  muser <- ctxmaybeuser <$> getContext

  let mMismatchMessage = msum (map signatorylinkelegdatamismatchmessage (documentsignatorylinks document))
  when (isAuthor (document, muser) && isCanceled document && isJust mMismatchMessage) $
    addFlash (OperationFailed, fromJust mMismatchMessage)

  authorsiglink <- guardJust $ getAuthorSigLink document

  let ispreparation = documentstatus document == Preparation
      isauthor = (userid <$> muser) == maybesignatory authorsiglink
  mauthoruser <- maybe (return Nothing) (dbQuery . GetUserByIDIncludeDeleted) (maybesignatory authorsiglink)

  let isincompany = isJust muser && ((usercompany <$> muser) == (usercompany <$> mauthoruser))
      isauthororincompany = isauthor || isincompany
      msiglink = find (isSigLinkFor muser) $ documentsignatorylinks document
  ad <- getAnalyticsData

  ctx <- getContext
  case (ispreparation, msiglink) of
    (True,  _)                       -> do
       Left <$> (simpleHtmlResonseClrFlash =<< pageDocumentDesign ctx document ad)
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
      -- This communicates to JavaScript how many pages there
      -- are. This should be totally changed to the following
      -- mechanism:
      --
      -- 1. JavaScript should ask for the first page
      -- 2. Server should serve first page
      -- 3. JavaScript should ask for next page
      -- 4. Server should respond with Enhance Your Calm (or just wait a little before returning response).
      -- 5. JavaScript should retry if Enhance Your Calm is the code
      -- 6. If the page returned 200 with content, then proceed with next page
      -- 7. If server returned 404 it means there are no more pages
      -- 8. JavaScript should fire 'full document loaded' event (and place fields on all pages).
      --
      -- Such architecture would allow incremental rendering of pages
      -- on the server side also, thus improving user experience a
      -- lot.
      simpleJsonResponse $ runJSONGen $ J.value "pages" $ length pages

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
   guardLoggedIn
   _ <- getDocByDocID docid
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
    doc <- dbQuery $ GetDocumentByDocumentID docid
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
                let contents =  pages !! 0
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
      let contents = pages !! (pageno - 1)
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      Log.debug $ "JPEG page found and returned for file " ++ show fileid ++ " and page " ++ show pageno
      return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
    _ -> do
      Log.debug $ "JPEG page not found in cache, responding 404 for file " ++ show fileid ++ " and page " ++ show pageno
      notFound (toResponse "temporarily unavailable (document has files pending for process)")

handleResend :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleResend docid signlinkid  = withUserPost $
  getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid `withDocumentM` do
  signlink <- guardJust . getSigLinkFor signlinkid =<< theDocument
  customMessage <- getOptionalField  asValidInviteText "customtext"
  actor <- guardJustM $ fmap mkAuthorActor getContext
  _ <- sendReminderEmail customMessage actor False signlink
  return (LinkIssueDoc docid)

--This only works for undelivered mails. We shoulkd check if current user is author
handleChangeSignatoryEmail :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleChangeSignatoryEmail docid slid = withUserPost $ do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Just email -> getDocByDocIDForAuthor docid `withDocumentM` do
      muser <- dbQuery $ GetUserByEmail (Email email)
      actor <- guardJustM $ mkAuthorActor <$> getContext
      dbUpdate $ ChangeSignatoryEmailWhenUndelivered slid muser email actor
      sl <- guardJust . getSigLinkFor slid =<< theDocument
      _ <- sendInvitationEmail1 sl
      return $ LoopBack
    _ -> return LoopBack

--This only works for undelivered mails. We shoulkd check if current user is author
handleChangeSignatoryPhone :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleChangeSignatoryPhone docid slid = withUserPost $ do
  mphone <- getOptionalField asValidPhone "phone"
  case mphone of
    Just phone -> getDocByDocIDForAuthor docid `withDocumentM` do
      actor <- guardJustM $ mkAuthorActor <$> getContext
      dbUpdate $ ChangeSignatoryPhoneWhenUndelivered slid phone actor
      -- get (updated) siglink from updated document
      sl <- guardJust . getSigLinkFor slid =<< theDocument
      _ <- sendInvitationEmail1 sl
      return $ LoopBack
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
       _doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh
       indoc <- dbQuery $ FileInDocument did fid
       when (not indoc) $ internalError
    (_,_,Just did,_) -> do
       guardLoggedIn
       _doc <- getDocByDocID did
       indoc <- dbQuery $ FileInDocument did fid
       when (not indoc) $ internalError
    (_,_,_,Just attid) -> do
       guardLoggedIn
       user <- guardJustM $ ctxmaybeuser <$> getContext
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

prepareEmailPreview :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
prepareEmailPreview docid slid = do
    mailtype <- getField' "mailtype"
    content <- flip E.catch (\(E.SomeException _) -> return "") $ case mailtype of
         "remind" -> do
             doc <- getDocByDocID docid
             Just sl <- return $ getSigLinkFor slid doc
             mailDocumentRemindContent  Nothing doc sl True
         "reject" -> do
             Just mh <- dbQuery $ GetDocumentSessionToken slid
             doc <- dbQuery $ GetDocumentByDocumentID docid
             Just sl <- return $ getSigLinkFor (slid,mh) doc
             x :: String <- mailDocumentRejectedContent Nothing sl True doc
             return x
         "invite" -> do
             doc <- getDocByDocID docid
             mailInvitationContent False Sign Nothing True doc
         _ -> fail "prepareEmailPreview"
    runJSONGenT $ J.value "content" content

-- | Switch to document language. Checks first if there is not logged in user. If so it will switch only if this is a different signatory.
switchLangWhenNeeded :: (Kontrakcja m) => Maybe SignatoryLink -> Document -> m ()
switchLangWhenNeeded mslid doc = do
  cu <- ctxmaybeuser <$> getContext
  when (isNothing cu || ((isJust mslid) && not (isSigLinkFor cu mslid))) $ switchLang (getLang doc)
-- GuardTime verification page. This can't be external since its a page in our system.

-- withAnonymousContext so the verify page looks like the user is not logged in
-- (e.g. for default footer & header)
handleShowVerificationPage :: Kontrakcja m =>  m Response
handleShowVerificationPage = withAnonymousContext gtVerificationPage

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
  guardLoggedIn
  getDocByDocID docid `withDocumentM` do
    whenM (isPreparation <$> theDocument) $ dbUpdate $ SetDocumentUnsavedDraft False
    runJSONGenT $ return ()
