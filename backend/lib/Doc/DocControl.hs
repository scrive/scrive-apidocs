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
    , handleDownloadClosedFile
    , handleSignShow
    , handleSignShowSaveMagicHash
    , handleEvidenceAttachment
    , handleIssueShowGet
    , handleIssueGoToSignview
    , handleIssueGoToSignviewPad
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
    , handlePadList
    , handleToStart
    , handleToStartShow
) where

import Control.Conditional (unlessM, whenM)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Data.String.Utils (replace, strip)
import Happstack.Server hiding (lookCookieValue, simpleHTTP, timeout)
import Log
import System.Directory
import System.IO.Temp
import Text.JSON hiding (Result)
import Text.StringTemplates.Templates
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length, take)
import qualified Data.Traversable as T
import qualified Text.JSON.Gen as J

import Analytics.Include
import AppView
import Attachment.AttachmentID (AttachmentID)
import Attachment.Model
import BrandedDomain.BrandedDomain
import Cookies
import DB
import DB.TimeZoneName
import Doc.API.Callback.Model
import Doc.Conditions (DocumentDoesNotExist(..))
import Doc.DocInfo
import Doc.DocMails
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocumentMonad (DocumentMonad, theDocument, withDocument, withDocumentM)
import Doc.DocUtils (fileFromMainFile)
import Doc.DocView
import Doc.DocViewMail
import Doc.Logging
import Doc.Model
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import EvidenceLog.Model (CurrentEvidenceEventType(..), InsertEvidenceEventWithAffectedSignatoryAndMsg(..))
import File.File (fileid)
import File.Model
import File.Storage (getFileIDContents)
import Happstack.Fields
import InputValidation
import Kontra
import KontraLink
import KontraPrelude
import MagicHash
import Redirect
import User.Email
import User.Model
import User.Utils
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.PDFUtil
import Util.SignatoryLinkUtils
import Util.Zlib (decompressIfPossible)
import qualified Doc.EvidenceAttachments as EvidenceAttachments
import qualified GuardTime as GuardTime

handleNewDocument :: Kontrakcja m => m Response
handleNewDocument = withUser $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  title <- renderTemplate_ "newDocumentTitle"
  actor <- guardJustM $ mkAuthorActor <$> getContext
  mtimezonename <- lookCookieValue "timezone"
  case mtimezonename of
    Nothing -> logInfo_ "'timezone' cookie not found"
    _ -> return ()
  timezone <- fromMaybe defaultTimeZoneName <$> T.sequence (mkTimeZoneName <$> mtimezonename)
  timestamp <- formatTimeSimpleWithTZ timezone (ctxtime ctx)
  doc <- dbUpdate $ NewDocument user (replace "  " " " $ title ++ " " ++ timestamp) Signable timezone 1 actor
  -- Default document on the frontend has different requirements,
  -- this sets up the signatories to match those requirements.
  withDocument doc $ do
      authorsiglink <- guardJust $ find (\sl -> signatoryisauthor sl) (documentsignatorylinks doc)
      othersiglink  <- guardJust $ find (\sl -> not $ signatoryisauthor sl)  (documentsignatorylinks doc)
      let fields  = [
              SignatoryNameField $ NameField {
                  snfID                     = (unsafeSignatoryFieldID 0)
                , snfNameOrder              = NameOrder 1
                , snfValue                  = ""
                , snfObligatory             = False
                , snfShouldBeFilledBySender = False
                , snfPlacements             = []
              }
            , SignatoryNameField $ NameField {
                  snfID                     = (unsafeSignatoryFieldID 0)
                , snfNameOrder              = NameOrder 2
                , snfValue                  = ""
                , snfObligatory             = False
                , snfShouldBeFilledBySender = False
                , snfPlacements             = []
              }
            , SignatoryEmailField $ EmailField {
                  sefID                     = (unsafeSignatoryFieldID 0)
                , sefValue                  = ""
                , sefObligatory             = True
                , sefShouldBeFilledBySender = False
                , sefPlacements             = []
              }
            , SignatoryMobileField $ MobileField {
                  smfID                     = (unsafeSignatoryFieldID 0)
                , smfValue                  = ""
                , smfObligatory             = False
                , smfShouldBeFilledBySender = False
                , smfPlacements             = []
              }
            , SignatoryCompanyField $ CompanyField {
                  scfID                     = (unsafeSignatoryFieldID 0)
                , scfValue                  = ""
                , scfObligatory             = False
                , scfShouldBeFilledBySender = False
                , scfPlacements             = []
              }
            , SignatoryCompanyNumberField $ CompanyNumberField {
                  scnfID                     = (unsafeSignatoryFieldID 0)
                , scnfValue                  = ""
                , scnfObligatory             = False
                , scnfShouldBeFilledBySender = False
                , scnfPlacements             = []
              }
            ]
          othersiglink' = othersiglink { signatorysignorder = SignOrder 1
                                       , signatoryfields = fields
                                       }
      _ <- dbUpdate $ ResetSignatoryDetails [authorsiglink, othersiglink'] actor
      dbUpdate $ SetDocumentUnsavedDraft True

{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}

formatTimeSimpleWithTZ :: (MonadDB m, MonadThrow m) => TimeZoneName -> UTCTime -> m String
formatTimeSimpleWithTZ tz t = do
  runQuery_ $ rawSQL "SELECT to_char($1 AT TIME ZONE $2, 'YYYY-MM-DD HH24:MI')" (t, tz)
  fetchOne runIdentity

showCreateFromTemplate :: Kontrakcja m => m (Either KontraLink String)
showCreateFromTemplate = withUser $ pageCreateFromTemplate =<< getContext

{- |
    Call after signing in order to save the document for any user, and
    put up the appropriate modal.
-}
handleAfterSigning :: (MonadLog m, MonadThrow m, TemplatesMonad m, DocumentMonad m, MonadBase IO m) => SignatoryLinkID -> m ()
handleAfterSigning slid = logSignatory slid $ do
  signatorylink <- guardJust . getSigLinkFor slid =<< theDocument
  maybeuser <- dbQuery $ GetUserByEmail (Email $ getEmail signatorylink)
  case maybeuser of
    Just user | isJust $ userhasacceptedtermsofservice user-> do
      _ <- dbUpdate $ SaveDocumentForUser user slid
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
{-# NOINLINE handleSignShowSaveMagicHash #-}
handleSignShowSaveMagicHash :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m Response
handleSignShowSaveMagicHash did sid mh = logDocumentAndSignatory did sid $ (do
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh) `withDocumentM` do
    dbUpdate $ AddDocumentSessionToken sid mh
    -- Redirect to propper page
    sendRedirect $ LinkSignDocNoMagicHash did sid) `catchDBExtraException` (\(DocumentDoesNotExist _) -> respond404)

-- |
--   /s/[documentid]/[signatorylinkid] and /sp/[documentid]/[signatorylinkid]

{-# NOINLINE handleSignShow #-}
handleSignShow :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
handleSignShow did slid = logDocumentAndSignatory did slid $ do
  mmagichash <- dbQuery $ GetDocumentSessionToken slid
  case mmagichash of
    Just magichash ->
      dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid magichash) `withDocumentM` do
        invitedlink <- guardJust . getSigLinkFor slid =<< theDocument

        -- We always switch to document langauge in case of pad signing
        switchLang . getLang =<< theDocument
        ctx <- getContext -- Order is important since ctx after switchLang changes
        ad <- getAnalyticsData
        needsToIdentify <- signatoryNeedsToIdentifyToView invitedlink
        if (needsToIdentify)
           then do
             addEventForVisitingSigningPageIfNeeded VisitedViewForAuthenticationEvidence invitedlink
             content <- theDocument >>= \d -> pageDocumentIdentifyView ctx d invitedlink ad
             simpleHtmlResonseClrFlash content
           else do
             addEventForVisitingSigningPageIfNeeded VisitedViewForSigningEvidence invitedlink
             unlessM ((isTemplate || isPreparation || isClosed) <$> theDocument) $ do
               dbUpdate . MarkDocumentSeen slid magichash =<< signatoryActor ctx invitedlink
               triggerAPICallbackIfThereIsOne =<< theDocument
             content <- theDocument >>= \d -> pageDocumentSignView ctx d invitedlink ad
             simpleHtmlResonseClrFlash content
    Nothing -> handleCookieFail slid did

-- |
--   /ts/[documentid] (doc has to be a draft)
{-# NOINLINE handleToStartShow #-}
handleToStartShow :: Kontrakcja m => DocumentID -> m (Either KontraLink Response)
handleToStartShow documentid = checkUserTOSGet $ do
  ctx <- getContext
  document <- getDocByDocIDForAuthor documentid
  ad <- getAnalyticsData
  content <- pageDocumentToStartView ctx document ad
  simpleHtmlResonseClrFlash content


-- If is not magic hash in session. It may mean that the
-- session expired and we deleted the credentials already or it
-- may mean that cookies are disabled. Lets try to find out if
-- there are any cookies, if there are none we show a page how
-- to enable cookies on iPhone that seems to be the only
-- offender.
handleCookieFail :: Kontrakcja m => SignatoryLinkID -> DocumentID -> m Response
handleCookieFail slid did = logDocumentAndSignatory did slid $ do
  cookies <- rqCookies <$> askRq
  if null cookies
    then sendRedirect LinkEnableCookies
    else do
      logInfo "Signview load after session timedout" $ object ["cookies" .= show cookies]
      ctx <- getContext
      ad <- getAnalyticsData
      let fields = standardPageFields ctx Nothing ad
      content <- if bdMainDomain (ctxbrandeddomain ctx) || isJust (ctxmaybeuser ctx)
        then renderTemplate "sessionTimeOut" fields
        else renderTemplate "sessionTimeOutWithoutHeaders" fields
      simpleHtmlResonseClrFlash content

{- |
   Redirect author of document to go to signview
   URL: /d/signview/{documentid}
   Method: POST
 -}
handleIssueGoToSignview :: Kontrakcja m => DocumentID -> m KontraLink
handleIssueGoToSignview docid = do
  guardLoggedIn
  ctx <- getContext
  doc <- getDocByDocID docid
  user <-  guardJust (ctxmaybeuser ctx)
  case (getMaybeSignatoryLink (doc,user)) of
    Just sl -> do
      dbUpdate $ AddDocumentSessionToken (signatorylinkid sl) (signatorymagichash sl)
      return $ LinkSignDocNoMagicHash docid (signatorylinkid sl)
    _ -> return LoopBack

{- |
   Redirect author of document to go to signview for any of the pad signatories
   URL: /d/signview/{documentid}/{signatorylinkid}
   Method: POST
 -}
handleIssueGoToSignviewPad :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleIssueGoToSignviewPad docid slid= do
  ctx <- getContext
  doc <- getDocByDocIDForAuthor docid
  user <- guardJust $ getContextUser ctx
  case (isAuthor <$> getMaybeSignatoryLink (doc,user), getMaybeSignatoryLink (doc,slid)) of
    (Just True,Just sl) | signatorylinkdeliverymethod sl == PadDelivery -> do
      dbUpdate $ AddDocumentSessionToken (signatorylinkid sl) (signatorymagichash sl)
      return $ LinkSignDocPad docid slid
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
  -- decompressIfPossible returns original content if it was not possible to decompress
  -- this is needed to handle attachments from our old version of service
  -- where they were apparently not compressed at all
  return $ toResponseBS mimetype $ decompressIfPossible $ EvidenceAttachments.content e

{- |
   Handles the request to show a document to a logged in user.
   URL: /d/{documentid}
   Method: GET
 -}
handleIssueShowGet :: Kontrakcja m => DocumentID -> m (Either KontraLink (Either Response String))
handleIssueShowGet docid = checkUserTOSGet $ do
  document <- getDocByDocID docid
  muser <- ctxmaybeuser <$> getContext

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
       -- Never cache design view. IE8 hack. Should be fixed in different wasy
       Left <$> (setHeaderBS "Cache-Control" "no-cache" <$> (simpleHtmlResonseClrFlash =<< pageDocumentDesign ctx document ad))
    (False, _) | isauthororincompany -> do
       Right <$> pageDocumentView ctx document msiglink (isincompany)
    (False, Just siglink)            -> do
       Left  <$> (simpleHtmlResonseClrFlash =<< pageDocumentSignView ctx document siglink ad)
    _                                -> do
       internalError


{- We return pending message if file is still pending, else we return JSON with number of pages-}
handleFilePages :: Kontrakcja m => FileID -> m Response
handleFilePages fid = logFile fid $ do
  checkFileAccess fid
  ePagesCount <- liftIO . getNumberOfPDFPages =<< getFileIDContents fid
  case ePagesCount of
    Right pc -> simpleJsonResponse . J.runJSONGen . J.value "pages" $ pc
    _ -> do
      logAttention_ "Counting number of pages failed"
      internalError

{- |
   Get some html to display the images of the files
   URL: /pages/{fileid}
   Method: GET
 -}
showPage :: Kontrakcja m => FileID -> Int -> m Response
showPage fid pageNo = logFile fid $ do
  logInfo_ "Checking file access"
  checkFileAccess fid
  pixelwidth <- guardJustM $ readField "pixelwidth"
  fileData <- getFileIDContents fid
  rp <- renderPage fileData pageNo pixelwidth
  case rp of
   Just pageData -> return $ setHeaderBS "Cache-Control" "max-age=604800" $ toResponseBS "image/png" $ BSL.fromStrict pageData
   Nothing -> do
     logAttention "Rendering PDF page failed" $ object [ "page" .= show pageNo]
     internalError

-- | Preview when authorized user is logged in (without magic hash)
showPreview :: Kontrakcja m => DocumentID -> FileID -> m Response
showPreview did fid = logDocumentAndFile did fid $ do
  guardLoggedIn
  pixelwidth <- fromMaybe 150 <$> readField "pixelwidth"
  void $ getDocByDocID did
  if fid == unsafeFileID 0
    then do
      emptyPreview <- liftIO $ BS.readFile "frontend/app/img/empty-preview.jpg"
      return . toResponseBS "image/jpeg" $ BSL.fromStrict emptyPreview
    else do
      checkFileAccessWith fid Nothing Nothing (Just did) Nothing
      previewResponse fid pixelwidth

-- | Preview from mail client with magic hash
showPreviewForSignatory :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> FileID -> m Response
showPreviewForSignatory did slid mh fid = logDocumentAndFile did fid $ do
  checkFileAccessWith fid (Just slid) (Just mh) (Just did) Nothing
  pixelwidth <- fromMaybe 150 <$> readField "pixelwidth"
  previewResponse fid pixelwidth

previewResponse :: Kontrakcja m => FileID -> Int -> m Response
previewResponse fid pixelwidth = do
  fileData <- getFileIDContents fid
  rp <- renderPage fileData 1 pixelwidth
  case rp of
   Just pageData -> return $ toResponseBS "image/png" $ BSL.fromStrict pageData
   Nothing -> do
     logAttention_ "Rendering PDF preview failed"
     internalError

handleDownloadClosedFile :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> String -> m Response
handleDownloadClosedFile did sid mh _nameForBrowser = do
  doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh
  if isClosed doc then do
    file <- guardJustM $ fileFromMainFile $ documentsealedfile doc
    content <- getFileIDContents $ fileid file
    return $ respondWithPDF True content
   else respond404

handleResend :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m (Either (FlashMessage, KontraLink) JSValue)
handleResend docid signlinkid = userWithPost $ do
  getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid `withDocumentM` do
    signlink <- guardJust . getSigLinkFor signlinkid =<< theDocument
    customMessage <- fmap strip <$> getField "customtext"
    actor <- guardJustM $ fmap mkAuthorActor getContext
    _ <- sendReminderEmail customMessage actor False signlink
    J.runJSONGenT (return ())

-- This only works for undelivered mails
handleChangeSignatoryEmail :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
handleChangeSignatoryEmail docid slid = do
  guardLoggedIn
  email <- getCriticalField asValidEmail "email"
  getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid `withDocumentM` do
    muser <- dbQuery $ GetUserByEmail (Email email)
    actor <- guardJustM $ mkAuthorActor <$> getContext
    dbUpdate $ ChangeSignatoryEmailWhenUndelivered slid muser email actor
    sl <- guardJust . getSigLinkFor slid =<< theDocument
    _ <- sendInvitationEmail1 sl
    J.runJSONGenT $ return ()

-- This only works for undelivered smses
handleChangeSignatoryPhone :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
handleChangeSignatoryPhone docid slid = do
  guardLoggedIn
  phone <- getCriticalField asValidPhone "phone"
  getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid `withDocumentM` do
    actor <- guardJustM $ mkAuthorActor <$> getContext
    dbUpdate $ ChangeSignatoryPhoneWhenUndelivered slid phone actor
    -- get (updated) siglink from updated document
    sl <- guardJust . getSigLinkFor slid =<< theDocument
    _ <- sendInvitationEmail1 sl
    J.runJSONGenT $ return ()

handlePadList :: Kontrakcja m => m Response
handlePadList = do
  ctx <- getContext
  ad <- getAnalyticsData
  case getContextUser ctx of
    Just _ -> simpleHtmlResonseClrFlash =<< pageDocumentPadList ctx  ad
    _ -> simpleHtmlResonseClrFlash =<< pageDocumentPadListLogin ctx  ad

handleToStart :: Kontrakcja m => m Response
handleToStart = do
  ctx <- getContext
  ad <- getAnalyticsData
  case (ctxmaybeuser ctx) of
    Just _ -> simpleHtmlResonseClrFlash =<< pageDocumentToStartList ctx  ad
    _ -> simpleHtmlResonseClrFlash =<< pageDocumentToStartLogin ctx  ad

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

  msid <- readField "signatory_id"
  mdid <- readField "document_id"
  mattid <- readField "attachment_id"

  -- If refering to something by SignatoryLinkID check out if in the
  -- session we have a properly stored access magic hash.
  mmh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msid
  checkFileAccessWith fid msid mmh mdid mattid

checkFileAccessWith :: Kontrakcja m =>
  FileID -> Maybe SignatoryLinkID -> Maybe MagicHash -> Maybe DocumentID -> Maybe AttachmentID -> m ()
checkFileAccessWith fid msid mmh mdid mattid =
  case (msid, mmh, mdid, mattid) of
    (Just sid, Just mh, Just did,_) -> do
       (dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh) `withDocumentM` do
        sl <- guardJustM $ getSigLinkFor sid <$> theDocument
        whenM (signatoryNeedsToIdentifyToView sl) $ do
          unless (isAuthor sl) $ do
            internalError
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
                                            [ AttachmentFilterByID attid
                                            , AttachmentFilterByFileID fid
                                            ]
                                            []
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
             mailattachments <- makeMailAttachments doc True
             mailDocumentRemindContent Nothing doc sl (not (null mailattachments))
         "invite" -> do
             doc <- getDocByDocID docid
             mailInvitationContent False Sign Nothing doc
         "confirm" -> do
             doc <- getDocByDocID docid
             mailClosedContent True doc
         _ -> fail "prepareEmailPreview"
    J.runJSONGenT $ J.value "content" content

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
                    (pth, fhandle) <- openTempFile systmp "vpath.pdf"
                    BSL.hPutStr fhandle content
                    return pth
            _ -> internalError
      ctx <- getContext
      J.toJSValue <$> GuardTime.verify (ctxgtconf ctx) filepath

handleMarkAsSaved :: Kontrakcja m => DocumentID -> m JSValue
handleMarkAsSaved docid = do
  guardLoggedIn
  getDocByDocID docid `withDocumentM` do
    whenM (isPreparation <$> theDocument) $ dbUpdate $ SetDocumentUnsavedDraft False
    J.runJSONGenT $ return ()

-- Add some event as signatory if this signatory has not signed yet, and document is pending
addEventForVisitingSigningPageIfNeeded :: (Kontrakcja m, DocumentMonad m) => CurrentEvidenceEventType -> SignatoryLink -> m ()
addEventForVisitingSigningPageIfNeeded ev sl = do
  ctx <- getContext
  doc <- theDocument
  when (isPending doc && isNothing (maybesigninfo sl)) $
    void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg ev  (return ()) (Just sl) Nothing =<< signatoryActor ctx sl
