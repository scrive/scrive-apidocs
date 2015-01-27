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
    , handleSignPadShow
    , handleAcceptAccountFromSign
    , handleEvidenceAttachment
    , handleIssueShowGet
    , handleIssueAuthorGoToSignview
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
    , handlePostSignview
    , handleToStart
    , handleToStartShow
) where

import Control.Applicative
import Control.Concurrent
import Control.Conditional (unlessM, whenM)
import Control.Monad.Catch (MonadMask)
import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Time (ZonedTime)
import Data.Maybe
import Data.String.Utils (replace)
import Happstack.Server hiding (simpleHTTP)
import System.Directory
import System.IO.Temp
import Text.JSON hiding (Result)
import Text.JSON.Gen hiding (value)
import Text.StringTemplates.Templates
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length, take)
import qualified Data.Map as Map
import qualified Data.Traversable as T
import qualified Text.JSON.Gen as J

import Analytics.Include
import AppView
import Attachment.AttachmentID (AttachmentID)
import Attachment.Model
import Control.Logic ((||^))
import DB
import DB.TimeZoneName
import Doc.API.Callback.Model
import Doc.DocInfo
import Doc.DocMails
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocumentMonad (DocumentMonad, withDocumentM, withDocument, theDocument)
import Doc.DocUtils (documentsealedfileM)
import Doc.DocView
import Doc.DocViewMail
import Doc.Model
import Doc.RenderedPages
import Doc.Rendering
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import EvidenceLog.Model (InsertEvidenceEventWithAffectedSignatoryAndMsg(..), CurrentEvidenceEventType(..))
import File.File (fileid)
import File.Model
import File.Storage (getFileIDContents)
import Happstack.Fields
import Happstack.MonadPlus (runMPlusT)
import InputValidation
import Kontra
import KontraLink
import MagicHash
import MinutesTime
import Redirect
import User.Email
import User.Model
import User.Utils
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import Util.Zlib (decompressIfPossible)
import Utils.Default
import qualified Doc.EvidenceAttachments as EvidenceAttachments
import qualified GuardTime as GuardTime
import qualified Log
import qualified User.Action

handleNewDocument :: Kontrakcja m => m KontraLink
handleNewDocument = do
  ctx <- getContext
  if (isJust $ ctxmaybeuser ctx)
     then withUserPost $ do
        user <- guardJustM $ ctxmaybeuser <$> getContext
        title <- renderTemplate_ "newDocumentTitle"
        actor <- guardJustM $ mkAuthorActor <$> getContext
        mtimezonename <- runMPlusT $ lookCookieValue "timezone"
        timezone <- fromMaybe defaultTimeZoneName <$> T.sequence (mkTimeZoneName <$> mtimezonename)
        timestamp <- formatTimeSimpleWithTZ timezone (ctxtime ctx)
        doc <- dbUpdate $ NewDocument defaultValue user (replace "  " " " $ title ++ " " ++ timestamp) Signable timezone 1 actor
        -- Default document on the frontend has different requirements,
        -- this sets up the signatories to match those requirements.
        withDocument doc $ do
            authorsiglink <- guardJust $ find (\sl -> signatoryisauthor sl) (documentsignatorylinks doc)
            othersiglink  <- guardJust $ find (\sl -> sl /= authorsiglink)  (documentsignatorylinks doc)
            let templateField ft = SignatoryField { sfID = unsafeSignatoryFieldID 0
                                                  , sfType = ft
                                                  , sfValue = ""
                                                  , sfShouldBeFilledBySender = False
                                                  , sfObligatory = False
                                                  , sfPlacements = []
                                                  }
                othersiglink' = othersiglink { signatorysignorder = SignOrder 1
                                             , signatoryfields = [ templateField FirstNameFT
                                                                 , templateField LastNameFT
                                                                 , templateField EmailFT
                                                                 , templateField MobileFT
                                                                 , templateField CompanyFT
                                                                 , templateField CompanyNumberFT
                                                                 ]
                                             }
            _ <- dbUpdate $ ResetSignatoryDetails [authorsiglink, othersiglink'] actor
            dbUpdate $ SetDocumentUnsavedDraft True
        return $ LinkIssueDoc (documentid doc)
     else return $ LinkLogin (ctxlang ctx) LoginTry
{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}

formatTimeSimpleWithTZ :: (MonadDB m, MonadMask m) => TimeZoneName -> UTCTime -> m String
formatTimeSimpleWithTZ tz mt = withTimeZone tz $ do
  runQuery_ $ rawSQL "SELECT $1, to_char($1, 'TZ')" (Single mt)
  (t::ZonedTime, _::String) <- fetchOne id
  return $ formatTime' "%Y-%m-%d %H:%M" t

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
{-# NOINLINE handleSignShowSaveMagicHash #-}
handleSignShowSaveMagicHash :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
handleSignShowSaveMagicHash did sid mh = do
  -- Getting some evidence
  ctx <- getContext
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh) `withDocumentM` do
    dbUpdate $ AddDocumentSessionToken sid mh

    invitedlink <- guardJust . getSigLinkFor sid =<< theDocument
    void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg SignatoryLinkVisited  (return ()) (Just invitedlink) Nothing =<< signatoryActor ctx invitedlink

    -- Redirect to propper page
    return $ LinkSignDocNoMagicHash did sid

-- |
--   /s/[documentid]/[signatorylinkid]
{-# NOINLINE handleSignShow #-}
handleSignShow :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
handleSignShow documentid signatorylinkid = do
  ctx <- getContext

  mmagichash <- dbQuery $ GetDocumentSessionToken signatorylinkid

  case mmagichash of
    Just magichash ->
      dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash documentid signatorylinkid magichash) `withDocumentM` do
        invitedlink <- guardJust . getSigLinkFor signatorylinkid =<< theDocument
        switchLangWhenNeeded  (Just invitedlink) =<< theDocument
        unlessM ((isTemplate ||^ isPreparation ||^ isClosed) <$> theDocument) $ do
          dbUpdate . MarkDocumentSeen signatorylinkid magichash =<< signatoryActor ctx invitedlink
          triggerAPICallbackIfThereIsOne =<< theDocument
        ad <- getAnalyticsData
        content <- theDocument >>= \d -> pageDocumentSignView ctx d invitedlink ad
        simpleHtmlResonseClrFlash content
    Nothing -> handleCookieFail signatorylinkid documentid

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

-- |
--   /sp/[documentid]/[signatorylinkid]
{-# NOINLINE handleSignPadShow #-}
handleSignPadShow :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
handleSignPadShow documentid signatorylinkid = do
  ctx <- getContext
  mmagichash <- dbQuery $ GetDocumentSessionToken signatorylinkid
  case mmagichash of
    Just magichash ->
      dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash documentid signatorylinkid magichash) `withDocumentM` do
        invitedlink <- guardJust . getSigLinkFor signatorylinkid =<< theDocument
        switchLangWhenNeeded  (Just invitedlink) =<< theDocument
        unlessM ((isTemplate ||^ isPreparation ||^ isClosed) <$> theDocument) $ do
          dbUpdate . MarkDocumentSeen signatorylinkid magichash =<< signatoryActor ctx invitedlink
          triggerAPICallbackIfThereIsOne =<< theDocument
        ad <- getAnalyticsData
        content <- theDocument >>= \d -> pageDocumentSignForPadView ctx d invitedlink ad
        simpleHtmlResonseClrFlash content
    Nothing -> handleCookieFail signatorylinkid documentid


-- If is not magic hash in session. It may mean that the
-- session expired and we deleted the credentials already or it
-- may mean that cookies are disabled. Lets try to find out if
-- there are any cookies, if there are none we show a page how
-- to enable cookies on iPhone that seems to be the only
-- offender.
handleCookieFail :: Kontrakcja m => SignatoryLinkID -> DocumentID -> m Response
handleCookieFail slid did = do
      cookies <- rqCookies <$> askRq
      if null cookies
         then sendRedirect LinkEnableCookies
         else do
           Log.mixlog_ $ "Signview load after session timedout for slid: " ++ show slid ++ ", did: " ++ show did
           renderTemplate_ "signSessionTimeOut" >>= renderFromBody

{- |
   Redirect author of document to go to signview
   URL: /d/signview/{documentid}
   Method: POST
 -}
handleIssueAuthorGoToSignview :: Kontrakcja m => DocumentID -> m KontraLink
handleIssueAuthorGoToSignview docid = do
  guardLoggedIn
  ctx <- getContext
  doc <- getDocByDocIDForAuthor docid
  user <-  guardJust (ctxmaybeuser ctx)
  case (isAuthor <$> getMaybeSignatoryLink (doc,user)) of
    Just True -> do
      let asl = fromJust $ getMaybeSignatoryLink (doc,user) -- Checked isJust in case
      dbUpdate $ AddDocumentSessionToken (signatorylinkid asl) (signatorymagichash asl)
      return $ LinkSignDocNoMagicHash docid (signatorylinkid asl)
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
  user <-  guardJust $ mplus (ctxmaybeuser ctx) (ctxmaybepaduser ctx)
  case (isAuthor <$> getMaybeSignatoryLink (doc,user), getMaybeSignatoryLink (doc,slid)) of
    (Just True,Just sl) | signatorylinkdeliverymethod sl == PadDelivery -> do
      dbUpdate $ AddDocumentSessionToken (signatorylinkid sl) (signatorymagichash sl)
      withDocument doc $ void $
        dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg SignatoryLinkVisited  (return ()) (Just sl) Nothing =<< signatoryActor ctx sl
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
       Right <$> pageDocumentView document msiglink (isincompany)
    (False, Just siglink)            -> do
       Left  <$> (simpleHtmlResonseClrFlash =<< pageDocumentSignView ctx document siglink ad)
    _                                -> do
       internalError


{- We return pending message if file is still pending, else we return JSON with number of pages-}
handleFilePages :: Kontrakcja m => FileID -> m Response
handleFilePages fid = do
  checkFileAccess fid
  mpixelwidth <- readField "pixelwidth"
  pages <- getRenderedPages fid (fromMaybe legacyWidthInPixels mpixelwidth) RenderingModeWholeDocument

  case pages of
    RenderedPages False _ -> do
      -- Here we steal Twitter's Enhance Your Calm status code.  Out
      -- mechanism upwards the stack will know to retry to ask us
      -- again before giving up.
      rsp <- simpleJsonResponse $ runJSONGen $ J.value "wait" "Rendering in progress"
      return (rsp { rsCode = 420 })
    RenderedPages True pngpages  -> do
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
      simpleJsonResponse $ runJSONGen $ J.value "pages" $ length pngpages

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
        emptyPreview <- liftIO $ BS.readFile "frontend/app/img/empty-preview.jpg"
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
        pages <- getRenderedPages fid 150 RenderingModeFirstPageOnly
        case pages of
            RenderedPages _ (contents:_) -> do
                let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
                return $ Just $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png") res
            _ -> do
                liftIO $ threadDelay 500000
                preview fid (value+1)


showPage' :: Kontrakcja m => FileID -> Int -> m Response
showPage' fileid pageno = do
  mpixelwidth <- readField "pixelwidth"
  pages <- getRenderedPages fileid (fromMaybe legacyWidthInPixels mpixelwidth) RenderingModeWholeDocument
  case pages of
    RenderedPages _ contents | pageno - 1 < length contents -> do
      let content = contents !! (pageno - 1)
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [content]) Nothing
      Log.mixlog_ $ "PNG page found and returned for file " ++ show fileid ++ " and page " ++ show pageno
      return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png")
             -- max-age same as for brandedSignviewImage
             $ setHeaderBS (BS.fromString "Cache-Control") (BS.fromString "max-age=604800") res

    RenderedPages False _ -> do
      return ((toResponse "") { rsCode = 420 })
    _ -> do
      Log.mixlog_ $ "JPEG page not found in cache, responding 404 for file " ++ show fileid ++ " and page " ++ show pageno
      notFound (toResponse "temporarily unavailable (document has files pending for process)")

handleDownloadClosedFile :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> String -> m Response
handleDownloadClosedFile did sid mh _nameForBrowser = do
  doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh
  if isClosed doc then do
    file <- guardJustM $ documentsealedfileM doc
    content <- getFileIDContents $ fileid file
    return $ respondWithPDF True content
   else respond404

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
    Just email -> getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid `withDocumentM` do
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


handlePadList :: Kontrakcja m => m Response
handlePadList = do
  ctx <- getContext
  ad <- getAnalyticsData
  case (ctxmaybeuser ctx `mplus` ctxmaybepaduser ctx) of
    Just _ -> simpleHtmlResonseClrFlash =<< pageDocumentPadList ctx  ad
    _ -> simpleHtmlResonseClrFlash =<< pageDocumentPadListLogin ctx  ad

handlePostSignview :: Kontrakcja m => m Response
handlePostSignview = do
  ctx <- getContext
  ad <- getAnalyticsData
  _ <- guardJust $ ctxmaybeuser ctx
  simpleHtmlResonseClrFlash =<< pagePostSignview ctx  ad

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
             mailattachments <- makeMailAttachments doc
             mailDocumentRemindContent Nothing doc sl (not (null mailattachments))
         "reject" -> do
             Just mh <- dbQuery $ GetDocumentSessionToken slid
             doc <- dbQuery $ GetDocumentByDocumentID docid
             Just sl <- return $ getSigLinkFor (slid,mh) doc
             x :: String <- mailDocumentRejectedContent Nothing sl doc
             return x
         "invite" -> do
             doc <- getDocByDocID docid
             mailInvitationContent False Sign Nothing doc
         "confirm" -> do
             doc <- getDocByDocID docid
             mailClosedContent True doc
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
      toJSValue <$> GuardTime.verify (ctxgtconf ctx) filepath

handleMarkAsSaved :: Kontrakcja m => DocumentID -> m JSValue
handleMarkAsSaved docid = do
  guardLoggedIn
  getDocByDocID docid `withDocumentM` do
    whenM (isPreparation <$> theDocument) $ dbUpdate $ SetDocumentUnsavedDraft False
    runJSONGenT $ return ()
