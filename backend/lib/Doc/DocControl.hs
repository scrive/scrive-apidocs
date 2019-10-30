{-# LANGUAGE ExtendedDefaultRules #-}

-- | DocControl represents the controler (in MVC) of the document.

module Doc.DocControl (
    -- Exported utils or test functions
      sendReminderEmail
    -- Top level handlers
    , handleNewDocument
    , showCreateFromTemplate
    , handleDownloadClosedFile
    , handleSignShow
    , handleSignShowSaveMagicHash
    , handleSignFromTemplate
    , handleEvidenceAttachment
    , handleIssueShowGet
    , handleIssueGoToSignview
    , handleIssueGoToSignviewPad
    , prepareEmailPreview
    , handleResend
    , showPage
    , showPreview
    , showPreviewForSignatory
    , handleFilePages
    , handleShowAfterForward
    , handleShowVerificationPage
    , handleVerify
    , handleMarkAsSaved
    , handleAfterSigning
    , handlePadList
    , handleToStart
    , handleToStartShow
    , handleNewDocumentWithBPID
) where

import Control.Conditional (unlessM, whenM)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Happstack.Server hiding (lookCookieValue, simpleHTTP, timeout)
import Log
import System.Directory
import System.IO.Temp
import Text.JSON hiding (Result)
import Text.StringTemplates.Templates
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Traversable as T
import qualified Text.JSON.Gen as J

import Analytics.Include
import AppView
import Attachment.AttachmentID (AttachmentID)
import Attachment.Model
import Chargeable.Model
import Cookies
import DB
import DB.TimeZoneName
import Doc.Action
import Doc.API.Callback.Model
import Doc.API.V2.DocumentUpdateUtils
import Doc.API.V2.Guards (guardDocumentStatus, guardThatDocumentCanBeStarted)
import Doc.Conditions
import Doc.DocInfo
import Doc.DocMails
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocumentMonad
  ( DocumentMonad, theDocument, withDocument, withDocumentID, withDocumentM )

import Doc.DocUtils (canSignatorySignNow, fileFromMainFile)
import Doc.DocView
import Doc.DocViewMail
import Doc.Logging
import Doc.Model
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import EvidenceLog.Model
  ( CurrentEvidenceEventType(..)
  , InsertEvidenceEventWithAffectedSignatoryAndMsg(..) )

import FeatureFlags.Model
import File.File (fileid)
import File.Model
import File.Storage (getFileIDContents)
import Happstack.Fields
import InternalResponse
import Kontra
import KontraLink
import Log.Identifier
import MagicHash
import Redirect
import Session.Model
import Templates (renderTextTemplate, renderTextTemplate_)
import Text.JSON.Convert
import User.Email
import User.Model
import User.Utils
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.PDFUtil
import Util.SignatoryLinkUtils
import qualified Doc.EvidenceAttachments as EvidenceAttachments
import qualified GuardTime as GuardTime

handleNewDocument :: Kontrakcja m => m InternalKontraResponse
handleNewDocument = withUser . with2FACheck $ \user -> do
  ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ userid user
  if ugsRequireBPIDForNewDoc (ugwpSettings ugwp)
    then do
      -- This is a special feature for RBS (CORE-1081)
      -- RBS needs to "force" their employees to enter their internal document
      -- number. To avoid adding this to designview, this extra step is added before
      -- creating a new document.
      -- When Scrive.Flow is functional, this should be removed and implemented using
      -- Scrive.Flow.
      ctx <- getContext
      pb  <- renderTextTemplate "showNewDocumentWithBPID" $ do
        entryPointFields ctx
      internalResponse <$> renderFromBodyWithFields pb (return ())
    else do
      docid <- handleNewDocument' user ugwp
      return . internalResponse $ LinkIssueDoc docid

handleNewDocument' :: Kontrakcja m => User -> UserGroupWithParents -> m DocumentID
handleNewDocument' user ugwp = do
  ctx           <- getContext
  title         <- renderTextTemplate_ "newDocumentTitle"
  actor         <- guardJustM $ mkAuthorActor <$> getContext
  mtimezonename <- (lookCookieValue "timezone" . rqHeaders) <$> askRq
  case mtimezonename of
    Nothing  -> logInfo_ "'timezone' cookie not found"
    (Just _) -> return ()
  timezone <- fromMaybe defaultTimeZoneName
    <$> T.sequence (mkTimeZoneName <$> mtimezonename)
  timestamp <- formatTimeSimpleWithTZ timezone (ctxTime ctx)
  doc       <- dbUpdate $ NewDocument user
                                      (T.replace "  " " " $ title <> " " <> timestamp)
                                      Signable
                                      timezone
                                      1
                                      actor
                                      Nothing
  -- Default document on the frontend has different requirements,
  -- this sets up the signatories to match those requirements.
  (authToView, authToSign, invitationDelivery, confirmationDelivery) <- do
    let features = ugwpFeatures ugwp
        ff =
          if useriscompanyadmin user then fAdminUsers features else fRegularUsers features
    return
      ( firstAllowedAuthenticationToView ff
      , firstAllowedAuthenticationToSign ff
      , firstAllowedInvitationDelivery ff
      , firstAllowedConfirmationDelivery ff
      )
  withDocument doc $ do
    authorsiglink <- guardJust
      $ find (\sl -> signatoryisauthor sl) (documentsignatorylinks doc)
    othersiglink <- guardJust
      $ find (\sl -> not $ signatoryisauthor sl) (documentsignatorylinks doc)
    let fields =
          [ SignatoryNameField $ NameField { snfID = (unsafeSignatoryFieldID 0)
                                           , snfNameOrder              = NameOrder 1
                                           , snfValue                  = ""
                                           , snfObligatory             = False
                                           , snfShouldBeFilledBySender = False
                                           , snfPlacements             = []
                                           }
          , SignatoryNameField $ NameField { snfID = (unsafeSignatoryFieldID 0)
                                           , snfNameOrder              = NameOrder 2
                                           , snfValue                  = ""
                                           , snfObligatory             = False
                                           , snfShouldBeFilledBySender = False
                                           , snfPlacements             = []
                                           }
          , SignatoryEmailField $ EmailField { sefID = (unsafeSignatoryFieldID 0)
                                             , sefValue                  = ""
                                             , sefObligatory             = True
                                             , sefShouldBeFilledBySender = False
                                             , sefEditableBySignatory    = False
                                             , sefPlacements             = []
                                             }
          , SignatoryMobileField $ MobileField { smfID = (unsafeSignatoryFieldID 0)
                                               , smfValue                  = ""
                                               , smfObligatory             = False
                                               , smfShouldBeFilledBySender = False
                                               , smfEditableBySignatory    = False
                                               , smfPlacements             = []
                                               }
          , SignatoryCompanyField $ CompanyField { scfID = (unsafeSignatoryFieldID 0)
                                                 , scfValue                  = ""
                                                 , scfObligatory             = False
                                                 , scfShouldBeFilledBySender = False
                                                 , scfPlacements             = []
                                                 }
          ]
        authorsiglink' = authorsiglink
          { signatorylinkdeliverymethod             = invitationDelivery
          , signatorylinkconfirmationdeliverymethod = confirmationDelivery
          , signatorylinkauthenticationtoviewmethod = authToView
          , signatorylinkauthenticationtosignmethod = authToSign
          }
        othersiglink' = othersiglink
          { signatorysignorder          = SignOrder 1
          , signatoryfields             = fields
          , signatorylinkdeliverymethod = invitationDelivery
          , signatorylinkconfirmationdeliverymethod = confirmationDelivery
          , signatorylinkauthenticationtoviewmethod = authToView
          , signatorylinkauthenticationtosignmethod = authToSign
          }
    void $ dbUpdate $ ResetSignatoryDetails [authorsiglink', othersiglink'] actor
    dbUpdate $ SetDocumentUnsavedDraft True
    logInfo "New document created" $ logObject_ doc
  return $ documentid doc

--
--  Document state transitions are described in DocState.
--
--  Here are all actions associated with transitions.
--

formatTimeSimpleWithTZ :: (MonadDB m, MonadThrow m) => TimeZoneName -> UTCTime -> m Text
formatTimeSimpleWithTZ tz t = do
  runQuery_ $ rawSQL "SELECT to_char($1 AT TIME ZONE $2, 'YYYY-MM-DD HH24:MI')" (t, tz)
  fetchOne runIdentity

showCreateFromTemplate :: Kontrakcja m => m InternalKontraResponse
showCreateFromTemplate = withUser . with2FACheck $ \_ -> do
  ctx  <- getContext
  page <- pageCreateFromTemplate ctx
  return $ internalResponse page

-- | Call after signing in order to save the document for any user,
-- and put up the appropriate modal.
handleAfterSigning
  :: (MonadLog m, MonadThrow m, TemplatesMonad m, DocumentMonad m, MonadBase IO m)
  => SignatoryLinkID
  -> m ()
handleAfterSigning slid = logSignatory slid $ do
  signatorylink <- guardJust . getSigLinkFor slid =<< theDocument
  maybeuser     <- dbQuery $ GetUserByEmail (Email $ getEmail signatorylink)
  case maybeuser of
    Just user | isJust $ userhasacceptedtermsofservice user -> do
      void $ dbUpdate $ SaveDocumentForUser user slid
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
handleSignShowSaveMagicHash
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m Response
handleSignShowSaveMagicHash did slid mh =
  logDocumentAndSignatory did slid
    $ (do
        dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh)
          `withDocumentM` do
                            guardThatDocumentIsReadableBySignatories =<< theDocument
                            sid <- getNonTempSessionID
                            dbUpdate $ AddDocumentSession sid slid
                            -- Redirect to propper page
                            sendRedirect $ LinkSignDocNoMagicHash did slid
      )
    `catchDBExtraException` (\(DocumentDoesNotExist _) -> respond404)
    `catchDBExtraException` (\SignatoryTokenDoesNotMatch -> respondLinkInvalid)
    `catchDBExtraException` (\SignatoryLinkIsForwarded -> respondLinkInvalid)
    `catchDBExtraException` (\(_ :: DocumentWasPurged) -> respondLinkInvalid)

handleSignFromTemplate :: Kontrakcja m => DocumentID -> MagicHash -> m Response
handleSignFromTemplate tplID mh = logDocument tplID $ do
  ctx <- getContext
  tpl <- dbQuery $ GetDocumentByDocumentIDAndShareableLinkHash tplID mh

  let actor = contextActor ctx
  mDocID <-
    withDocument tpl
    $ dbUpdate
    $ CloneDocumentWithUpdatedAuthor Nothing tpl actor
    $ \doc -> doc { documenttype              = Signable
                  , documenttemplateid        = Just (documentid tpl)
                  , documentfromshareablelink = True
                  }

  case mDocID of
    Nothing -> do
      logAttention "Cloning shareable template failed" $ object [identifier tplID]
      respondLinkInvalid

    Just docID -> withDocumentID docID $ do
      let startDocument = do
            -- Guards
            guardDocumentStatus Preparation =<< theDocument
            guardThatDocumentCanBeStarted =<< theDocument
            -- Parameters
            timezone <- documenttimezonename <$> theDocument
            clearDocFields actor
            dbUpdate $ PreparationToPending actor timezone
            t <- ctxTime <$> getContext
            dbUpdate $ SetDocumentInviteTime t actor
            postDocumentPreparationChange False timezone
      startDocument
        `catches` [ Handler $ \(SomeDBExtraException e) -> do
          -- We are not rolling back here, because respondLinkInvalid will do it.
                      logInfo "SignFromTemplate Error:"
                        $ object ["extra_exception" .= jsonToAeson (J.toJSValue e)]
                      respondLinkInvalid
                  ]
      mSL <- (find (not . isAuthor) . documentsignatorylinks) <$> theDocument
      case mSL of
        Nothing -> do
          logAttention
              "Can't find suitable signatory for shareable\
                       \ template"
            $ object [identifier docID]
          respondLinkInvalid
        Just sl -> do
          sid <- getNonTempSessionID
          dbUpdate $ ChargeUserGroupForStartingDocument docID
          dbUpdate $ AddDocumentSession sid (signatorylinkid sl)
          sendRedirect $ LinkSignDocNoMagicHash docID $ signatorylinkid sl

-- |
--   /s/[documentid]/[signatorylinkid] and /sp/[documentid]/[signatorylinkid]

{-# NOINLINE handleSignShow #-}
handleSignShow :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
handleSignShow did slid = logDocumentAndSignatory did slid $ do
  sid          <- ctxSessionID <$> getContext
  validSession <- dbQuery $ CheckDocumentSession sid slid
  if validSession
    then do
      doc         <- dbQuery $ GetDocumentByDocumentID did
      invitedlink <- guardJust $ getSigLinkFor slid doc
      -- We always switch to document language in case of pad signing
      switchLang $ getLang doc
      ctx             <- getContext -- Order is important since ctx after switchLang changes
      ad              <- getAnalyticsData
      needsToIdentify <- signatoryNeedsToIdentifyToView invitedlink doc
      if needsToIdentify
        then doc `withDocument` do
          addEventForVisitingSigningPageIfNeeded VisitedViewForAuthenticationEvidence
                                                 invitedlink
          content <- pageDocumentIdentifyView ctx doc invitedlink ad
          simpleHtmlResponse content
        else do
          if isClosed doc
            then do
              content <- pageDocumentSignView ctx doc invitedlink ad
              simpleHtmlResponse content
            else doc `withDocument` do
              addEventForVisitingSigningPageIfNeeded VisitedViewForSigningEvidence
                                                     invitedlink
              unlessM ((isTemplate || isPreparation) <$> theDocument) $ do
                dbUpdate . MarkDocumentSeen slid =<< signatoryActor ctx invitedlink
                triggerAPICallbackIfThereIsOne =<< theDocument
              content <- theDocument >>= \d -> pageDocumentSignView ctx d invitedlink ad
              simpleHtmlResponse content
    else handleCookieFail slid did

-- |
--   /ts/[documentid] (doc has to be a draft)
{-# NOINLINE handleToStartShow #-}
handleToStartShow :: Kontrakcja m => DocumentID -> m InternalKontraResponse
handleToStartShow documentid = withUser . withTosCheck $ \_ -> do
  ctx      <- getContext
  document <- getDocByDocIDForAuthor documentid
  ad       <- getAnalyticsData
  content  <- pageDocumentToStartView ctx document ad
  internalResponse <$> (simpleHtmlResponse content)

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
      ctx     <- getContext
      ad      <- getAnalyticsData
      content <- renderTextTemplate "sessionTimeOut" (standardPageFields ctx Nothing ad)
      simpleHtmlResponse content

{- |
   Redirect author of document to go to signview
   URL: /d/signview/{documentid}
   Method: POST
 -}
handleIssueGoToSignview :: Kontrakcja m => DocumentID -> m InternalKontraResponse
handleIssueGoToSignview docid = withUser $ \user -> do
  doc <- getDocByDocID docid
  case (getMaybeSignatoryLink (doc, user)) of
    Just sl -> do
      sid <- getNonTempSessionID
      dbUpdate $ AddDocumentSession sid (signatorylinkid sl)
      return $ internalResponse $ LinkSignDocNoMagicHash docid (signatorylinkid sl)
    _ -> return $ internalResponse $ LoopBack

{- |
   Redirect author of document to go to signview for any of the pad signatories
   URL: /d/signview/{documentid}/{signatorylinkid}
   Method: POST
 -}
handleIssueGoToSignviewPad
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleIssueGoToSignviewPad docid slid = do
  ctx  <- getContext
  doc  <- getDocByDocIDForAuthor docid
  user <- guardJust $ getContextUser ctx
  case
      (isAuthor <$> getMaybeSignatoryLink (doc, user), getMaybeSignatoryLink (doc, slid))
    of
      (Just True, Just sl) | signatorylinkdeliverymethod sl == PadDelivery -> do
        sid <- getNonTempSessionID
        dbUpdate $ AddDocumentSession sid (signatorylinkid sl)
        return $ LinkSignDocPad docid slid
      _ -> return LoopBack

handleEvidenceAttachment :: Kontrakcja m => DocumentID -> Text -> m InternalKontraResponse
handleEvidenceAttachment docid aname =
  logDocument docid $ localData ["attachment_name" .= aname] $ withUser $ \_ -> do
    doc <- getDocByDocID docid
    es  <- guardJustM $ EvidenceAttachments.extractAttachment doc aname
    return $ internalResponse $ toResponseBS "text/html" $ es

{- |
   Handles the request to show a document to a logged in user.
   URL: /d/{documentid}
   Method: GET
 -}
handleIssueShowGet :: Kontrakcja m => DocumentID -> m InternalKontraResponse
handleIssueShowGet docid = withUser . withTosCheck . with2FACheck $ \_ -> do
  document      <- getDocByDocID docid
  muser         <- ctxMaybeUser <$> getContext

  authorsiglink <- guardJust $ getAuthorSigLink document

  let ispreparation = documentstatus document == Preparation
      isauthor      = (userid <$> muser) == maybesignatory authorsiglink
  mauthoruser <- maybe (return Nothing)
                       (dbQuery . GetUserByIDIncludeDeleted)
                       (maybesignatory authorsiglink)

  let isincompany = case muser of
        Nothing -> False
        Just user ->
          useriscompanyadmin user
            && Just (usergroupid user)
            == (usergroupid <$> mauthoruser)
      msiglink = find (isSigLinkFor muser) $ documentsignatorylinks document
  ad  <- getAnalyticsData

  ctx <- getContext
  case (ispreparation, msiglink) of
    (True, _) -> do
       -- Never cache design view. IE8 hack. Should be fixed in different wasy
      internalResponse
        <$> (   setHeaderBS "Cache-Control" "no-cache"
            <$> (simpleHtmlResponse =<< pageDocumentDesign ctx document ad)
            )
    (False, Just sl)
      | isauthor -> if isClosed document
        -- If authenticate to view archived is set for author, we can't show him
        -- the signed document in author's view before he authenticates.
        then signatoryNeedsToIdentifyToView sl document >>= \case
          True ->
            fmap internalResponse
              .   simpleHtmlResponse
              =<< pageDocumentIdentifyView ctx document sl ad
          False ->
            internalResponse <$> pageDocumentView ctx document msiglink isincompany
        else internalResponse <$> pageDocumentView ctx document msiglink isincompany
      | canSignatorySignNow document sl -> do
       -- We also need to authenticate signatory in current session.
        sid <- getNonTempSessionID
        dbUpdate $ AddDocumentSession sid (signatorylinkid sl)
       -- Simply loading pageDocumentSignView doesn't work when signatory needs
        -- to authenticate to view, redirect to proper sign view.
        return $ internalResponse $ LinkSignDocNoMagicHash docid $ signatorylinkid sl
      | isincompany ->  internalResponse
      <$> pageDocumentView ctx document msiglink isincompany
      | otherwise -> internalError -- can this happen at all?
    (False, Nothing) | isincompany ->
      internalResponse <$> pageDocumentView ctx document msiglink isincompany
    _ -> internalError


{- We return pending message if file is still pending, else we return JSON with number of pages-}
handleFilePages :: Kontrakcja m => FileID -> m Response
handleFilePages fid = logFile fid $ do
  checkFileAccess fid
  ePagesCount <- liftIO . getNumberOfPDFPages =<< getFileIDContents fid
  case ePagesCount of
    Right pc -> simpleJsonResponse . J.runJSONGen . J.value "pages" $ pc
    _        -> do
      logAttention_ "Counting number of pages failed"
      internalError

-- | Get some HTML to display the images of the files.
--
-- URL: /pages/{fileid}
-- Method: GET
--
showPage :: Kontrakcja m => FileID -> Int -> m Response
showPage fid pageNo = logFile fid $ do
  logInfo_ "Checking file access"
  checkFileAccess fid
  pixelwidth <- guardJustM $ readField "pixelwidth"
  let clampedPixelWidth = min 2000 (max 100 pixelwidth)
  fileData <- getFileIDContents fid
  rp       <- renderPage fileData pageNo clampedPixelWidth
  case rp of
    Just pageData ->
      return
        . setHeaderBS "Cache-Control" "max-age=604800"
        . toResponseBS "image/png"
        $ BSL.fromStrict pageData
    Nothing -> do
      logAttention "Rendering PDF page failed" $ object ["page" .= show pageNo]
      internalError

-- | Preview when authorized user is logged in (without magic hash)
showPreview :: Kontrakcja m => DocumentID -> FileID -> m InternalKontraResponse
showPreview did fid = logDocumentAndFile did fid $ withUser $ \_ -> do
  pixelwidth <- fromMaybe 150 <$> readField "pixelwidth"
  let clampedPixelWidth = min 2000 (max 100 pixelwidth)
  void $ getDocByDocID did
  if fid == unsafeFileID 0
    then do
      emptyPreview <- liftIO $ BS.readFile "frontend/app/img/empty-preview.jpg"
      return . internalResponse . toResponseBS "image/jpeg" $ BSL.fromStrict emptyPreview
    else do
      checkFileAccessWith fid Nothing Nothing (Just did) Nothing
      internalResponse <$> previewResponse fid clampedPixelWidth

-- | Preview from mail client with magic hash
showPreviewForSignatory
  :: Kontrakcja m
  => DocumentID
  -> SignatoryLinkID
  -> Maybe MagicHash
  -> FileID
  -> m Response
showPreviewForSignatory did slid mmh fid = logDocumentAndFile did fid $ do
  case mmh of
    Nothing -> showActualPreviewForSignatoryWithFileAccessCheck
    Just mh -> do
      hasAccess <- dbQuery $ CheckIfMagicHashIsValid did slid mh
      if hasAccess
        then showActualPreviewForSignatoryWithFileAccessCheck
        else showFallbackImage
  where
    showActualPreviewForSignatoryWithFileAccessCheck = do
      checkFileAccessWith fid (Just slid) mmh (Just did) Nothing
      pixelwidth <- fromMaybe 150 <$> readField "pixelwidth"
      let clampedPixelWidth = min 2000 (max 100 pixelwidth)
      previewResponse fid clampedPixelWidth
    showFallbackImage = do
      imgData <- liftIO . BS.readFile $ "files/images/no_preview.png"
      return $ toResponseBS "image/png" $ BSL.fromStrict imgData

previewResponse :: Kontrakcja m => FileID -> Int -> m Response
previewResponse fid pixelwidth = do
  let clampedPixelWidth = min 2000 (max 100 pixelwidth)
  fileData <- getFileIDContents fid
  rp       <- renderPage fileData 1 clampedPixelWidth
  case rp of
    Just pageData -> return $ toResponseBS "image/png" $ BSL.fromStrict pageData
    Nothing       -> do
      logAttention_ "Rendering PDF preview failed"
      internalError

handleDownloadClosedFile
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> Text -> m Response
handleDownloadClosedFile did sid mh _nameForBrowser = do
  doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh
  guardThatDocumentIsReadableBySignatories doc
  if isClosed doc
    then do
      file    <- guardJustM $ fileFromMainFile $ documentsealedfile doc
      content <- getFileIDContents $ fileid file
      return $ respondWithPDF True content
    else respond404

handleResend :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m ()
handleResend docid signlinkid = guardLoggedInOrThrowInternalError $ do
  getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid `withDocumentM` do
    signlink      <- guardJust . getSigLinkFor signlinkid =<< theDocument
    customMessage <- fmap T.strip <$> getField "customtext"
    actor         <- guardJustM $ fmap mkAuthorActor getContext
    void $ sendReminderEmail customMessage actor False signlink
    return ()

handlePadList :: Kontrakcja m => m Response
handlePadList = do
  ctx <- getContext
  ad  <- getAnalyticsData
  case getContextUser ctx of
    Just _  -> simpleHtmlResponse =<< pageDocumentPadList ctx ad
    Nothing -> simpleHtmlResponse =<< pageDocumentPadListLogin ctx ad

handleToStart :: Kontrakcja m => m Response
handleToStart = do
  ctx <- getContext
  ad  <- getAnalyticsData
  case ctxMaybeUser ctx of
    Just _  -> simpleHtmlResponse =<< pageDocumentToStartList ctx ad
    Nothing -> simpleHtmlResponse =<< pageDocumentToStartLogin ctx ad

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

  msid   <- readField "signatory_id"
  mdid   <- readField "document_id"
  mattid <- readField "attachment_id"

  checkFileAccessWith fid msid Nothing mdid mattid

checkFileAccessWith
  :: Kontrakcja m
  => FileID
  -> Maybe SignatoryLinkID
  -> Maybe MagicHash
  -> Maybe DocumentID
  -> Maybe AttachmentID
  -> m ()
checkFileAccessWith fid msid mmh mdid mattid = case (msid, mdid, mattid) of
  (Just slid, Just did, _) -> do
    (validSession, doc) <- case mmh of
      Just mh -> do
        doc' <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh
        return (True, doc')
      Nothing -> do
        sid  <- ctxSessionID <$> getContext
        doc' <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkID did slid
        vs   <- dbQuery $ CheckDocumentSession sid slid
        return (vs, doc')
    if validSession
      then do -- Valid magic hash or session
        guardThatDocumentIsReadableBySignatories doc
        sl <- guardJust $ getSigLinkFor slid doc
        whenM (signatoryNeedsToIdentifyToView sl doc) $ do
          -- If document is not closed, author never needs to identify to
          -- view. However if it's closed, then he might need to.
          when (isClosed doc || not (isAuthor sl)) $ do
            internalError
        checkFileInDocument did
      else guardLoggedInOrThrowInternalError $ do
        _doc <- getDocByDocID did
        checkFileInDocument did
  (_, Just did, _) -> guardLoggedInOrThrowInternalError $ do
    _doc <- getDocByDocID did
    checkFileInDocument did
  (_, _, Just attid) -> guardLoggedInOrThrowInternalError $ do
    user <- guardJustM $ ctxMaybeUser <$> getContext
    atts <- dbQuery $ GetAttachments
      [ AttachmentsSharedInUsersUserGroup (userid user)
      , AttachmentsOfAuthorDeleteValue (userid user) True
      , AttachmentsOfAuthorDeleteValue (userid user) False
      ]
      [AttachmentFilterByID attid, AttachmentFilterByFileID fid]
      []
    when (length atts /= 1) $ internalError
  _ -> internalError

  where
    checkFileInDocument :: Kontrakcja m => DocumentID -> m ()
    checkFileInDocument did = do
      indoc <- dbQuery $ FileInDocument did fid
      when (not indoc) $ internalError

prepareEmailPreview :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
prepareEmailPreview docid slid = do
  mailtype <- getField' "mailtype"
  content  <- flip E.catch (\(E.SomeException _) -> return "") $ case mailtype of
    "remind" -> do
      doc <- getDocByDocID docid
      let Just sl = getSigLinkFor slid doc
      mailattachments <- makeMailAttachments doc True
      mailDocumentRemindContent Nothing doc sl (not (null mailattachments))
    "invite" -> do
      doc <- getDocByDocID docid
      mailInvitationContent False Sign Nothing doc
    "confirm" -> do
      doc <- getDocByDocID docid
      mailClosedContent True doc
    _ -> fail "prepareEmailPreview"
  J.runJSONGenT $ J.value "content" $ T.unpack content

handleShowAfterForward :: Kontrakcja m => DocumentID -> m Response
handleShowAfterForward did =
  withAnonymousContext $ simpleHtmlResponse =<< afterForwardPage did

-- GuardTime verification page. This can't be external since its a
-- page in our system.  withAnonymousContext so the verify page looks
-- like the user is not logged in (e.g. for default footer & header)
handleShowVerificationPage :: Kontrakcja m => m Response
handleShowVerificationPage = withAnonymousContext gtVerificationPage

handleVerify :: Kontrakcja m => m JSValue
handleVerify = do
  fileinput <- getDataFn' (lookInput "file")
  filepath  <- case fileinput of
    Just (Input (Left  filepath) _ _) -> return filepath
    Just (Input (Right content ) _ _) -> liftIO $ do
      systmp         <- getTemporaryDirectory
      (pth, fhandle) <- openTempFile systmp "vpath.pdf"
      BSL.hPutStr fhandle content
      return pth
    _ -> internalError
  ctx <- getContext
  J.toJSValue <$> GuardTime.verify (ctxGtConf ctx) filepath

handleMarkAsSaved :: Kontrakcja m => DocumentID -> m JSValue
handleMarkAsSaved docid = guardLoggedInOrThrowInternalError $ do
  getDocByDocID docid `withDocumentM` do
    whenM (isPreparation <$> theDocument) $ dbUpdate (SetDocumentUnsavedDraft False)
    J.runJSONGenT $ return ()


handleNewDocumentWithBPID :: Kontrakcja m => m InternalKontraResponse
handleNewDocumentWithBPID = withUser $ \user -> do
  actor <- guardJustM $ mkAuthorActor <$> getContext
  bpid  <- fromMaybe "" <$> getField "bpid"
  ugwp  <- dbQuery . UserGroupGetWithParentsByUserID $ userid user
  docid <- handleNewDocument' user ugwp
  withDocumentID docid $ do
    let tag = DocumentTag { tagname = "bpid", tagvalue = bpid }
    draftData <- theDocument >>= \doc -> return $ doc
      { documenttags      = S.insert tag $ documenttags doc
      , documentshowarrow = False
      }
    applyDraftDataToDocument draftData actor
  return . internalResponse . LinkIssueDoc $ docid

-- | Add some event as signatory if this signatory has not signed yet,
-- and document is pending
addEventForVisitingSigningPageIfNeeded
  :: (Kontrakcja m, DocumentMonad m) => CurrentEvidenceEventType -> SignatoryLink -> m ()
addEventForVisitingSigningPageIfNeeded ev sl = do
  ctx <- getContext
  doc <- theDocument
  when (isPending doc && isSignatoryAndHasNotSigned sl) $ do
    updateMTimeAndObjectVersion $ ctxTime ctx
    void
      $   dbUpdate
      .   InsertEvidenceEventWithAffectedSignatoryAndMsg ev (return ()) (Just sl) Nothing
      =<< signatoryActor ctx sl

guardThatDocumentIsReadableBySignatories :: Kontrakcja m => Document -> m ()
guardThatDocumentIsReadableBySignatories doc =
  unless (isAccessibleBySignatories doc) respondLinkInvalid
