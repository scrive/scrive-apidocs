module Doc.Action (
    postDocumentPreparationChange
  , postDocumentRejectedChange
  , postDocumentForwardChange
  , postDocumentCanceledChange
  , postDocumentPendingChange
  , postDocumentClosedActions
  , findAndTimeoutDocuments
  , commonDocumentClosingActions
  , saveDocumentForPortalSignatories
  , saveDocumentForSignatory
  ) where

import Control.Conditional (unlessM, whenM)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG
import Log
import Text.StringTemplates.Templates (TemplatesMonad)

import BrandedDomain.Model
import Chargeable
import CronEnv
import DB
import DB.TimeZoneName
import Doc.API.Callback.Model
import Doc.AutomaticReminder.Model
import Doc.DigitalSignature (addDigitalSignature)
import Doc.DocInfo
import Doc.DocMails
  ( PortalMailKind(..), sendClosedEmails, sendDocumentErrorEmail
  , sendDocumentTimeoutedEmail, sendForwardSigningMessages, sendInvitationEmails
  , sendPartyProcessFinalizedNotification, sendPortalMail, sendRejectEmails
  )
import Doc.DocSeal (sealDocument)
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Extending.Model
import Doc.Logging
import Doc.Model
import Doc.Sealing.Model
import Doc.SealStatus (SealStatus(..))
import Doc.SignatoryLinkID
import Doc.Signing.Model ()
import Doc.Types.SignatoryAccessToken
import EventStream.Class
import File.Storage
import Flow.Model
import GuardTime (GuardTimeConfMonad)
import Kontra
import Log.Identifier
import MailContext
import PdfToolsLambda.Class
import Templates (runTemplatesT)
import ThirdPartyStats.Core
import ThirdPartyStats.Planhat
import User.Action
import User.Email
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import qualified Flow.Model as Flow (selectInstanceIdByDocumentId)
import qualified MailContext.Internal

-- | Log a document event, adding some standard properties.
logDocEvent
  :: (MonadDB m, MonadThrow m, MonadTime m)
  => EventName
  -> User
  -> [EventProperty]
  -> Document
  -> m ()
logDocEvent name user extraProps doc = do
  ug  <- dbQuery . UserGroupGetByUserID $ user ^. #id
  now <- currentTime
  let uid      = user ^. #id
      email    = Email $ getEmail user
      fullname = getFullName user
      deliverymethod =
        maybe "undefined" (showt . signatorylinkdeliverymethod) (getSigLinkFor uid doc)
  asyncLogEvent
    name
    (  extraProps
    ++ [ UserIDProp uid
       , DocIDProp (documentid doc)
       , TimeProp now
       , MailProp email
       , NameProp fullname
       , stringProp "Company Name" $ ug ^. #name
       , stringProp "Delivery Method" deliverymethod
       , stringProp "Type"            (showt $ documenttype doc)
       , stringProp "Language"        (showt $ documentlang doc)
       , numProp "Days to sign" (fromIntegral $ documentdaystosign doc)
       , numProp "Signatories"  (fromIntegral . length $ documentsignatorylinks doc)
       , stringProp "Signup Method" (showt $ user ^. #signupMethod)
       ]
    )
    EventMixpanel

postDocumentPreparationChange
  :: (Kontrakcja m, DocumentMonad m) => Bool -> TimeZoneName -> m ()
postDocumentPreparationChange authorsignsimmediately tzn = do
  triggerAPICallbackIfThereIsOne =<< theDocument
  unlessM (isPending <$> theDocument)
    $   theDocument
    >>= stateMismatchError "postDocumentPreparationChange" Pending
  logInfo_ "Preparation -> Pending; Sending invitation emails"
  updateDocument $ const initialiseSignatoryAPIMagicHashes
  saveDocumentForSignatories

  -- Stat logging
  now     <- currentTime
  mauthor <- getDocAuthor =<< theDocument
  case mauthor of
    Nothing     -> return ()
    Just author -> do
      asyncLogEvent SetUserProps
                    (simplePlanhatAction "Document started" author now)
                    EventPlanhat
      asyncLogEvent SetUserProps (userMixpanelData author now) EventMixpanel
      theDocument >>= logDocEvent "Doc Sent" author []

  did   <- documentid <$> theDocument
  mFlow <- selectInstanceIdByDocumentId did
  when (isNothing mFlow) $ do
    theDocument >>= \d -> logInfo "Sending invitation emails for document" $ logObject_ d
    sendInvitationEmails authorsignsimmediately
    theDocument >>= \d -> setAutomaticReminder (documentid d) (documentdaystoremind d) tzn
  where
    userMixpanelData author time =
      [UserIDProp (author ^. #id), someProp "Last Doc Sent" time]

initialiseSignatoryAPIMagicHashes :: (DocumentMonad m, Kontrakcja m) => m ()
initialiseSignatoryAPIMagicHashes = do
  sls <- documentsignatorylinks <$> theDocument
  forM_ sls $ \sl -> when (signatorylinkdeliverymethod sl == APIDelivery) $ do
    mh <- random
    void . dbUpdate $ NewSignatoryAccessTokenWithHash (signatorylinkid sl)
                                                      SignatoryAccessTokenForAPI
                                                      Nothing
                                                      mh

postDocumentRejectedChange
  :: Kontrakcja m => SignatoryLinkID -> Maybe Text -> Document -> m ()
postDocumentRejectedChange siglinkid customMessage doc@Document {..} =
  logDocument documentid $ do
    triggerAPICallbackIfThereIsOne doc
    unless (isRejected doc) $ stateMismatchError "postDocumentRejectedChange" Rejected doc
    logInfo_ "Pending -> Rejected; send reject emails"
    logInfo "Sending rejection emails for document" $ logObject_ doc
    ctx <- getContext
    -- Log the fact that the current user rejected a document.
    maybe (return ())
          (\user -> logDocEvent "Doc Rejected" user [] doc)
          (ctx ^. #maybeUser)
    sendRejectEmails customMessage (fromJust $ getSigLinkFor siglinkid doc) doc
    return ()

postDocumentForwardChange
  :: Kontrakcja m => Maybe Text -> SignatoryLink -> SignatoryLinkID -> Document -> m ()
postDocumentForwardChange customMessage originalSignatory newslid doc =
  logDocument (documentid doc) $ do
    let newsl = fromJust (getSigLinkFor newslid doc)
    triggerAPICallbackIfThereIsOne doc
    unless (isPending doc) $ stateMismatchError "originalSignatory" Pending doc
    logInfo "Sending forward emails for document" $ logObject_ doc
    sendForwardSigningMessages customMessage originalSignatory newsl doc
    return ()


postDocumentCanceledChange :: Kontrakcja m => Document -> m ()
postDocumentCanceledChange doc@Document {..} = logDocument documentid $ do
  triggerAPICallbackIfThereIsOne doc
  unless (isCanceled doc) $ stateMismatchError "postDocumentCanceledChange" Canceled doc
  logInfo_ "Pending -> Canceled"
  mauthor <- getDocAuthor doc
  case mauthor of
    Nothing     -> return ()
    Just author -> logDocEvent "Doc Canceled" author [] doc


-- | After a party has signed or approved - check if we need to close
-- document and take further actions.
postDocumentPendingChange
  :: ( CryptoRNG m
     , TemplatesMonad m
     , MonadFileStorage m
     , MonadBaseControl IO m
     , DocumentMonad m
     , MonadMask m
     , MonadLog m
     , MonadIO m
     , MailContextMonad m
     , GuardTimeConfMonad m
     , MonadEventStream m
     )
  => Document
  -> SignatoryLink
  -> m ()
postDocumentPendingChange olddoc signatoryLink = do
  unlessM (isPending <$> theDocument)
    $   theDocument
    >>= stateMismatchError "postDocumentPendingChange" Pending

  chargeForItemSingle CIClosingSignature $ documentid olddoc

  allSignedOrApproved <- allPartiesSignedOrApproved <$> theDocument
  maybeFlowInstanceId <- Flow.selectInstanceIdByDocumentId $ documentid olddoc
  case maybeFlowInstanceId of
    Just instanceId -> do
      logInfo "Document is part of the flow."
        $ object [logPair_ instanceId, logPair_ olddoc]
      unless allSignedOrApproved $ theDocument >>= triggerAPICallbackIfThereIsOne
    Nothing -> do
      if allSignedOrApproved
        then do
          commonDocumentClosingActions olddoc
          document <- theDocument
          when
              (  signatorylinkconfirmationdeliverymethod signatoryLink
              == NoConfirmationDelivery
              )
            $ sendPartyProcessFinalizedNotification document signatoryLink
        else do
          document <- theDocument
          theDocument >>= triggerAPICallbackIfThereIsOne
          whenM
              (   (/=) (documentcurrentsignorder olddoc)
              .   documentcurrentsignorder
              <$> theDocument
              )
            $ do
                theDocument >>= \d -> logInfo "Resending invitation emails" $ logObject_ d
                sendInvitationEmails False
                sendInbetweenPortalInvitations
          sendPartyProcessFinalizedNotification document signatoryLink
  where
    allPartiesSignedOrApproved =
      all
          (  (isSignatory --> isSignatoryAndHasSigned)
          && (isApprover --> isApproverAndHasApproved)
          )
        . documentsignatorylinks

-- | Common document closing actions used in the standard single-document
--   workflow and the multi-document Flow action consumers
commonDocumentClosingActions
  :: ( CryptoRNG m
     , TemplatesMonad m
     , MonadFileStorage m
     , MonadBaseControl IO m
     , DocumentMonad m
     , MonadMask m
     , MonadLog m
     , MonadIO m
     , MailContextMonad m
     , GuardTimeConfMonad m
     , MonadEventStream m
     )
  => Document
  -> m ()
commonDocumentClosingActions olddoc = do
  document <- theDocument
  logInfo "All have signed, document will be closed" $ logObject_ document
  time <- view #time <$> getMailContext
  dbUpdate $ CloseDocument (systemActor time)
  chargeForItemSingle CIClosingDocument $ documentid olddoc
  when (documentfromshareablelink olddoc)
    . chargeForItemSingle CIShareableLink
    $ documentid olddoc
  mauthor <- getDocAuthor document
  case mauthor of
    Nothing     -> return ()
    Just author -> do
      logDocEvent "Doc Closed" author [] document
      -- report
      now <- currentTime
      asyncLogEvent SetUserProps
                    (simplePlanhatAction "Document closed" author now)
                    EventPlanhat
      asyncLogEvent SetUserProps (userMixpanelData author now) EventMixpanel
  dbUpdate . ScheduleDocumentSealing . view (#brandedDomain % #id) =<< getMailContext
  where
    userMixpanelData author time =
      [UserIDProp (author ^. #id), someProp "Last Doc Closed" time]

-- | Prepare final PDF if needed, apply digital signature, and send
-- out confirmation emails if there has been a change in the seal status.  Precondition: document must be
-- closed or in error.
postDocumentClosedActions
  :: ( TemplatesMonad m
     , MailContextMonad m
     , CryptoRNG m
     , MonadIO m
     , MonadLog m
     , MonadFileStorage m
     , MonadBaseControl IO m
     , MonadMask m
     , DocumentMonad m
     , GuardTimeConfMonad m
     , PdfToolsLambdaMonad m
     , MonadEventStream m
     )
  => Bool -- ^ Commit to DB after we have sealed the document
  -> Bool -- ^ Prepare final PDF even if it has already been prepared
  -> m Bool
postDocumentClosedActions commitAfterSealing forceSealDocument = do
  mcxt <- getMailContext
  doc0 <- theDocument

  unlessM ((isClosed || isDocumentError) <$> theDocument) internalError

  dbUpdate . ExtendSignatoryAccessTokensForAccessBeforeClosing =<< theDocumentID

  whenM ((\d -> forceSealDocument || isNothing (documentsealedfile d)) <$> theDocument)
    $ do

        whenM (isDocumentError <$> theDocument) $ do
          currentTime >>= dbUpdate . FixClosedErroredDocument . systemActor

        logInfo_ "Running sealDocument"
        sealDocument $ mcxt ^. #brandedDomain % #url

        -- Here there is a race condition: when we commit, other callers
        -- of postDocumentClosedActions may see a document that lacks a
        -- digital signature, and may attempt to send incorrect
        -- correction mail.  Consider adding state to keep track of what
        -- mail we actually send out, after design of adding digital
        -- signature to appendices.

        when commitAfterSealing commit -- so that post-sign view can render pages as soon as possible

  whenM ((\d -> isDocumentError d && not (isDocumentError doc0)) <$> theDocument) $ do
    mauthor <- getDocAuthor =<< theDocument
    case mauthor of
      Nothing     -> return ()
      Just author -> do
        logInfo "Sending seal error emails" $ logObject_ doc0
        sendDocumentErrorEmail author =<< theDocument
    theDocument >>= triggerAPICallbackIfThereIsOne

  unlessM (isDocumentError <$> theDocument) $ do
    unlessM (hasDigitalSignature <$> theDocument) $ do
      unlessM addDigitalSignature $ do
        internalError


    whenM ((\d -> documentsealstatus doc0 /= documentsealstatus d) <$> theDocument) $ do

      sealFixed <- theDocument >>= \d ->
        return
          $  documentsealstatus doc0
          == Just Missing
                        -- document was already pdf-sealed, but without
                        -- digital signature, that means we sent out an
                        -- earlier confirmation mail without a digital
                        -- seal in the attached document.
          && hasDigitalSignature d
                        -- and now it has a digital signature, so we
                        -- fixed something and we will indicate that
                        -- this is a correction to the earlier
                        -- confirmation mail.
      theDocument >>= sendClosedEmails sealFixed
      theDocument >>= triggerAPICallbackIfThereIsOne

  resultisok <-
    (\d -> documentsealstatus d /= Just Missing && documentstatus d == Closed)
      <$> theDocument

  when resultisok $ do
    whenM (hasGuardtimeSignature <$> theDocument) $ do
      now <- currentTime
      theDocument >>= \d -> dbUpdate $ ScheduleDocumentExtending (documentid d) now

  -- Sealing or signing of document could have failed. We need to tell the caller,
  -- so that the action can be re-scheduled
  return resultisok

stateMismatchError
  :: (MonadBase IO m, MonadLog m) => Text -> DocumentStatus -> Document -> m a
stateMismatchError funame expected doc = do
  logInfo "State mismatch error"
    $ object ["function" .= funame, "expected_status" .= show expected, logPair_ doc]
  internalError

getDocAuthor :: (MonadDB m, MonadThrow m, MonadBase IO m) => Document -> m (Maybe User)
getDocAuthor doc = do
  authorid <- guardJust $ getAuthorUserId doc
  dbQuery $ GetUserByID authorid

{- |
    Goes through each signatory, and if a user exists this saves it for that user
    by linking the signatory to the user's account.
-}
saveDocumentForSignatories :: (Kontrakcja m, DocumentMonad m) => m ()
saveDocumentForSignatories =
  documentsignatorylinks <$> theDocument >>= mapM_ saveDocumentForSignatory . filter
    (not . isAuthor)

saveDocumentForPortalSignatories :: (Kontrakcja m, DocumentMonad m) => m ()
saveDocumentForPortalSignatories = do
  sigs <- documentsignatorylinks <$> theDocument
  let portalSigFilter =
        (not . isAuthor) && (\sl -> signatorylinkdeliverymethod sl == PortalDelivery)
  mapM_ saveDocumentForSignatory $ filter portalSigFilter sigs

-- | Invite portal signatories that have just got activated to sign/approve/view
-- the document. Triggered for signatories of signing order 2 upwards.
sendInbetweenPortalInvitations
  :: ( CryptoRNG m
     , MonadThrow m
     , MonadLog m
     , TemplatesMonad m
     , DocumentMonad m
     , MailContextMonad m
     , MonadBase IO m
     )
  => m ()
sendInbetweenPortalInvitations = do
  mauthor    <- getDocAuthor =<< theDocument
  mPortalUrl <- case mauthor of
    Nothing     -> return Nothing
    Just author -> do
      ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ author ^. #id
      return $ ugwpSettings ugwp ^. #portalUrl

  case (mauthor, mPortalUrl) of
    (Just author, Just portalUrl) -> do
      doc <- theDocument
      -- Note that 'nubbing' the signatories here is probably unneccessary, but
      -- we still do it for consistency.
      let signatoriesWhoNeedInviting = nubPortalSignatories
            [ sl
            | sl <- documentsignatorylinks doc
            , signatorylinkdeliverymethod sl == PortalDelivery  -- is portal signatory
            , not $ isForwarded sl  -- redundant, but here for clarity
            , signatorysignorder sl <= documentcurrentsignorder doc  -- is actived
            , documentprevioussignorder doc < signatorysignorder sl
            ]  -- wasn't previously activated

      forM_ signatoriesWhoNeedInviting $ \sl -> do
        uctx <- getCreateUserContextWithoutContext
        sendPortalMail PortalInvitation author portalUrl sl uctx
        saveDocumentForSignatory sl

    _ -> return ()

{- |
    Saves the document for the given signatorylink.  It does this by checking to see
    if there is a user with a matching email, and if there is it hooks up the signatory
    link to that user.
-}
saveDocumentForSignatory
  :: ( CryptoRNG m
     , MonadThrow m
     , MonadLog m
     , TemplatesMonad m
     , DocumentMonad m
     , MailContextMonad m
     , MonadBase IO m
     )
  => SignatoryLink
  -> m ()
saveDocumentForSignatory sl = do
  let sigemail = getEmail sl
  muser <- case sigemail of
    "" -> return Nothing
    _  -> dbQuery $ GetUserByEmail (Email sigemail)
  whenJust muser $ \user -> do
    dbUpdate $ SaveDocumentForUser user (signatorylinkid sl)

-- | Time out documents once per day after midnight.  Do it in chunks
-- so that we don't choke the server in case there are many documents to time out
findAndTimeoutDocuments
  :: ( MonadBaseControl IO m
     , MonadReader CronEnv m
     , MonadIO m
     , MonadDB m
     , MonadCatch m
     , MonadLog m
     , CryptoRNG m
     )
  => m ()
findAndTimeoutDocuments = do
  now            <- currentTime
  noreplyAddress <- asks ceMailNoreplyAddress
  gt             <- asks ceTemplates
  docs           <- dbQuery $ GetTimeoutedButPendingDocumentsChunk now 10
  forM_ docs . flip withDocument $ do
    lang    <- documentlang <$> theDocument
    mauthor <- getDocAuthor =<< theDocument
    runTemplatesT (lang, gt) . dbUpdate $ TimeoutDocument (systemActor now)
    case mauthor of
      Nothing     -> return ()
      Just author -> do
        ugwp <- guardJustM . dbQuery $ UserGroupGetWithParents (author ^. #groupID)
        bd   <- dbQuery $ GetBrandedDomainByUserID (author ^. #id)
        let mc = MailContext { lang               = lang
                             , brandedDomain      = bd
                             , time               = now
                             , mailNoreplyAddress = noreplyAddress
                             }
        runTemplatesT (lang, gt) . runMailContextT mc $ do
          when (ugwpSettings ugwp ^. #sendTimeoutNotification) $ do
            sendDocumentTimeoutedEmail =<< theDocument
    triggerAPICallbackIfThereIsOne =<< theDocument
    logInfo_ "Document timed out"
  unless (null docs) $ do
    commit
    findAndTimeoutDocuments
