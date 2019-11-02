module Doc.Action (
    postDocumentPreparationChange
  , postDocumentRejectedChange
  , postDocumentForwardChange
  , postDocumentCanceledChange
  , postDocumentPendingChange
  , postDocumentClosedActions
  , findAndTimeoutDocuments
  ) where

import Control.Conditional (ifM, unlessM, whenM)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG
import Log
import Text.StringTemplates.Templates (TemplatesMonad)

import BrandedDomain.Model
import Chargeable.Model
import CronEnv
import DB
import DB.TimeZoneName
import Doc.API.Callback.Model
import Doc.AutomaticReminder.Model
import Doc.DigitalSignature (addDigitalSignature)
import Doc.DocInfo
import Doc.DocMails
  ( sendClosedEmails, sendDocumentErrorEmail, sendDocumentTimeoutedEmail
  , sendForwardSigningMessages, sendInvitationEmails
  , sendPartyProcessFinalizedNotification, sendRejectEmails )

import Doc.DocSeal (sealDocument)
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Extending.Model
import Doc.Logging
import Doc.Model
import Doc.Sealing.Model
import Doc.SealStatus (SealStatus(..), hasGuardtimeSignature)
import Doc.SignatoryLinkID
import Doc.Signing.Model ()
import Doc.Types.SignatoryAccessToken
import File.Storage
import GuardTime (GuardTimeConfMonad)
import Kontra
import Log.Identifier
import MailContext
import PdfToolsLambda.Conf
import Templates (runTemplatesT)
import ThirdPartyStats.Core
import ThirdPartyStats.Planhat
import User.Email
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import qualified MailContext.Internal as I

-- | Log a document event, adding some standard properties.
logDocEvent
  :: (MonadDB m, MonadThrow m, MonadTime m)
  => EventName
  -> User
  -> [EventProperty]
  -> Document
  -> m ()
logDocEvent name user extraProps doc = do
  ug  <- dbQuery . UserGroupGetByUserID . userid $ user
  now <- currentTime
  let
    uid      = userid user
    email    = Email $ getEmail user
    fullname = getFullName user
    deliverymethod =
      fromMaybe "undefined"
        $   showt
        .   signatorylinkdeliverymethod
        <$> getSigLinkFor uid doc
  asyncLogEvent
    name
    (  extraProps
    ++ [ UserIDProp uid
       , DocIDProp (documentid doc)
       , TimeProp now
       , MailProp email
       , NameProp fullname
       , stringProp "Company Name" $ ugName ug
       , stringProp "Delivery Method" deliverymethod
       , stringProp "Type"            (showt $ documenttype doc)
       , stringProp "Language"        (showt $ documentlang doc)
       , numProp "Days to sign" (fromIntegral $ documentdaystosign doc)
       , numProp "Signatories"  (fromIntegral $ length $ documentsignatorylinks doc)
       , stringProp "Signup Method" (showt $ usersignupmethod user)
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
  updateDocument $ \_ -> initialiseSignatoryAPIMagicHashes
  msaved <- saveDocumentForSignatories
  case msaved of
    Just msg -> do
      logAttention "Failed to save document for signatories" $ object ["error" .= msg]
    Nothing -> return ()
  theDocument >>= \d -> logInfo "Sending invitation emails for document" $ logObject_ d

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

  sendInvitationEmails authorsignsimmediately
  theDocument >>= \d -> setAutomaticReminder (documentid d) (documentdaystoremind d) tzn
  return ()
  where
    userMixpanelData author time =
      [UserIDProp (userid author), someProp "Last Doc Sent" time]

initialiseSignatoryAPIMagicHashes :: (DocumentMonad m, Kontrakcja m) => m ()
initialiseSignatoryAPIMagicHashes = do
  sls <- documentsignatorylinks <$> theDocument
  forM_ sls $ \sl -> when (signatorylinkdeliverymethod sl == APIDelivery) $ do
    mh <- random
    void $ dbUpdate $ NewSignatoryAccessTokenWithHash (signatorylinkid sl)
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
     )
  => Document
  -> SignatoryLink
  -> m ()
postDocumentPendingChange olddoc signatoryLink = do
  unlessM (isPending <$> theDocument)
    $   theDocument
    >>= stateMismatchError "postDocumentPendingChange" Pending

  dbUpdate $ ChargeUserGroupForClosingSignature (documentid olddoc)

  ifM
      (allPartiesSignedOrApproved <$> theDocument)
  {-then-}(do
            document <- theDocument
            logInfo "All have signed, document will be closed" $ logObject_ document
            time <- view #time <$> getMailContext
            dbUpdate $ CloseDocument (systemActor time)
            dbUpdate $ ChargeUserGroupForClosingDocument $ documentid olddoc
            when (documentfromshareablelink olddoc) $ do
              dbUpdate $ ChargeUserGroupForShareableLink $ documentid olddoc
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
            dbUpdate
              .   ScheduleDocumentSealing
              .   view (#brandedDomain % #id)
              =<< getMailContext
            if signatorylinkconfirmationdeliverymethod signatoryLink == NoConfirmationDelivery
              then sendPartyProcessFinalizedNotification document signatoryLink
              else return ()
          )
  {-else-}$ do
              document <- theDocument
              theDocument >>= triggerAPICallbackIfThereIsOne
              whenM
                  (   ((/=) (documentcurrentsignorder olddoc) . documentcurrentsignorder)
                  <$> theDocument
                  )
                $ do
                    theDocument >>= \d -> logInfo "Resending invitation emails" $ logObject_ d
                    sendInvitationEmails False
              sendPartyProcessFinalizedNotification document signatoryLink
  where
    allPartiesSignedOrApproved =
      all
          (  (isSignatory --> isSignatoryAndHasSigned)
          && (isApprover --> isApproverAndHasApproved)
          )
        . documentsignatorylinks
    userMixpanelData author time =
      [UserIDProp (userid author), someProp "Last Doc Closed" time]

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
        sealDocument $ mcxt ^. mctxDomainUrl

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

    unlessM (hasGuardtimeSignature <$> theDocument) $ do
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
          && hasGuardtimeSignature d
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
  authorid <- guardJust $ getAuthorSigLink doc >>= maybesignatory
  dbQuery $ GetUserByID authorid

{- |
    Goes through each signatory, and if a user exists this saves it for that user
    by linking the signatory to the user's account.
-}
saveDocumentForSignatories :: (Kontrakcja m, DocumentMonad m) => m (Maybe Text)
saveDocumentForSignatories =
  documentsignatorylinks <$> theDocument >>= foldM foldSaveForSig Nothing . filter
    (not . isAuthor)
  where
    {- |
        Wraps up the saveDocumentForSignatory so we can use it in a fold
    -}
    foldSaveForSig
      :: (Kontrakcja m, DocumentMonad m) => Maybe Text -> SignatoryLink -> m (Maybe Text)
    foldSaveForSig (Just msg) _       = return $ Just msg
    foldSaveForSig Nothing    siglink = saveDocumentForSignatory siglink
    {- |
        Saves the document for the given signatorylink.  It does this by checking to see
        if there is a user with a matching email, and if there is it hooks up the signatory
        link to that user.
    -}
    saveDocumentForSignatory
      :: (Kontrakcja m, DocumentMonad m) => SignatoryLink -> m (Maybe Text)
    saveDocumentForSignatory sl = do
      let sigemail = getEmail sl
      muser <- case (sigemail) of
        "" -> return Nothing
        _  -> dbQuery $ GetUserByEmail (Email sigemail)
      case muser of
        Nothing   -> return Nothing
        Just user -> do
          res <- dbUpdate $ SaveDocumentForUser user (signatorylinkid sl)
          if res then return Nothing else return $ Just "saveDocumentForSignatory failed"

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
  noreplyAddress <- ceMailNoreplyAddress <$> ask
  gt             <- asks ceTemplates
  docs           <- dbQuery $ GetTimeoutedButPendingDocumentsChunk now 10
  forM_ docs $ flip withDocument $ do
    lang    <- documentlang <$> theDocument
    mauthor <- getDocAuthor =<< theDocument
    runTemplatesT (lang, gt) $ dbUpdate $ TimeoutDocument (systemActor now)
    case mauthor of
      Nothing     -> return ()
      Just author -> do
        ugwp <- guardJustM $ dbQuery $ UserGroupGetWithParents (usergroupid author)
        bd   <- dbQuery $ GetBrandedDomainByUserID (userid author)
        let mc = I.MailContext { lang               = lang
                               , brandedDomain      = bd
                               , time               = now
                               , mailNoreplyAddress = noreplyAddress
                               }
        runTemplatesT (lang, gt) . runMailContextT mc $ do
          when (ugwpSettings ugwp ^. #ugsSendTimeoutNotification) $ do
            sendDocumentTimeoutedEmail =<< theDocument
    triggerAPICallbackIfThereIsOne =<< theDocument
    logInfo_ "Document timed out"
  unless (null docs) $ do
    commit
    findAndTimeoutDocuments
