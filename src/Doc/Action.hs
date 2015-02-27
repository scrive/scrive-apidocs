{-# LANGUAGE NoImplicitPrelude #-}
module Doc.Action (
    postDocumentPreparationChange
  , postDocumentRejectedChange
  , postDocumentCanceledChange
  , postDocumentPendingChange
  , postDocumentClosedActions
  , findAndDoPostDocumentClosedActions
  , findAndExtendDigitalSignatures
  , findAndTimeoutDocuments
  ) where

import Control.Applicative
import Control.Conditional (whenM, unlessM, ifM)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.List hiding (head, tail)
import Data.Maybe hiding (fromJust)
import Data.Time
import Text.StringTemplates.Templates (TemplatesMonad)

import ActionQueue.Scheduler
import Amazon (AmazonMonad)
import AppConf (guardTimeConf)
import Control.Logic
import Crypto.RNG
import DB
import DB.TimeZoneName
import Doc.API.Callback.Model
import Doc.AutomaticReminder.Model
import Doc.DigitalSignature (addDigitalSignature, extendDigitalSignature)
import Doc.DocInfo
import Doc.DocMails (sendInvitationEmails, sendRejectEmails, sendDocumentErrorEmail, sendClosedEmails, runMailTInScheduler)
import Doc.DocSeal (sealDocument)
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID, withDocument)
import Doc.DocUtils
import Doc.Model
import Doc.SealStatus (SealStatus(..), hasGuardtimeSignature)
import Doc.SignatoryLinkID
import ForkAction (forkAction)
import GuardTime (GuardTimeConfMonad, runGuardTimeConfT)
import Kontra
import MailContext (MailContextMonad(..), MailContext(..))
import MinutesTime
import OurPrelude
import Templates (runTemplatesT)
import ThirdPartyStats.Core
import User.Email
import User.Model
import User.Utils
import Util.Actor
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import Utils.Default (defaultValue)
import qualified Log

-- | Log a document event, adding some standard properties.
logDocEvent :: (MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m) => EventName -> User -> [EventProperty] -> Document -> m ()
logDocEvent name user extraProps doc = do
  comp <- getCompanyForUser user
  now <- currentTime
  ip <- mctxipnumber <$> getMailContext
  let uid = userid user
      email = Email $ getEmail user
      fullname = getFullName user
      deliverymethod = fromMaybe "undefined" $ show . signatorylinkdeliverymethod <$> getSigLinkFor uid doc
  asyncLogEvent name $ extraProps ++ [
    UserIDProp uid,
    DocIDProp  (documentid doc),
    TimeProp   now,
    MailProp   email,
    IPProp     ip,
    NameProp   fullname,
    stringProp "Company Name" $ getCompanyName $ comp,
    stringProp "Delivery Method" deliverymethod,
    stringProp "Type" (show $ documenttype doc),
    stringProp "Language" (show $ documentlang doc),
    numProp "Days to sign" (fromIntegral $ documentdaystosign doc),
    numProp "Signatories" (fromIntegral $ length $ documentsignatorylinks doc),
    stringProp "Signup Method" (show $ usersignupmethod user)]

postDocumentPreparationChange :: (Kontrakcja m, DocumentMonad m) => Bool -> TimeZoneName -> m ()
postDocumentPreparationChange authorsignsimmediately tzn = do
  docid <- theDocumentID
  triggerAPICallbackIfThereIsOne =<< theDocument
  unlessM (isPending <$> theDocument) $
    theDocument >>= stateMismatchError "postDocumentPreparationChange" Pending
  Log.mixlog_ $ "Preparation -> Pending; Sending invitation emails: " ++ show docid
  msaved <- saveDocumentForSignatories
  case msaved of
    Just msg -> do
      Log.attention_ $ "Failed to save document #" ++ (show docid) ++ " for signatories " ++ msg
    Nothing -> return ()
  theDocument >>= \d -> Log.mixlog_ $ "Sending invitation emails for document #" ++ show docid ++ ": " ++ documenttitle d

  -- Stat logging
  now <- currentTime
  author <- getDocAuthor =<< theDocument
  docssent <- dbQuery $ GetDocsSent (userid author)
  -- Log the current time as the last doc sent time
  asyncLogEvent SetUserProps [UserIDProp (userid author),
                              someProp "Last Doc Sent" now,
                              numProp "Docs sent" (fromIntegral $ docssent)
                              ]
  theDocument >>= logDocEvent "Doc Sent" author []

  sendInvitationEmails authorsignsimmediately
  scheduleAutoreminderIfThereIsOne tzn =<< theDocument
  return ()

postDocumentRejectedChange :: Kontrakcja m => SignatoryLinkID -> Maybe String -> Document -> m ()
postDocumentRejectedChange siglinkid customMessage doc@Document{..} = do
  triggerAPICallbackIfThereIsOne doc
  unless (isRejected doc) $
    stateMismatchError "postDocumentRejectedChange" Rejected doc
  Log.mixlog_ $ "Pending -> Rejected; send reject emails: " ++ show documentid
  Log.mixlog_ $ "Sending rejection emails for document #" ++ show documentid ++ ": " ++ documenttitle
  ctx <- getContext
  -- Log the fact that the current user rejected a document.
  maybe (return ())
        (\user -> logDocEvent "Doc Rejected" user [] doc)
        (ctxmaybeuser ctx)
  sendRejectEmails customMessage ($(fromJust) $ getSigLinkFor siglinkid doc) doc
  return ()

postDocumentCanceledChange :: Kontrakcja m => Document -> m ()
postDocumentCanceledChange doc@Document{..} = do
  triggerAPICallbackIfThereIsOne doc
  unless (isCanceled doc) $
    stateMismatchError "postDocumentCanceledChange" Canceled doc
  Log.mixlog_ $ "Pending -> Canceled" ++ show documentid
  author <- getDocAuthor doc
  logDocEvent "Doc Canceled" author [] doc


-- | After a party has signed - check if we need to close document and
-- take further actions.
postDocumentPendingChange :: (CryptoRNG m, TemplatesMonad m, AmazonMonad m, MonadBaseControl IO m, DocumentMonad m, MonadMask m, Log.MonadLog m, MonadIO m, KontraMonad m, GuardTimeConfMonad m, MailContextMonad m)
                          => Document -> m ()
postDocumentPendingChange olddoc = do
  unlessM (isPending <$> theDocument) $
    theDocument >>= stateMismatchError "postDocumentPendingChange" Pending

  ifM (allSignatoriesSigned <$> theDocument)
  {-then-} (do
      theDocument >>= \d -> Log.mixlog_ $ "All have signed; " ++ show (documentstatus d) ++ " -> Closed: " ++ show (documentid d)
      time <- ctxtime <$> getContext
      dbUpdate $ CloseDocument (systemActor time)
      author <- theDocument >>= getDocAuthor
      theDocument >>= logDocEvent "Doc Closed" author []
      asyncLogEvent SetUserProps [UserIDProp (userid author),
                                  someProp "Last Doc Closed" time]
      commit -- ... so that the forked thread can see our changes
      theDocument >>= \d -> forkAction ("Sealing document #" ++ show (documentid d) ++ ": " ++ (documenttitle d)) $ do
        -- We fork so that the client can get the response to the
        -- signing request without having to wait for the sealing
        -- operations
        postDocumentClosedActions True False)
  {-else-} $ do
      theDocument >>= triggerAPICallbackIfThereIsOne
      whenM ((\d -> documentcurrentsignorder d /= documentcurrentsignorder olddoc) <$> theDocument) $ do
        theDocument >>= \d -> Log.mixlog_ $ "Resending invitation emails for document #" ++ show (documentid d) ++ ": " ++ (documenttitle d)
        sendInvitationEmails False
  where
    allSignatoriesSigned = all (isSignatory =>>^ hasSigned) . documentsignatorylinks

-- | Prepare final PDF if needed, apply digital signature, and send
-- out confirmation emails if there has been a change in the seal status.  Precondition: document must be
-- closed or in error.
postDocumentClosedActions :: (TemplatesMonad m, MailContextMonad m, CryptoRNG m, MonadIO m, Log.MonadLog m, AmazonMonad m, MonadBaseControl IO m, MonadMask m, DocumentMonad m, GuardTimeConfMonad m)
  => Bool -- ^ Commit to DB after we have sealed the document
  -> Bool -- ^ Prepare final PDF even if it has already been prepared
  -> m ()
postDocumentClosedActions commitAfterSealing forceSealDocument = do

  doc0 <- theDocument

  unlessM ((isClosed ||^ isDocumentError) <$> theDocument) internalError

  whenM ((\d -> forceSealDocument || isNothing (documentsealedfile d)) <$> theDocument) $ do

    whenM (isDocumentError <$> theDocument) $ do
      currentTime >>= dbUpdate . FixClosedErroredDocument . systemActor

    sealDocument

    -- Here there is a race condition: when we commit, other callers
    -- of postDocumentClosedActions may see a document that lacks a
    -- digital signature, and may attempt to send incorrect
    -- correction mail.  Consider adding state to keep track of what
    -- mail we actually send out, after design of adding digital
    -- signature to appendices.

    when commitAfterSealing commit -- so that post-sign view can render pages as soon as possible

  whenM ((\d -> isDocumentError d && not (isDocumentError doc0)) <$> theDocument) $ do

    Log.mixlog_ $ "Sending seal error emails for document #" ++ show (documentid doc0) ++ ": " ++ documenttitle doc0
    theDocument >>= \d -> flip sendDocumentErrorEmail d =<< getDocAuthor d
    theDocument >>= triggerAPICallbackIfThereIsOne

  unlessM (isDocumentError <$> theDocument) $ do

    unlessM (hasGuardtimeSignature <$> theDocument) $ do
      addDigitalSignature

    whenM ((\d -> documentsealstatus doc0 /= documentsealstatus d) <$> theDocument) $ do

      sealFixed <- theDocument >>= \d -> return $
                      documentsealstatus doc0 == Just Missing
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

-- | Post-process documents that lack final PDF or digital signature
findAndDoPostDocumentClosedActions :: (MonadReader SchedulerData m, MonadBaseControl IO m, CryptoRNG m, MonadDB m, MonadMask m, MonadIO m, Log.MonadLog m, AmazonMonad m)
  => Maybe Int -- ^ Only consider documents signed within the latest number of hours given.
  -> m ()
findAndDoPostDocumentClosedActions
  mhours
  = do
  now <- currentTime
  let signtimefilter = case mhours of
        Nothing    -> []
        Just hours -> [DocumentFilterByLatestSignTimeAfter ((60 * hours) `minutesBefore` now)]

  docs <- dbQuery $ GetDocuments [DocumentsOfWholeUniverse]
            ([ DocumentFilterStatuses [Closed] -- here we could include DocumentError as well to get automatic resealing attempted periodically
             , DocumentFilterBySealStatus [Missing] -- sealed file lacks digital signature, or no sealed file at all
             , DocumentFilterByLatestSignTimeBefore (5 `minutesBefore` now) -- avoid documents that the server is processing in its post-closed thread.
             ] ++ signtimefilter)
            [] (0,100)
  when (not (null docs)) $ do
    Log.mixlog_ $ "findAndDoPostDocumentClosedActions: considering " ++ show (length docs) ++ " document(s)"
  gtConf <- asks (guardTimeConf . sdAppConf)
  forM_ docs $ \doc -> do
    void $ runMailTInScheduler doc $ runGuardTimeConfT gtConf $ withDocument doc $ postDocumentClosedActions False False
    commit

-- | Extend (replace with keyless) signatures of documents older than latest publication code (if they are not already extended)
findAndExtendDigitalSignatures :: (MonadBaseControl IO m, MonadReader SchedulerData m, CryptoRNG m, AmazonMonad m, MonadDB m, MonadMask m, MonadIO m, Log.MonadLog m) => m ()
findAndExtendDigitalSignatures = do
  lpt <- latest_publication_time
  Log.mixlog_ $ "extendSignatures: latest publication time is " ++ show lpt
  docs <- dbQuery $ GetDocuments [DocumentsOfWholeUniverse]
            [ DocumentFilterStatuses [Closed]
            , DocumentFilterByLatestSignTimeBefore lpt
            , DocumentFilterBySealStatus
              [ Guardtime{ extended = False, private = True }
              , Guardtime{ extended = False, private = False }
              ]
            ] [] (0,50)
  when (not (null docs)) $ do
    Log.mixlog_ $ "findAndExtendDigitalSignatures: considering " ++ show (length docs) ++ " document(s)"
  forM_ docs $ \d ->
    case documentsealstatus d of
      Just (Guardtime{ extended = False }) -> do
        void $ withDocument d extendDigitalSignature
        commit
      _ -> return ()

-- | Estimate when the latest Guardtime publication code was published
-- (sometime after the 15th of the month).
latest_publication_time :: (MonadDB m, MonadThrow m, MonadTime m) => m UTCTime
latest_publication_time = localTimeToUTC utc . f . utcToLocalTime utc <$> currentTime
  where
    f LocalTime{..} = LocalTime {
        localDay = if localDay > fifteenth
          then fifteenth
          else addGregorianMonthsClip (-1) fifteenth
      , localTimeOfDay = midnight
      }
      where
        fifteenth = fromGregorian year month 15
        (year, month, _) = toGregorian localDay

stateMismatchError :: (MonadBase IO m, Log.MonadLog m) => String -> DocumentStatus -> Document -> m a
stateMismatchError funame expected Document{documentstatus, documentid} = do
  Log.mixlog_ $ funame ++ ": document #" ++ show documentid ++ " in " ++ show documentstatus ++ " state, expected " ++ show expected
  internalError

getDocAuthor :: (MonadDB m, MonadThrow m, MonadBase IO m) => Document -> m User
getDocAuthor doc = do
  authorid <- guardJust $ getAuthorSigLink doc >>= maybesignatory
  guardJustM $ dbQuery $ GetUserByID authorid

{- |
    Goes through each signatory, and if a user exists this saves it for that user
    by linking the signatory to the user's account.
-}
saveDocumentForSignatories :: (Kontrakcja m, DocumentMonad m) => m (Maybe String)
saveDocumentForSignatories =
  documentsignatorylinks <$> theDocument >>= foldM foldSaveForSig Nothing . filter (not . isAuthor)
  where
    {- |
        Wraps up the saveDocumentForSignatory so we can use it in a fold
    -}
    foldSaveForSig :: (Kontrakcja m, DocumentMonad m) => Maybe String -> SignatoryLink -> m (Maybe String)
    foldSaveForSig (Just msg) _ = return $ Just msg
    foldSaveForSig Nothing siglink = saveDocumentForSignatory siglink
    {- |
        Saves the document for the given signatorylink.  It does this by checking to see
        if there is a user with a matching email, and if there is it hooks up the signatory
        link to that user.
    -}
    saveDocumentForSignatory :: (Kontrakcja m, DocumentMonad m) => SignatoryLink -> m (Maybe String)
    saveDocumentForSignatory sl = do
      let sigemail = getEmail sl
      muser <- case (sigemail) of
                "" -> return Nothing
                _  -> dbQuery $ GetUserByEmail (Email sigemail)
      case muser of
        Nothing -> return Nothing
        Just user -> do
              res <- dbUpdate $ SaveDocumentForUser user (signatorylinkid sl)
              if res then return Nothing
                     else return $ Just "saveDocumentForSignatory failed"

-- | Time out documents once per day after midnight.  Do it in chunks
-- so that we don't choke the server in case there are many documents to time out
findAndTimeoutDocuments :: (MonadBaseControl IO m, MonadReader SchedulerData m, MonadIO m, MonadDB m, MonadCatch m, Log.MonadLog m) => m ()
findAndTimeoutDocuments = do
  now <- currentTime
  docs <- dbQuery $ GetTimeoutedButPendingDocumentsChunk now 100
  forM_ docs $ flip withDocument $ do
    gt <- getGlobalTemplates
    runTemplatesT (defaultValue, gt) $ dbUpdate $ TimeoutDocument (systemActor now)
    triggerAPICallbackIfThereIsOne =<< theDocument
    theDocumentID >>= \did -> Log.mixlog_ $ "Document timedout " ++ (show did)
  when (not (null docs)) $ do
    commit
    findAndTimeoutDocuments
