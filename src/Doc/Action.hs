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
  , sendReminderEmail
  , sendInvitationEmail1
  , sendAllReminderEmails
  ) where

import ActionQueue.Scheduler
import ActionQueue.UserAccountRequest
import Amazon (AmazonMonad)
import AppConf (hostpart, mailsConfig, brandedDomains, guardTimeConf)
import BrandedDomains (findBrandedDomain, bdurl)
import Company.Model
import Control.Applicative
import Control.Conditional (whenM, unlessM, ifM)
import Control.Logic
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG
import DB
import qualified Data.ByteString as BS
import Data.List hiding (head, tail)
import Data.Maybe hiding (fromJust)
import Doc.API.Callback.Model
import Doc.DigitalSignature (addDigitalSignature, extendDigitalSignature)
import Doc.DocInfo
import Doc.DocSeal (sealDocument)
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID, withDocument)
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import Doc.DocViewSMS
import Doc.Model
import Doc.SealStatus (SealStatus(..), hasGuardtimeSignature)
import Doc.SignatoryLinkID
import File.File
import File.Model
import ForkAction (forkAction)
import GuardTime (GuardTimeConfMonad, GuardTimeConfT(..), runGuardTimeConfT)
import IPAddress (noIP)
import InputValidation
import Instances ()
import Kontra
import KontraLink
import qualified Log
import MailContext (runMailContextT, MailContextMonad(..), MailContext(..), MailContextT(..))
import Mails.SendMail
import MinutesTime (MinutesTime, toCalendarTimeInUTC, fromClockTime, minutesBefore)
import OurPrelude
import SMS.SMS (scheduleSMS)
import System.Time (toClockTime, CalendarTime(..), Month(..))
import Templates (runTemplatesT)
import Text.StringTemplates.Templates (TemplatesMonad, TemplatesT)
import ThirdPartyStats.Core
import User.Action
import User.Model
import User.Utils
import Util.Actor
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import Utils.Default (defaultValue)

-- | Log a document event, adding some standard properties.
logDocEvent :: (MailContextMonad m, MonadDB m) => EventName -> User -> [EventProperty] -> Document -> m ()
logDocEvent name user extraProps doc = do
  comp <- getCompanyForUser user
  now <- getMinutesTime
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

postDocumentPreparationChange :: (Kontrakcja m, DocumentMonad m) => Bool -> m ()
postDocumentPreparationChange skipauthorinvitation = do
  docid <- theDocumentID
  triggerAPICallbackIfThereIsOne =<< theDocument
  unlessM (isPending <$> theDocument) $
    theDocument >>= stateMismatchError "postDocumentPreparationChange" Pending
  Log.docevent $ "Preparation -> Pending; Sending invitation emails: " ++ show docid
  msaved <- saveDocumentForSignatories
  case msaved of
    Just msg -> do
      Log.error $ "Failed to save document #" ++ (show docid) ++ " for signatories " ++ msg
    Nothing -> return ()
  theDocument >>= \d -> Log.server $ "Sending invitation emails for document #" ++ show docid ++ ": " ++ documenttitle d

  -- Stat logging
  now <- getMinutesTime
  author <- getDocAuthor =<< theDocument
  docssent <- dbQuery $ GetDocsSent (userid author)
  -- Log the current time as the last doc sent time
  asyncLogEvent SetUserProps [UserIDProp (userid author),
                              someProp "Last Doc Sent" now,
                              numProp "Docs sent" (fromIntegral $ docssent)
                              ]
  json <- documentJSON Nothing False True False Nothing Nothing =<< theDocument
  asyncLogEvent (UploadDocInfo json) [UserIDProp (userid author),
                                      DocIDProp docid]
  theDocument >>= logDocEvent "Doc Sent" author []

  sendInvitationEmails skipauthorinvitation
  sendInvitationEmailsToViewers

  return ()

postDocumentRejectedChange :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
postDocumentRejectedChange siglinkid doc@Document{..} = do
  triggerAPICallbackIfThereIsOne doc
  unless (isRejected doc) $
    stateMismatchError "postDocumentRejectedChange" Rejected doc
  Log.docevent $ "Pending -> Rejected; send reject emails: " ++ show documentid
  Log.server $ "Sending rejection emails for document #" ++ show documentid ++ ": " ++ documenttitle
  ctx <- getContext
  -- Log the fact that the current user rejected a document.
  maybe (return ())
        (\user -> logDocEvent "Doc Rejected" user [] doc)
        (ctxmaybeuser ctx)
  customMessage <- getOptionalField  asValidInviteText "customtext"
  sendRejectEmails customMessage ($(fromJust) $ getSigLinkFor siglinkid doc) doc
  return ()

postDocumentCanceledChange :: Kontrakcja m => Document -> m ()
postDocumentCanceledChange doc@Document{..} = do
  triggerAPICallbackIfThereIsOne doc
  unless (isCanceled doc) $
    stateMismatchError "postDocumentCanceledChange" Canceled doc
  Log.docevent $ "Pending -> Canceled (ElegDataMismatch); Sending cancelation emails: " ++ show documentid
  -- if canceled because of ElegDataMismatch, send out emails
  author <- getDocAuthor doc
  let f sl = do
        msg <- signatorylinkelegdatamismatchmessage sl
        fn <- signatorylinkelegdatamismatchfirstname sl
        ln <- signatorylinkelegdatamismatchlastname sl
        pno <- signatorylinkelegdatamismatchpersonalnumber sl
        return (msg,fn,ln,pno)
  let issues = (catMaybes (map f (documentsignatorylinks)))
  mapM_ (\r -> logDocEvent "Doc Canceled" author [reasonProp r] doc) issues

  when (not (null issues)) $ do
      Log.server $ "Sending cancelation emails for document #" ++ show documentid ++ ": " ++ documenttitle
      sendElegDataMismatchEmails author doc
  where
    reasonProp = stringProp "Reason" . show

-- | After a party has signed - check if we need to close document and
-- take further actions.
postDocumentPendingChange :: (CryptoRNG m, TemplatesMonad m, AmazonMonad m, MonadBaseControl IO m, MonadBase IO m, DocumentMonad m, MonadIO m, KontraMonad m, GuardTimeConfMonad m, MailContextMonad m)
                          => Document -> m ()
postDocumentPendingChange olddoc = do
  unlessM (isPending <$> theDocument) $
    theDocument >>= stateMismatchError "postDocumentPendingChange" Pending

  ifM (allSignatoriesSigned <$> theDocument)
  {-then-} (do
      theDocument >>= \d -> Log.docevent $ "All have signed; " ++ show (documentstatus d) ++ " -> Closed: " ++ show (documentid d)
      time <- ctxtime <$> getContext
      dbUpdate $ CloseDocument (systemActor time)
      author <- theDocument >>= getDocAuthor
      theDocument >>= logDocEvent "Doc Closed" author []
      asyncLogEvent SetUserProps [UserIDProp (userid author),
                                  someProp "Last Doc Closed" time]
      kCommit -- ... so that the forked thread can see our changes
      theDocument >>= \d -> forkAction ("Sealing document #" ++ show (documentid d) ++ ": " ++ (documenttitle d)) $ do
        -- We fork so that the client can get the response to the
        -- signing request without having to wait for the sealing
        -- operations
        postDocumentClosedActions True False)
  {-else-} $ do
      theDocument >>= triggerAPICallbackIfThereIsOne
      whenM ((\d -> documentcurrentsignorder d /= documentcurrentsignorder olddoc) <$> theDocument) $ do
        theDocument >>= \d -> Log.server $ "Resending invitation emails for document #" ++ show (documentid d) ++ ": " ++ (documenttitle d)
        sendInvitationEmails False
  where
    allSignatoriesSigned = all (isSignatory =>>^ hasSigned) . documentsignatorylinks

-- | Prepare final PDF if needed, apply digital signature, and send
-- out confirmation emails if there has been a change in the seal status.  Precondition: document must be
-- closed or in error.
postDocumentClosedActions :: ( TemplatesMonad m, MailContextMonad m, CryptoRNG m, MonadIO m, AmazonMonad m
                             , MonadBaseControl IO m, DocumentMonad m , GuardTimeConfMonad m)
                          => Bool -> Bool -> m ()
postDocumentClosedActions
  commitAfterSealing -- ^ Commit to DB after we have sealed the document
  forceSealDocument -- ^ Prepare final PDF even if it has already been prepared

  = do

  doc0 <- theDocument

  unlessM ((isClosed ||^ isDocumentError) <$> theDocument) internalError

  whenM ((\d -> forceSealDocument || isNothing (documentsealedfile d)) <$> theDocument) $ do

    whenM (isDocumentError <$> theDocument) $ do
      getMinutesTime >>= dbUpdate . FixClosedErroredDocument . systemActor

    sealDocument

    -- Here there is a race condition: when we commit, other callers
    -- of postDocumentClosedActions may see a document that lacks a
    -- digital signature, and may attempt to send incorrect
    -- correction mail.  Consider adding state to keep track of what
    -- mail we actually send out, after design of adding digital
    -- signature to appendices.

    when commitAfterSealing kCommit -- so that post-sign view can render pages as soon as possible

  whenM ((\d -> isDocumentError d && not (isDocumentError doc0)) <$> theDocument) $ do

    Log.server $ "Sending seal error emails for document #" ++ show (documentid doc0) ++ ": " ++ documenttitle doc0
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
findAndDoPostDocumentClosedActions :: (MonadReader SchedulerData m, MonadBaseControl IO m, CryptoRNG m, MonadDB m, AmazonMonad m) => Maybe Int -> m ()
findAndDoPostDocumentClosedActions
  mhours -- ^ Only consider documents signed within the latest number of hours given.
  = do
  now <- getMinutesTime
  let signtimefilter = case mhours of
        Nothing    -> []
        Just hours -> [DocumentFilterByLatestSignTimeAfter ((60 * hours) `minutesBefore` now)]

  docs <- dbQuery $ GetDocuments [DocumentsOfWholeUniverse]
            ([ DocumentFilterPurged False
             , DocumentFilterStatuses [Closed] -- here we could include DocumentError as well to get automatic resealing attempted periodically
             , DocumentFilterBySealStatus [Missing] -- sealed file lacks digital signature, or no sealed file at all
             , DocumentFilterByLatestSignTimeBefore (5 `minutesBefore` now) -- avoid documents that the server is processing in its post-closed thread.
             ] ++ signtimefilter)
            [] (0,100)
  when (not (null docs)) $ do
    Log.debug $ "findAndDoPostDocumentClosedActions: considering " ++ show (length docs) ++ " document(s)"
  forM_ docs $ \doc -> do
    void $ runMailTInScheduler (flip withDocument (postDocumentClosedActions False False)) doc
    kCommit

-- | Extend (replace with keyless) signatures of documents older than latest publication code (if they are not already extended)
findAndExtendDigitalSignatures :: (MonadBaseControl IO m, MonadReader SchedulerData m, CryptoRNG m, AmazonMonad m, MonadDB m) => m ()
findAndExtendDigitalSignatures = do
  lpt <- latest_publication_time
  Log.debug $ "extendSignatures: latest publication time is " ++ show lpt
  docs <- dbQuery $ GetDocuments [DocumentsOfWholeUniverse]
            [ DocumentFilterPurged False
            , DocumentFilterStatuses [Closed]
            , DocumentFilterByLatestSignTimeBefore lpt
            , DocumentFilterBySealStatus
              [ Guardtime{ extended = False, private = True }
              , Guardtime{ extended = False, private = False }
              ]
            ] [] (0,50)
  when (not (null docs)) $ do
    Log.debug $ "findAndExtendDigitalSignatures: considering " ++ show (length docs) ++ " document(s)"
  forM_ docs $ \d ->
    case documentsealstatus d of
      Just (Guardtime{ extended = False }) -> do
        void $ withDocument d extendDigitalSignature
        kCommit
      _ -> return ()

-- | Estimate when the latest Guardtime publication code was published
-- (sometime after the 15th of the month).
latest_publication_time :: MonadDB m => m MinutesTime
latest_publication_time = do
  now <- toCalendarTimeInUTC `fmap` getMinutesTime
  let fifteenth = now{ ctDay = 15, ctHour = 0, ctMin = 0, ctSec = 0, ctPicosec = 0 }
  return $ fromClockTime $ toClockTime $
    if now > fifteenth
    then fifteenth
    else if ctMonth fifteenth == January
         then fifteenth{ ctYear = pred (ctYear fifteenth)
                       , ctMonth = December
                       }
         else fifteenth{ ctMonth = pred (ctMonth fifteenth)}

type MailT m = GuardTimeConfT (MailContextT (TemplatesT m))

runMailTInScheduler :: (MonadReader SchedulerData m, MonadIO m, MonadDB m)
              => (Document -> MailT m a) -> Document -> m a
runMailTInScheduler m doc = do
  appConf <- asks sdAppConf
  now <- getMinutesTime
  mauthor <- maybe (return Nothing) (dbQuery . GetUserByEmail) $
             Email . getEmail <$> getAuthorSigLink doc
  let mbd = flip findBrandedDomain (brandedDomains appConf) =<< userassociateddomain =<< mauthor
  let mctx = MailContext { mctxhostpart = fromMaybe (hostpart appConf) (bdurl <$> mbd)
                         , mctxmailsconfig = mailsConfig appConf
                         , mctxlang = documentlang doc
                         , mctxcurrentBrandedDomain = mbd
                         , mctxipnumber = noIP
                         , mctxtime = now
                         , mctxmaybeuser = Nothing
                         }
  templates <- getGlobalTemplates
  runMailT (getLang doc, templates) mctx (guardTimeConf appConf) $ m doc
  where
   runMailT lt mctx gtConf =
     runTemplatesT lt . runMailContextT mctx . runGuardTimeConfT gtConf

stateMismatchError :: (MonadBase IO m, Log.MonadLog m) => String -> DocumentStatus -> Document -> m a
stateMismatchError funame expected Document{documentstatus, documentid} = do
  Log.debug $ funame ++ ": document #" ++ show documentid ++ " in " ++ show documentstatus ++ " state, expected " ++ show expected
  internalError

getDocAuthor :: (MonadDB m, MonadBase IO m) => Document -> m User
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

-- EMAILS

sendElegDataMismatchEmails :: Kontrakcja m => User -> Document -> m ()
sendElegDataMismatchEmails author document = do
    let signlinks = [sl | sl <- documentsignatorylinks document
                        , isActivatedSignatory (documentcurrentsignorder document) sl
                        , not $ isAuthor sl]
        badsig = $(fromJust) $ find (isJust . signatorylinkelegdatamismatchmessage) (documentsignatorylinks document)
        msg = $(fromJust) $ signatorylinkelegdatamismatchmessage badsig
        badname  = getFullName badsig
        bademail = getEmail badsig
    forM_ signlinks $ sendDataMismatchEmailSignatory document (signatorylinkid badsig) badname msg
    sendDataMismatchEmailAuthor author (lines msg) badname bademail document

sendDataMismatchEmailSignatory :: Kontrakcja m => Document -> SignatoryLinkID -> String -> String -> SignatoryLink -> m ()
sendDataMismatchEmailSignatory document badid badname msg signatorylink = do
    mctx <- getMailContext
    let isbad = badid == signatorylinkid signatorylink
    case getAuthorSigLink document of
      Nothing -> error "No author in Document"
      Just authorsl -> do
        void $ sendNotifications authorsl
          (do
            mail <- mailMismatchSignatory
                    (getEmail authorsl)
                    (getFullName authorsl)
                    (mctxhostpart mctx ++ (show $ LinkSignDoc document signatorylink))
                    (getFullName signatorylink)
                    badname
                    msg
                    isbad
                    document
            scheduleEmailSendout (mctxmailsconfig mctx) $ mail { to = [getMailAddress signatorylink]})
          (scheduleSMS =<<  smsMismatchSignatory document signatorylink)

sendDataMismatchEmailAuthor :: Kontrakcja m => User -> [String] -> String -> String -> Document -> m ()
sendDataMismatchEmailAuthor author messages badname bademail document = do
    mctx <- getMailContext
    let authorname = getFullName authorlink
        authoremail = getEmail authorlink
        authorlink = $(fromJust) $ getAuthorSigLink document
    void $ sendNotifications authorlink
      (do
        mail <- mailMismatchAuthor authorname messages badname bademail (getLang author) document
        scheduleEmailSendout (mctxmailsconfig mctx) $ mail { to = [MailAddress {fullname = authorname, email = authoremail }]})
      (scheduleSMS =<<  smsMismatchAuthor document authorlink)

{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendDocumentErrorEmail :: (CryptoRNG m, MailContextMonad m, MonadDB m, TemplatesMonad m) => User -> Document -> m ()
sendDocumentErrorEmail author document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (\sl -> if isAuthor sl
                            then sendDocumentErrorEmailToAuthor
                            else sendDocumentErrorEmailToSignatory sl)
  where
    sendDocumentErrorEmailToAuthor = do
      mctx <- getMailContext
      let authorlink = $(fromJust) $ getAuthorSigLink document
      sendNotifications authorlink
        (do
          mail <- mailDocumentErrorForAuthor (getLang author) document
          scheduleEmailSendout (mctxmailsconfig mctx) $ mail {
                                   to = [getMailAddress authorlink]
                                 })
        (scheduleSMS =<< smsDocumentErrorAuthor document authorlink)

    -- | Helper function to send emails to invited parties
    -- ??: Should this be in DocControl or in an email-specific file?
    sendDocumentErrorEmailToSignatory signatorylink = do
      mctx <- getMailContext
      sendNotifications signatorylink
        (do
          mail <- mailDocumentErrorForSignatory document
          scheduleEmailSendout (mctxmailsconfig mctx) $ mail {
                                   to = [getMailAddress signatorylink]
                                 , mailInfo = Invitation (documentid document) (signatorylinkid signatorylink)
                                 })
         (scheduleSMS =<<  smsDocumentErrorSignatory document signatorylink)

{- |
   Send emails to all of the invited parties.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendInvitationEmails :: (CryptoRNG m, TemplatesMonad m, DocumentMonad m, MailContextMonad m) => Bool -> m ()
sendInvitationEmails skipauthorinvitation = do
  signlinks <- theDocument >>= \d -> return
                  [sl | sl <- documentsignatorylinks d
                      , isSignatory sl
                      , isCurrentSignatory (documentcurrentsignorder d) sl
                      , signatorylinkdeliverymethod sl `elem` [EmailDelivery, MobileDelivery,EmailAndMobileDelivery]
                      , not $ hasSigned sl
                      , ((not $ isAuthor sl) || (isAuthor sl && not skipauthorinvitation))
                      ]
  forM_ signlinks sendInvitationEmail1

sendInvitationEmailsToViewers :: (Kontrakcja m, DocumentMonad m) => m ()
sendInvitationEmailsToViewers = do
  signlinks <- theDocument >>= \d -> return
                  [sl | sl <- documentsignatorylinks d
                      , not $ isSignatory sl
                      , not $ isAuthor sl
                      , signatorylinkdeliverymethod sl `elem` [EmailDelivery, MobileDelivery,EmailAndMobileDelivery]
                      ]
  forM_ signlinks sendInvitationEmail1


{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendInvitationEmail1 :: (CryptoRNG m, TemplatesMonad m, DocumentMonad m, MailContextMonad m) => SignatoryLink -> m (Maybe String)
sendInvitationEmail1 signatorylink | not (isAuthor signatorylink) = do
  did <- theDocumentID
  mctx <- getMailContext
  -- send invitation to sign to invited person
  sent <- sendNotifications signatorylink

    (do
      mail <- theDocument >>= mailInvitation True (Sign <| isSignatory signatorylink |> View) (Just signatorylink) False
      -- ?? Do we need to read in the contents? -EN
      -- _attachmentcontent <- liftIO $ documentFileID document >>= getFileContents ctx
      scheduleEmailSendout (mctxmailsconfig mctx) $
                           mail { to = [getMailAddress signatorylink]
                                , mailInfo = Invitation did (signatorylinkid signatorylink)
                                })
     (scheduleSMS =<< smsInvitation signatorylink =<< theDocument)

  if sent then do
      res <- documentinvitetext <$> theDocument >>= \text ->
              dbUpdate $ AddInvitationEvidence (signatorylinkid signatorylink) (Just text <| text /= "" |> Nothing) $ systemActor $ mctxtime mctx
      if res then return Nothing
             else return $ Just "sendInvitationEmail1 failed"
   else return Nothing

sendInvitationEmail1 authorsiglink = do
  mctx <- getMailContext
  if (isSignatory authorsiglink)
     then do
       void $ sendNotifications authorsiglink
          (do
            -- send invitation to sign to author when it is his turn to sign
            mail <- theDocument >>= \d -> mailDocumentAwaitingForAuthor (getLang d) d
            scheduleEmailSendout (mctxmailsconfig mctx) $
                                 mail { to = [getMailAddress authorsiglink] })
          (scheduleSMS =<<  flip smsInvitationToAuthor authorsiglink =<< theDocument)
       return Nothing
     else return Nothing

{- |
    Send a reminder email (and update the modification time on the document)
-}
sendReminderEmail :: (Kontrakcja m, DocumentMonad m) => Maybe String -> Actor -> SignatoryLink -> m SignatoryLink
sendReminderEmail custommessage  actor siglink = do
  mctx <- getMailContext
  sent <- sendNotifications siglink
    (do
      mail <- theDocument >>= mailDocumentRemind custommessage siglink False
      mailattachments <- makeMailAttachments =<< theDocument
      docid <- theDocumentID
      scheduleEmailSendout (mctxmailsconfig mctx) $ mail {
                               to = [getMailAddress siglink]
                             , mailInfo = Invitation docid (signatorylinkid siglink)
                             , attachments = if isJust $ maybesigninfo siglink
                                             then mailattachments
                                             else []
                             })
    (scheduleSMS =<< flip smsReminder siglink =<< theDocument)
  when sent $ do
    whenM ((\d -> isPending d &&  not (hasSigned siglink)) <$> theDocument) $ do
      -- Reset delivery status if the signatory has not signed yet
      Log.debug $ "Reminder mail send for signatory that has not signed " ++ show (signatorylinkid siglink)
      dbUpdate $ PostReminderSend siglink custommessage actor
    triggerAPICallbackIfThereIsOne =<< theDocument
  return siglink

-- | Send emails to all parties when a document is closed.  If
-- 'sealFixed', then there were earlier emails sent that contained a
-- document that wasn't digitally sealed, so now we resend the
-- document with digital seal.
sendClosedEmails :: (CryptoRNG m, MailContextMonad m, MonadDB m, TemplatesMonad m) => Bool -> Document -> m ()
sendClosedEmails sealFixed document = do
    mctx <- getMailContext
    mailattachments <- makeMailAttachments document
    let signatorylinks = documentsignatorylinks document
    forM_ signatorylinks $ \sl -> do
      ml <- if (isGood $ asValidEmail $ getEmail sl)
               then handlePostSignSignup (Email $ getEmail sl) (getFirstName sl) (getLastName sl) (getCompanyName sl) (getCompanyNumber sl)
               else return $ Nothing
      let sendMail = do
            mail <- mailDocumentClosed ml sl sealFixed document
            scheduleEmailSendout (mctxmailsconfig mctx) $
                                 mail { to = [getMailAddress sl]
                                      , attachments = mailattachments
                                      }
      let sendSMS withMail = (scheduleSMS =<< smsClosedNotification document sl withMail sealFixed)
      let useMail = isGood $ asValidEmail $ getEmail sl
          useSMS = isGood $  asValidPhoneForSMS $ getMobile sl
      when useMail $ sendMail
      when useSMS  $ sendSMS useMail

makeMailAttachments :: (MonadDB m, MonadIO m) => Document -> m [(String, Either BS.ByteString FileID)]
makeMailAttachments document = do
  let mainfile = documentsealedfile document `mplus` documentfile document
  let
      aattachments = map authorattachmentfile $ documentauthorattachments document
      sattachments = concatMap (maybeToList . signatoryattachmentfile) $ concatMap signatoryattachments $ documentsignatorylinks document
      allfiles' = maybeToList mainfile ++ aattachments ++ sattachments
  allfiles <- mapM (dbQuery . GetFileByFileID) allfiles'
  --use the doc title rather than file name for the main file (see jira #1152)
  let filenames =  (documenttitle document ++ ".pdf") : map filename ($(tail) allfiles)

  let filecontents = map (Right . fileid) allfiles
  return $ zip filenames filecontents

{- |
   Send an email to the author and to all signatories who were sent an invitation  when the document is rejected
 -}
sendRejectEmails :: Kontrakcja m => Maybe String -> SignatoryLink -> Document -> m ()
sendRejectEmails customMessage signalink document = do
  let activatedSignatories = [sl | sl <- documentsignatorylinks document
                                 , isActivatedSignatory (documentcurrentsignorder document) sl || isAuthor sl
                                 , signatorylinkdeliverymethod sl == EmailDelivery || isAuthor sl
                                 ]
  forM_ activatedSignatories $ \sl -> do
    void $ sendNotifications sl
      (do
         mctx <- getMailContext
         mail <- mailDocumentRejected customMessage signalink False document
         scheduleEmailSendout (mctxmailsconfig mctx) $ mail {
                                  to = [getMailAddress sl]
                                })
      (scheduleSMS =<<  smsRejectNotification document sl signalink)

{- |
   Send reminder to all parties in document. No custom text
 -}
sendAllReminderEmails :: (Kontrakcja m, DocumentMonad m) => Actor -> User -> m [SignatoryLink]
sendAllReminderEmails actor user = do
    ifM (isPending <$> theDocument)
    {-then-} (do
      isEligible <- isEligibleForReminder user <$> theDocument
      unsignedsiglinks <- filter isEligible . documentsignatorylinks <$> theDocument
      sequence . map (sendReminderEmail Nothing actor) $ unsignedsiglinks)
    {-else-} $ do
      return []

{- |
   Try to sign up a new user. Returns the confirmation link for the new user.
   Nothing means there is already an account or there was an error creating the user.
 -}
handlePostSignSignup :: (CryptoRNG m, MonadDB m, TemplatesMonad m, MailContextMonad m) => Email -> String -> String -> String -> String -> m (Maybe KontraLink)
handlePostSignSignup email fn ln cnm cnr = do
  mctx <- getMailContext
  let lang = mctxlang mctx
  muser <- dbQuery $ GetUserByEmail email
  case (muser, muser >>= userhasacceptedtermsofservice) of
    (Just user, Nothing) -> do
      -- there is an existing user that hasn't been activated
      -- return the existing link
      l <- newUserAccountRequestLink lang (userid user) BySigning
      return $ Just l
    (Nothing, Nothing) -> do
      -- this email address is new to the system, so create the user
      -- and send an invite
      company <- dbUpdate $ CreateCompany

      _ <- dbUpdate $ SetCompanyInfo (companyid company) $ (companyinfo company) {
                    companyname = cnm
                  , companynumber = cnr
              }
      mnewuser <- createUser email (fn, ln) (companyid company,True) lang
      case mnewuser of
        Nothing -> return Nothing
        Just newuser -> do
          l <- newUserAccountRequestLink lang (userid newuser) BySigning
          return $ Just l
    (_, _) -> return Nothing




-- Notification sendout

-- | Send out mail and/or SMS or not, depending on delivery method.  Return 'False' iff nothing was sent.
sendNotifications :: (Monad m, Log.MonadLog m) => SignatoryLink -> m () -> m () -> m Bool
sendNotifications sl domail dosms = do
  Log.debug $ "Chosen delivery method: " ++ show (signatorylinkdeliverymethod sl) ++ " for phone=" ++ getMobile sl ++ ", email=" ++ getEmail sl
  case signatorylinkdeliverymethod sl of
    EmailDelivery   -> domail >> return True
    MobileDelivery  -> dosms >> return True
    EmailAndMobileDelivery -> domail >> dosms >> return True
    _               -> return False


-- | Time out documents once per day after midnight.  Do it in chunks
-- so that we don't choke the server in case there are many documents to time out
findAndTimeoutDocuments :: (MonadBaseControl IO m, MonadReader SchedulerData m, MonadIO m, MonadDB m) => m ()
findAndTimeoutDocuments = do
  now <- getMinutesTime
  docs <- dbQuery $ GetTimeoutedButPendingDocumentsChunk now 100
  forM_ docs $ flip withDocument $ do
    gt <- getGlobalTemplates
    runTemplatesT (defaultValue, gt) $ dbUpdate $ TimeoutDocument (systemActor now)
    triggerAPICallbackIfThereIsOne =<< theDocument
    theDocumentID >>= \did -> Log.debug $ "Document timedout " ++ (show did)
  when (not (null docs)) $ do
    kCommit
    findAndTimeoutDocuments
