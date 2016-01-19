module Doc.DocMails (
    sendInvitationEmails
  , sendInvitationEmail1
  , sendReminderEmail
  , sendAllReminderEmails
  , sendAllReminderEmailsExceptAuthor
  , sendForwardEmail
  , sendClosedEmails
  , sendRejectEmails
  , sendDocumentErrorEmail
  , sendPinCode
  , makeMailAttachments
  , runMailTInScheduler
  , MailT
  ) where

import Control.Conditional ((<|), (|>), ifM)
import Control.Monad.Catch
import Control.Monad.Reader
import Log
import Text.StringTemplates.Templates (TemplatesMonad, TemplatesT)
import qualified Data.ByteString as BS

import ActionQueue.Scheduler (SchedulerData, sdAppConf, getGlobalTemplates)
import AppConf (mailsConfig)
import BrandedDomain.Model
import Crypto.RNG
import DB
import Doc.API.Callback.Model
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import Doc.DocViewSMS
import Doc.Logging
import Doc.Model
import EvidenceLog.Model (InsertEvidenceEventWithAffectedSignatoryAndMsg(..), CurrentEvidenceEventType(..))
import File.File
import File.Model
import InputValidation
import Kontra
import KontraPrelude
import Log.Identifier
import MailContext (getMailContext, MailContext(..), MailContextMonad, MailContextT, runMailContextT)
import Mails.SendMail
import SMS.SMS (scheduleSMS)
import Templates (runTemplatesT)
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendDocumentErrorEmail :: (CryptoRNG m, MailContextMonad m, MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m) => User -> Document -> m ()
sendDocumentErrorEmail author document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (\sl -> if isAuthor sl
                            then sendDocumentErrorEmailToAuthor
                            else sendDocumentErrorEmailToSignatory sl)
  where
    sendDocumentErrorEmailToAuthor = do
      mctx <- getMailContext
      let authorlink = $(fromJust) $ getAuthorSigLink document
      sendNotifications authorlink True
        (do
          mail <- mailDocumentErrorForAuthor (getLang author) document
          scheduleEmailSendout (mctxmailsconfig mctx) $ mail {
                                   to = [getMailAddress authorlink]
                                 })
        (smsDocumentErrorAuthor document authorlink >>= scheduleSMS document)

    -- | Helper function to send emails to invited parties
    -- ??: Should this be in DocControl or in an email-specific file?
    sendDocumentErrorEmailToSignatory signatorylink = do
      mctx <- getMailContext
      sendNotifications signatorylink False
        (do
          mail <- mailDocumentErrorForSignatory document
          scheduleEmailSendoutWithAuthorSenderThroughService (documentid document) (mctxmailsconfig mctx) $ mail {
                                   to = [getMailAddress signatorylink]
                                 , mailInfo = Invitation (documentid document) (signatorylinkid signatorylink)
                                 })
         (smsDocumentErrorSignatory document signatorylink >>= scheduleSMS document)

{- |
   Send emails to all of the invited parties, respecting the sign order.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendInvitationEmails :: (CryptoRNG m, MonadThrow m, MonadLog m, TemplatesMonad m, DocumentMonad m, MailContextMonad m) => Bool -> m ()
sendInvitationEmails authorsignsimmediately = do
  signlinks <- theDocument >>= \d -> return
                  [sl | sl <- documentsignatorylinks d
                      , matchingSignOrder d sl
                      , not (authorsignsimmediately && onlyAuthorSigns d)
                      , signatorylinkdeliverymethod sl `elem` [EmailDelivery, MobileDelivery,EmailAndMobileDelivery]
                      , not $ hasSigned sl
                      , ((not $ isAuthor sl) || (isAuthor sl && not authorsignsimmediately))
                  ]
  forM_ signlinks sendInvitationEmail1
  where matchingSignOrder d sl = so > documentprevioussignorder d
                              && so <= documentcurrentsignorder d
          where so = signatorysignorder sl
        onlyAuthorSigns d = all (\sl -> isAuthor sl || not (signatoryispartner sl)) (documentsignatorylinks d)

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendInvitationEmail1 :: (CryptoRNG m, MonadThrow m, MonadLog m, TemplatesMonad m, DocumentMonad m, MailContextMonad m) => SignatoryLink -> m ()
sendInvitationEmail1 signatorylink | not (isAuthor signatorylink) = do
  did <- theDocumentID
  mctx <- getMailContext
  -- send invitation to sign to invited person
  sent <- sendNotifications signatorylink False

    (do
      mail <- theDocument >>= mailInvitation True (Sign <| isSignatory signatorylink |> View) (Just signatorylink)
      -- ?? Do we need to read in the contents? -EN
      -- _attachmentcontent <- liftIO $ documentFileID document >>= getFileContents ctx
      scheduleEmailSendoutWithAuthorSenderThroughService did (mctxmailsconfig mctx) $
                           mail { to = [getMailAddress signatorylink]
                                , mailInfo = Invitation did (signatorylinkid signatorylink)
                                })
     (theDocument >>= \doc -> smsInvitation signatorylink doc >>= scheduleSMS doc)

  when sent $ do
    documentinvitetext <$> theDocument >>= \text ->
            void $ dbUpdate $ InsertEvidenceEventWithAffectedSignatoryAndMsg
              InvitationEvidence
              (return ())
              (Just signatorylink)
              (Just text <| text /= "" |> Nothing)
              (systemActor $ mctxtime mctx)

sendInvitationEmail1 authorsiglink = do
  mctx <- getMailContext
  when (isSignatory authorsiglink) $ do
       void $ sendNotifications authorsiglink False
          (do
            -- send invitation to sign to author when it is his turn to sign
            mail <- theDocument >>= \d -> mailDocumentAwaitingForAuthor (getLang d) d
            scheduleEmailSendout (mctxmailsconfig mctx) $
                                 mail { to = [getMailAddress authorsiglink] })
          (theDocument >>= \doc -> smsInvitationToAuthor doc authorsiglink >>= scheduleSMS doc)

{- |
    Send a reminder email (and update the modification time on the document)
-}
sendReminderEmail :: (MonadLog m, MonadCatch m, TemplatesMonad m, CryptoRNG m, DocumentMonad m, MailContextMonad m) =>
                          Maybe String -> Actor -> Bool -> SignatoryLink  -> m SignatoryLink
sendReminderEmail custommessage actor automatic siglink = logSignatory (signatorylinkid siglink) $ do
  mctx <- getMailContext
  doc <- theDocument
  let domail = do
       mailattachments <- makeMailAttachments doc
       mail <- mailDocumentRemind automatic custommessage siglink (not (null mailattachments)) doc
       docid <- theDocumentID
       scheduleEmailSendoutWithAuthorSenderThroughService docid (mctxmailsconfig mctx) $ mail {
           to = [getMailAddress siglink]
         -- We only collect delivery information, if signatory had not signed yet
         , mailInfo = if (isNothing $ maybesigninfo siglink)
           then Invitation docid (signatorylinkid siglink)
           else None
         -- We only add attachment after document is signed
         , attachments = attachments mail ++ (if isJust $ maybesigninfo siglink
          then mailattachments
          else [])
       }
      dosms = scheduleSMS doc =<< smsReminder automatic doc siglink

  sent <- case maybesigninfo siglink of
           Just _ -> do
             -- reminders for signed documents should use confirmation delivery method
             -- and fallback to invitation delivery method if confirmation delivery method is NoConfirmation
             let noConfirmationMethod = signatorylinkconfirmationdeliverymethod siglink == NoConfirmationDelivery
                 sendemail =   signatorylinkconfirmationdeliverymethod siglink `elem` [EmailConfirmationDelivery, EmailAndMobileConfirmationDelivery]
                             || (noConfirmationMethod && signatorylinkdeliverymethod siglink `elem` [EmailDelivery, EmailAndMobileDelivery])
                 sendsms =   signatorylinkconfirmationdeliverymethod siglink `elem` [MobileConfirmationDelivery, EmailAndMobileConfirmationDelivery]
                           || (noConfirmationMethod && signatorylinkdeliverymethod siglink `elem` [MobileDelivery, EmailAndMobileDelivery])
             case (sendemail, sendsms) of
               (True, True) -> domail >> dosms >> return True
               (True, False) -> domail >> return True
               (False, True) -> dosms >> return True
               (False, False) -> return False
           Nothing -> sendNotifications siglink False domail dosms

  when sent $ do
    when (isPending doc &&  not (hasSigned siglink)) $ do
      -- Reset delivery status if the signatory has not signed yet
      logInfo_ "Reminder mail send for signatory that has not signed"
      dbUpdate $ PostReminderSend siglink custommessage automatic actor
    triggerAPICallbackIfThereIsOne =<< theDocument
  return siglink

-- | Send emails to all parties when a document is closed.  If
-- 'sealFixed', then there were earlier emails sent that contained a
-- document that wasn't digitally sealed, so now we resend the
-- document with digital seal.  If the main file is deemed too large
-- to attach, a link to it is used instead of attaching it.
sendClosedEmails :: (CryptoRNG m, MailContextMonad m, MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m) => Bool -> Document -> m ()
sendClosedEmails sealFixed document = do
    mctx <- getMailContext
    mailattachments <- makeMailAttachments document
    let signatorylinks = documentsignatorylinks document
    forM_ signatorylinks $ \sl -> do
      let sendMail = do
            mail <- mailDocumentClosed False sl sealFixed (not (null mailattachments)) document
            let scheduleEmailFunc
                  | signatoryisauthor sl = scheduleEmailSendout
                  | otherwise            = scheduleEmailSendoutWithAuthorSender (documentid document)
            scheduleEmailFunc (mctxmailsconfig mctx) $
                                 mail { to = [getMailAddress sl]
                                      , attachments = attachments mail ++ mailattachments
                                      , replyTo =
                                          let maybeAuthor = find signatoryisauthor signatorylinks
                                          in if signatoryisauthor sl && isJust maybeAuthor
                                                then Nothing
                                                else fmap getMailAddress maybeAuthor
                                      }
      let sendSMS withMail = (scheduleSMS document =<< smsClosedNotification document sl withMail sealFixed)
      let useMail = isGood $ asValidEmail $ getEmail sl
          useSMS = isGood $  asValidPhoneForSMS $ getMobile sl
      case (signatorylinkconfirmationdeliverymethod sl) of
        NoConfirmationDelivery -> return ()
        EmailConfirmationDelivery -> when useMail $ sendMail
        MobileConfirmationDelivery -> when useSMS $ sendSMS False
        EmailAndMobileConfirmationDelivery -> do
                                              when useMail $ sendMail
                                              when useSMS  $ sendSMS useMail

makeMailAttachments :: (MonadDB m, MonadThrow m) => Document -> m [(String, Either BS.ByteString FileID)]
makeMailAttachments document = do
  let mainfile = mainfileid <$> documentsealedfile document `mplus` documentfile document
  let
      aattachments = map authorattachmentfileid $ documentauthorattachments document
      sattachments = concatMap (maybeToList . signatoryattachmentfile) $ concatMap signatoryattachments $ documentsignatorylinks document
      allfiles' = maybeToList mainfile ++
                  if isClosed document then [] else aattachments ++ sattachments
  allfiles <- mapM (dbQuery . GetFileByFileID) allfiles'
  let maxFileSize = 10 * 1024 * 1024
  if sum (map filesize allfiles) > maxFileSize
    then return []
    else do
      let filenames =  (documenttitle document ++ ".pdf") : map filename ($(tail) allfiles)
      return $ zip filenames (map (Right . fileid) allfiles)

{- |
   Send an email to the author and to all signatories who were sent an invitation  when the document is rejected
 -}
sendRejectEmails :: Kontrakcja m => Maybe String -> SignatoryLink -> Document -> m ()
sendRejectEmails customMessage signalink document = do
  let activatedSignatories = [sl | sl <- documentsignatorylinks document
                                 , isActivatedSignatory (documentcurrentsignorder document) sl || isAuthor sl
                                 , signatorylinkdeliverymethod sl `elem` [EmailDelivery, EmailAndMobileDelivery, MobileDelivery] || isAuthor sl
                                 ]
  forM_ activatedSignatories $ \sl -> do
    void $ sendNotifications sl True
      (do
         mctx <- getMailContext
         mail <- mailDocumentRejected True customMessage (signatoryisauthor sl) signalink document
         scheduleEmailSendout (mctxmailsconfig mctx) $ mail {
                                  to = [getMailAddress sl]
                                })
      (scheduleSMS document =<< smsRejectNotification document sl signalink)

{- |
   Send reminder to all parties in document that can sign
 -}

sendAllReminderEmails :: (MonadLog m, TemplatesMonad m, MonadCatch m, CryptoRNG m, DocumentMonad m, MailContextMonad m) =>
                          Actor -> Bool -> m [SignatoryLink]
sendAllReminderEmails = sendAllReminderEmailsWithFilter (const True)


{- |
   Send reminder to all parties in document that can sign, except author
 -}

sendAllReminderEmailsExceptAuthor :: (MonadLog m, TemplatesMonad m, MonadCatch m, CryptoRNG m, DocumentMonad m, MailContextMonad m) =>
                                        Actor -> Bool -> m [SignatoryLink]
sendAllReminderEmailsExceptAuthor  = sendAllReminderEmailsWithFilter (not . isAuthor)

{- |
   Send reminder to all parties in document - excluding ones that do not pass given filter
 -}
sendAllReminderEmailsWithFilter :: (MonadLog m, TemplatesMonad m, MonadCatch m, CryptoRNG m, DocumentMonad m, MailContextMonad m) =>
                                        (SignatoryLink -> Bool) -> Actor -> Bool -> m [SignatoryLink]
sendAllReminderEmailsWithFilter f actor automatic = do
    ifM (isPending <$> theDocument)
    {-then-} (do
      unsignedsiglinks <- filter <$> isEligibleForReminder <$> theDocument <*> (documentsignatorylinks <$> theDocument)
      sequence . map (sendReminderEmail Nothing actor automatic) $ filter f unsignedsiglinks)
    {-else-} $ do
      return []



{- |
    Send a forward email
-}
sendForwardEmail :: (MonadLog m, TemplatesMonad m, MonadThrow m, CryptoRNG m, DocumentMonad m, MailContextMonad m) =>
                          String -> Bool -> SignatoryLink -> m ()
sendForwardEmail email noContent asiglink = do
  mctx <- getMailContext
  mailattachments <- makeMailAttachments =<< theDocument
  mail <- mailForwardSigned asiglink (not (null mailattachments)) =<< theDocument
  did <- documentid <$> theDocument
  scheduleEmailSendoutWithAuthorSenderThroughService did (mctxmailsconfig mctx) $ mail {
                               to = [MailAddress "" email]
                             , content =  if (noContent)
                                             then ""
                                             else content mail
                             , attachments = attachments mail ++ mailattachments
                             }
  return ()



sendPinCode:: (MonadLog m, TemplatesMonad m, MonadThrow m, DocumentMonad m, CryptoRNG m, MailContextMonad m, KontraMonad m) =>
                          SignatoryLink -> String -> String -> m ()
sendPinCode sl phone pin = do
  ctx <- getContext
  doc <- theDocument
  void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg
              SMSPinSendEvidence
              (return ())
              (Just sl)
              (Just phone)
              =<< (signatoryActor ctx sl)
  scheduleSMS doc =<< smsPinCodeSendout doc sl phone pin


-- Notification sendout

-- | Send out mail and/or SMS or not, depending on delivery method.  Return 'False' iff nothing was sent. Email is always sent to authors if alwaysEmailAuthor is True.
sendNotifications :: (Monad m, MonadLog m) => SignatoryLink -> Bool -> m () -> m () -> m Bool
sendNotifications sl alwaysEmailAuthor domail dosms = do
  logInfo "Delivery method chosen for a signatory" $ object [
      identifier_ $ signatorylinkid sl
    , "method" .= show (signatorylinkdeliverymethod sl)
    , "phone" .= getMobile sl
    , "email" .= getEmail sl
    ]
  case (forceAuthorEmail, signatorylinkdeliverymethod sl) of
    (_, EmailDelivery) -> domail >> return True
    (_, EmailAndMobileDelivery) -> domail >> dosms >> return True
    (True, MobileDelivery) -> domail >> dosms >> return True
    (False, MobileDelivery) -> dosms >> return True
    (True, _) -> domail >> return True
    _ -> return False
  where forceAuthorEmail = alwaysEmailAuthor && isAuthor sl

type MailT m = MailContextT (TemplatesT m)

-- | Set up mail and template context, with language and branding
-- based on document data, and the rest from SchedulerData
runMailTInScheduler :: (MonadReader SchedulerData m, MonadThrow m, MonadDB m, MonadIO m, MonadLog m) => Document -> MailT m a -> m a
runMailTInScheduler doc m = do
  appConf <- asks sdAppConf
  now <- currentTime
  mauthor <- maybe (return Nothing) (dbQuery . GetUserByID) $ join $ maybesignatory <$> getAuthorSigLink doc
  bd <- maybe (dbQuery GetMainBrandedDomain) (dbQuery . GetBrandedDomainByUserID) (userid <$> mauthor)
  let mctx = MailContext {
          mctxhostpart = bdUrl $ bd
        , mctxmailsconfig = mailsConfig appConf
        , mctxlang = documentlang doc
        , mctxcurrentBrandedDomain = bd
        , mctxtime = now
        }
  templates <- getGlobalTemplates
  runMailT (getLang doc, templates) mctx m
  where
   runMailT lt mctx =
     runTemplatesT lt . runMailContextT mctx
