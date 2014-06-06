{-# LANGUAGE NoImplicitPrelude #-}
module Doc.DocMails (
    sendInvitationEmails
  , sendInvitationEmail1
  , sendInvitationEmailsToViewers
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

import ActionQueue.Scheduler (SchedulerData, sdAppConf, getGlobalTemplates)
import AppConf (hostpart, mailsConfig)
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Control.Applicative ((<$>), (<*>), Applicative)
import Control.Conditional (ifM)
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Logic
import Crypto.RNG
import DB
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.Model
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import Doc.DocViewSMS
import EvidenceLog.Model (InsertEvidenceEventWithAffectedSignatoryAndMsg(..), CurrentEvidenceEventType(..))
import SMS.SMS (scheduleSMS)
import InputValidation
import IPAddress (noIP)
import File.Model
import File.File
import Kontra
import KontraLink
import MailContext (getMailContext, MailContext(..), MailContextMonad, MailContextT, runMailContextT)
import Mails.SendMail
import OurPrelude
import User.Model
import User.Email
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import qualified Log
import Templates (runTemplatesT)
import Text.StringTemplates.Templates (TemplatesMonad, TemplatesT)
import Util.Actor
import Util.SignatoryLinkUtils
import ActionQueue.UserAccountRequest
import User.Action
import Data.Maybe hiding (fromJust)
import qualified Data.ByteString as BS
import Doc.API.Callback.Model
import Company.Model
import MinutesTime

{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendDocumentErrorEmail :: (CryptoRNG m, MailContextMonad m, MonadDB m, Log.MonadLog m, TemplatesMonad m) => User -> Document -> m ()
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
          scheduleEmailSendoutWithDocumentAuthorSender (documentid document) (mctxmailsconfig mctx) $ mail {
                                   to = [getMailAddress signatorylink]
                                 , mailInfo = Invitation (documentid document) (signatorylinkid signatorylink)
                                 })
         (scheduleSMS =<<  smsDocumentErrorSignatory document signatorylink)

{- |
   Send emails to all of the invited parties.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendInvitationEmails :: (CryptoRNG m, Log.MonadLog m, TemplatesMonad m, DocumentMonad m, MailContextMonad m) => Bool -> m ()
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
sendInvitationEmail1 :: (CryptoRNG m, Log.MonadLog m, TemplatesMonad m, DocumentMonad m, MailContextMonad m) => SignatoryLink -> m ()
sendInvitationEmail1 signatorylink | not (isAuthor signatorylink) = do
  did <- theDocumentID
  mctx <- getMailContext
  -- send invitation to sign to invited person
  sent <- sendNotifications signatorylink

    (do
      mail <- theDocument >>= mailInvitation True (Sign <| isSignatory signatorylink |> View) (Just signatorylink)
      -- ?? Do we need to read in the contents? -EN
      -- _attachmentcontent <- liftIO $ documentFileID document >>= getFileContents ctx
      scheduleEmailSendoutWithDocumentAuthorSender did (mctxmailsconfig mctx) $
                           mail { to = [getMailAddress signatorylink]
                                , mailInfo = Invitation did (signatorylinkid signatorylink)
                                })
     (scheduleSMS =<< smsInvitation signatorylink =<< theDocument)

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
       void $ sendNotifications authorsiglink
          (do
            -- send invitation to sign to author when it is his turn to sign
            mail <- theDocument >>= \d -> mailDocumentAwaitingForAuthor (getLang d) d
            scheduleEmailSendout (mctxmailsconfig mctx) $
                                 mail { to = [getMailAddress authorsiglink] })
          (scheduleSMS =<<  flip smsInvitationToAuthor authorsiglink =<< theDocument)

{- |
    Send a reminder email (and update the modification time on the document)
-}
sendReminderEmail :: (Log.MonadLog m, TemplatesMonad m,MonadIO m, CryptoRNG m, DocumentMonad m, MailContextMonad m) =>
                          Maybe String -> Actor -> Bool -> SignatoryLink  -> m SignatoryLink
sendReminderEmail custommessage  actor automatic siglink = do
  mctx <- getMailContext
  doc <- theDocument
  let domail = do
       mailattachments <- makeMailAttachments doc
       mail <- mailDocumentRemind custommessage siglink (not (null mailattachments)) doc
       docid <- theDocumentID
       scheduleEmailSendoutWithDocumentAuthorSender docid (mctxmailsconfig mctx) $ mail {
                                                             to = [getMailAddress siglink]
                                                           , mailInfo = Invitation docid (signatorylinkid siglink)
                                                           , attachments = if isJust $ maybesigninfo siglink
                                                                           then mailattachments
                                                                           else []
                                                           }
      dosms = scheduleSMS =<< smsReminder doc siglink

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
           Nothing -> sendNotifications siglink domail dosms

  when sent $ do
    when (isPending doc &&  not (hasSigned siglink)) $ do
      -- Reset delivery status if the signatory has not signed yet
      Log.mixlog_ $ "Reminder mail send for signatory that has not signed " ++ show (signatorylinkid siglink)
      dbUpdate $ PostReminderSend siglink custommessage automatic actor
    triggerAPICallbackIfThereIsOne =<< theDocument
  return siglink

-- | Send emails to all parties when a document is closed.  If
-- 'sealFixed', then there were earlier emails sent that contained a
-- document that wasn't digitally sealed, so now we resend the
-- document with digital seal.  If the main file is deemed too large
-- to attach, a link to it is used instead of attaching it.
sendClosedEmails :: (CryptoRNG m, MailContextMonad m, MonadDB m, Log.MonadLog m, TemplatesMonad m) => Bool -> Document -> m ()
sendClosedEmails sealFixed document = do
    mctx <- getMailContext
    mailattachments <- makeMailAttachments document
    let signatorylinks = documentsignatorylinks document
    forM_ signatorylinks $ \sl -> do
      ml <- if (isGood $ asValidEmail $ getEmail sl)
               then handlePostSignSignup (Email $ getEmail sl) (getFirstName sl) (getLastName sl) (getCompanyName sl) (getCompanyNumber sl)
               else return $ Nothing
      let sendMail = do
            mail <- mailDocumentClosed False ml sl sealFixed (not (null mailattachments)) document
            scheduleEmailSendout (mctxmailsconfig mctx) $
                                 mail { to = [getMailAddress sl]
                                      , attachments = mailattachments
                                      }
      let sendSMS withMail = (scheduleSMS =<< smsClosedNotification document sl withMail sealFixed)
      let useMail = isGood $ asValidEmail $ getEmail sl
          useSMS = isGood $  asValidPhoneForSMS $ getMobile sl
      case (signatorylinkconfirmationdeliverymethod sl) of
        NoConfirmationDelivery -> return ()
        EmailConfirmationDelivery -> when useMail $ sendMail
        MobileConfirmationDelivery -> when useSMS $ sendSMS False
        EmailAndMobileConfirmationDelivery -> do
                                              when useMail $ sendMail
                                              when useSMS  $ sendSMS useMail

makeMailAttachments :: (MonadDB m, MonadIO m) => Document -> m [(String, Either BS.ByteString FileID)]
makeMailAttachments document = do
  let mainfile = documentsealedfile document `mplus` documentfile document
  let
      aattachments = map authorattachmentfile $ documentauthorattachments document
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
                                 , signatorylinkdeliverymethod sl == EmailDelivery || isAuthor sl
                                 ]
  forM_ activatedSignatories $ \sl -> do
    void $ sendNotifications sl
      (do
         mctx <- getMailContext
         mail <- mailDocumentRejected customMessage signalink document
         scheduleEmailSendout (mctxmailsconfig mctx) $ mail {
                                  to = [getMailAddress sl]
                                })
      (scheduleSMS =<<  smsRejectNotification document sl signalink)

{- |
   Send reminder to all parties in document that can sign
 -}

sendAllReminderEmails :: (Log.MonadLog m, TemplatesMonad m,MonadIO m, CryptoRNG m, DocumentMonad m, MailContextMonad m, Applicative m) =>
                          Actor -> Bool -> m [SignatoryLink]
sendAllReminderEmails = sendAllReminderEmailsWithFilter (const True)


{- |
   Send reminder to all parties in document that can sign, except author
 -}

sendAllReminderEmailsExceptAuthor :: (Log.MonadLog m, TemplatesMonad m,MonadIO m, CryptoRNG m, DocumentMonad m, MailContextMonad m, Applicative m) =>
                                        Actor -> Bool -> m [SignatoryLink]
sendAllReminderEmailsExceptAuthor  = sendAllReminderEmailsWithFilter (not . isAuthor)

{- |
   Send reminder to all parties in document - excluding ones that do not pass given filter
 -}
sendAllReminderEmailsWithFilter :: (Log.MonadLog m, TemplatesMonad m,MonadIO m, CryptoRNG m, DocumentMonad m, MailContextMonad m, Applicative m) =>
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
sendForwardEmail :: (Log.MonadLog m, TemplatesMonad m,MonadIO m, CryptoRNG m, DocumentMonad m, MailContextMonad m) =>
                          String -> Bool -> SignatoryLink -> m ()
sendForwardEmail email noContent asiglink = do
  mctx <- getMailContext
  mailattachments <- makeMailAttachments =<< theDocument
  mail <- mailForwardSigned asiglink (not (null mailattachments)) =<< theDocument
  did <- documentid <$> theDocument
  scheduleEmailSendoutWithDocumentAuthorSender did (mctxmailsconfig mctx) $ mail {
                               to = [MailAddress "" email]
                             , content =  if (noContent)
                                             then ""
                                             else content mail
                             , attachments = mailattachments
                             }
  return ()



sendPinCode:: (Log.MonadLog m, TemplatesMonad m,MonadIO m, DocumentMonad m, CryptoRNG m, MailContextMonad m, KontraMonad m) =>
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
  scheduleSMS =<< smsPinCodeSendout doc sl phone pin

{- |
   Try to sign up a new user. Returns the confirmation link for the new user.
   Nothing means there is already an account or there was an error creating the user.
 -}
handlePostSignSignup :: (CryptoRNG m, MonadDB m, TemplatesMonad m, MailContextMonad m) => Email -> String -> String -> String -> String -> m (Maybe KontraLink)
handlePostSignSignup email fn ln cnm cnr = do
  mctx <- getMailContext
  muser <- dbQuery $ GetUserByEmail email
  case (muser, muser >>= userhasacceptedtermsofservice) of
    (Just user, Nothing) -> do
      -- there is an existing user that hasn't been activated
      -- return the existing link
      l <- newUserAccountRequestLink (lang $ usersettings user) (userid user) BySigning
      return $ Just l
    (Nothing, Nothing) -> do
      -- this email address is new to the system, so create the user
      -- and send an invite
      company <- dbUpdate $ CreateCompany

      _ <- dbUpdate $ SetCompanyInfo (companyid company) $ (companyinfo company) {
                    companyname = cnm
                  , companynumber = cnr
              }
      mnewuser <- createUser email (fn, ln) (companyid company,True) (mctxlang mctx)
      case mnewuser of
        Nothing -> return Nothing
        Just newuser -> do
          l <- newUserAccountRequestLink (mctxlang mctx) (userid newuser) BySigning
          return $ Just l
    (_, _) -> return Nothing


-- Notification sendout

-- | Send out mail and/or SMS or not, depending on delivery method.  Return 'False' iff nothing was sent.
sendNotifications :: (Monad m, Log.MonadLog m) => SignatoryLink -> m () -> m () -> m Bool
sendNotifications sl domail dosms = do
  Log.mixlog_ $ "Chosen delivery method: " ++ show (signatorylinkdeliverymethod sl) ++ " for phone=" ++ getMobile sl ++ ", email=" ++ getEmail sl
  case signatorylinkdeliverymethod sl of
    EmailDelivery   -> domail >> return True
    MobileDelivery  -> dosms >> return True
    EmailAndMobileDelivery -> domail >> dosms >> return True
    _               -> return False

type MailT m = MailContextT (TemplatesT m)

-- | Set up mail and template context, with language and branding
-- based on document data, and the rest from SchedulerData
runMailTInScheduler :: (MonadReader SchedulerData m, MonadIO m, MonadDB m,Log.MonadLog m) => Document -> MailT m a -> m a
runMailTInScheduler doc m = do
  appConf <- asks sdAppConf
  now <- getMinutesTime
  mauthor <- maybe (return Nothing) (dbQuery . GetUserByID) $ join $ maybesignatory <$> getAuthorSigLink doc
  mbd <- maybe (return Nothing) (dbQuery . GetBrandedDomainByUserID) (userid <$> mauthor)
  let mctx = MailContext { mctxhostpart = fromMaybe (hostpart appConf) (bdurl <$> mbd)
                         , mctxmailsconfig = mailsConfig appConf
                         , mctxlang = documentlang doc
                         , mctxcurrentBrandedDomain = mbd
                         , mctxipnumber = noIP
                         , mctxtime = now
                         , mctxmaybeuser = Nothing
                         }
  templates <- getGlobalTemplates
  runMailT (getLang doc, templates) mctx m
  where
   runMailT lt mctx =
     runTemplatesT lt . runMailContextT mctx
