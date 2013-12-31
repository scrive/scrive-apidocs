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
  , runMailTInScheduler
  , MailT
  ) where

import ActionQueue.Scheduler (SchedulerData, sdAppConf, getGlobalTemplates)
import AppConf (brandedDomains, hostpart, mailsConfig)
import BrandedDomains (findBrandedDomain, bdurl)
import Control.Applicative ((<$>), (<*>), Applicative)
import Control.Conditional (whenM, ifM)
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
          scheduleEmailSendoutWithDocumentAuthorSender (documentid document) (mctxmailsconfig mctx) $ mail {
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
      scheduleEmailSendoutWithDocumentAuthorSender did (mctxmailsconfig mctx) $
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
sendReminderEmail :: (Log.MonadLog m, TemplatesMonad m,MonadIO m, CryptoRNG m, DocumentMonad m, MailContextMonad m) =>
                          Maybe String -> Actor -> Bool -> SignatoryLink  -> m SignatoryLink
sendReminderEmail custommessage  actor automatic siglink = do
  mctx <- getMailContext
  sent <- sendNotifications siglink
    (do
      mail <- theDocument >>= mailDocumentRemind custommessage siglink False
      mailattachments <- makeMailAttachments =<< theDocument
      docid <- theDocumentID
      scheduleEmailSendoutWithDocumentAuthorSender docid (mctxmailsconfig mctx) $ mail {
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
      Log.mixlog_ $ "Reminder mail send for signatory that has not signed " ++ show (signatorylinkid siglink)
      dbUpdate $ PostReminderSend siglink custommessage automatic actor
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
                          String -> m ()
sendForwardEmail email = do
  mctx <- getMailContext
  mail <- mailForwardSigned =<< theDocument
  mailattachments <- makeMailAttachments =<< theDocument
  did <- documentid <$> theDocument
  scheduleEmailSendoutWithDocumentAuthorSender did (mctxmailsconfig mctx) $ mail {
                               to = [MailAddress "" email]
                             , attachments = mailattachments
                             }
  return ()




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
  Log.mixlog_ $ "Chosen delivery method: " ++ show (signatorylinkdeliverymethod sl) ++ " for phone=" ++ getMobile sl ++ ", email=" ++ getEmail sl
  case signatorylinkdeliverymethod sl of
    EmailDelivery   -> domail >> return True
    MobileDelivery  -> dosms >> return True
    EmailAndMobileDelivery -> domail >> dosms >> return True
    _               -> return False

type MailT m = MailContextT (TemplatesT m)

-- | Set up mail and template context, with language and branding
-- based on document data, and the rest from SchedulerData
runMailTInScheduler :: (MonadReader SchedulerData m, MonadIO m, MonadDB m) => Document -> MailT m a -> m a
runMailTInScheduler doc m = do
  appConf <- asks sdAppConf
  now <- getMinutesTime
  mauthor <- maybe (return Nothing) (dbQuery . GetUserByID) $ join $ maybesignatory <$> getAuthorSigLink doc
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
  runMailT (getLang doc, templates) mctx m
  where
   runMailT lt mctx =
     runTemplatesT lt . runMailContextT mctx
