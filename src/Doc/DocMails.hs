{-# LANGUAGE NoImplicitPrelude #-}
module Doc.DocMails (
    sendInvitationEmails
  , sendInvitationEmail1
  , sendInvitationEmailsToViewers
  , sendReminderEmail
  , sendAllReminderEmails
  , sendAllReminderEmailsExceptAuthor
  , sendClosedEmails
  , sendRejectEmails
  , sendDocumentErrorEmail
  , sendElegDataMismatchEmails
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Logic
import Crypto.RNG
import DB
import Doc.Model
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import Doc.DocViewSMS
import Doc.SignatoryLinkID
import Doc.DocumentID
import SMS.SMS (scheduleSMS)
import InputValidation
import File.Model
import File.File
import Kontra
import KontraLink
import Mails.SendMail
import OurPrelude
import User.Model
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import qualified Log
import Text.StringTemplates.Templates
import Util.Actor
import Util.SignatoryLinkUtils
import ActionQueue.UserAccountRequest
import User.Action
import Data.List hiding (head, tail)
import Data.Maybe hiding (fromJust)
import qualified Data.ByteString as BS
import Doc.API.Callback.Model
import Company.Model

sendElegDataMismatchEmails :: Kontrakcja m => Context -> Document -> User -> m ()
sendElegDataMismatchEmails ctx document author = do
    let signlinks = [sl | sl <- documentsignatorylinks document
                        , isActivatedSignatory (documentcurrentsignorder document) sl
                        , not $ isAuthor sl]
        badsig = $(fromJust) $ find (isJust . signatorylinkelegdatamismatchmessage) (documentsignatorylinks document)
        msg = $(fromJust) $ signatorylinkelegdatamismatchmessage badsig
        badname  = getFullName badsig
        bademail = getEmail badsig
    forM_ signlinks $ sendDataMismatchEmailSignatory ctx document (signatorylinkid badsig) badname msg
    sendDataMismatchEmailAuthor ctx document author (lines msg) badname bademail

sendDataMismatchEmailSignatory :: Kontrakcja m => Context -> Document -> SignatoryLinkID -> String -> String -> SignatoryLink -> m ()
sendDataMismatchEmailSignatory ctx document badid badname msg signatorylink = do
    let isbad = badid == signatorylinkid signatorylink
    case getAuthorSigLink document of
      Nothing -> error "No author in Document"
      Just authorsl -> do
        void $ sendNotifications authorsl
          (do
            mail <- mailMismatchSignatory
                    ctx
                    document
                    (getEmail authorsl)
                    (getFullName authorsl)
                    (ctxhostpart ctx ++ (show $ LinkSignDoc document signatorylink))
                    (getFullName signatorylink)
                    badname
                    msg
                    isbad
            scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress signatorylink]})
          (scheduleSMS =<<  smsMismatchSignatory document signatorylink)

sendDataMismatchEmailAuthor :: Kontrakcja m => Context -> Document -> User -> [String] -> String -> String -> m ()
sendDataMismatchEmailAuthor ctx document author messages badname bademail = do
    let authorname = getFullName authorlink
        authoremail = getEmail authorlink
        authorlink = $(fromJust) $ getAuthorSigLink document
    void $ sendNotifications authorlink
      (do
        mail <- mailMismatchAuthor ctx document authorname messages badname bademail (getLang author)
        scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress {fullname = authorname, email = authoremail }]})
      (scheduleSMS =<<  smsMismatchAuthor document authorlink)

{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendDocumentErrorEmail :: (CryptoRNG m, KontraMonad m , MonadDB m, TemplatesMonad m) => Document -> User -> m ()
sendDocumentErrorEmail document author = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (\sl -> if isAuthor sl
                            then sendDocumentErrorEmailToAuthor
                            else sendDocumentErrorEmailToSignatory sl)
  where
    sendDocumentErrorEmailToAuthor = do
      ctx <- getContext
      let authorlink = $(fromJust) $ getAuthorSigLink document
      sendNotifications authorlink
        (do
          mail <- mailDocumentErrorForAuthor ctx document (getLang author)
          scheduleEmailSendout (ctxmailsconfig ctx) $ mail {
                                   to = [getMailAddress authorlink]
                                 })
        (scheduleSMS =<< smsDocumentErrorAuthor document authorlink)

    -- | Helper function to send emails to invited parties
    -- ??: Should this be in DocControl or in an email-specific file?
    sendDocumentErrorEmailToSignatory signatorylink = do
      ctx <- getContext
      sendNotifications signatorylink
        (do
          mail <- mailDocumentErrorForSignatory ctx document
          scheduleEmailSendout (ctxmailsconfig ctx) $ mail {
                                   to = [getMailAddress signatorylink]
                                 , mailInfo = Invitation (documentid document) (signatorylinkid signatorylink)
                                 })
         (scheduleSMS =<<  smsDocumentErrorSignatory document signatorylink)

{- |
   Send emails to all of the invited parties.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendInvitationEmails :: Kontrakcja m => Context -> Document -> Bool -> m ()
sendInvitationEmails ctx document skipauthorinvitation = do
  let signlinks = [sl | sl <- documentsignatorylinks document
                      , isSignatory sl
                      , isCurrentSignatory (documentcurrentsignorder document) sl
                      , signatorylinkdeliverymethod sl `elem` [EmailDelivery, MobileDelivery,EmailAndMobileDelivery]
                      , not $ hasSigned sl
                      , ((not $ isAuthor sl) || (isAuthor sl && not skipauthorinvitation))
                      ]
  forM_ signlinks (sendInvitationEmail1 ctx document)

sendInvitationEmailsToViewers :: Kontrakcja m => Context -> Document -> m ()
sendInvitationEmailsToViewers ctx document = do
  let signlinks = [sl | sl <- documentsignatorylinks document
                      , not $ isSignatory sl
                      , not $ isAuthor sl
                      , signatorylinkdeliverymethod sl `elem` [EmailDelivery, MobileDelivery,EmailAndMobileDelivery]
                      ]
  forM_ signlinks (sendInvitationEmail1 ctx document)


{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendInvitationEmail1 :: Kontrakcja m => Context -> Document -> SignatoryLink -> m (Either String Document)
sendInvitationEmail1 ctx document signatorylink | not (isAuthor signatorylink) = do
  -- send invitation to sign to invited person
  sent <- sendNotifications signatorylink

    (do
      mail <- mailInvitation True ctx (Sign <| isSignatory signatorylink |> View) document (Just signatorylink) False
      -- ?? Do we need to read in the contents? -EN
      -- _attachmentcontent <- liftIO $ documentFileID document >>= getFileContents ctx
      scheduleEmailSendout (ctxmailsconfig ctx) $
                           mail { to = [getMailAddress signatorylink]
                                , mailInfo = Invitation (documentid document) (signatorylinkid signatorylink)
                                })
     (scheduleSMS =<< smsInvitation document signatorylink)

  mdoc <- runMaybeT $ do
    when sent $ do
      True <- dbUpdate $ AddInvitationEvidence (documentid document) (signatorylinkid signatorylink) (Just (documentinvitetext document) <|documentinvitetext document /= "" |> Nothing) $ systemActor $ ctxtime ctx
      return ()
    dbQuery $ GetDocumentByDocumentID (documentid document)
  return $ maybe (Left "sendInvitationEmail1 failed") Right mdoc

sendInvitationEmail1 ctx document authorsiglink =
  if (isSignatory authorsiglink)
     then do
       void $ sendNotifications authorsiglink
          (do
            -- send invitation to sign to author when it is his turn to sign
            mail <- mailDocumentAwaitingForAuthor ctx document $ getLang document
            scheduleEmailSendout (ctxmailsconfig ctx) $
                                 mail { to = [getMailAddress authorsiglink] })
          (scheduleSMS =<<  smsInvitationToAuthor document authorsiglink)
       return $ Right document
     else return $ Right document

{- |
    Send a reminder email (and update the modification time on the document)
-}
sendReminderEmail :: (Log.MonadLog m, TemplatesMonad m,MonadIO m, CryptoRNG m, MonadDB m, HasMailContext c) =>
                          Maybe String -> c -> Actor -> Document -> Bool -> SignatoryLink  -> m SignatoryLink
sendReminderEmail custommessage ctx actor doc automatic siglink = do
  sent <- sendNotifications siglink
    (do
      mail <- mailDocumentRemind custommessage ctx doc siglink False
      mailattachments <- makeMailAttachments doc
      scheduleEmailSendout (mctxmailsconfig $ mailContext ctx) $ mail {
                               to = [getMailAddress siglink]
                             , mailInfo = Invitation (documentid doc) (signatorylinkid siglink)
                             , attachments = if isJust $ maybesigninfo siglink
                                             then mailattachments
                                             else []
                             })
    (scheduleSMS =<< smsReminder ctx doc siglink)
  when sent $ do
    when (isPending doc &&  not (hasSigned siglink)) $ do
      -- Reset delivery status if the signatory has not signed yet
      Log.debug $ "Reminder mail send for signatory that has not signed " ++ show (signatorylinkid siglink)
      dbUpdate $ PostReminderSend doc siglink custommessage automatic actor
    triggerAPICallbackIfThereIsOne doc
  return siglink

-- | Send emails to all parties when a document is closed.  If
-- 'sealFixed', then there were earlier emails sent that contained a
-- document that wasn't digitally sealed, so now we resend the
-- document with digital seal.
sendClosedEmails :: (CryptoRNG m, HasMailContext c, MonadDB m, TemplatesMonad m) => c -> Document -> Bool -> m ()
sendClosedEmails ctx document sealFixed = do
    mailattachments <- makeMailAttachments document
    let signatorylinks = documentsignatorylinks document
    forM_ signatorylinks $ \sl -> do
      ml <- if (isGood $ asValidEmail $ getEmail sl)
               then handlePostSignSignup ctx (Email $ getEmail sl) (getFirstName sl) (getLastName sl) (getCompanyName sl) (getCompanyNumber sl)
               else return $ Nothing
      let sendMail = do
            mail <- mailDocumentClosed ctx document ml sl sealFixed
            scheduleEmailSendout (mctxmailsconfig (mailContext ctx)) $
                                 mail { to = [getMailAddress sl]
                                      , attachments = mailattachments
                                      }
      let sendSMS withMail = (scheduleSMS =<< smsClosedNotification ctx document sl withMail sealFixed)
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
sendRejectEmails :: Kontrakcja m => Maybe String -> Context -> Document -> SignatoryLink -> m ()
sendRejectEmails customMessage ctx document signalink = do
  let activatedSignatories = [sl | sl <- documentsignatorylinks document
                                 , isActivatedSignatory (documentcurrentsignorder document) sl || isAuthor sl
                                 , signatorylinkdeliverymethod sl == EmailDelivery || isAuthor sl
                                 ]
  forM_ activatedSignatories $ \sl -> do
    void $ sendNotifications sl
      (do
         mail <- mailDocumentRejected customMessage ctx document signalink False
         scheduleEmailSendout (ctxmailsconfig ctx) $ mail {
                                  to = [getMailAddress sl]
                                })
      (scheduleSMS =<<  smsRejectNotification document sl signalink)

{- |
   Send reminder to all parties in document that can sign, except author
 -}

sendAllReminderEmails :: (Log.MonadLog m, TemplatesMonad m,MonadIO m, CryptoRNG m, MonadDB m, HasMailContext c) =>
                                c -> Actor -> DocumentID -> Bool -> m [SignatoryLink]
sendAllReminderEmails = sendAllReminderEmailsWithFilter (const True)


{- |
   Send reminder to all parties in document that can sign, except author
 -}

sendAllReminderEmailsExceptAuthor :: (Log.MonadLog m, TemplatesMonad m,MonadIO m, CryptoRNG m, MonadDB m, HasMailContext c) =>
                                        c -> Actor -> DocumentID -> Bool -> m [SignatoryLink]
sendAllReminderEmailsExceptAuthor  = sendAllReminderEmailsWithFilter (not . isAuthor)

{- |
   Send reminder to all parties in document - excluding ones that do not pass given filter
 -}
sendAllReminderEmailsWithFilter :: (Log.MonadLog m, TemplatesMonad m,MonadIO m, CryptoRNG m, MonadDB m, HasMailContext c) =>
                                        (SignatoryLink -> Bool) -> c -> Actor -> DocumentID -> Bool -> m [SignatoryLink]
sendAllReminderEmailsWithFilter f ctx actor docid automatic = do
    doc <- dbQuery $ GetDocumentByDocumentID docid
    case (documentstatus doc) of
          Pending -> do
            let unsignedsiglinks = filter f $ filter (isEligibleForReminder doc) $ documentsignatorylinks doc
            sequence . map (sendReminderEmail Nothing ctx actor doc automatic) $ unsignedsiglinks
          _ -> return []


{- |
   Try to sign up a new user. Returns the confirmation link for the new user.
   Nothing means there is already an account or there was an error creating the user.
 -}
handlePostSignSignup :: (CryptoRNG m, MonadDB m, TemplatesMonad m, HasMailContext c) => c -> Email -> String -> String -> String -> String -> m (Maybe KontraLink)
handlePostSignSignup ctx email fn ln cnm cnr = do
  let lang = mctxlang (mailContext ctx)
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
      mnewuser <- createUser' ctx email (fn, ln) (companyid company,True) lang
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

