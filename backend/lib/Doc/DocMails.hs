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
  , runMailT
  , MailT
  ) where

import Control.Conditional ((<|), (|>), ifM)
import Control.Monad.Catch
import Control.Monad.Reader
import Crypto.RNG
import Log
import Text.StringTemplates.Templates (TemplatesMonad, TemplatesT)
import qualified Data.ByteString as BS

import BrandedDomain.Model
import DB
import Doc.API.Callback.Model
import Doc.DocInfo
import Doc.DocStateData hiding (DocumentStatus(..))
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID)
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import Doc.DocViewSMS
import Doc.Logging
import Doc.Model
import EvidenceLog.Model (CurrentEvidenceEventType(..), InsertEvidenceEventWithAffectedSignatoryAndMsg(..))
import File.File
import File.Model
import InputValidation
import Kontra
import KontraPrelude
import Log.Identifier
import MailContext (MailContext(..), MailContextMonad, MailContextT, getMailContext, runMailContextT)
import Mails.MailsData
import Mails.SendMail
import SMS.SMS (scheduleSMS)
import Templates (KontrakcjaGlobalTemplates, runTemplatesT)
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Doc.DocStateData as DS
import qualified SMS.SMS as SMS

{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendDocumentErrorEmail :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m, MailContextMonad m) => User -> Document -> m ()
sendDocumentErrorEmail author document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (\sl -> if isAuthor sl
                            then sendDocumentErrorEmailToAuthor
                            else sendDocumentErrorEmailToSignatory sl)
  where
    sendDocumentErrorEmailToAuthor = do
      let authorlink = fromJust $ getAuthorSigLink document
      sendNotifications authorlink True
        (do
          mail <- mailDocumentErrorForAuthor (getLang author) document
          scheduleEmailSendout $ mail {
                                   to = [getMailAddress authorlink]
                                 })
        (smsDocumentErrorAuthor document authorlink >>= scheduleSMS document)

    -- | Helper function to send emails to invited parties
    -- ??: Should this be in DocControl or in an email-specific file?
    sendDocumentErrorEmailToSignatory signatorylink = do
      sendNotifications signatorylink False
        (do
          mail <- mailDocumentErrorForSignatory document
          scheduleEmailSendoutWithAuthorSenderThroughService
            (documentid document)
            (mail {
                to = [getMailAddress signatorylink]
              , kontraInfoForMail = (Just $ OtherDocumentMail $ documentid document)
              })
        )
        (do
          sms <- smsDocumentErrorSignatory document signatorylink
          scheduleSMS document $ sms { SMS.kontraInfoForSMS = Just (SMS.OtherDocumentSMS $ documentid document) }
        )

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
      scheduleEmailSendoutWithAuthorSenderThroughService
        did
        (mail {
            to = [getMailAddress signatorylink]
          , kontraInfoForMail = (Just $ DocumentInvitationMail did $ signatorylinkid signatorylink)
          })
    )
    (do
      sms <- smsInvitation signatorylink =<< theDocument
      theDocument >>= \doc -> scheduleSMS doc $ sms { SMS.kontraInfoForSMS = Just $ SMS.DocumentInvitationSMS (documentid doc) (signatorylinkid signatorylink)}
    )

  when sent $ do
    documentinvitetext <$> theDocument >>= \text ->
            void $ dbUpdate $ InsertEvidenceEventWithAffectedSignatoryAndMsg
              InvitationEvidence
              (return ())
              (Just signatorylink)
              (Just text <| text /= "" |> Nothing)
              (systemActor $ mctxtime mctx)

sendInvitationEmail1 authorsiglink = do
  when (isSignatory authorsiglink) $ do
       did <- documentid <$> theDocument
       void $ sendNotifications authorsiglink False
          (do
            -- send invitation to sign to author when it is his turn to sign
            mail <- theDocument >>= \d -> mailDocumentAwaitingForAuthor (getLang d) d
            scheduleEmailSendout
              (mail {
                  to = [getMailAddress authorsiglink]
                , kontraInfoForMail = Just $ DocumentInvitationMail did $ signatorylinkid authorsiglink
                })
          )
          (do
            sms <- (\doc -> smsInvitationToAuthor doc authorsiglink) =<< theDocument
            theDocument >>= \doc -> scheduleSMS doc $ sms { SMS.kontraInfoForSMS = Just $ SMS.DocumentInvitationSMS (documentid doc) (signatorylinkid authorsiglink)}
          )

{- |
    Send a reminder email (and update the modification time on the document)
-}
sendReminderEmail :: (MonadLog m, MonadCatch m, TemplatesMonad m, CryptoRNG m, DocumentMonad m, MailContextMonad m) =>
                          Maybe String -> Actor -> Bool -> SignatoryLink  -> m SignatoryLink
sendReminderEmail custommessage actor automatic siglink = logSignatory (signatorylinkid siglink) $ do
  doc <- theDocument
  let domail = do
       mailattachments <- makeMailAttachments doc True
       mail <- mailDocumentRemind automatic custommessage siglink (not (null mailattachments)) doc
       docid <- theDocumentID
       scheduleEmailSendoutWithAuthorSenderThroughService docid $ mail {
           to = [getMailAddress siglink]
         -- We only collect delivery information, if signatory had not signed yet
         , kontraInfoForMail = if (isNothing $ maybesigninfo siglink)
            then Just $ DocumentInvitationMail (documentid doc) (signatorylinkid siglink)
            else Just $ OtherDocumentMail $ documentid doc
         -- We only add attachment after document is signed
         , attachments = attachments mail ++ (if documentstatus doc == DS.Closed
          then mailattachments
          else [])
       }
      dosms = scheduleSMS doc =<< smsReminder automatic doc siglink
      useInvitationMethod = case ( documentstatus doc
                                 , maybesigninfo siglink
                                 , signatoryispartner siglink
                                 , signatorylinkconfirmationdeliverymethod siglink
                                 ) of
                              (_, Just _, True, NoConfirmationDelivery) -> True -- partner that signed, but has no
                                                                               -- confirmation method should
                                                                               -- fallback to invitation method
                              (_, Just _, True, _) -> False -- partner that signed should use confirmation method
                              (_, Nothing, True, _) -> True -- partner that didn't sign should use invitation method
                              (DS.Closed, _, False, NoConfirmationDelivery) -> True -- viewer of signed document with no
                                                                                -- confirmation method, should
                                                                                -- fallback to invitation method
                              (DS.Closed, _, False, _) -> False -- viewer of signed document should use confirmation method
                              (_, _, False, _)  -> True  -- viewer of pending document should use invitation method
      invMethod = signatorylinkdeliverymethod siglink
      confMethod = signatorylinkconfirmationdeliverymethod siglink
      (sendemail, sendsms) = if useInvitationMethod then
                                 ( invMethod `elem` [EmailDelivery, EmailAndMobileDelivery]
                                 , invMethod `elem` [MobileDelivery, EmailAndMobileDelivery]
                                 )
                             else
                                 ( confMethod `elem` [EmailConfirmationDelivery, EmailAndMobileConfirmationDelivery]
                                 , confMethod `elem` [MobileConfirmationDelivery, EmailAndMobileConfirmationDelivery]
                                 )

  sent <- case (sendemail, sendsms) of
           (True, True) -> domail >> dosms >> return True
           (True, False) -> domail >> return True
           (False, True) -> dosms >> return True
           (False, False) -> return False

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
sendClosedEmails :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, TemplatesMonad m, MailContextMonad m) => Bool -> Document -> m ()
sendClosedEmails sealFixed document = do
    mailattachments <- makeMailAttachments document True
    let signatorylinks = documentsignatorylinks document
    forM_ signatorylinks $ \sl -> do
      let sendMail = do
            mail <- mailDocumentClosed False sl sealFixed (not (null mailattachments)) document
            let scheduleEmailFunc
                  | signatoryisauthor sl = scheduleEmailSendout
                  | otherwise            = scheduleEmailSendoutWithAuthorSender (documentid document)
            scheduleEmailFunc $ mail { to = [getMailAddress sl]
                                     , attachments = attachments mail ++ mailattachments
                                     , kontraInfoForMail = (Just $ OtherDocumentMail $ documentid document)
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


makeMailAttachments :: (MonadDB m, MonadThrow m) => Document -> Bool -> m [(String, Either BS.ByteString FileID)]
makeMailAttachments doc withAttachments = map (\(n,f) -> (n, Right $ fileid f)) <$> if (isClosed doc)
  then makeMailAttachmentsForClosedDocument doc withAttachments
  else makeMailAttachmentsForNotClosedDocument doc withAttachments


makeMailAttachmentsForClosedDocument :: (MonadDB m, MonadThrow m) => Document -> Bool -> m [(String, File)]
makeMailAttachmentsForClosedDocument doc withAttachments = do
  mainfile <- do
    file <- dbQuery $ GetFileByFileID $ mainfileid $ fromJust $ (documentsealedfile doc) `mplus` (documentfile doc)
    return [(documenttitle doc ++ ".pdf", file)]
  aattachments <- case withAttachments of
    False -> return []
    True -> forM (filter (not . authorattachmentaddtosealedfile) $ documentauthorattachments doc) $ \aatt -> do
      file <- dbQuery $ GetFileByFileID $ authorattachmentfileid aatt
      return [(authorattachmentname aatt ++ ".pdf", file)]
  let
    allMailAttachments = mainfile ++ (concat aattachments)
    maxFileSize = 10 * 1024 * 1024
  if sum (map (filesize . snd) allMailAttachments) > maxFileSize
    then return []
    else return allMailAttachments

makeMailAttachmentsForNotClosedDocument :: (MonadDB m, MonadThrow m) => Document -> Bool -> m [(String, File)]
makeMailAttachmentsForNotClosedDocument doc withAttachments = do
  mainfile <- do
    file <- dbQuery $ GetFileByFileID $ mainfileid $ fromJust (documentfile doc)
    return [(documenttitle doc ++ ".pdf", file)]
  aattachments <- case withAttachments of
    False -> return []
    True -> forM (documentauthorattachments doc) $ \aatt -> do
      file <- dbQuery $ GetFileByFileID $ authorattachmentfileid aatt
      return [(authorattachmentname aatt ++ ".pdf", file)]
  sattachments <- case withAttachments of
    False -> return []
    True -> forM (concatMap signatoryattachments $ documentsignatorylinks doc) $ \satt -> case signatoryattachmentfile satt of
      Nothing -> return []
      Just sattfid -> do
        file <- dbQuery $ GetFileByFileID sattfid
        return [(filename file, file)]
  let
    allMailAttachments = mainfile ++ (concat aattachments) ++ (concat sattachments)
    maxFileSize = 10 * 1024 * 1024
  if sum (map (filesize . snd) allMailAttachments) > maxFileSize
    then return []
    else return allMailAttachments

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
         mail <- mailDocumentRejected True customMessage (signatoryisauthor sl) signalink document
         scheduleEmailSendout $ mail { to = [getMailAddress sl] }
      )
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
                          String -> Bool -> Bool -> SignatoryLink -> m ()
sendForwardEmail email noContent noAttachments asiglink = do
  doc <- theDocument
  mailattachments <- makeMailAttachments doc (not noAttachments)
  mail <- mailForwardSigned asiglink (not (null mailattachments)) =<< theDocument
  did <- documentid <$> theDocument
  scheduleEmailSendoutWithAuthorSenderThroughService did $ mail {
                               to = [MailAddress "" email]
                             , content =  if (noContent)
                                             then ""
                                             else content mail
                             , attachments = attachments mail ++ mailattachments
                             , kontraInfoForMail = Just $ OtherDocumentMail $ documentid doc
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
    , "method" .= (show $ signatorylinkdeliverymethod sl)
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
-- based on document data, and the rest from CronEnv
runMailT :: (MonadThrow m, MonadDB m, MonadIO m, MonadLog m) => KontrakcjaGlobalTemplates -> String -> Document -> MailT m a -> m a
runMailT templates mailNoreplyAddress doc m = do
  now <- currentTime
  mauthor <- maybe (return Nothing) (dbQuery . GetUserByID) $ join $ maybesignatory <$> getAuthorSigLink doc
  bd <- maybe (dbQuery GetMainBrandedDomain) (dbQuery . GetBrandedDomainByUserID) (userid <$> mauthor)
  let mctx = MailContext {
          mctxlang = documentlang doc
        , mctxcurrentBrandedDomain = bd
        , mctxtime = now
        , mctxmailNoreplyAddress = mailNoreplyAddress
        }
  runTemplatesT (getLang doc, templates) . runMailContextT mctx $ m
