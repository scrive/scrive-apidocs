module Doc.DocMails (
    sendInvitationEmails
  , sendInvitationEmail1
  , sendReminderEmail
  , sendAllReminderEmails
  , sendAllReminderEmailsExceptAuthor
  , sendForwardEmail
  , sendClosedEmails
  , sendDocumentTimeoutedEmail
  , sendRejectEmails
  , sendForwardSigningMessages
  , sendDocumentErrorEmail
  , sendPartyProcessFinalizedNotification
  , sendPinCode
  , sendPortalMail
  , PortalMailKind(..)
  , makeMailAttachments
  , runMailT
  , MailT
  ) where

import Control.Arrow (second)
import Control.Conditional ((<|), (|>), ifM, whenM)
import Control.Monad.Catch
import Control.Monad.Reader
import Crypto.RNG
import Log
import Text.StringTemplates.Templates (TemplatesMonad, TemplatesT)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

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
import Doc.SignatoryUtils
import EventStream.Class
import EvidenceLog.Model
  ( CurrentEvidenceEventType(..)
  , InsertEvidenceEventWithAffectedSignatoryAndMsg(..)
  )
import File.Model
import File.Types
import Folder.Model
import InputValidation
import Kontra
import KontraLink
import Log.Identifier
import MailContext
import Mails.MailsData
import Mails.SendMail
import SMS.SMS (scheduleSMS)
import Templates (KontrakcjaGlobalTemplates, runTemplatesT)
import User.Action
import User.Email
import User.Model
import User.UserAccountRequest
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Doc.DocStateData as DS
import qualified MailContext.Internal
import qualified SMS.SMS as SMS

{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendDocumentErrorEmail
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , MonadLog m
     , TemplatesMonad m
     , MailContextMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => User
  -> Document
  -> m ()
sendDocumentErrorEmail author document = do
  let signlinks = documentsignatorylinks document
  forM_
    signlinks
    (\sl -> if isAuthor sl
      then sendDocumentErrorEmailToAuthor
      else sendDocumentErrorEmailToSignatory sl
    )
  where
    sendDocumentErrorEmailToAuthor = do
      let authorlink = fromJust $ getAuthorSigLink document
      sendNotifications
        authorlink
        True
        (do
          mail <- mailDocumentErrorForAuthor (getLang author) document
          scheduleEmailSendout $ mail { to = [getMailAddress authorlink] }
        )
        (smsDocumentErrorAuthor document authorlink >>= scheduleSMS document)

    -- | Helper function to send emails to invited parties
    -- ??: Should this be in DocControl or in an email-specific file?
    sendDocumentErrorEmailToSignatory signatorylink = do
      sendNotifications
        signatorylink
        False
        (do
          mail <- mailDocumentErrorForSignatory document
          scheduleEmailSendoutWithAuthorSenderThroughService
            (documentid document)
            (mail { to                = [getMailAddress signatorylink]
                  , kontraInfoForMail = Just . OtherDocumentMail $ documentid document
                  }
            )
        )
        (do
          sms <- smsDocumentErrorSignatory document signatorylink
          scheduleSMS document $ sms
            { SMS.kontraInfoForSMS = Just (SMS.OtherDocumentSMS $ documentid document)
            }
        )

{- |
   Send emails to all of the invited parties, respecting the sign order.
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendInvitationEmails
  :: ( CryptoRNG m
     , MonadThrow m
     , MonadLog m
     , TemplatesMonad m
     , DocumentMonad m
     , MailContextMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => Bool
  -> m ()
sendInvitationEmails authorsignsimmediately = do
  signlinks <- theDocument >>= \d -> return
    [ sl
    | sl <- documentsignatorylinks d
    , matchingSignOrder d sl
    , not (authorsignsimmediately && onlyAuthorSigns d)
    , signatorylinkdeliverymethod sl
      `elem` [EmailDelivery, MobileDelivery, EmailAndMobileDelivery]
    , notSignedOrNotApprovedOrIsAViewer sl
    , not (isAuthor sl) || (isAuthor sl && not authorsignsimmediately)
    ]
  forM_ signlinks sendInvitationEmail1
  where
    matchingSignOrder d sl =
      let so = signatorysignorder sl
      in  so > documentprevioussignorder d && so <= documentcurrentsignorder d
    onlyAuthorSigns d = all
      (\sl -> isAuthor sl || not (isSignatory || isApprover $ sl))
      (documentsignatorylinks d)

sendPartyProcessFinalizedNotification
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadLog m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => Document
  -> SignatoryLink
  -> m ()
sendPartyProcessFinalizedNotification document signatoryLink = do
  case signatoryrole signatoryLink of
    SignatoryRoleApprover ->
      sendPartyProcessFinalizedNotification' document signatoryLink DocumentApproved
    SignatoryRoleSigningParty ->
      sendPartyProcessFinalizedNotification' document signatoryLink DocumentSigned
    _ -> pure ()

sendPartyProcessFinalizedNotification'
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadLog m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => Document
  -> SignatoryLink
  -> ProcessFinishedAction
  -> m ()
sendPartyProcessFinalizedNotification' document signatoryLink action = do
  case signatorylinknotificationdeliverymethod signatoryLink of
    NoNotificationDelivery -> pure ()
    EmailAndMobileNotificationDelivery ->
      sendPartyProcessFinalizedNotificationEmail document signatoryLink action
        >> sendPartyProcessFinalizedNotificationSms document signatoryLink action
    EmailNotificationDelivery ->
      sendPartyProcessFinalizedNotificationEmail document signatoryLink action
    MobileNotificationDelivery ->
      sendPartyProcessFinalizedNotificationSms document signatoryLink action

sendPartyProcessFinalizedNotificationEmail
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadLog m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => Document
  -> SignatoryLink
  -> ProcessFinishedAction
  -> m ()
sendPartyProcessFinalizedNotificationEmail document signatoryLink action = do
  email <- mailPartyProcessFinalizedNotification document signatoryLink action
  scheduleEmailSendout email

sendPartyProcessFinalizedNotificationSms
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadLog m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => Document
  -> SignatoryLink
  -> ProcessFinishedAction
  -> m ()
sendPartyProcessFinalizedNotificationSms document signatoryLink action = do
  sms <- smsPartyProcessFinalizedNotification document signatoryLink action
  scheduleSMS document sms

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendInvitationEmail1
  :: ( CryptoRNG m
     , MonadThrow m
     , MonadLog m
     , TemplatesMonad m
     , DocumentMonad m
     , MailContextMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => SignatoryLink
  -> m ()
sendInvitationEmail1 signatorylink | not (isAuthor signatorylink) = do
  did  <- theDocumentID
  mctx <- getMailContext
  let invitationTo = if
        | isSignatory signatorylink -> Sign
        | isApprover signatorylink  -> Approve
        | otherwise                 -> View
  -- send invitation to sign to invited person
  sent <- sendNotifications
    signatorylink
    False

    (do
      mail <- theDocument >>= mailInvitation True invitationTo (Just signatorylink)
      -- ?? Do we need to read in the contents? -EN
      -- _attachmentcontent <- liftIO $ documentFileID document >>= getFileContents ctx
      scheduleEmailSendoutWithAuthorSenderThroughService
        did
        (mail
          { to                = [getMailAddress signatorylink]
          , kontraInfoForMail = Just . DocumentInvitationMail did $ signatorylinkid
                                  signatorylink
          }
        )
    )
    (do
      sms <- smsInvitation invitationTo signatorylink =<< theDocument
      theDocument >>= \doc -> scheduleSMS doc $ sms
        { SMS.kontraInfoForSMS = Just $ SMS.DocumentInvitationSMS
                                   (documentid doc)
                                   (signatorylinkid signatorylink)
        }
    )

  let eventFields = do
        F.value "used_email_address" . email $ getMailAddress signatorylink
        F.value "used_mobile_number" $ getMobile signatorylink

  when sent $ do
    documentinvitetext <$> theDocument >>= \text ->
      void . dbUpdate $ InsertEvidenceEventWithAffectedSignatoryAndMsg
        InvitationEvidence
        eventFields
        (Just signatorylink)
        (Just text <| text /= "" |> Nothing)
        (systemActor $ mctx ^. #time)

sendInvitationEmail1 authorsiglink = do
  when (isSignatory authorsiglink) $ do
    did <- documentid <$> theDocument
    void $ sendNotifications
      authorsiglink
      False
      (do
         -- send invitation to sign to author when it is his turn to sign
        mail <- theDocument >>= \d -> mailDocumentAwaitingForAuthor (getLang d) d
        scheduleEmailSendout
          (mail
            { to                = [getMailAddress authorsiglink]
            , kontraInfoForMail = Just . DocumentInvitationMail did $ signatorylinkid
                                    authorsiglink
            }
          )
      )
      (do
        sms <- (`smsInvitationToAuthor` authorsiglink) =<< theDocument
        theDocument >>= \doc -> scheduleSMS doc $ sms
          { SMS.kontraInfoForSMS = Just $ SMS.DocumentInvitationSMS
                                     (documentid doc)
                                     (signatorylinkid authorsiglink)
          }
      )


-- | Send a reminder email (and update the modification time on the document).
sendReminderEmail
  :: ( MonadLog m
     , MonadCatch m
     , TemplatesMonad m
     , CryptoRNG m
     , DocumentMonad m
     , MailContextMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => Maybe Text
  -> Actor
  -> Bool
  -> SignatoryLink
  -> m SignatoryLink
sendReminderEmail custommessage actor automatic siglink =
  logSignatory (signatorylinkid siglink) $ do
    doc <- theDocument
    let
      domail = do
        mailattachments <- makeMailAttachments doc True
        let forceLink      = shouldForceEmailLink siglink
            documentAttach = not forceLink && not (null mailattachments)
        mail <- mailDocumentRemind automatic
                                   custommessage
                                   doc
                                   siglink
                                   documentAttach
                                   forceLink
        docid <- theDocumentID
        scheduleEmailSendoutWithAuthorSenderThroughService docid $ mail
          { to                = [getMailAddress siglink]
          -- We only collect delivery information, if signatory had not
          -- signed yet
          , kontraInfoForMail = if notSignedOrNotApprovedOrIsAViewer siglink
                                  then Just $ DocumentInvitationMail
                                    (documentid doc)
                                    (signatorylinkid siglink)
                                  else Just . OtherDocumentMail $ documentid doc
          -- We only add attachment after document is signed
          , attachments       = attachments mail
                                  <> (if documentstatus doc == DS.Closed && not forceLink
                                       then mailattachments
                                       else []
                                     )
          }
      dosms = scheduleSMS doc =<< smsReminder automatic doc siglink
      useInvitationMethod =
        case
            ( documentstatus doc
            , maybesigninfo siglink
            , isSignatory siglink || isApprover siglink
            , signatorylinkconfirmationdeliverymethod siglink
            )
          of

          -- A signing party or approver that signed/approved, but has
          -- no confirmation method, should fallback to invitation
          -- method.
            (_, Just _, True, NoConfirmationDelivery) -> True

            -- A signing party or approver that signed/approved should
            -- use confirmation method.
            (_        , Just _ , True , _) -> False

            -- A signing party/approver that didn't sign/approve
            -- should use invitation method.
            (_        , Nothing, True , _) -> True

            -- A viewer of a signed document with no confirmation
            -- method, should fallback to invitation method.
            (DS.Closed, _, False, NoConfirmationDelivery) -> True

            -- A viewer of a signed document should use confirmation method.
            (DS.Closed, _      , False, _) -> False

            -- A viewer of a pending document should use invitation method.
            (_        , _      , False, _) -> True

      invMethod            = signatorylinkdeliverymethod siglink
      confMethod           = signatorylinkconfirmationdeliverymethod siglink
      (sendemail, sendsms) = if useInvitationMethod
        then
          ( invMethod `elem` [EmailDelivery, EmailAndMobileDelivery]
          , invMethod `elem` [MobileDelivery, EmailAndMobileDelivery]
          )
        else
          ( confMethod
            `elem` [ EmailConfirmationDelivery
                   , EmailAndMobileConfirmationDelivery
                   , EmailLinkConfirmationDelivery
                   , EmailLinkAndMobileConfirmationDelivery
                   ]
          , confMethod
            `elem` [MobileConfirmationDelivery, EmailAndMobileConfirmationDelivery]
          )

    sent <- case (sendemail, sendsms) of
      (True , True ) -> domail >> dosms >> return True
      (True , False) -> domail >> return True
      (False, True ) -> dosms >> return True
      (False, False) -> return False

    when sent $ do
      when (isPending doc && notSignedOrNotApprovedOrIsAViewer siglink) $ do
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
sendClosedEmails
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , MonadLog m
     , TemplatesMonad m
     , MailContextMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => Bool
  -> Document
  -> m ()
sendClosedEmails sealFixed document = do
  mailattachments <- makeMailAttachments document True
  let signatorylinks = documentsignatorylinks document

  forM_ signatorylinks $ \sl -> do
    let scheduleEmailFunc
          | signatoryisauthor sl = scheduleEmailSendout
          | otherwise = scheduleEmailSendoutWithAuthorSender (documentid document)

        sendMail forceLink = do
          mail <- mailDocumentClosed False
                                     sl
                                     sealFixed
                                     (not (null mailattachments))
                                     forceLink
                                     document
          scheduleEmailFunc $ mail
            { to                = [getMailAddress sl]
            , attachments = attachments mail <> if forceLink then [] else mailattachments
            , kontraInfoForMail = Just $ DocumentConfirmationMail (documentid document)
                                                                  (signatorylinkid sl)
            , replyTo           = let maybeAuthor = find signatoryisauthor signatorylinks
                                  in  if signatoryisauthor sl && isJust maybeAuthor
                                        then Nothing
                                        else fmap getMailAddress maybeAuthor
            }

    let sendSMS withMail =
          scheduleSMS document =<< smsClosedNotification document sl withMail sealFixed

    let useMail = isGood . asValidEmail $ getEmail sl
        useSMS  = isGood . asValidPhoneForSMS $ getMobile sl

    case signatorylinkconfirmationdeliverymethod sl of
      NoConfirmationDelivery             -> return ()
      EmailConfirmationDelivery          -> when useMail $ sendMail False
      EmailLinkConfirmationDelivery      -> when useMail $ sendMail True
      MobileConfirmationDelivery         -> when useSMS $ sendSMS False
      EmailAndMobileConfirmationDelivery -> do
        when useMail $ sendMail False
        when useSMS $ sendSMS useMail
      EmailLinkAndMobileConfirmationDelivery -> do
        when useMail $ sendMail True
        when useSMS $ sendSMS useMail

-- | Construct the list of attachments. Return nothing if the attachments would
-- be above the maximum size. The implementation changed over time, but nobody
-- could be bothered to update the type signature.
makeMailAttachments
  :: (MonadDB m, MonadThrow m)
  => Document
  -> Bool
  -> m [(Text, Either BS.ByteString FileID)]
makeMailAttachments doc withAttachments = do
  let
    mainPdfName   = documenttitle doc <> ".pdf"

    documentFiles = case documentfile doc of
      Just (ClosedFile DigitallySignedFile {..}) -> [(mainPdfName, digitallySignedFile)]
      Just ClosedVerimiQesFile {..} ->
        [ (mainPdfName, mainfileWithQesSignatures)
        , (documenttitle doc <> "_evidence.pdf", digitallySignedFile evidenceFile)
        ]
      _ | Just mainfile <- documentinputfile doc -> [(mainPdfName, mainfile)]
      _ ->
        unexpectedError
          "`makeMailAttachments` was called on a document without an associated main file."


  separateAuthorAttachments <- do
    let attachments
          | isClosed doc = filter (not . authorattachmentaddtosealedfile)
          $ documentauthorattachments doc
          | otherwise = documentauthorattachments doc
    forM attachments $ \attachment -> do
      file <- dbQuery . GetFileByFileID $ authorattachmentfileid attachment
      return (authorattachmentname attachment, file)

  separateSignatoryAttachments <- do
    let attachmentfileids
          | isClosed doc
          = []
          | otherwise
          = mapMaybe signatoryattachmentfile
            . concatMap signatoryattachments
            $ documentsignatorylinks doc
    -- This replicates existing behaviour -- but why aren't we using
    -- `signatoryattachmentname`? See ticket CORE-2498.
    forM attachmentfileids $ \attachmentfileid -> do
      attachmentfile <- dbQuery $ GetFileByFileID attachmentfileid
      return (filename attachmentfile, attachmentfile)

  let mailAttachments
        | withAttachments
        = documentFiles <> separateAuthorAttachments <> separateSignatoryAttachments
        | otherwise
        = documentFiles
      maxFileSize = 10 * 1024 * 1024

  return $ if sum (map (filesize . snd) mailAttachments) <= maxFileSize
    then map (second $ Right . fileid) mailAttachments
    else []


{- |
   Send an email to the author when the document is timedout
 -}
sendDocumentTimeoutedEmail
  :: ( MonadLog m
     , TemplatesMonad m
     , MonadCatch m
     , CryptoRNG m
     , DocumentMonad m
     , MailContextMonad m
     )
  => Document
  -> m ()
sendDocumentTimeoutedEmail document = do
  let authorsiglink = fromJust $ getAuthorSigLink document
  mail <- mailDocumentTimedout document
  scheduleEmailSendout $ mail { to = [getMailAddress authorsiglink] }

{- |
   Send an email to the author and to all signatories who were sent an invitation  when the document is rejected
 -}
sendRejectEmails :: Kontrakcja m => Maybe Text -> SignatoryLink -> Document -> m ()
sendRejectEmails customMessage signalink document = do
  let activatedSignatories =
        [ sl
        | sl <- documentsignatorylinks document
        , isActivatedSignatory (documentcurrentsignorder document) sl || isAuthor sl
        , signatorylinkdeliverymethod sl
          `elem` [EmailDelivery, EmailAndMobileDelivery, MobileDelivery]
          ||     isAuthor sl
        ]
  forM_ activatedSignatories $ \sl -> do
    void $ sendNotifications
      sl
      True
      (do
        mail <- mailDocumentRejected True
                                     customMessage
                                     (signatoryisauthor sl)
                                     signalink
                                     document
        scheduleEmailSendout $ mail { to = [getMailAddress sl] }
      )
      (scheduleSMS document =<< smsRejectNotification document sl signalink)

sendForwardSigningMessages
  :: Kontrakcja m => Maybe Text -> SignatoryLink -> SignatoryLink -> Document -> m ()
sendForwardSigningMessages customMessage originalSignatory newSignatory doc = do
  sendForwardSigningMessagesForNewSignatory customMessage
                                            originalSignatory
                                            newSignatory
                                            doc
  unless (isAuthor originalSignatory) $ do
    sendForwardSigningMessagesToAuthor originalSignatory newSignatory doc

sendForwardSigningMessagesForNewSignatory
  :: Kontrakcja m => Maybe Text -> SignatoryLink -> SignatoryLink -> Document -> m ()
sendForwardSigningMessagesForNewSignatory customMessage originalsiglink newsiglink doc =
  do
    let sendMail = do
          mail <- mailForwardSigningForNewSignatory customMessage
                                                    originalsiglink
                                                    newsiglink
                                                    doc
          scheduleEmailSendout $ mail
            { to                = [getMailAddress newsiglink]
            , kontraInfoForMail = Just $ DocumentInvitationMail
                                    (documentid doc)
                                    (signatorylinkid newsiglink)
            }

    let sendSMS =
          scheduleSMS doc
            =<< smsForwardSigningForNewSignatory originalsiglink newsiglink doc

    let useMail = isGood . asValidEmail $ getEmail newsiglink
        useSMS  = isGood . asValidPhoneForSMS $ getMobile newsiglink

    case signatorylinkdeliverymethod newsiglink of
      APIDelivery            -> return ()
      PadDelivery            -> return ()
      EmailDelivery          -> when useMail sendMail
      MobileDelivery         -> when useSMS sendSMS
      EmailAndMobileDelivery -> do
        when useMail sendMail
        when useSMS  sendSMS
      PortalDelivery -> return ()

sendForwardSigningMessagesToAuthor
  :: Kontrakcja m => SignatoryLink -> SignatoryLink -> Document -> m ()
sendForwardSigningMessagesToAuthor originalsiglink newsiglink doc = do
  let authorsiglink = fromJust $ getAuthorSigLink doc
  let sendMail = do
        mail <- mailForwardSigningForAuthor originalsiglink newsiglink doc
        scheduleEmailSendout $ mail
          { to                = [getMailAddress authorsiglink]
          , kontraInfoForMail = Just . OtherDocumentMail $ documentid doc
          }

  let sendSMS =
        scheduleSMS doc =<< smsForwardSigningForAuthor originalsiglink newsiglink doc

  let useMail = isGood . asValidEmail $ getEmail authorsiglink
      useSMS  = isGood . asValidPhoneForSMS $ getMobile authorsiglink

  case signatorylinkconfirmationdeliverymethod authorsiglink of
    NoConfirmationDelivery             -> return ()
    EmailConfirmationDelivery          -> when useMail sendMail
    EmailLinkConfirmationDelivery      -> when useMail sendMail
    MobileConfirmationDelivery         -> when useSMS sendSMS
    EmailAndMobileConfirmationDelivery -> do
      when useMail sendMail
      when useSMS  sendSMS
    EmailLinkAndMobileConfirmationDelivery -> do
      when useMail sendMail
      when useSMS  sendSMS



{- |
   Send reminder to all parties in document that can sign
 -}

sendAllReminderEmails
  :: ( MonadLog m
     , TemplatesMonad m
     , MonadCatch m
     , CryptoRNG m
     , DocumentMonad m
     , MailContextMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => Actor
  -> Bool
  -> m [SignatoryLink]
sendAllReminderEmails = sendAllReminderEmailsWithFilter (const True)


{- |
   Send reminder to all parties in document that can sign, except author
 -}

sendAllReminderEmailsExceptAuthor
  :: ( MonadLog m
     , TemplatesMonad m
     , MonadCatch m
     , CryptoRNG m
     , DocumentMonad m
     , MailContextMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => Actor
  -> Bool
  -> m [SignatoryLink]
sendAllReminderEmailsExceptAuthor = sendAllReminderEmailsWithFilter (not . isAuthor)

{- |
   Send reminder to all parties in document - excluding ones that do not pass given filter
 -}
sendAllReminderEmailsWithFilter
  :: ( MonadLog m
     , TemplatesMonad m
     , MonadCatch m
     , CryptoRNG m
     , DocumentMonad m
     , MailContextMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => (SignatoryLink -> Bool)
  -> Actor
  -> Bool
  -> m [SignatoryLink]
sendAllReminderEmailsWithFilter f actor automatic = do
  ifM
      (isPending <$> theDocument)
  {-then-}(do
            doc <- theDocument
            let unsignedsiglinks =
                  filter (isEligibleForReminder doc) $ documentsignatorylinks doc
                text           = documentinvitetext doc
                mCustomMessage = if T.null text then Nothing else Just text
            mapM (sendReminderEmail mCustomMessage actor automatic)
              $ filter f unsignedsiglinks
          )
  {-else-}$ do
              return []

{- |
    Send a forward email
-}
sendForwardEmail
  :: ( MonadLog m
     , TemplatesMonad m
     , MonadThrow m
     , CryptoRNG m
     , DocumentMonad m
     , MailContextMonad m
     )
  => Text
  -> Bool
  -> Bool
  -> SignatoryLink
  -> m ()
sendForwardEmail email noContent noAttachments asiglink = do
  doc             <- theDocument
  mailattachments <- makeMailAttachments doc (not noAttachments)
  mail <- mailForwardSigned asiglink (not (null mailattachments)) =<< theDocument
  did             <- documentid <$> theDocument
  scheduleEmailSendoutWithAuthorSenderThroughService did $ mail
    { to                = [MailAddress "" email]
    , content           = if noContent then "" else content mail
    , attachments       = attachments mail <> mailattachments
    , kontraInfoForMail = Just . OtherDocumentMail $ documentid doc
    }
  return ()



sendPinCode
  :: ( MonadLog m
     , TemplatesMonad m
     , MonadThrow m
     , DocumentMonad m
     , CryptoRNG m
     , MailContextMonad m
     , KontraMonad m
     , MonadCatch m
     , MonadEventStream m
     )
  => SignatoryLink
  -> Text
  -> Text
  -> m ()
sendPinCode sl phone pin = do
  ctx <- getContext
  doc <- theDocument
  -- Record evidence only for auth-to-view (i.e. if the document is not closed).
  whenM ((== AuthenticationToView) . mkAuthKind <$> theDocument) $ do
    void
      $   dbUpdate
      .   InsertEvidenceEventWithAffectedSignatoryAndMsg SMSPinSendEvidence
                                                         (return ())
                                                         (Just sl)
                                                         (Just phone)
      =<< signatoryActor ctx sl
  scheduleSMS doc =<< smsPinCodeSendout doc sl phone pin

-- | Send an invitation or reminder email to a signatory that was configured to
-- use the `PortalMethod` `DeliveryMethod`.
sendPortalMail
  :: ( MonadLog m
     , TemplatesMonad m
     , MonadThrow m
     , CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     )
  => PortalMailKind
  -> User  -- ^ The author of the document that triggered the invitation.
  -> Text  -- ^ The portal URL configured for the user group of the document author.
  -> SignatoryLink  -- ^ The signatory we want to invite.
  -> CreateUserContext
  -> m ()
sendPortalMail kind authorUser portalUrl sl cuctx = do
  let email = Email $ getEmail sl
      name  = getFullName sl
      role  = signatoryrole sl
  muser <- dbQuery $ GetUserByEmail email
  case muser of
    -- existing, activated user account
    Just user | isJust $ user ^. #hasAcceptedTOS -> do
      let link = LinkPortalInviteWithAccount portalUrl email
      mail <- constructPortalMail kind authorUser link role
      scheduleEmailSendout
        $ mail { to = [MailAddress name (unEmail email)], kontraInfoForMail = Nothing }

    -- account not yet activated, or not existing
    _ -> do
      user <- whenNothing muser $ createUserForPortal (getLang authorUser) email cuctx
      uar  <- newUserAccountRequest $ user ^. #id
      let link =
            LinkPortalInviteWithoutAccount portalUrl email (uarToken uar) (uarExpires uar)
      mail <- constructPortalMail kind authorUser link role
      scheduleEmailSendout
        $ mail { to = [MailAddress name (unEmail email)], kontraInfoForMail = Nothing }

createUserForPortal
  :: (MonadLog m, TemplatesMonad m, MonadThrow m, CryptoRNG m, MonadDB m)
  => Lang
  -> Email
  -> CreateUserContext
  -> m User
createUserForPortal lang email cuctx = do
  ugFolder <- dbUpdate . FolderCreate $ defaultFolder
  let ug0 = set #homeFolderID (Just $ ugFolder ^. #id) defaultUserGroup
  ug     <- dbUpdate $ UserGroupCreate ug0
  mnuser <- createUser email ("", "") (ug ^. #id, True) lang PortalInvite cuctx
  case mnuser of
    Nothing    -> unexpectedError "User was not created"
    Just nuser -> return nuser

-- Notification sendout

-- | Send out mail and/or SMS or not, depending on delivery method.  Return 'False' iff nothing was sent. Email is always sent to authors if alwaysEmailAuthor is True.
sendNotifications
  :: (Monad m, MonadLog m) => SignatoryLink -> Bool -> m () -> m () -> m Bool
sendNotifications sl alwaysEmailAuthor domail dosms = do
  logInfo "Delivery method chosen for a signatory" $ object
    [ identifier $ signatorylinkid sl
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
-- based on document data, and the rest from CronEnv
runMailT
  :: (MonadThrow m, MonadDB m, MonadIO m, MonadLog m)
  => KontrakcjaGlobalTemplates
  -> Text
  -> Document
  -> MailT m a
  -> m a
runMailT templates mailNoreplyAddress doc m = do
  now <- currentTime
  bd  <- case getAuthorUserId doc of
    Just uid -> dbQuery $ GetBrandedDomainByUserID uid
    Nothing  -> dbQuery GetMainBrandedDomain
  let mctx = MailContext { lang               = documentlang doc
                         , brandedDomain      = bd
                         , time               = now
                         , mailNoreplyAddress = mailNoreplyAddress
                         }
  runTemplatesT (getLang doc, templates) . runMailContextT mctx $ m

-- Local utils
notSignedOrNotApprovedOrIsAViewer :: SignatoryLink -> Bool
notSignedOrNotApprovedOrIsAViewer =
  isSignatoryAndHasNotSigned || isApproverAndHasNotApproved || isViewer
