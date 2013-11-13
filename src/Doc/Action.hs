{-# LANGUAGE NoImplicitPrelude #-}
module Doc.Action (
    postDocumentPreparationChange
  , postDocumentPendingChange
  , postDocumentRejectedChange
  , postDocumentCanceledChange
  , sendReminderEmail
  , sendInvitationEmail1
  , sendAllReminderEmails
  , sendClosedEmails
  , timeoutDocuments
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Logic
import Crypto.RNG
import Data.Char
import DB
import Doc.DocSeal
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
import Util.MonadUtils
import ThirdPartyStats.Core
import ActionQueue.UserAccountRequest
import ActionQueue.Scheduler
import User.Action
import User.Utils
import Data.List hiding (head, tail)
import Data.Maybe hiding (fromJust)
import qualified Data.ByteString as BS
import ForkAction
import Doc.API.Callback.Model
import Company.Model

-- | Log a document event, adding some standard properties.
logDocEvent :: Kontrakcja m => EventName -> Document -> User -> [EventProperty] -> m ()
logDocEvent name doc user extraProps = do
  comp <- getCompanyForUser user
  now <- getMinutesTime
  ip <- ctxipnumber <$> getContext
  let uid = userid user
      email = Email $ getEmail user
      fullname = getFullName user
      deliverymethod = fromMaybe "undefined" $ show . signatorylinkdeliverymethod <$> getSigLinkFor doc uid
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

postDocumentPreparationChange :: Kontrakcja m => Document -> Bool -> m ()
postDocumentPreparationChange doc@Document{documenttitle} skipauthorinvitation = do
  let docid = documentid doc
  triggerAPICallbackIfThereIsOne doc
  unless (isPending doc) $
    stateMismatchError "postDocumentPreparationChange" Pending doc
  Log.docevent $ "Preparation -> Pending; Sending invitation emails: " ++ show docid
  ctx <- getContext
  msaveddoc <- saveDocumentForSignatories doc
  document' <- case msaveddoc of
    Left msg -> do
      Log.error $ "Failed to save document #" ++ (show docid) ++ " for signatories " ++ msg
      return doc
    Right saveddoc -> return saveddoc
  Log.server $ "Sending invitation emails for document #" ++ show docid ++ ": " ++ documenttitle

  -- Stat logging
  now <- getMinutesTime
  author <- getDocAuthor doc
  docssent <- dbQuery $ GetDocsSent (userid author)
  -- Log the current time as the last doc sent time
  asyncLogEvent SetUserProps [UserIDProp (userid author),
                              someProp "Last Doc Sent" now,
                              numProp "Docs sent" (fromIntegral $ docssent)
                              ]
  json <- documentJSON Nothing False True False Nothing Nothing doc
  asyncLogEvent (UploadDocInfo json) [UserIDProp (userid author),
                                      DocIDProp (documentid doc)]
  logDocEvent "Doc Sent" doc author []

  sendInvitationEmails ctx document' skipauthorinvitation
  sendInvitationEmailsToViewers ctx document'

  return ()

postDocumentPendingChange :: Kontrakcja m => Document -> Document -> m ()
postDocumentPendingChange doc@Document{documentid, documenttitle} olddoc = do
  triggerAPICallbackIfThereIsOne doc
  unless (isPending doc) $
    stateMismatchError "postDocumentPendingChange" Pending doc
  case undefined of
    _ | allSignatoriesSigned doc -> do
      Log.docevent $ "All have signed; " ++ show documentstatus ++ " -> Closed: " ++ show documentid
      ctx <- getContext
      let time = ctxtime ctx
      dbUpdate $ CloseDocument documentid (systemActor time)
      closeddoc <- dbQuery $ GetDocumentByDocumentID documentid

      Log.docevent $ "Pending -> Closed; Sending emails: " ++ show documentid
      author <- getDocAuthor doc
      logDocEvent "Doc Closed" doc author []
      asyncLogEvent SetUserProps [UserIDProp (userid author),
                                  someProp "Last Doc Closed" time]
      kCommit
      forkAction ("Sealing document #" ++ show documentid ++ ": " ++ documenttitle) $ do
        enewdoc <- sealDocument closeddoc
        case enewdoc of
          Just newdoc -> sendClosedEmails ctx newdoc False
          Nothing -> do
            Log.server $ "Sending seal error emails for document #" ++ show documentid ++ ": " ++ documenttitle
            sendDocumentErrorEmail closeddoc author
        return ()
    _ -> when (documentcurrentsignorder doc /= documentcurrentsignorder olddoc) $ do
      ctx <- getContext
      Log.server $ "Resending invitation emails for document #" ++ show documentid ++ ": " ++ documenttitle
      sendInvitationEmails ctx doc False
      return ()
  where
    allSignatoriesSigned = all (isSignatory =>>^ hasSigned) . documentsignatorylinks

postDocumentRejectedChange :: Kontrakcja m => Document -> SignatoryLinkID -> m ()
postDocumentRejectedChange doc@Document{..} siglinkid = do
  triggerAPICallbackIfThereIsOne doc
  unless (isRejected doc) $
    stateMismatchError "postDocumentRejectedChange" Rejected doc
  Log.docevent $ "Pending -> Rejected; send reject emails: " ++ show documentid
  Log.server $ "Sending rejection emails for document #" ++ show documentid ++ ": " ++ documenttitle
  ctx <- getContext
  -- Log the fact that the current user rejected a document.
  maybe (return ())
        (\user -> logDocEvent "Doc Rejected" doc user [])
        (ctxmaybeuser ctx)
  customMessage <- getOptionalField  asValidInviteText "customtext"
  sendRejectEmails customMessage ctx doc ($(fromJust) $ getSigLinkFor doc siglinkid)
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
  mapM_ (\r -> logDocEvent "Doc Canceled" doc author [reasonProp r]) issues

  when (not (null issues)) $ do
      ctx <- getContext
      Log.server $ "Sending cancelation emails for document #" ++ show documentid ++ ": " ++ documenttitle
      sendElegDataMismatchEmails ctx doc author
  where
    reasonProp = stringProp "Reason" . show

stateMismatchError :: Kontrakcja m => String -> DocumentStatus -> Document -> m a
stateMismatchError funame expected Document{documentstatus, documentid} = do
  Log.debug $ funame ++ ": document #" ++ show documentid ++ " in " ++ show documentstatus ++ " state, expected " ++ show expected
  internalError

getDocAuthor :: Kontrakcja m => Document -> m User
getDocAuthor doc = do
  authorid <- guardJust $ getAuthorSigLink doc >>= maybesignatory
  guardJustM $ dbQuery $ GetUserByID authorid

{- |
    Goes through each signatory, and if a user exists this saves it for that user
    by linking the signatory to the user's account.
-}
saveDocumentForSignatories :: Kontrakcja m => Document -> m (Either String Document)
saveDocumentForSignatories doc =
  foldM foldSaveForSig (Right doc) . filter (not . isAuthor) $ (documentsignatorylinks doc)
  where
    {- |
        Wraps up the saveDocumentForSignatory so we can use it in a fold
    -}
    foldSaveForSig :: Kontrakcja m => (Either String Document) -> SignatoryLink -> m (Either String Document)
    foldSaveForSig (Left msg) _ = return $ Left msg
    foldSaveForSig (Right doc') siglink = saveDocumentForSignatory doc' siglink
    {- |
        Saves the document for the given signatorylink.  It does this by checking to see
        if there is a user with a matching email, and if there is it hooks up the signatory
        link to that user.
    -}
    saveDocumentForSignatory :: Kontrakcja m => Document -> SignatoryLink -> m (Either String Document)
    saveDocumentForSignatory doc' sl = do
      let sigemail = getEmail sl
      muser <- case (sigemail) of
                "" -> return Nothing
                _  -> dbQuery $ GetUserByEmail (Email sigemail)
      case muser of
        Nothing -> return $ Right doc'
        Just user -> do
          udoc <- do
            mdoc <- runMaybeT $ do
              True <- dbUpdate $ SaveDocumentForUser (documentid  doc') user (signatorylinkid sl)
              newdoc <- dbQuery $ GetDocumentByDocumentID (documentid  doc')
              return newdoc
            return $ maybe (Left "saveDocumentForSignatory failed") Right mdoc
          return udoc

-- EMAILS

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
sendReminderEmail :: Kontrakcja m => Maybe String -> Context -> Actor -> Document -> SignatoryLink -> m SignatoryLink
sendReminderEmail custommessage ctx actor doc siglink = do
  sent <- sendNotifications siglink
    (do
      mail <- mailDocumentRemind custommessage ctx doc siglink False
      mailattachments <- makeMailAttachments doc
      scheduleEmailSendout (ctxmailsconfig ctx) $ mail {
                               to = [getMailAddress siglink]
                             , mailInfo = Invitation (documentid doc) (signatorylinkid siglink)
                             , attachments = if isJust $ maybesigninfo siglink
                                             then mailattachments
                                             else []
                             })
    (scheduleSMS =<< smsReminder doc siglink)
  when sent $ do
    when (isPending doc &&  not (hasSigned siglink)) $ do -- Why are these conditions not checked before?
      Log.debug $ "Reminder mail send for signatory that has not signed " ++ show (signatorylinkid siglink)
      dbUpdate $ PostReminderSend doc siglink custommessage actor
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
  let dropPDFSuffix name | ".pdf" `isSuffixOf` (map toLower name) = reverse . drop 4 $ reverse name
                         | otherwise = name
  --use the doc title rather than file name for the main file (see jira #1152)
  let filenames = map dropPDFSuffix $ documenttitle document : map filename ($(tail) allfiles)

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
   Send reminder to all parties in document. No custom text
 -}
sendAllReminderEmails :: Kontrakcja m => Context -> Actor -> User -> DocumentID -> m [SignatoryLink]
sendAllReminderEmails ctx actor user docid = do
    doc <- dbQuery $ GetDocumentByDocumentID docid
    case (documentstatus doc) of
          Pending -> do
            let isEligible = isEligibleForReminder user doc
                unsignedsiglinks = filter isEligible $ documentsignatorylinks doc
            sequence . map (sendReminderEmail Nothing ctx actor doc) $ unsignedsiglinks
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


-- | Time out documents once per day after midnight.  Do it in chunks
-- so that we don't choke the server in case there are many documents to time out
timeoutDocuments :: (MonadBaseControl IO m, MonadReader SchedulerData m, MonadIO m, MonadDB m) => m ()
timeoutDocuments = do
  now <- getMinutesTime
  docs <- dbQuery $ GetTimeoutedButPendingDocumentsChunk now 100
  forM_ docs $ \doc -> do
    gt <- getGlobalTemplates
    runReaderT (dbUpdate $ TimeoutDocument (documentid doc) (systemActor now)) gt
    triggerAPICallbackIfThereIsOne doc
    Log.debug $ "Document timedout " ++ (show $ documentid doc)
  when (not (null docs)) $ do
    kCommit
    timeoutDocuments
