module Doc.Action
    (
     postDocumentChangeAction,
     getCustomTextField,
     sendReminderEmail,
     sendInvitationEmail1
                
    )
    where

import Data.Char
import DB.Classes
import Doc.DocSeal
import Doc.Model
import Doc.DocStateData
import Doc.DocStorage
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import InputValidation
import File.Model
import Kontra
import KontraLink
import Mails.SendMail
import Misc
import Redirect
import User.Model
import Util.HasSomeUserInfo
import qualified Log
import Templates.Templates
import Templates.Trans
import Util.SignatoryLinkUtils
import Util.MonadUtils
import Stats.Control

import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length)
import ForkAction
import EvidenceLog.Model

{- |
   Perform the appropriate action when transitioning between documentstatuses.
   This function should always be called after changing the document.
 -}
postDocumentChangeAction :: Kontrakcja m => Document -> Document -> Maybe SignatoryLinkID -> m ()
postDocumentChangeAction document@Document  { documentstatus
                                            , documentid
                                            , documentcancelationreason
                                            , documenttitle
                                            }
                      olddocument@Document  { documentstatus = oldstatus }
                            msignalinkid
    | documentstatus == Pending &&
      (all (((not . isAuthor) &&^ isSignatory) =>>^ hasSigned) $ documentsignatorylinks document) &&
      (not $ hasSigned $ getAuthorSigLink document) &&
      (isSignatory $ getAuthorSigLink document) = do
          Log.docevent $ "All have signed but author; Pending -> AwaitingAuthor: " ++ show documentid
          ctx <- getContext
          d <- guardRightM $ runDBUpdate $ PendingToAwaitingAuthor documentid (SystemActor $ ctxtime ctx)
          postDocumentChangeAction d olddocument msignalinkid
    | (documentstatus == Pending ||
       documentstatus == AwaitingAuthor) &&
      (all (isSignatory =>>^ hasSigned) $ documentsignatorylinks document) = do
          Log.docevent $ "All have signed; " ++ show documentstatus ++ " -> Closed: " ++ show documentid
          ctx <- getContext
          d <- guardRightM $ runDBUpdate $ CloseDocument documentid (SystemActor (ctxtime ctx))
          postDocumentChangeAction d olddocument msignalinkid
    -- No status change ;
    | documentstatus == oldstatus = do
        -- if sign order has changed, we need to send another invitations
        when (documentcurrentsignorder document /= documentcurrentsignorder olddocument) $ do
            ctx <- getContext
            Log.server $ "Resending invitation emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
            _ <- sendInvitationEmails ctx document
            return ()
    -- Preparation -> Pending
    -- main action: sendInvitationEmails
    | oldstatus == Preparation && documentstatus == Pending = do
        Log.docevent $ "Preparation -> Pending; Sending invitation emails: " ++ show documentid
        ctx <- getContext
        msaveddoc <- saveDocumentForSignatories document
        document' <- case msaveddoc of
          (Left msg) -> do
            Log.error $ "Failed to save document #" ++ (show documentid) ++ " for signatories " ++ msg
            return document
          (Right saveddoc) -> return saveddoc
        -- we don't need to forkIO here since we only schedule emails here
        Log.server $ "Sending invitation emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
        edoc <- sendInvitationEmails ctx document'
        _ <- case edoc of
          Left _ -> do
            _ <- addDocumentSendStatEvents document'
            runDB $ forM (documentsignatorylinks document') $ \sl ->
              addSignStatInviteEvent document' sl (ctxtime ctx)
          Right doc2 -> do
            _ <- addDocumentSendStatEvents doc2
            runDB $ forM (documentsignatorylinks doc2) $ \sl ->
              addSignStatInviteEvent doc2 sl (ctxtime ctx)
        return ()
    -- Preparation -> Closed (only author signs)
    -- main action: sealDocument and sendClosedEmails
    | oldstatus == Preparation && documentstatus == Closed = do
        Log.docevent $ "Preparation -> Closed; Sealing document, sending emails: " ++ show documentid
        _ <- addDocumentCloseStatEvents document
        ctx@Context{ctxlocale, ctxglobaltemplates} <- getContext
        forkAction ("Sealing document #" ++ show documentid ++ ": " ++ BS.toString documenttitle) $ \env -> do
          let newctx = ctx {ctxdbenv = env}
          enewdoc <- ioRunDB env $ sealDocument newctx document
          case enewdoc of
             Right newdoc -> runTemplatesT (ctxlocale, ctxglobaltemplates) $ sendClosedEmails newctx newdoc
             Left errmsg -> Log.error $ "Sealing of document #" ++ show documentid ++ " failed, could not send document confirmations: " ++ errmsg
        return ()
    -- Pending -> AwaitingAuthor
    -- main action: sendAwaitingEmail
    | oldstatus == Pending && documentstatus == AwaitingAuthor = do
        Log.docevent $ "Pending -> AwaitingAuthor; Send email to author: " ++ show documentid
        ctx <- getContext
        -- we don't need to forkIO here since we only schedule emails here
        author <- getDocAuthor
        Log.server $ "Sending awaiting email for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
        sendAwaitingEmail ctx document author
        return ()
    -- Pending -> Closed OR AwaitingAuthor -> Closed
    -- main action: sendClosedEmails
    | (oldstatus == Pending || oldstatus == AwaitingAuthor) && documentstatus == Closed = do
        Log.docevent $ show oldstatus ++ " -> Closed; Sending emails: " ++ show documentid
        _ <- addDocumentCloseStatEvents document
        ctx@Context{ctxlocale, ctxglobaltemplates} <- getContext
        author <- getDocAuthor
        forkAction ("Sealing document #" ++ show documentid ++ ": " ++ BS.toString documenttitle) $ \env -> do
          let newctx = ctx {ctxdbenv = env}
          enewdoc <- ioRunDB env $ sealDocument newctx document
          case enewdoc of
            Right newdoc -> runTemplatesT (ctxlocale, ctxglobaltemplates) $ sendClosedEmails newctx newdoc
            Left errmsg -> do
              _ <- ioRunDB env $ dbUpdate $ ErrorDocument documentid errmsg
                   (SystemActor (ctxtime ctx))
              Log.server $ "Sending seal error emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
              runTemplatesT (ctxlocale, ctxglobaltemplates) $ sendDocumentErrorEmail newctx document author
              return ()
        return ()
    -- Pending -> Rejected
    -- main action: sendRejectAuthorEmail
    | oldstatus == Pending && documentstatus == Rejected = do
        Log.docevent $ "Pending -> Rejected; send reject emails: " ++ show documentid
        _ <- addDocumentRejectStatEvents document
        ctx <- getContext
        customMessage <- getCustomTextField "customtext"
        -- we don't need to forkIO here since we only schedule emails here
        Log.server $ "Sending rejection emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
        sendRejectEmails (fmap BS.toString customMessage) ctx document (fromJust msignalink)
        return ()
    -- Pending -> Canceled
    -- main action: if canceled because of ElegDataMismatch, send out emails
    | oldstatus == Pending && documentstatus == Canceled = do
        Log.docevent $ "Pending -> Canceled (ElegDataMismatch); Sending cancelation emails: " ++ show documentid
        _ <- addDocumentCancelStatEvents document

        if isJust documentcancelationreason && isELegDataMismatch (fromJust documentcancelationreason)
          then do
          ctx <- getContext
          author <- getDocAuthor
          Log.server $ "Sending cancelation emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
          sendElegDataMismatchEmails ctx document author
          else do
          -- should we send cancelation emails?
          return ()
        return ()
    --  -> DocumentError
    | DocumentError msg <- documentstatus = do
        Log.docevent $ "DocumentError; " ++ msg ++ " : " ++ show documentid
        ctx <- getContext
        author <- getDocAuthor
        Log.server $ "Sending error emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
        sendDocumentErrorEmail ctx document author
        return ()

    -- transition with no necessary action; do nothing
    -- FIXME: log status change
    | otherwise =
         return ()
    where msignalink = maybe Nothing (getSigLinkFor document) msignalinkid
          getDocAuthor :: (Kontrakcja n) => n User
          getDocAuthor = do
            authorid <- guardJust $ getAuthorSigLink document >>= maybesignatory
            (author :: User) <- guardJustM $ runDBQuery $ GetUserByID authorid
            return author


{- |
    Goes through each signatory, and if a user exists this saves it for that user
    by linking the signatory to the user's account.
-}
saveDocumentForSignatories :: Kontrakcja m => Document -> m (Either String Document)
saveDocumentForSignatories doc@Document{documentsignatorylinks} =
  foldM foldSaveForSig (Right doc) .filter (not . isAuthor) $ documentsignatorylinks
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
    saveDocumentForSignatory doc'@Document{documentid,documentservice}
                             SignatoryLink{signatorylinkid,signatorydetails} = do
      let sigemail = getValueOfType EmailFT signatorydetails
      muser <- runDBQuery $ GetUserByEmail documentservice (Email sigemail)
      case muser of
        Nothing -> return $ Right doc'
        Just user -> do
          Context{ctxtime, ctxipnumber} <- getContext
          let actor = SignatoryActor ctxtime ctxipnumber (Just $ userid user) (BS.toString sigemail) signatorylinkid
          udoc <- runDBUpdate $ SaveDocumentForUser documentid user signatorylinkid actor
          return udoc

-- EMAILS

sendElegDataMismatchEmails :: Kontrakcja m => Context -> Document -> User -> m ()
sendElegDataMismatchEmails ctx document author = do
    let signlinks = [sl | sl <- documentsignatorylinks document
                        , isActivatedSignatory (documentcurrentsignorder document) sl
                        , not $ isAuthor sl]
        Just (ELegDataMismatch msg badid _ _ _) = documentcancelationreason document
        badsig = fromJust $ find (\sl -> badid == signatorylinkid sl) (documentsignatorylinks document)
        badname  = BS.toString $ getFullName badsig
        bademail = BS.toString $ getEmail badsig
    forM_ signlinks $ sendDataMismatchEmailSignatory ctx document badid badname msg
    sendDataMismatchEmailAuthor ctx document author badname bademail

sendDataMismatchEmailSignatory :: Kontrakcja m => Context -> Document -> SignatoryLinkID -> String -> String -> SignatoryLink -> m ()
sendDataMismatchEmailSignatory ctx document badid badname msg signatorylink = do
    let SignatoryLink { signatorylinkid, signatorydetails = sigdets } = signatorylink
        isbad = badid == signatorylinkid
    case getAuthorSigLink document of
      Nothing -> error "No author in Document"
      Just authorsl -> do
        mail <- mailMismatchSignatory
                ctx
                document
                (BS.toString $ getEmail authorsl)
                (BS.toString $ getFullName authorsl)
                (ctxhostpart ctx ++ (show $ LinkSignDoc document signatorylink))
                (BS.toString $ getFullName signatorylink)
                badname
                msg
                isbad
        scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress sigdets]}

sendDataMismatchEmailAuthor :: Kontrakcja m => Context -> Document -> User -> String -> String -> m ()
sendDataMismatchEmailAuthor ctx document author badname bademail = do
    let authorname = getFullName $ fromJust $ getAuthorSigLink document
        authoremail = getEmail $ fromJust $ getAuthorSigLink document
    mail <- mailMismatchAuthor ctx document (BS.toString authorname) badname bademail (getLocale author)
    scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress {fullname = authorname, email = authoremail }]}

{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendDocumentErrorEmail :: TemplatesMonad m => Context -> Document -> User -> m ()
sendDocumentErrorEmail ctx document author = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (\sl -> if isAuthor sl
                            then sendDocumentErrorEmailToAuthor
                            else sendDocumentErrorEmailToSignatory sl)
  where
    sendDocumentErrorEmailToAuthor = do
      let authorlink = fromJust $ getAuthorSigLink document
      mail <- mailDocumentErrorForAuthor ctx document (getLocale author)
      ioRunDB (ctxdbenv ctx) $ scheduleEmailSendout' (ctxmailsconfig ctx) $ mail {
          to = [getMailAddress authorlink]
        , from = documentservice document
      }
    -- | Helper function to send emails to invited parties
    -- ??: Should this be in DocControl or in an email-specific file?
    sendDocumentErrorEmailToSignatory signatorylink = do
      let SignatoryLink { signatorylinkid
                        , signatorydetails } = signatorylink
          Document { documentid } = document
      mail <- mailDocumentErrorForSignatory ctx document
      ioRunDB (ctxdbenv ctx) $ scheduleEmailSendout' (ctxmailsconfig ctx) $ mail {
            to = [getMailAddress signatorydetails]
          , mailInfo = Invitation documentid  signatorylinkid
          , from = documentservice document
      }

{- |
   Send emails to all of the invited parties.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendInvitationEmails :: Kontrakcja m => Context -> Document -> m (Either String Document)
sendInvitationEmails ctx document = do
  let signlinks = [sl | sl <- documentsignatorylinks document
                      , isCurrentSignatory (documentcurrentsignorder document) sl
                      , not $ isAuthor sl]
  case signlinks of
    [] -> return $ Left "No signatories."
    _ -> do
      edocs <- forM signlinks (sendInvitationEmail1 ctx document)
      return $ msum edocs


{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendInvitationEmail1 :: Kontrakcja m => Context -> Document -> SignatoryLink -> m (Either String Document)
sendInvitationEmail1 ctx document signatorylink = do
  let SignatoryLink { signatorylinkid
                    , signatorydetails } = signatorylink
      Document { documentid } = document
  mail <- mailInvitation True ctx (Sign <| isSignatory signatorylink |> View) document (Just signatorylink)
  -- ?? Do we need to read in the contents? -EN
  -- _attachmentcontent <- liftIO $ getFileContents ctx $ head $ documentfiles document
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail {
        to = [getMailAddress signatorydetails]
      , mailInfo = Invitation documentid signatorylinkid
      , from = documentservice document
  }
  ioRunDB (ctxdbenv ctx) $ dbUpdate $ AddInvitationEvidence documentid signatorylinkid (SystemActor (ctxtime ctx))

{- |
    Send a reminder email (and update the modification time on the document)
-}
sendReminderEmail :: Kontrakcja m => Maybe BS.ByteString -> Context -> Document -> SignatoryLink -> m SignatoryLink
sendReminderEmail custommessage ctx doc siglink = do
  mail <- mailDocumentRemind custommessage ctx doc siglink
  mailattachments <- liftIO $ makeMailAttachments ctx doc
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail {
      to = [getMailAddress siglink]
    , mailInfo = Invitation (documentid doc) (signatorylinkid siglink)
    , attachments = if isJust $ maybesigninfo siglink
                      then mailattachments
                      else []
    , from = documentservice doc
    }
  --this is needed so the last event time in archive looks good
  _ <- runDBUpdate $ SetDocumentModificationData (documentid doc) (ctxtime ctx)
  return siglink

{- |
   Send emails to all parties when a document is closed.
 -}
sendClosedEmails :: TemplatesMonad m => Context -> Document -> m ()
sendClosedEmails ctx document = do
    let signatorylinks = documentsignatorylinks document
    mail <- mailDocumentClosed ctx document
    mailattachments <- liftIO $ makeMailAttachments ctx document
    ioRunDB (ctxdbenv ctx) $ scheduleEmailSendout' (ctxmailsconfig ctx) $
      mail { to = map getMailAddress signatorylinks
           , attachments = mailattachments
           , from = documentservice document
           }


{- |
   Send an email to the author when the document is awaiting approval
 -}
sendAwaitingEmail :: Kontrakcja m => Context -> Document -> User -> m ()
sendAwaitingEmail ctx document author = do
  let Just authorsiglink = getAuthorSigLink document
  mail <- mailDocumentAwaitingForAuthor ctx document (getLocale author)
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress authorsiglink]
                                                  , from = documentservice document  }

makeMailAttachments :: Context -> Document -> IO [(String, BS.ByteString)]
makeMailAttachments ctx document = do
  let mainfile = head $ case documentsealedfiles document of
        [] -> documentfiles document
        _ -> documentsealedfiles document
  let
      aattachments = map authorattachmentfile $ documentauthorattachments document
      sattachments = concatMap (maybeToList . signatoryattachmentfile) $ concatMap signatoryattachments $ documentsignatorylinks document
      allfiles' = [mainfile] ++ aattachments ++ sattachments
  allfiles <- liftM catMaybes $ mapM (ioRunDB (ctxdbenv ctx) . dbQuery . GetFileByFileID) allfiles'
  let dropPDFSuffix name | ".pdf" `isSuffixOf` (map toLower name) = reverse . drop 4 $ reverse name
                         | otherwise = name
  --use the doc title rather than file name for the main file (see jira #1152)
  let filenames = map (dropPDFSuffix . BS.toString) $ documenttitle document : map filename (tail allfiles)

  filecontents <- sequence $ map (getFileContents ctx) allfiles
  return $ zip filenames filecontents

{- |
   Send an email to the author and to all signatories who were sent an invitation  when the document is rejected
 -}
sendRejectEmails :: Kontrakcja m => Maybe String -> Context -> Document -> SignatoryLink -> m ()
sendRejectEmails customMessage ctx document signalink = do
  let activatedSignatories = [sl | sl <- documentsignatorylinks document
                                 , isActivatedSignatory (documentcurrentsignorder document) sl || isAuthor sl]
  forM_ activatedSignatories $ \sl -> do
    mail <- mailDocumentRejected customMessage ctx document signalink
    scheduleEmailSendout (ctxmailsconfig ctx) $ mail {   to = [getMailAddress sl]
                                                      , from = documentservice document }



{- |
    If the custom text field is empty then that's okay, but if it's invalid
    then we want to fail.
-}
getCustomTextField :: Kontrakcja m => String -> m (Maybe BS.ByteString)
getCustomTextField = getValidateAndHandle asValidInviteText customTextHandler
    where
    customTextHandler textresult =
        logIfBad textresult
            >>= flashValidationMessage
            >>= withFailureIfBad

