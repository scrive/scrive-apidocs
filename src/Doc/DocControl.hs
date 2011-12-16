{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{- |
   DocControl represents the controler (in MVC) of the document.
 -}
module Doc.DocControl where

import ActionSchedulerState
import AppView
import DB.Classes
import DB.Types
import DBError
import Doc.CSVUtils
import Doc.DocSeal
import Doc.Transitory
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocStateUpdate
import Doc.DocStorage
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import Doc.DocRegion
import InputValidation
import File.Model
import Kontra
import KontraLink
import ListUtil
import Mails.SendMail
import MinutesTime
import Misc
import Redirect
import User.Model
import User.UserControl
import Util.HasSomeUserInfo
import Util.StringUtil
import qualified AppLogger as Log
import Templates.Templates
import Templates.LocalTemplates
import Util.CSVUtil
import Util.FlashUtil
import Util.KontraLinkUtils
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Util.MonadUtils
import Doc.Invariants
import Stats.Control
import User.Utils
import API.Service.Model

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.Either
import Data.List
import Data.Maybe
import Data.Word
import Happstack.Server hiding (simpleHTTP)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import Text.JSON hiding (Result)
import ForkAction
import qualified ELegitimation.BankID as BankID
{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}


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
          d <- guardRightM $ doc_update $ PendingToAwaitingAuthor documentid (ctxtime ctx)
          postDocumentChangeAction d olddocument msignalinkid
    | (documentstatus == Pending ||
       documentstatus == AwaitingAuthor) &&
      (all (isSignatory =>>^ hasSigned) $ documentsignatorylinks document) = do
          Log.docevent $ "All have signed; " ++ show documentstatus ++ " -> Closed: " ++ show documentid
          ctx <- getContext
          d <- guardRightM $ doc_update $ CloseDocument documentid (ctxtime ctx) (ctxipnumber ctx)
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
          Left _ -> addDocumentSendStatEvents document'
          Right doc2 -> addDocumentSendStatEvents doc2
        return ()
    -- Preparation -> Closed (only author signs)
    -- main action: sealDocument and sendClosedEmails
    | oldstatus == Preparation && documentstatus == Closed = do
        Log.docevent $ "Preparation -> Closed; Sealing document, sending emails: " ++ show documentid
        _ <- addDocumentCloseStatEvents document
        ctx@Context{ctxlocale, ctxglobaltemplates} <- getContext
        forkAction ("Sealing document #" ++ show documentid ++ ": " ++ BS.toString documenttitle) $ \conn -> do
          let newctx = ctx {ctxdbconn = conn}
          enewdoc <- ioRunDB conn $ sealDocument newctx document
          case enewdoc of
             Right newdoc -> runWithTemplates ctxlocale ctxglobaltemplates $ sendClosedEmails newctx newdoc
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
        forkAction ("Sealing document #" ++ show documentid ++ ": " ++ BS.toString documenttitle) $ \conn -> do
          let newctx = ctx {ctxdbconn = conn}
          enewdoc <- ioRunDB conn $ sealDocument newctx document
          case enewdoc of
            Right newdoc -> runWithTemplates ctxlocale ctxglobaltemplates $ sendClosedEmails newctx newdoc
            Left errmsg -> do
              _ <- ioRunDB conn $ doc_update' $ ErrorDocument documentid errmsg
              Log.server $ "Sending seal error emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
              runWithTemplates ctxlocale ctxglobaltemplates $ sendDocumentErrorEmail newctx document author
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
          udoc <- doc_update $ SaveDocumentForUser documentid user signatorylinkid
          return udoc

-- EMAILS

sendElegDataMismatchEmails :: TemplatesMonad m => Context -> Document -> User -> m ()
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

sendDataMismatchEmailSignatory :: TemplatesMonad m => Context -> Document -> SignatoryLinkID -> String -> String -> SignatoryLink -> m ()
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
        scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress sigdets]}

sendDataMismatchEmailAuthor :: TemplatesMonad m => Context -> Document -> User -> String -> String -> m ()
sendDataMismatchEmailAuthor ctx document author badname bademail = do
    let authorname = getFullName $ fromJust $ getAuthorSigLink document
        authoremail = getEmail $ fromJust $ getAuthorSigLink document
    mail <- mailMismatchAuthor ctx document (BS.toString authorname) badname bademail (getLocale author)
    scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress {fullname = authorname, email = authoremail }]}

{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendDocumentErrorEmail :: TemplatesMonad m => Context -> Document -> User -> m ()
sendDocumentErrorEmail ctx document author = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (\sl -> if isAuthor sl
                            then sendDocumentErrorEmailToAuthor ctx document author
                            else sendDocumentErrorEmailToSignatory ctx document sl)

sendDocumentErrorEmailToAuthor :: TemplatesMonad m => Context -> Document -> User -> m ()
sendDocumentErrorEmailToAuthor ctx document author = do
  let authorlink = fromJust $ getAuthorSigLink document
  mail <- mailDocumentErrorForAuthor ctx document (getLocale author)
  scheduleEmailSendout (ctxesenforcer ctx) $ mail
    { to = [getMailAddress authorlink]
    , from = documentservice document
    }

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendDocumentErrorEmailToSignatory :: TemplatesMonad m => Context -> Document -> SignatoryLink -> m ()
sendDocumentErrorEmailToSignatory ctx document signatorylink = do
  let SignatoryLink { signatorylinkid
                    , signatorydetails } = signatorylink
      Document { documentid } = document
  mail <- mailDocumentErrorForSignatory ctx document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        to = [getMailAddress signatorydetails]
      , mailInfo = Invitation documentid  signatorylinkid
      , from = documentservice document
  }

{- |
   Send emails to all of the invited parties.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendInvitationEmails :: TemplatesMonad m => Context -> Document -> m (Either String Document)
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
sendInvitationEmail1 :: TemplatesMonad m => Context -> Document -> SignatoryLink -> m (Either String Document)
sendInvitationEmail1 ctx document signatorylink = do
  let SignatoryLink { signatorylinkid
                    , signatorydetails } = signatorylink
      Document { documentid } = document
  mail <- mailInvitation True ctx (Sign <| isSignatory signatorylink |> View) document (Just signatorylink)
  -- ?? Do we need to read in the contents? -EN
  -- _attachmentcontent <- liftIO $ getFileContents ctx $ head $ documentfiles document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        to = [getMailAddress signatorydetails]
      , mailInfo = Invitation documentid signatorylinkid
      , from = documentservice document
  }
  ioRunDB (ctxdbconn ctx) $ doc_update' $ AddInvitationEvidence documentid signatorylinkid (ctxtime ctx) (ctxipnumber ctx) 

{- |
    Send a reminder email
-}
sendReminderEmail :: Kontrakcja m => Maybe BS.ByteString -> Context -> Document -> SignatoryLink -> m SignatoryLink
sendReminderEmail custommessage ctx doc siglink = do
  mail <- mailDocumentRemind custommessage ctx doc siglink
  mailattachments <- liftIO $ makeMailAttachments ctx doc
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
      to = [getMailAddress siglink]
    , mailInfo = Invitation (documentid doc) (signatorylinkid siglink)
    , attachments = if isJust $ maybesigninfo siglink
                      then mailattachments
                      else []
    , from = documentservice doc
    }
  return siglink

{- |
   Send emails to all parties when a document is closed.
 -}
sendClosedEmails :: TemplatesMonad m => Context -> Document -> m ()
sendClosedEmails ctx document = do
    let signatorylinks = documentsignatorylinks document
    mail <- mailDocumentClosed ctx document
    mailattachments <- liftIO $ makeMailAttachments ctx document
    scheduleEmailSendout (ctxesenforcer ctx) $
      mail { to = map getMailAddress signatorylinks
           , attachments = mailattachments
           , from = documentservice document
           }


{- |
   Send an email to the author when the document is awaiting approval
 -}
sendAwaitingEmail :: TemplatesMonad m => Context -> Document -> User -> m ()
sendAwaitingEmail ctx document author = do
  let Just authorsiglink = getAuthorSigLink document
  mail <- mailDocumentAwaitingForAuthor ctx document (getLocale author)
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress authorsiglink]
                                                  , from = documentservice document  }

makeMailAttachments :: Context -> Document -> IO [(BS.ByteString,BS.ByteString)]
makeMailAttachments ctx document = do
  let mainfile = head $ case documentsealedfiles document of
        [] -> documentfiles document
        _ -> documentsealedfiles document
  let
      aattachments = map authorattachmentfile $ documentauthorattachments document
      sattachments = concatMap (maybeToList . signatoryattachmentfile) $ documentsignatoryattachments document
      allfiles' = [mainfile] ++ aattachments ++ sattachments
  allfiles <- liftM catMaybes $ mapM (ioRunDB (ctxdbconn ctx) . dbQuery . GetFileByFileID) allfiles'
  let
      filenames = map filename allfiles
  filecontents <- sequence $ map (getFileContents ctx) allfiles
  return $ zip filenames filecontents

{- |
   Send an email to the author and to all signatories who were sent an invitation  when the document is rejected
 -}
sendRejectEmails :: TemplatesMonad m => Maybe String -> Context -> Document -> SignatoryLink -> m ()
sendRejectEmails customMessage ctx document signalink = do
  let activatedSignatories = [sl | sl <- documentsignatorylinks document
                                 , isActivatedSignatory (documentcurrentsignorder document) sl || isAuthor sl]
  forM_ activatedSignatories $ \sl -> do
    mail <- mailDocumentRejected customMessage ctx document signalink
    scheduleEmailSendout (ctxesenforcer ctx) $ mail {   to = [getMailAddress sl]
                                                      , from = documentservice document }








-- END EMAILS

{- |
    Handles an account setup from within the sign view.
-}
handleAcceptAccountFromSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> ActionID -> MagicHash -> m KontraLink
handleAcceptAccountFromSign documentid
                            signatorylinkid
                            signmagichash
                            actionid
                            magichash = do
  muser <- handleAccountSetupFromSign actionid magichash
  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid signmagichash
  signatorylink <- guardJust $ getSigLinkFor document signatorylinkid
  case muser of
    Nothing | isClosed document -> addFlashM $ modalSignedClosedNoAccount document signatorylink actionid magichash
    Nothing -> addFlashM $ modalSignedNotClosedNoAccount document signatorylink actionid magichash
    Just _ -> addFlashM $ flashMessageAccountActivatedFromSign
  return $ LinkSignDoc document signatorylink

{- |
    Handles an account removal from within the sign view.
-}
handleDeclineAccountFromSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> ActionID -> MagicHash -> m KontraLink
handleDeclineAccountFromSign documentid
                             signatorylinkid
                             signmagichash
                             actionid
                             magichash = do
  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid signmagichash
  signatorylink <- guardJust $ getSigLinkFor document signatorylinkid
  userid <- guardJust $ maybesignatory signatorylink
  user <- guardJustM $ runDBQuery $ GetUserByID userid
  handleAccountRemovalFromSign user signatorylink actionid magichash
  addFlashM flashMessageAccountRemovedFromSign
  return $ LinkSignDoc document signatorylink

{- |
   Control the signing of a document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
   Method: POST
 -}
signDocument :: Kontrakcja m
             => DocumentID      -- ^ The DocumentID of the document to sign
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL
             -> m KontraLink
signDocument documentid
             signatorylinkid = do
  magichash <- guardJustM $ readField "magichash"
  fieldnames <- getAndConcat "fieldname"
  fieldvalues <- getAndConcat "fieldvalue"
  let fields = zip fieldnames fieldvalues
  mprovider <- readField "eleg"
  edoc <- case mprovider of
           Nothing -> Right <$> signDocumentWithEmail documentid signatorylinkid magichash fields
           Just provider -> do
               signature     <- getDataFnM $ look "signature"
               transactionid <- getDataFnM $ look "transactionid"
               esinfo <- BankID.verifySignatureAndGetSignInfo documentid signatorylinkid magichash provider signature transactionid
               case esinfo of
                    BankID.Problem msg -> return $ Left msg
                    BankID.Mismatch msg sfn sln spn -> do
                        document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid magichash
                        handleMismatch document signatorylinkid msg sfn sln spn
                        return $ Left msg
                    BankID.Sign sinfo ->  Right <$>  signDocumentWithEleg documentid signatorylinkid magichash fields sinfo
  case edoc of
    Right (Right (doc, olddoc)) -> do
      postDocumentChangeAction doc olddoc (Just signatorylinkid)
      udoc <- guardJustM $ doc_query $ GetDocumentByDocumentID documentid
      handleAfterSigning udoc signatorylinkid
    Right (Left (DBActionNotAvailable message)) -> do
      addFlash (OperationFailed, message)
      return LoopBack
    Right (Left (DBDatabaseNotAvailable message)) -> do
      addFlash (OperationFailed, message)
      return LoopBack
    Left msg -> do
      addFlash  (OperationFailed, msg)
      return LoopBack
    _ -> mzero

handleMismatch :: Kontrakcja m =>  Document -> SignatoryLinkID -> String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> m ()
handleMismatch doc sid msg sfn sln spn = do
        ctx <- getContext
        Log.eleg $ "Information from eleg did not match information stored for signatory in document." ++ show msg
        Right newdoc <- doc_update $ CancelDocument (documentid doc) (ELegDataMismatch msg sid sfn sln spn) (ctxtime ctx) (ctxipnumber ctx)
        postDocumentChangeAction newdoc doc (Just sid)

{- |
    Call after signing in order to save the document for any new user,
    put up the appropriate modal, and register the necessary nagging email actions.
    This is factored into it's own function because that way it can be used by eleg too.
-}
handleAfterSigning :: Kontrakcja m => Document -> SignatoryLinkID -> m KontraLink
handleAfterSigning document@Document{documentid} signatorylinkid = do
  ctx <- getContext
  signatorylink <- guardJust $ getSigLinkFor document signatorylinkid
  maybeuser <- runDBQuery $ GetUserByEmail (currentServiceID ctx) (Email $ getEmail signatorylink)
  case maybeuser of
    Nothing -> do
      let sfield t = getValueOfType t $ signatorydetails signatorylink
          fullname = (sfield FirstNameFT, sfield LastNameFT)
          email = sfield EmailFT
      muser <- createUserBySigning fullname email (documentid, signatorylinkid)
      case muser of
        Just (user, actionid, magichash) -> do
          _ <- doc_update $ SaveDocumentForUser documentid user signatorylinkid
          Log.debug $ "the doc " ++ (show $ documentid) ++ ".isClosed = " ++ (show $ isClosed document)
          Log.debug $ "doc " ++ (show document)
          if isClosed document
            then addFlashM $ modalSignedClosedNoAccount document signatorylink actionid magichash
            else addFlashM $ modalSignedNotClosedNoAccount document signatorylink actionid magichash
          return ()
        _ -> return ()
    Just user -> do
     _ <- doc_update $ SaveDocumentForUser documentid user signatorylinkid
     let userlocale = locale $ usersettings user
     if isClosed document
       then addFlashM $ modalSignedClosedHasAccount userlocale document signatorylink (isJust $ ctxmaybeuser ctx)
       else addFlashM $ modalSignedNotClosedHasAccount userlocale document signatorylink (isJust $ ctxmaybeuser ctx)
  return $ LinkSignDoc document signatorylink

{- |
   Control rejecting the document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
 -}
rejectDocument :: Kontrakcja m
               => DocumentID
               -> SignatoryLinkID
               -> m KontraLink
rejectDocument documentid
               signatorylinkid1 = do
  magichash <- guardJustM $ readField "magichash"
  customtext <- getCustomTextField "customtext"

  edocs <- rejectDocumentWithChecks documentid signatorylinkid1 magichash customtext

  case edocs of
    Left (DBActionNotAvailable message) -> do
      addFlash (OperationFailed, message)
      getHomeOrUploadLink
    Left (DBDatabaseNotAvailable message) -> do
      addFlash (OperationFailed, message)
      getHomeOrUploadLink
    Left _ -> mzero
    Right (document, olddocument) -> do
      postDocumentChangeAction document olddocument (Just signatorylinkid1)
      addFlashM $ modalRejectedView document
      return $ LoopBack

getDocumentLocale :: DocumentID -> DB (Maybe Locale)
getDocumentLocale documentid = do
  mdoc <- doc_query' $ GetDocumentByDocumentID documentid
  return $ fmap getLocale mdoc --TODO: store lang on doc

{- |
   Show the document to be signed
 -}
handleSignShowOldRedirectToNew :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
handleSignShowOldRedirectToNew did sid mh = do
  doc<- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash did sid mh
  invitedlink <- guardJust $ getSigLinkFor doc sid
  return $ LinkSignDoc doc invitedlink

handleSignShow :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m String
handleSignShow documentid
               signatorylinkid = do
  magichash <- guardJustM $ readField "magichash"
  Context { ctxtime
          , ctxipnumber
          , ctxflashmessages } <- getContext
  _ <- markDocumentSeen documentid signatorylinkid magichash ctxtime ctxipnumber
  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid magichash
  invitedlink <- guardJust $ getSigLinkFor document signatorylinkid
  let isFlashNeeded = Data.List.null ctxflashmessages
                        && not (hasSigned invitedlink)
  -- add a flash if needed
  case document of
    _ | not isFlashNeeded -> return ()
    _ | not (isSignatory invitedlink) ->
      addFlashM flashMessageOnlyHaveRightsToViewDoc
    _ | document `allowsIdentification` ELegitimationIdentification ->
      addFlashM flashMessagePleaseSignWithEleg
    _ -> addFlashM $ flashMessagePleaseSign document

  ctx <- getContext
  case document of
    _ | isAttachment document ->
      pageAttachmentForSignatory document invitedlink
    _ | isSignatory invitedlink ->
      pageDocumentForSignatory (LinkSignDoc document invitedlink) document ctx invitedlink
    _ -> pageDocumentForViewer ctx document (Just invitedlink)


maybeAddDocumentCancelationMessage :: Kontrakcja m => Document -> m ()
maybeAddDocumentCancelationMessage document = do
  let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
  when (isCanceled document && isJust mMismatchMessage) $
    addFlash (OperationFailed, fromJust mMismatchMessage)
  return ()

{- |
   Handles the request to show a document to a user.
   There are two cases:
    1. author in which case they get pageDocumentForAuthor
    2. Within company of author in which case they get pageDocumentForViewer
   URL: /d/{documentid}
   Method: GET
 -}
handleIssueShowGet :: Kontrakcja m => DocumentID -> m (Either KontraLink (Either KontraLink String))
handleIssueShowGet docid =
  checkUserTOSGet $ do
    document <- guardRightM $ getDocByDocID docid
    ctx@Context { ctxmaybeuser } <- getContext
    mdstep <- getDesignStep docid
    case (mdstep, documentfunctionality document) of
      (Just (DesignStep3 _ _), BasicFunctionality) -> return $ Left $ LinkIssueDoc docid
      _ -> do
        -- authors & signatories get a view with buttons
        case (isAuthor (document, ctxmaybeuser),
              ctxmaybeuser >>= maybeInvitedLink document,
              isAttachment document,
              documentstatus document) of
          (True, _, True, Preparation) -> Right <$> pageAttachmentDesign document
          (_, _, True, _) -> Right <$> pageAttachmentView document
          (True, _, _, _) -> do
            let Just author = ctxmaybeuser
                showadvancedoption = Just AdvancedMode /= preferreddesignmode (usersettings author)
            let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
            when (isCanceled document && isJust mMismatchMessage) $
              addFlash (OperationFailed, fromJust mMismatchMessage)
            ctx2 <- getContext -- need to get new context because we may have added flash msg
            step <- getDesignStep (documentid document)
            filesforattachments' <- mapassocM (runDBQuery . GetFileByFileID) $ map authorattachmentfile $ documentauthorattachments document
            let filesforattachments = [(fid, f) | (fid, Just f) <- filesforattachments']
            case (documentstatus document) of
              Preparation -> do
                mattachments <- getDocsByLoggedInUser
                case mattachments of
                  Left _ -> Right <$> pageDocumentDesign ctx2 document step showadvancedoption [] filesforattachments
                  Right attachments -> Right <$> pageDocumentDesign ctx2 document step showadvancedoption (filter isAttachment attachments) filesforattachments
              _ ->  Right <$> pageDocumentForAuthor ctx2 document
          (_, Just invitedlink, _, _) -> Right <$> pageDocumentForSignatory (LinkSignDoc document invitedlink) document ctx invitedlink
          -- others in company can just look (but not touch)
          (False, _, _, _) -> Right <$> pageDocumentForViewer ctx document Nothing
     where
       maybeInvitedLink :: Document -> User -> Maybe SignatoryLink
       maybeInvitedLink doc user =
         (find (isSigLinkFor $ userid user) $ documentsignatorylinks doc) >>=
           (\sl -> if SignatoryPartner `elem` signatoryroles sl && isNothing (maybesigninfo sl)
                     then Just sl
                     else Nothing)

getDesignStep :: Kontrakcja m => DocumentID -> m (Maybe DesignStep)
getDesignStep docid = do
    step3 <- isFieldSet "step3"
    step2 <- isFieldSet "step2"
    signlast <- isFieldSet "signlast"

    mperson <- getOptionalField asValidNumber "person"
    aftercsvupload <- isFieldSet "aftercsvupload"
    case docid of
      _ | step2 -> return $ Just $ DesignStep2 docid mperson (if aftercsvupload
                                                                then (Just AfterCSVUpload)
                                                                else Nothing) signlast
      _ | step3 -> return $ Just $ DesignStep3 docid signlast
      _ -> return Nothing

{- |
   Modify a document. Typically called with the "Underteckna" or "Save" button
   If document is in preparation, we move it to pending
   If document is in AwaitingAuthor, we move it to Closed
   Otherwise, we do mzero (NOTE: this is not the correct action to take)
   User must be logged in.
   Document must exist
   User must be author
   URL: /d/{documentid}
   Method: POST
 -}
handleIssueShowPost :: Kontrakcja m => DocumentID -> m KontraLink
handleIssueShowPost docid = withUserPost $ do
  document <- guardRightM $ getDocByDocID docid
  Context { ctxmaybeuser = muser } <- getContext
  guard (isAuthor (document, muser)) -- still need this because others can read document
  sign              <- isFieldSet "sign"
  send              <- isFieldSet "final"
  template          <- isFieldSet "template"
  contract          <- isFieldSet "contract"
  csvupload         <- isFieldSet "csvupload"
  updateattachments <- isFieldSet "updateattachments"
  switchtoadvanced  <- isFieldSet "changefunctionality"
  sigattachments    <- isFieldSet "sigattachments"
  changedoclocale   <- isFieldSet "changedoclocale"
  -- Behold!
  case documentstatus document of
    Preparation | sign              -> handleIssueSign                 document
    Preparation | send              -> handleIssueSend                 document
    Preparation | template          -> handleIssueSaveAsTemplate       document
    Preparation | contract          -> handleIssueChangeToContract     document
    Preparation | csvupload         -> handleIssueCSVUpload            document
    Preparation | updateattachments -> handleIssueUpdateAttachments    document
    Preparation | switchtoadvanced  -> handleIssueChangeFunctionality  document
    Preparation | sigattachments    -> handleIssueUpdateSigAttachments document
    Preparation | changedoclocale   -> handleIssueLocaleChange         document
    Preparation                     -> handleIssueSave                 document
    AwaitingAuthor                  -> handleIssueSignByAuthor         document
    _ -> return $ LinkContracts

handleIssueSign :: Kontrakcja m => Document -> m KontraLink
handleIssueSign document = do
    Log.debug "handleIssueSign"
    ctx <- getContext
    -- unless (document `allowsIdentification` EmailIdentification) mzero | This need to be refactored | Breaks templates
    udoc <- guardRightM $ updateDocument ctx document
    mdocs <- splitUpDocument udoc
    case mdocs of
      Right docs -> do
        mndocs <- mapM (forIndividual udoc) docs
        case (lefts mndocs, rights mndocs) of
          ([], [d]) -> do
            addFlashM $ modalSendConfirmationView d
            return $ LinkIssueDoc (documentid d)
          ([], ds) -> do
            if isJust $ ctxservice ctx
              then do
              --sessionid <- readCookieValue "sessionId"
              --return $ LinkConnectUserToSession (ctxservice ctx) (fromJust $ ctxmaybeuser ctx) sessionid LinkCSVLandPage
              return $ LinkCSVLandPage (length ds)
              else do
              addFlashM $ flashMessageCSVSent $ length ds
              Log.debug (show $ map documenttype ds)
              case documenttype (head ds) of
                Signable Contract -> return $ LinkContracts
                Signable Offer    -> return $ LinkOffers
                Signable Order    -> return $ LinkOrders
                _                 -> return $ LinkUpload
          (ls, _) -> do
            Log.debug $ "handleIssueSign had lefts: " ++ intercalate ";" (map show ls)
            return LoopBack
      Left link -> return link
    where
      forIndividual :: Kontrakcja m => Document -> Document -> m (Either KontraLink Document)
      forIndividual udoc doc = do
        mprovider <- readField "eleg"
        mndoc <- case mprovider of
                   Nothing ->  Right <$> authorSignDocument (documentid doc) Nothing
                   Just provider -> do
                      signature     <- getDataFnM $ look "signature"
                      transactionid <- getDataFnM $ look "transactionid"
                      esinfo <- BankID.verifySignatureAndGetSignInfoForAuthor (documentid doc) provider signature transactionid
                      case esinfo of
                        BankID.Problem msg -> return $ Left msg
                        BankID.Mismatch msg _ _ _ -> return $ Left msg
                        BankID.Sign sinfo -> Right <$>  authorSignDocument (documentid doc) (Just sinfo)
        case mndoc of
          Right (Right newdocument) -> do
            postDocumentChangeAction newdocument udoc Nothing
            return $ Right newdocument
          _ -> return $ Left LoopBack


handleIssueSend :: Kontrakcja m => Document -> m KontraLink
handleIssueSend document = do
    Log.debug "handleIssueSend"
    ctx <- getContext
    udoc <- guardRightM $ updateDocument ctx document
    mdocs <- splitUpDocument udoc
    case mdocs of
      Right docs -> do
        mndocs <- mapM (forIndividual udoc) docs
        case (lefts mndocs, rights mndocs) of
          ([], [d]) -> do
            addFlashM $ modalSendConfirmationView d
            return $ LinkIssueDoc (documentid d)
          ([], ds) -> do
            if isJust $ ctxservice ctx
              then do
              --sessionid <- readCookieValue "sessionId"
              --return $ LinkConnectUserToSession (ctxservice ctx) (fromJust $ ctxmaybeuser ctx) sessionid LinkCSVLandPage
              return $ LinkCSVLandPage (length ds)
              else do
              addFlashM $ flashMessageCSVSent $ length ds
              Log.debug (show $ map documenttype ds)
              case documenttype (head ds) of
                Signable Contract -> return $ LinkContracts
                Signable Offer    -> return $ LinkOffers
                Signable Order    -> return $ LinkOrders
                _ -> return $ LinkUpload
          (ls, _) -> do
            Log.debug $ "handleIssueSend had lefts: " ++ intercalate ";" (map show ls)
            mzero
      Left link -> return link
    where
      forIndividual udoc doc = do
        mndoc <- authorSendDocument (documentid doc)
        case mndoc of
          Right newdocument -> do
            postDocumentChangeAction newdocument udoc Nothing
            return ()
          Left _ -> return ()
        return mndoc

handleIssueSaveAsTemplate :: Kontrakcja m => Document -> m KontraLink
handleIssueSaveAsTemplate document = do
  ctx <- getContext
  udoc <- guardRightM $ updateDocument ctx document
  _ndoc <- guardRightM $ doc_update $ TemplateFromDocument $ documentid udoc
  addFlashM flashDocumentTemplateSaved
  return $ LinkTemplates

markDocumentAuthorReadAndSeen :: Kontrakcja m => Document -> MinutesTime -> Word32 -> m ()
markDocumentAuthorReadAndSeen Document{documentid, documentsignatorylinks} time ipnumber =
  mapM_ mark $ filter isAuthor documentsignatorylinks
  where
    mark SignatoryLink{signatorylinkid, signatorymagichash} = do
      _ <- doc_update $ MarkInvitationRead documentid signatorylinkid time
      _ <- doc_update $ MarkDocumentSeen documentid signatorylinkid signatorymagichash time ipnumber
      return ()

-- TODO | I belive this is dead. Some time ago if you were editing template you could create contract from it.
--        But this probably is gone now.
-- This is not dead! Look on or around line 631. -- Eric
handleIssueChangeToContract :: Kontrakcja m => Document -> m KontraLink
handleIssueChangeToContract document = do
  ctx <- getContext
  signlast <- isFieldSet "signlast"
  contract <- guardRightM $ signableFromTemplateWithUpdatedAuthor (documentid document)
  ncontract <- guardRightM $ updateDocument ctx contract
  return $ LinkDesignDoc $ DesignStep3 (documentid ncontract) signlast

{- |
    If the document has a multiple part this will pump csv values through it to create multiple docs, and then
    save the original as a template if it isn't already.  This will make sure to clean the csv data.  It just returns
    a list containing the original doc on it's own, if the doc hasn't got a multiple part.

    I feel like this is quite dangerous to do all at once, maybe need a transaction?!
-}
splitUpDocument :: Kontrakcja m => Document -> m (Either KontraLink [Document])
splitUpDocument doc =
  case (documentcsvupload doc, getCSVCustomFields doc) of
    (Just _, Left _) -> mzero
    (Nothing, _) -> return $ Right [doc]
    (Just csvupload, Right csvcustomfields) ->
      case (cleanCSVContents (documentallowedidtypes doc) (length csvcustomfields) $ csvcontents csvupload) of
        (_prob:_, _) -> do
          signlast <- isFieldSet "signlast"
          addFlashM flashMessageInvalidCSV
          return $ Left $ LinkDesignDoc $ DesignStep2 (documentid doc) (Just (1 + csvsignatoryindex csvupload)) (Just AfterCSVUpload) signlast
        ([], CleanCSVData{csvbody}) -> do
          mdocs <- mapM (createDocFromRow doc (csvsignatoryindex csvupload)) csvbody
          if Data.List.null (lefts mdocs)
            then return $ Right (rights mdocs)
            else mzero
  where createDocFromRow :: Kontrakcja m => Document -> Int -> [BS.ByteString] -> m (Either String Document)
        createDocFromRow udoc sigindex xs =
          doc_update $ DocumentFromSignatoryData (documentid udoc) sigindex (item 0) (item 1) (item 2) (item 3) (item 4) (item 5) (drop 6 xs)
          where item n | n<(length xs) = xs !! n
                       | otherwise = BS.empty
{- |
   Handles a csv file upload.  This'll parse the file, and save the info
   on the document and relevant signatory.
-}
handleIssueCSVUpload :: Kontrakcja m => Document -> m KontraLink
handleIssueCSVUpload document = do
  ctx <- getContext
  udoc <- guardRightM $ updateDocument ctx document
  signlast <- isFieldSet "signlast"

  mcsvsigindex <- getOptionalField asValidNumber "csvsigindex"
  mcsvfile <- getCSVFile "csv"
  case (mcsvsigindex, mcsvfile) of
    (Nothing, Nothing) -> return $ LinkDesignDoc $ DesignStep2 (documentid udoc) Nothing Nothing signlast
    (Nothing, Just _) ->  do
      Log.error "something weird happened, got csv file but there's no relevant person index"
      mzero
    (Just csvsigindex, Nothing) -> return $ LinkDesignDoc $ DesignStep2 (documentid udoc) (Just $ csvsigindex + 1) Nothing signlast
    (Just csvsigindex, Just (title, contents)) ->  do
      let csvupload = CSVUpload { csvtitle = title
                                , csvcontents = contents
                                , csvsignatoryindex = csvsigindex
                                }
      ndoc <- guardRightM $ doc_update $ AttachCSVUpload (documentid udoc) csvupload
      return $ LinkDesignDoc $ DesignStep2 (documentid ndoc) (Just $ csvsigindex + 1) (Just AfterCSVUpload) signlast

makeSigAttachment :: BS.ByteString -> BS.ByteString -> BS.ByteString -> SignatoryAttachment
makeSigAttachment name desc email =
  SignatoryAttachment { signatoryattachmentfile = Nothing
                      , signatoryattachmentemail = email
                      , signatoryattachmentname = name
                      , signatoryattachmentdescription = desc
                      }


zipSigAttachments :: BS.ByteString -> BS.ByteString -> BS.ByteString -> [SignatoryAttachment]
zipSigAttachments name desc emailsstring =
  let emails = [trim e | e <- splitOn ',' $ BS.toString emailsstring
                       , not $ Data.List.null $ trim e]
  in map (makeSigAttachment name desc . BS.fromString) emails

handleIssueLocaleChange :: Kontrakcja m => Document -> m KontraLink
handleIssueLocaleChange doc = do
  ctx <- getContext
  udoc <- guardRightM $ updateDocument ctx doc
  handleIssueLocaleChangeAfterUpdate udoc

{- |
    Handles the locale change after the document has been updated
    to capture the latest changes.  Separated this out to make it easier
    to unit test, because mocking up the post variables required for
    the updateDocument function would just be too much!
-}
handleIssueLocaleChangeAfterUpdate  :: Kontrakcja m => Document -> m KontraLink
handleIssueLocaleChangeAfterUpdate doc@Document{documentid} = do
  Context{ctxtime} <- getContext

  docregion <- getCriticalField (return . maybe (getRegion doc) fst . listToMaybe . reads) "docregion"

  udoc <- guardRightM . doc_update $ SetDocumentLocale documentid (mkLocaleFromRegion docregion) ctxtime

  when (not . regionelegavailable $ getRegionInfo udoc) $ do
    _ <- guardRightM . doc_update $ SetEmailIdentification documentid ctxtime
    return ()

  signlast <- isFieldSet "signlast"

  return (LinkDesignDoc (DesignStep3 documentid signlast))


handleIssueUpdateSigAttachments :: Kontrakcja m => Document -> m KontraLink
handleIssueUpdateSigAttachments doc = do
  ctx <- getContext
  mudoc <- updateDocument ctx doc
  udoc <- guardRight mudoc

  sigattachmentnames  <- getAndConcat "sigattachname"
  sigattachmentdescs  <- getAndConcat "sigattachdesc"
  sigattachmentemails <- getAndConcat "sigattachemails"

  signlast <- isFieldSet "signlast"

  let sigattachments = concat $ zipWith3 zipSigAttachments sigattachmentnames sigattachmentdescs sigattachmentemails
  ndoc <- guardRightM $ updateSigAttachments (documentid udoc) sigattachments
  return (LinkDesignDoc (DesignStep3 (documentid ndoc) signlast))

handleIssueUpdateAttachments :: Kontrakcja m => Document -> m KontraLink
handleIssueUpdateAttachments doc = withUserPost $ do
    ctx <- getContext
    udoc <- guardRightM $ updateDocument ctx doc

    attidsnums <- getCriticalFieldList asValidID "attachmentid"
    removeatt <- getCriticalFieldList asValidBool "removeattachment"
    signlast <- isFieldSet "signlast"

    let existingattachments = map (authorattachmentfile) (documentauthorattachments udoc)
        idsforremoval = [read $ BS.toString f | (f, r) <- zip attidsnums removeatt
                                              , r] :: [FileID]
    fileinputs <- getDataFnM $ lookInputs "attachment"
    mattachments <- sequence $ map (makeDocumentFromFile Attachment) fileinputs
    -- read in the ids as both FileID and DocumentID
    -- if the FileID exists in the existing author attachments
    -- it's not a DocumentID
    -- otherwise, consider it a DocumentID to add
    Log.debug $ show mattachments
    let didsforadd = [ did | (did :: DocumentID, fid) <- [ ( read $ BS.toString sid, read $ BS.toString sid)
                                                               | (sid, r) <- zip attidsnums removeatt
                                                         , not r]
                     , not $ fid `elem` existingattachments]
                     ++ (map documentid $ catMaybes mattachments) :: [DocumentID]
    docsforadd <- liftM catMaybes $ mapM (doc_query . GetDocumentByDocumentID) didsforadd
    let idsforadd = concat $ map documentfiles docsforadd

    ndoc <- guardRightM $ updateDocAuthorAttachments (documentid udoc) idsforadd idsforremoval
    return $ LinkDesignDoc $ DesignStep3 (documentid ndoc) signlast

{- |
    Deals with a switch to the document's functionality.
    This'll also doc_update the user preferences that they would like
    to continue with this functionality by default in the future.
-}
handleIssueChangeFunctionality :: Kontrakcja m => Document -> m KontraLink
handleIssueChangeFunctionality document = do
  guardLoggedIn
  ctx <- getContext
  udoc <- guardRightM $ updateDocument ctx document
  signlast <- isFieldSet "signlast"
  defaultadvanced <- isFieldSet "defaultadvanced"
  when defaultadvanced $ do
    SignatoryLink { maybesignatory = Just authorid } <- guardJust $ getAuthorSigLink udoc
    _ <- runDBUpdate $ SetPreferredDesignMode authorid (Just AdvancedMode)
    return ()
  return $ LinkDesignDoc $ DesignStep2 (documentid udoc) Nothing Nothing signlast

{- |
    This will get and parse a csv file.  It
    also deals with any flash messages.  It returns a pair
    of (title, contents).
-}
getCSVFile :: Kontrakcja m => String -> m (Maybe (BS.ByteString, [[BS.ByteString]]))
getCSVFile fieldname = do
  input <- getDataFn' (lookInput fieldname)
  csvresult <- liftIO $ asCSVFile input
  flashValidationMessage (Nothing, csvresult) >>= asMaybe
  where
    asCSVFile :: Maybe Input -> IO (Result (BS.ByteString, [[BS.ByteString]]))
    asCSVFile input = do
      case input of
        Just(Input contentspec (Just filename) _ ) -> do
          content <- case contentspec of
                       Left filepath -> BSL.readFile filepath
                       Right content -> return content
          if BSL.null content
            then return Empty
            else do
              let title = BS.fromString (basename filename)
              case parseCSV content of
                 Left _ -> return $ Bad flashMessageFailedToParseCSV
                 Right contents
                   | length contents > rowlimit -> return $ Bad $ flashMessageCSVHasTooManyRows rowlimit
                   | otherwise -> return $ Good (title, map (map BS.fromString) contents)
        _ -> return Empty
    rowlimit :: Int = 500

handleIssueSave :: Kontrakcja m => Document -> m KontraLink
handleIssueSave document = do
    ctx <- getContext
    _ <- updateDocument ctx document
    if (isTemplate document)
     then do
          addFlashM flashDocumentTemplateSaved
          return $ LinkTemplates
     else do
          addFlashM flashDocumentDraftSaved
          return $ LinkContracts

handleIssueSignByAuthor :: Kontrakcja m => Document -> m KontraLink
handleIssueSignByAuthor doc = do
     mprovider <- readField "eleg"
     mndoc <- case mprovider of
                   Nothing ->  Right <$> authorSignDocumentFinal (documentid doc) Nothing
                   Just provider -> do
                      signature     <- getDataFnM $ look "signature"
                      transactionid <- getDataFnM $ look "transactionid"
                      esinfo <- BankID.verifySignatureAndGetSignInfoForAuthor (documentid doc) provider signature transactionid
                      case esinfo of
                        BankID.Problem msg -> return $ Left msg
                        BankID.Mismatch msg _ _ _ -> return $ Left msg
                        BankID.Sign sinfo -> Right <$>  authorSignDocumentFinal (documentid doc) (Just sinfo)

     case mndoc of
         Right (Right ndoc) -> do
             postDocumentChangeAction ndoc doc Nothing
             addFlashM flashAuthorSigned
             return $ LinkIssueDoc (documentid doc)
         _ -> return LoopBack

-- | Check if current user is author or has company rights to view the document
withAuthorisedViewer :: Kontrakcja m => DocumentID -> m (Either KontraLink a) -> m (Either KontraLink a)
withAuthorisedViewer docid action = do
  _ <- guardRightM $ getDocByDocID docid
  action

{- |
   Show the document with title in the url
   URL: /d/{documentid}/{title}
   Method: GET
 -}
handleFileGet :: Kontrakcja m => FileID -> String -> m (Either KontraLink Response)
handleFileGet fileid' _title = do
  withUserGet $ onlySuperUser $ do
   ctx <- getContext
   contents <- liftIO $ getFileIDContents ctx fileid'

   if BS.null contents
      then mzero
      else do
          let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
          let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
          return res2

{- |
   Get multiple post/get params and return them in an array
 -}
getAndConcat :: Kontrakcja m => String -> m [BS.ByteString]
getAndConcat fname = do
  values <- getDataFnM $ lookInputList fname
  return $ map concatChunks values

makePlacements :: Kontrakcja m
               => [BS.ByteString]
               -> [BS.ByteString]
               -> [Int]
               -> [Int]
               -> [Int]
               -> [Int]
               -> [Int]
               -> m [(BS.ByteString, BS.ByteString, FieldPlacement)]
makePlacements placedsigids
               placedfieldids
               placedxs
               placedys
               placedpages
               placedwidths
               placedheights = do
    let placements = zipWith5 FieldPlacement
                        placedxs
                        placedys
                        placedpages
                        placedwidths
                        placedheights

    return $ zip3 placedsigids placedfieldids placements

filterPlacementsByID :: [(BS.ByteString, BS.ByteString, FieldPlacement)]
                        -> BS.ByteString
                        -> BS.ByteString
                        -> [FieldPlacement]
filterPlacementsByID placements sigid fieldid =
    [x | (s, f, x) <- placements, s == sigid, f == fieldid]

fieldDefAndSigID :: [(BS.ByteString, BS.ByteString, FieldPlacement)]
                    -> BS.ByteString
                    -> BS.ByteString
                    -> BS.ByteString
                    -> BS.ByteString
                    -> (BS.ByteString, SignatoryField)
fieldDefAndSigID placements fn fv fid sigid = (sigid,
  SignatoryField {
      sfType = CustomFT fn $ BS.length fv > 0
    , sfValue = fv
    , sfPlacements = filterPlacementsByID placements sigid fid
  })

makeFieldDefs :: [(BS.ByteString, BS.ByteString, FieldPlacement)]
              -> [BS.ByteString]
              -> [BS.ByteString]
              -> [BS.ByteString]
              -> [BS.ByteString]
              -> [(BS.ByteString, SignatoryField)]
makeFieldDefs placements = zipWith4 (fieldDefAndSigID placements)

filterFieldDefsByID :: [(BS.ByteString, SignatoryField)]
                    -> BS.ByteString
                    -> [SignatoryField]
filterFieldDefsByID fielddefs sigid =
    [x | (s, x) <- fielddefs, s == sigid]

makeSignatory ::[(BS.ByteString, BS.ByteString, FieldPlacement)]
                -> [(BS.ByteString, SignatoryField)]
                -> BS.ByteString
                -> BS.ByteString
                -> BS.ByteString
                -> BS.ByteString
                -> SignOrder
                -> BS.ByteString
                -> BS.ByteString
                -> BS.ByteString
                -> SignatoryDetails
makeSignatory pls fds sid sfn  ssn  se  sso  sc  spn  scn = SignatoryDetails {
    signatorysignorder = sso
  , signatoryfields = [
      sf FirstNameFT sfn "fstname"
    , sf LastNameFT ssn "sndname"
    , sf EmailFT se "email"
    , sf CompanyFT sc "company"
    , sf PersonalNumberFT spn "personalnumber"
    , sf CompanyNumberFT scn "companynumber"
    ] ++ filterFieldDefsByID fds sid
  }
  where
    sf ftype value texttype = SignatoryField {
        sfType = ftype
      , sfValue = value
      , sfPlacements = filterPlacementsByID pls sid (BS.fromString texttype)
    }

makeSignatories :: [(BS.ByteString, BS.ByteString, FieldPlacement)]
                   -> [(BS.ByteString, SignatoryField)]
                   -> [BS.ByteString]
                   -> [BS.ByteString]
                   -> [SignOrder]
                   -> [BS.ByteString]
                   -> [BS.ByteString]
                   -> [BS.ByteString]
                   -> [BS.ByteString]
                   -> [BS.ByteString]
                   -> [SignatoryDetails]
makeSignatories placements fielddefs
                sigids
                signatoriesemails
                signatoriessignorders
                signatoriescompanies
                signatoriespersonalnumbers
                signatoriescompanynumbers
                signatoriesfstnames
                signatoriessndnames
  = zipWith8 (makeSignatory placements fielddefs)
    sigids
    signatoriesfstnames
    signatoriessndnames
    signatoriesemails
    signatoriessignorders
    signatoriescompanies
    signatoriespersonalnumbers
    signatoriescompanynumbers
    where
        zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs)
            = z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
        zipWith8 _ _ _ _ _ _ _ _ _ = []


-- NEED FIX !!!!  Reverse engineering is not a good design pattern | MR
-- I will fix this when doing backbone anyway.
makeAuthorDetails :: [(BS.ByteString, BS.ByteString, FieldPlacement)]
                     -> [(BS.ByteString, SignatoryField)]
                     -> SignatoryDetails
                     -> SignatoryDetails
makeAuthorDetails pls fielddefs sigdetails@SignatoryDetails{signatoryfields = sigfields} =
  sigdetails {
    signatoryfields =
      concatMap f sigfields ++ filterFieldDefsByID fielddefs (BS.fromString "author")
  }
  where
    f sf = case sfType sf of
      EmailFT -> [g "email"]
      FirstNameFT -> [g "fstname"]
      LastNameFT -> [g "sndname"]
      CompanyFT -> [g "company"]
      PersonalNumberFT -> [g "personalnumber"]
      CompanyNumberFT -> [g "companynumber"]
      CustomFT _ _ -> []
      where
        g ftype = sf { sfPlacements = filterPlacementsByID pls (BS.fromString "author") (BS.fromString ftype) }

{- |
   Save a document from data in the post params.

 -}
updateDocument :: Kontrakcja m => Context -> Document -> m (Either String Document)
updateDocument Context{ ctxtime } document@Document{ documentid, documentfunctionality } = do
  -- each signatory has these predefined fields
  signatoriesfstnames        <- getAndConcat "signatoryfstname"
  signatoriessndnames        <- getAndConcat "signatorysndname"
  signatoriescompanies       <- getAndConcat "signatorycompany"
  signatoriespersonalnumbers <- getAndConcat "signatorypersonalnumber"
  signatoriescompanynumbers  <- getAndConcat "signatorycompanynumber"
  signatoriesemails          <- map (fromMaybe BS.empty) <$> getOptionalFieldList asValidEmail "signatoryemail"
  signatoriessignorders      <- map (SignOrder . fromMaybe 1 . fmap (max 1 . fst) . BSC.readInteger) <$> getAndConcat "signatorysignorder" -- a little filtering here, but we want signatories to have sign order > 0
  signatoriesroles           <- getAndConcat "signatoryrole"
  liftIO $ print signatoriesroles
  liftIO $ print signatoriessignorders


  -- if the post doesn't contain this one, we parse the old way
  sigids <- getAndConcat "sigid"

  daystosign <- readField "daystosign"

  invitetext <- fmap (fromMaybe defaultInviteMessage) $ getCustomTextField "invitetext"

  mcsvsigindex <- getOptionalField asValidNumber "csvsigindex"

  docname <- getCriticalField (return . BS.fromString) "docname"

  -- each custom field must have this
  fieldnames  <- getAndConcat "fieldname"
  fieldids    <- getAndConcat "fieldid"
  fieldsigids <- getAndConcat "fieldsigid"
  fieldvalues <- getAndConcat "fieldvalue"

  -- each placed field must have these values
  placedxs       <- getCriticalFieldList asValidNumber "placedx"
  placedys       <- getCriticalFieldList asValidNumber "placedy"
  placedpages    <- getCriticalFieldList asValidNumber "placedpage"
  placedwidths   <- getCriticalFieldList asValidNumber "placedwidth"
  placedheights  <- getCriticalFieldList asValidNumber "placedheight"
  placedsigids   <- getAndConcat "placedsigid"
  placedfieldids <- getAndConcat "placedfieldid"

  Log.debug $ show placedfieldids


  authorrole <- getFieldWithDefault "" "authorrole"
  authorsignorder <- (SignOrder . fromMaybe 1) <$> getValidateAndHandle asValidNumber asMaybe "authorsignorder"

  docfunctionality <- getCriticalField (return . maybe documentfunctionality fst . listToMaybe . reads) "docfunctionality"

  validmethods <- getAndConcat "validationmethod"
  let docallowedidtypes =
        case mapJust (idmethodFromString . BS.toString) validmethods of
          [] -> [EmailIdentification]
          ims -> ims

  placements <- makePlacements placedsigids
                                placedfieldids
                                placedxs
                                placedys
                                placedpages
                                placedwidths
                                placedheights

  let fielddefs = makeFieldDefs placements
                                fieldnames
                                fieldvalues
                                fieldids
                                fieldsigids

  let signatories = makeSignatories placements fielddefs
                        sigids
                        signatoriesemails
                        (signatoriessignorders ++ repeat (SignOrder 1))
                        signatoriescompanies
                        signatoriespersonalnumbers
                        signatoriescompanynumbers
                        signatoriesfstnames
                        signatoriessndnames
  Log.debug $ "signatories " ++ show signatories
                        -- authornote: we need to store the author info somehow!
  let Just authorsiglink = getAuthorSigLink document
      authordetails = (makeAuthorDetails placements fielddefs $ signatorydetails authorsiglink) { signatorysignorder = authorsignorder }
  Log.debug $ "set author sign order to " ++ (show authorsignorder)

  let isauthorsig = authorrole == "signatory"
      signatories2 = zip signatories roles2
      authordetails2 = (authordetails, if isauthorsig
                                       then [SignatoryPartner, SignatoryAuthor]
                                       else [SignatoryAuthor])
      roles2 = map guessRoles signatoriesroles
      guessRoles x | x == BS.fromString "signatory" = [SignatoryPartner]
                   | otherwise = []
  -- FIXME: tell the user what happened!
  -- when (daystosign<1 || daystosign>99) mzero

  --let emails = zip signatoriesemails
  --              (sequence $ map (doc_query . GetUserByEmail . Email) signatoriesemails)

  -- author is gotten above, no?
  -- Just author <- doc_query $ GetUserByUserID $ unAuthor $ documentauthor documentis
  when (docfunctionality == AdvancedFunctionality) $ do
    _ <- doc_update $ SetDocumentAdvancedFunctionality documentid ctxtime
    return ()
  _ <- doc_update $ SetDocumentTitle documentid docname ctxtime
  _ <- doc_update $ SetInviteText documentid invitetext ctxtime
  when (isJust mcsvsigindex) $ ignore $ doc_update $ SetCSVSigIndex documentid (fromJust mcsvsigindex) ctxtime
  case docallowedidtypes of
       [ELegitimationIdentification] -> ignore $ doc_update $ SetElegitimationIdentification documentid ctxtime
       [EmailIdentification] -> ignore $ doc_update $ SetEmailIdentification documentid ctxtime
       i -> Log.debug $ "I don't know how to set this kind of identificaiton: " ++ show i
  when (isJust daystosign) $ ignore $ doc_update $ SetDaysToSign documentid (fromJust daystosign) ctxtime
  aa <- doc_update $ ResetSignatoryDetails documentid (authordetails2 : signatories2) ctxtime
  Log.debug $ "final document returned " ++ show aa
  return aa

getDocumentsForUserByType :: Kontrakcja m => DocumentType -> User -> m [Document]
getDocumentsForUserByType doctype user = do
  mysigdocs <- doc_query $ GetDocumentsBySignatory user
  mydocuments <- if useriscompanyadmin user
                   then do
                     mycompanydocs <- doc_query $ GetDocumentsByCompany user
                     return $ union mysigdocs mycompanydocs
                   else return mysigdocs

  return . filter ((\d -> documenttype d == doctype)) $ nub mydocuments


handleAttachmentViewForViewer :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m Response
handleAttachmentViewForViewer docid siglinkid mh = do
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  let pending JpegPagesPending = True
      pending _                = False
      files                    = map authorattachmentfile (documentauthorattachments doc)
  case files of
    [] -> return $ toResponse ""
    f  -> do
      b <- mapM (\file -> maybeScheduleRendering file (documentid doc)) f
      if any pending b
        then notFound (toResponse "temporary unavailable (document has files pending for process)")
        else do
        pages <- Doc.DocView.showFilesImages2 (documentid doc) Nothing $ zip f b
        simpleResponse pages

handleAttachmentViewForAuthor :: Kontrakcja m => DocumentID -> m Response
handleAttachmentViewForAuthor docid = do
  doc <- guardRightM $ getDocByDocID docid
  let pending JpegPagesPending = True
      pending _                = False
      files                    = map authorattachmentfile (documentauthorattachments doc)
  case files of
    [] -> return $ toResponse ""
    f  -> do
      b <- mapM (\file -> maybeScheduleRendering file (documentid doc)) f
      if any pending b
        then notFound (toResponse "temporary unavailable (document has files pending for process)")
        else do
        pages <- Doc.DocView.showFilesImages2 (documentid doc) Nothing $ zip f b
        simpleResponse pages

{- We return pending message if file is still pending, else we return JSON with number of pages-}
handleFilePages :: Kontrakcja m => DocumentID -> FileID -> m JSValue
handleFilePages did fid = do
  (mdoc,_) <- jsonDocumentGetterWithPermissionCheck did
  when (isNothing mdoc ) mzero
  let doc = fromJust mdoc
  let allfiles = (documentfiles doc) ++ (documentsealedfiles doc)  ++ (authorattachmentfile <$> documentauthorattachments doc)
  case find (== fid) allfiles of
    Nothing -> return $ JSObject $ toJSObject [("error",JSString $ toJSString $ "File #" ++ show fid ++ " not found in document #" ++ show did)]
    Just _  -> do
      jpages <- maybeScheduleRendering fid did
      case jpages of
       JpegPagesPending -> return $ JSObject $ toJSObject [("wait",JSString $ toJSString "Temporary unavailable (file is still pending)")]
       JpegPagesError _ -> return $ JSObject $ toJSObject [("error",JSString $ toJSString "rendering failed")]
       JpegPages pages  -> return $ JSObject $ toJSObject [("pages",JSArray $ map pageinfo pages)]
  where
      pageinfo (_,width,height) = JSObject $ toJSObject [("width",JSRational True $ toRational width),
                                                         ("height",JSRational True $ toRational height)
                                                        ]

-- get rid of duplicates
-- FIXME: nub is very slow
prepareDocsForList :: [Document] -> [Document]
prepareDocsForList =
  sortBy (\d1 d2 -> compare (documentmtime d2) (documentmtime d1)) . nub

handlePageOfDocument :: Kontrakcja m => DocumentID -> m (Either KontraLink Response)
handlePageOfDocument docid = checkUserTOSGet $ handlePageOfDocument' docid Nothing

handlePageOfDocumentForSignatory :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m Response
handlePageOfDocumentForSignatory docid siglinkid sigmagichash = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    handlePageOfDocument' docid $ Just (siglinkid, sigmagichash)

handlePageOfDocument' :: Kontrakcja m => DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> m Response
handlePageOfDocument' documentid mtokens = do
  Log.debug $ "Request for doc " ++ show documentid
  edoc <- case mtokens of
    Nothing         -> getDocByDocID documentid
    Just (slid, mh) -> getDocByDocIDSigLinkIDAndMagicHash documentid slid mh
  case edoc of
    Left l -> do
      Log.debug ("Could not get Document " ++ show l)
      mzero
    Right Document { documentfiles
                   , documentsealedfiles
                   , documentstatus
                   } -> do
      let pending JpegPagesPending = True
          pending _                = False
          files                    = if documentstatus == Closed
                                      then documentsealedfiles
                                      else documentfiles
      case files of
         [] -> notFound $ toResponse "temporarily unavailable (document has no files)"
         f  -> do
             b <- mapM (\file -> maybeScheduleRendering file documentid) f
             if any pending b
                then notFound (toResponse "temporarily unavailable (document has files pending for process)")
                else do
                    pages <- Doc.DocView.showFilesImages2 documentid mtokens $ zip f b
                    simpleResponse pages

handleDocumentUpload :: Kontrakcja m => DocumentID -> BS.ByteString -> BS.ByteString -> m ()
handleDocumentUpload docid content1 filename = do
  Log.debug $ "Uploading file for doc #" ++ show docid
  fileresult <- attachFile docid filename content1
  case fileresult of
    Left err -> do
      Log.debug $ "Got an error in handleDocumentUpload: " ++ show err
      return ()
    Right _document ->
        return ()
  return ()

handleDocumentUploadNoLogin :: Kontrakcja m => DocumentID -> BS.ByteString -> BS.ByteString -> m ()
handleDocumentUploadNoLogin docid content1 filename = do
  Log.debug $ "Uploading file for doc " ++ show docid
  ctx <- getContext
  content14 <- liftIO $ preprocessPDF ctx content1 docid
  file <- runDB $ dbUpdate $ NewFile filename content14
  fileresult <- doc_update (AttachFile docid (fileid file) (ctxtime ctx))
  case fileresult of
    Left err -> do
      Log.debug $ "Got an error in handleDocumentUpload: " ++ show err
      return ()
    Right _document -> do
        return ()
  return ()



handleIssueNewDocument :: Kontrakcja m => m KontraLink
handleIssueNewDocument = withUserPost $ do
    Log.debug $ "Creating a new document"
    input <- getDataFnM (lookInput "doc")
    mdocprocess <- getDocProcess
    let docprocess = fromMaybe (Contract) mdocprocess
    Log.debug $ "Creating new document of process : " ++ show docprocess
    mdoc <- makeDocumentFromFile (Signable docprocess) input
    case mdoc of
      Nothing -> return LinkUpload
      Just doc -> do
        Log.debug $ "Document #" ++ show (documentid doc) ++ " created"
        _ <- addDocumentCreateStatEvents doc
        return $ LinkIssueDoc $ documentid doc

handleCreateNewTemplate:: Kontrakcja m => m KontraLink
handleCreateNewTemplate = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  mdoc <- makeDocumentFromFile (Template Contract) input
  case mdoc of
    Nothing -> return $ LinkTemplates
    Just doc -> do
      _ <- addDocumentCreateStatEvents doc
      return $ LinkIssueDoc $ documentid doc

handleCreateNewAttachment:: Kontrakcja m => m KontraLink
handleCreateNewAttachment = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  mdoc <- makeDocumentFromFile Attachment input
  when (isJust mdoc) $ do
    _<- addDocumentCreateStatEvents $ fromJust mdoc
    return ()
  return LinkAttachments

makeDocumentFromFile :: Kontrakcja m => DocumentType -> Input -> m (Maybe Document)
makeDocumentFromFile doctype (Input contentspec (Just filename) _contentType) = do
    Log.debug $ "makeDocumentFromFile: beggining"
    guardLoggedIn
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
    if BSL.null content
      then do
        Log.debug "makeDocumentFromFile: no content"
        return Nothing
      else do
          Log.debug "Got the content, creating document"
          let title = BS.fromString (basename filename)
          doc <- guardRightM $ newDocument title doctype
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ Just doc
makeDocumentFromFile _ _ = mzero -- to complete the patterns


handleRubbishRestore :: Kontrakcja m => m KontraLink
handleRubbishRestore = do
  Context { ctxmaybeuser = Just user } <- getContext
  docids <- getCriticalFieldList asValidDocID "doccheck"
  mapM_ (guardRightM . doc_update . RestoreArchivedDocument user) $ map DocumentID docids
  addFlashM flashMessageRubbishRestoreDone
  return $ LinkRubbishBin

handleRubbishReallyDelete :: Kontrakcja m => m KontraLink
handleRubbishReallyDelete = do
  Context { ctxmaybeuser = Just user } <- getContext
  docids <- getCriticalFieldList asValidDocID "doccheck"
  mapM_ (guardRightM . doc_update . ReallyDeleteDocument user) $ map DocumentID docids
  addFlashM flashMessageRubbishHardDeleteDone
  return $ LinkRubbishBin

handleTemplateShare :: Kontrakcja m => m KontraLink
handleTemplateShare = withUserPost $ do
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashM $ flashMessageSingleTemplateShareDone $ documenttitle d
      _ -> addFlashM flashMessageMultipleTemplateShareDone
    return $ LinkTemplates

handleAttachmentShare :: Kontrakcja m => m KontraLink
handleAttachmentShare = withUserPost $ do
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashM $ flashMessageSingleAttachmentShareDone $ documenttitle d
      _ -> addFlashM  flashMessageMultipleAttachmentShareDone
    return $ LinkAttachments

handleIssueShare :: Kontrakcja m => m [Document]
handleIssueShare = do
  idnumbers <- getCriticalFieldList asValidDocID "doccheck"
  let ids = map DocumentID idnumbers
  guardRightM $ shareDocuments ids

handleAttachmentRename :: Kontrakcja m => DocumentID -> m KontraLink
handleAttachmentRename docid = withUserPost $ do
  Context {ctxtime} <- getContext
  newname <- getCriticalField (return . BS.fromString) "docname"
  doc <- guardRightM $ doc_update $ SetDocumentTitle docid newname ctxtime
  return $ LinkIssueDoc $ documentid doc

handleBulkContractRemind :: Kontrakcja m => m KontraLink
handleBulkContractRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Contract)
    return $ LinkContracts

handleBulkOfferRemind :: Kontrakcja m => m KontraLink
handleBulkOfferRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Offer)
    return $ LinkOffers

handleBulkOrderRemind :: Kontrakcja m => m KontraLink
handleBulkOrderRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Order)
    return $ LinkOrders

{- |
    This sends out bulk reminders.  The functionality is offered in the document
    and offers list page.  It will make sure the user is actually the author of everything,
    and send out reminders only to signatories who haven't accepted or signed on those that are
    pending.  This returns all the signatory links that were reminded.
-}
handleIssueBulkRemind :: Kontrakcja m => DocumentType -> m [SignatoryLink]
handleIssueBulkRemind doctype = do
    ctx@Context{ctxmaybeuser = Just user } <- getContext
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    let ids = map DocumentID idnumbers
    remindedsiglinks <- fmap concat . sequence . map (\docid -> docRemind ctx user docid) $ ids
    case (length remindedsiglinks) of
      0 -> addFlashM $ flashMessageNoBulkRemindsSent doctype
      _ -> addFlashM $ flashMessageBulkRemindsSent doctype
    return remindedsiglinks
    where
      docRemind :: Kontrakcja m => Context -> User -> DocumentID -> m [SignatoryLink]
      docRemind ctx user docid = do
        doc <- queryOrFail $ GetDocumentByDocumentID docid
        failIfNotAuthor doc user
        case (documentstatus doc) of
          Pending -> do
            let isElegible = isEligibleForReminder (Just user) doc
                unsignedsiglinks = filter isElegible $ documentsignatorylinks doc
            sequence . map (sendReminderEmail Nothing ctx doc) $ unsignedsiglinks
          _ -> return []

{- |
   Get some html to display the images of the files
   URL: /pagesofdoc/{documentid}
   Method: GET
   FIXME: Should probably check for permissions to view
 -}
showPage :: Kontrakcja m => DocumentID -> FileID -> Int -> m (Either KontraLink Response)
showPage docid fileid pageno = do
  msid <- readField "signatorylinkid"
  mmh <- readField "magichash"
  edoc <- case (msid, mmh) of
           (Just sid, Just mh) -> Right <$> guardRightM (getDocByDocIDSigLinkIDAndMagicHash docid sid mh)
           _ ->  checkUserTOSGet $ guardRightM (getDocByDocID docid)
  case edoc of
       Right doc -> do
           unless (fileInDocument doc fileid) mzero
           Right <$> showPage' fileid pageno
       Left rdir -> return $ Left rdir


showPreview:: Kontrakcja m => DocumentID -> FileID -> m (Either KontraLink Response)
showPreview docid fileid = withAuthorisedViewer docid $ do
    iprev <- preview docid fileid 0
    case iprev of
         Just res -> return $ Right res
         Nothing ->   return $ Left $ LinkDocumentPreview docid Nothing fileid

showPreviewForSignatory:: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> FileID -> m (Either KontraLink Response)
showPreviewForSignatory docid siglinkid sigmagichash fileid = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    iprev <- preview docid fileid 0
    case iprev of
         Just res -> return $ Right res
         Nothing ->   return $ Left $ LinkDocumentPreview docid (getMaybeSignatoryLink (doc,siglinkid)) fileid

preview :: Kontrakcja m =>  DocumentID -> FileID -> Int -> m (Maybe Response)
preview did fid value
  | value > 10 = return Nothing
  | otherwise  =   do
        Context{ctxnormalizeddocuments} <- getContext
        docmap <- liftIO $ readMVar ctxnormalizeddocuments
        case Map.lookup fid docmap of
            Just (JpegPages pages) -> do
                let (contents,_,_) =  pages !! 0
                scaledContent <- liftIO $ scaleForPreview did contents
                let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [scaledContent]) Nothing
                return $ Just $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
            other -> do
                when_ (other == Nothing) $ maybeScheduleRendering fid did
                liftIO $ threadDelay 500000
                preview did fid (value+1)


showPage' :: Kontrakcja m => FileID -> Int -> m Response
showPage' fileid pageno = do
  Context{ctxnormalizeddocuments} <- getContext
  docmap <- liftIO $ readMVar ctxnormalizeddocuments
  case Map.lookup fileid docmap of
    Just (JpegPages pages) -> do
      let (contents,_,_) =  pages !! (pageno - 1)
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      Log.debug $ "JPEG page found and returned for file " ++ show fileid ++ " and page " ++ show pageno
      return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
    _ -> do
      Log.debug $ "JPEG page not found in cache, responding 404 for file " ++ show fileid ++ " and page " ++ show pageno
      notFound (toResponse "temporarily unavailable (document has files pending for process)")

handleCancel :: Kontrakcja m => DocumentID -> m KontraLink
handleCancel docid = withUserPost $ do
  doc <- guardRightM $ getDocByDocID docid
  ctx@Context { ctxtime, ctxipnumber } <- getContext
  if isPending doc || isAwaitingAuthor doc
    then do
    customMessage <- getCustomTextField "customtext"
    mdoc' <- doc_update $ CancelDocument (documentid doc) ManualCancel ctxtime ctxipnumber
    case mdoc' of
      Right doc' -> do
        sendCancelMailsForDocument customMessage ctx doc
        addFlashM $ flashMessageCanceled doc'
      Left errmsg -> addFlash (OperationFailed, errmsg)
    else addFlashM flashMessageCannotCancel
  return (LinkIssueDoc $ documentid doc)

{-
handleWithdrawn:: DocumentID -> Kontra KontraLink
handleWithdrawn docid = do
  mdoc <- doc_query $ GetDocumentByDocumentID docid
  case (mdoc) of
    Just doc -> withDocumentAutho doc $ do
                          doc_update $ WithdrawnDocument $ documentid doc
                          return (LinkIssueDoc $ documentid doc)
    Nothing -> return LinkMain
-}

handleRestart :: Kontrakcja m => DocumentID -> m KontraLink
handleRestart docid = withUserPost $ do
  doc <- guardRightM $ getDocByDocID docid
  doc2 <- guardRightM $ restartDocument doc
  addFlashM $ flashDocumentRestarted doc2
  return $ LinkIssueDoc (documentid doc2)

handleResend :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleResend docid signlinkid  = withUserPost $ do
  ctx@Context { ctxmaybeuser = Just user } <- getContext
  doc <- guardRightM $ getDocByDocID docid
  guard (isAuthor (doc, user)) -- only author can resend
  signlink <- guardJust $ getSigLinkFor doc signlinkid
  customMessage <- getCustomTextField "customtext"
  _ <- sendReminderEmail customMessage ctx doc signlink
  addFlashM $ flashRemindMailSent signlink
  return (LinkIssueDoc docid)

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

--This only works for undelivered mails. We shoulkd check if current user is author
handleChangeSignatoryEmail :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleChangeSignatoryEmail docid slid = withUserPost $ do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Just email -> do
      ctx@Context { ctxmaybeuser = Just user } <- getContext
      edoc <- getDocByDocID docid
      case edoc of
        Left _ -> return LoopBack
        Right doc -> do
          guard $ isAuthor (doc, user)
          muser <- runDBQuery $ GetUserByEmail (documentservice doc) (Email email)
          mnewdoc <- doc_update $ ChangeSignatoryEmailWhenUndelivered docid slid muser email
          case mnewdoc of
            Right newdoc -> do
              -- get (updated) siglink from updated document
              sl <- guardJust (getSigLinkFor newdoc slid)
              _ <- sendInvitationEmail1 ctx newdoc sl
              return $ LoopBack
            _ -> return LoopBack
    _ -> return LoopBack

sendCancelMailsForDocument :: Kontrakcja m => (Maybe BS.ByteString) -> Context -> Document -> m ()
sendCancelMailsForDocument customMessage ctx document = do
  let activated_signatories = filter (isActivatedSignatory $ documentcurrentsignorder document) $ documentsignatorylinks document
  forM_ activated_signatories $ \slnk -> do
      m <- mailCancelDocumentByAuthor True customMessage ctx document
      let mail = m {to = [getMailAddress slnk]}
      scheduleEmailSendout (ctxesenforcer ctx) mail

failIfNotAuthor :: Kontrakcja m => Document -> User -> m ()
failIfNotAuthor document user = guard (isAuthor (document, user))

checkLinkIDAndMagicHash :: Kontrakcja m => Document -> SignatoryLinkID -> MagicHash -> m ()
checkLinkIDAndMagicHash document linkid magichash1 = do
  siglink <- guardJust $ getSigLinkFor document linkid
  guard $ signatorymagichash siglink == magichash1
  return ()

handleShowUploadPage :: Kontrakcja m => m (Either KontraLink String)
handleShowUploadPage = checkUserTOSGet $ do
    showTemplates <- isFieldSet "showTemplates"
    tooLarge <- isFieldSet "tooLarge"
    mdocprocess <- getDocProcess
    when tooLarge $ addFlashM modalPdfTooLarge
    uploadPage mdocprocess showTemplates

getDocProcess :: Kontrakcja m => m (Maybe DocumentProcess)
getDocProcess = getOptionalField asDocType "doctype"
  where
    asDocType :: String -> Result DocumentProcess
    asDocType val
      | val == show Offer = Good $ Offer
      | val == show Contract = Good $ Contract
      | val == show Order = Good $ Order
      | otherwise = Empty

idmethodFromString :: String -> Maybe IdentificationType
idmethodFromString idmethod
  | idmethod == "email" = Just EmailIdentification
  | idmethod == "eleg"  = Just ELegitimationIdentification
  | otherwise           = Nothing

handleCreateFromTemplate :: Kontrakcja m => m KontraLink
handleCreateFromTemplate = withUserPost $ do
  Context { ctxmaybeuser, ctxtime } <- getContext
  docid <- readField "template"
  Log.debug $ show "Creating document from template : " ++ show docid
  case docid of
    Just did -> do
      let user = fromJust ctxmaybeuser
      document <- queryOrFail $ GetDocumentByDocumentID $ did
      Log.debug $ show "Matching document found"
      let haspermission = maybe False
                                (\sl -> isSigLinkFor (userid user) sl
                                        || (isSigLinkFor (usercompany user) sl
                                              && isShared document))
                                $ getAuthorSigLink document
      enewdoc <- if haspermission
                    then do
                      Log.debug $ show "Valid persmision to create from template"
                      mcompany <- getCompanyForUser user
                      doc_update $ SignableFromDocumentIDWithUpdatedAuthor user mcompany did ctxtime
                    else mzero
      case enewdoc of
        Right newdoc -> do
          _ <- addDocumentCreateStatEvents newdoc
          Log.debug $ show "Document created from template"
          return $ LinkIssueDoc $ documentid newdoc
        Left _ -> mzero
    Nothing -> mzero

{- |
   The FileID matches the AuthorAttachment.
 -}
authorAttachmentHasFileID :: FileID -> AuthorAttachment -> Bool
authorAttachmentHasFileID fid attachment =
  fid == authorattachmentfile attachment

{- |
   The FileID matches the SignatoryAttachment.
-}
sigAttachmentHasFileID :: FileID -> SignatoryAttachment -> Bool
sigAttachmentHasFileID fid attachment =
  maybe False (fid ==) (signatoryattachmentfile attachment)

{- |
   Download the attachment with the given fileid
 -}

handleDownloadFile :: Kontrakcja m => DocumentID -> FileID -> String -> m Response
handleDownloadFile did fid _nameForBrowser = do
  msid <- readField "signatorylinkid"
  mmh <- readField "magichash"
  doc <- case (msid, mmh) of
           (Just sid, Just mh) -> guardRightM $ getDocByDocIDSigLinkIDAndMagicHash did sid mh
           _ ->                   guardRightM $ getDocByDocID did
  unless (fileInDocument doc fid) mzero
  respondWithFile doc fid

respondWithFile :: Kontrakcja m =>  Document -> FileID -> m Response
respondWithFile _doc fid =  do
    ctx <- getContext
    respondWithPDF =<< liftIO (getFileIDContents ctx fid)

respondWithPDF :: Kontrakcja m => BS.ByteString -> m Response
respondWithPDF contents = do
  let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
  return res2

-- Fix for broken production | To be removed after fixing is done
isBroken :: Document -> Bool
isBroken doc = isClosed doc && (not $ Data.List.null $ documentfiles doc)  && (Data.List.null $ documentsealedfiles doc)

handleFixDocument :: Kontrakcja m => DocumentID -> m KontraLink
handleFixDocument docid = onlySuperUser $ do
    ctx <- getContext
    mdoc <- doc_query $ GetDocumentByDocumentID docid
    case (mdoc) of
       Nothing -> return LoopBack
       Just doc -> if (isBroken doc)
                    then do
                        runDB $ sealDocument ctx doc
                        return LoopBack
                    else return LoopBack

showDocumentsToFix :: Kontrakcja m => m String
showDocumentsToFix = onlySuperUser $ do
    docs <- doc_query $ GetDocuments Nothing
    documentsToFixView $ filter isBroken docs

handleDeleteSigAttach :: Kontrakcja m => DocumentID -> SignatoryLinkID ->  m KontraLink
handleDeleteSigAttach docid siglinkid = do
  mh <- guardJustM $ readField "magichash"
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  siglink <- guardJust $ getSigLinkFor doc siglinkid
  fid <- (read . BS.toString) <$> getCriticalField asValidID "deletesigattachment"
  let email = getEmail siglink
  Log.debug $ "delete Sig attachment " ++ (show fid) ++ "  " ++ (BS.toString email)
  _ <- doc_update $ DeleteSigAttachment docid email fid
  return $ LinkSignDoc doc siglink

handleSigAttach :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleSigAttach docid siglinkid = do
  mh <- guardJustM $ readField "magichash"
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  siglink <- guardJust $ getSigLinkFor doc siglinkid
  attachname <- getCriticalField asValidFieldValue "attachname"
  let email = getEmail siglink
  _ <- guardJust $  find (\sa -> signatoryattachmentemail sa == email
                                && signatoryattachmentname sa == attachname) (documentsignatoryattachments doc)
  (Input contentspec _ _) <- getDataFnM (lookInput "sigattach")
  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  content <- liftIO $ preprocessPDF ctx (concatChunks content1) docid
  file <- runDB $ dbUpdate $ NewFile attachname content
  d <- guardRightM $ doc_update $ SaveSigAttachment docid attachname email (fileid file)
  return $ LinkSignDoc d siglink

jsonDocumentsList ::  Kontrakcja m => m JSValue
jsonDocumentsList = do
    Just user <- ctxmaybeuser <$> getContext
    lang <- getLang . ctxlocale <$> getContext
    doctype <- getFieldWithDefault "" "documentType"
    allDocs <- case (doctype) of
        "Contract" -> getDocumentsForUserByType (Signable Contract) user
        "Offer" -> getDocumentsForUserByType (Signable Offer) user
        "Order" -> getDocumentsForUserByType (Signable Order) user
        "Template" -> do
            mydocuments <- doc_query $ GetDocumentsByAuthor (userid user)
            return $ filter isTemplate mydocuments
        "Attachment" -> do
            mydocuments <- doc_query $ GetDocumentsByAuthor (userid user)
            return $ filter ((==) Attachment . documenttype) mydocuments
        "Rubbish" -> do
            if useriscompanyadmin user
                then doc_query $ GetDeletedDocumentsByCompany user
                else doc_query $ GetDeletedDocumentsByUser user
        "Template|Contract" -> do
            let tfilter doc = (Template Contract == documenttype doc)
            userdocs <- doc_query $ GetDocumentsByAuthor (userid user)
            shareddocs <- doc_query $ GetDocumentsSharedInCompany user
            return $ nub . filter tfilter $ userdocs ++ shareddocs
        "Template|Offer" -> do
            let tfilter doc = (Template Offer == documenttype doc)
            userdocs <- doc_query $ GetDocumentsByAuthor (userid user)
            shareddocs <- doc_query $ GetDocumentsSharedInCompany user
            return $ nub . filter tfilter $ userdocs ++ shareddocs
        "Template|Order" -> do
            let tfilter doc = (Template Order == documenttype doc)
            userdocs <- doc_query $ GetDocumentsByAuthor (userid user)
            shareddocs <- doc_query $ GetDocumentsSharedInCompany user
            return $ nub . filter tfilter $ userdocs ++ shareddocs
        _ -> do
            Log.error "Documents list : No valid document type provided"
            return []
    Log.debug $ "Documents list: Number of documents found "  ++  (show $ length allDocs)
    params <- getListParamsNew
    let docs = docSortSearchPage params allDocs
    cttime <- liftIO $ getMinutesTime
    docsJSONs <- mapM (fmap JSObject . docForListJSON (timeLocaleForLang lang) cttime user) $ list docs
    return $ JSObject $ toJSObject [("list",JSArray docsJSONs),
                                    ("paging", pagingParamsJSON docs)]


jsonDocument :: Kontrakcja m => DocumentID -> m JSValue
jsonDocument did = do
    (mdoc,msiglink) <- jsonDocumentGetterWithPermissionCheck did
    cttime <- liftIO $ getMinutesTime
    rsp <- case mdoc of
         Nothing -> return $ JSObject $ toJSObject [("error",JSString $ toJSString "No document avaible")]
         Just doc -> JSObject <$> documentJSON msiglink cttime doc
    return rsp

jsonDocumentGetterWithPermissionCheck ::   Kontrakcja m => DocumentID -> m (Maybe Document, Maybe SignatoryLink)
jsonDocumentGetterWithPermissionCheck did = do
    ctx <- getContext
    mmagichashh <- readField "magichash"
    msignatorylink <- readField "signatoryid"
    case (msignatorylink,mmagichashh) of
        (Just slid,Just mh) -> do
                   mdoc <- doc_query $ GetDocumentByDocumentID did
                   let msiglink = join $ find ((== slid) . signatorylinkid) <$> (documentsignatorylinks <$> mdoc)
                   if (validSigLink slid mh mdoc)
                     then return (mdoc,msiglink)
                     else return (Nothing,Nothing)
        _ -> do
                    mdoc <- toMaybe <$> getDocByDocID did
                    let msiglink = join $ getMaybeSignatoryLink <$> pairMaybe mdoc (ctxmaybeuser ctx)
                    return $ (mdoc, msiglink)



handleInvariantViolations :: Kontrakcja m => m Response
handleInvariantViolations = onlySuperUser $ do
  Context{ ctxtime } <- getContext
  docs <- doc_query $ GetDocuments Nothing
  let probs = listInvariantProblems ctxtime docs
      res = case probs of
        [] -> "No problems!"
        _  -> intercalate "\n" probs
  return $ Response 200 Map.empty nullRsFlags (BSL.fromString res) Nothing



prepareEmailPreview :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
prepareEmailPreview docid slid = do
    ctx <- getContext
    Log.debug "Making email preview"
    mailtype <- getFieldWithDefault "" "mailtype"
    mdoc <- doc_query $ GetDocumentByDocumentID docid
    when (isNothing mdoc) $ (Log.debug "No document found") >> mzero
    let doc = fromJust mdoc
    content <- case mailtype of
         "cancel" -> mailCancelDocumentByAuthorContent True Nothing ctx doc
         "remind" -> do
             let msl = find ((== slid) . signatorylinkid) $ documentsignatorylinks doc
             case msl of
               Just sl -> mailDocumentRemindContent  Nothing ctx doc sl
               Nothing -> return ""
         "reject" -> do
             let msl = find ((== slid) . signatorylinkid) $ documentsignatorylinks doc
             case msl of
               Just sl -> mailRejectMailContent Nothing ctx  doc sl
               Nothing -> return ""
         _ -> return ""
    return $ JSObject $ toJSObject [("content",JSString $ toJSString $ content)]


handleCSVLandpage :: Kontrakcja m => Int -> m String
handleCSVLandpage c = do
  text <- csvLandPage c
  return text

-- for upsales bug
handleUpsalesDeleted :: Kontrakcja m => m Response
handleUpsalesDeleted = onlySuperUser $ do
  docs <- doc_query $ GetDocuments $ Just $ ServiceID $ BS.fromString "upsales"
  let deleteddocs = [[show $ documentid d, showDateYMD $ documentctime d]
                    | d <- docs
                    , isDeletedFor $ getAuthorSigLink d]
  let header = ["document_id", "date created"]
  let csv = toCSV header deleteddocs
  ok $ setHeader "Content-Disposition" "attachment;filename=upsalesdocsdeleted.csv"
     $ setHeader "Content-Type" "text/csv"
     $ toResponse csv
