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
import Doc.DocState
import Doc.DocStateQuery
import Doc.DocStateUpdate
import Doc.DocStorage
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import Doc.DocProcess
import InputValidation
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
import qualified Amazon as AWS
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

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.Either
import Data.List
import Data.Maybe
import Data.Word
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update, query)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import Text.JSON hiding (Result)

import ForkAction

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
    -- No status change ;
    | documentstatus == oldstatus =
        -- if sign order has changed, we need to send another invitations
        when (documentcurrentsignorder document /= documentcurrentsignorder olddocument) $ do
            ctx <- getContext
            Log.server $ "Resending invitation emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
            sendInvitationEmails ctx document
    -- Preparation -> Pending
    -- main action: sendInvitationEmails
    | oldstatus == Preparation && documentstatus == Pending = do 
        _ <- addDocumentSendStatEvents document  
        ctx <- getContext
        msaveddoc <- saveDocumentForSignatories document
        document' <- case msaveddoc of
          (Left msg) -> do
            Log.error $ "Failed to save document #" ++ (show documentid) ++ " for signatories " ++ msg
            return document
          (Right saveddoc) -> return saveddoc
        -- we don't need to forkIO here since we only schedule emails here
        Log.server $ "Sending invitation emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
        sendInvitationEmails ctx document'
        return ()
    -- Preparation -> Closed (only author signs)
    -- main action: sealDocument and sendClosedEmails
    | oldstatus == Preparation && documentstatus == Closed = do
        _ <- addDocumentCloseStatEvents document
        ctx@Context{ctxtemplates} <- getContext
        forkAction ("Sealing document #" ++ show documentid ++ ": " ++ BS.toString documenttitle) $ do
          enewdoc <- sealDocument ctx document
          case enewdoc of
            Right newdoc -> runLocalTemplates ctxtemplates $ sendClosedEmails ctx newdoc
            Left errmsg -> Log.error $ "Sealing of document #" ++ show documentid ++ " failed, could not send document confirmations: " ++ errmsg
        return ()
    -- Pending -> AwaitingAuthor
    -- main action: sendAwaitingEmail
    | oldstatus == Pending && documentstatus == AwaitingAuthor = do
        ctx <- getContext
        -- we don't need to forkIO here since we only schedule emails here
        Log.server $ "Sending awaiting email for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
        sendAwaitingEmail ctx document
        return ()
    -- Pending -> Closed OR AwaitingAuthor -> Closed
    -- main action: sendClosedEmails
    | (oldstatus == Pending || oldstatus == AwaitingAuthor) && documentstatus == Closed = do
        _ <- addDocumentCloseStatEvents document          
        ctx@Context{ctxtemplates} <- getContext
        forkAction ("Sealing document #" ++ show documentid ++ ": " ++ BS.toString documenttitle) $ do
          enewdoc <- sealDocument ctx document
          case enewdoc of
            Right newdoc -> runLocalTemplates ctxtemplates $ sendClosedEmails ctx newdoc
            Left errmsg -> do
              _ <- update $ ErrorDocument documentid errmsg
              Log.server $ "Sending seal error emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
              runLocalTemplates ctxtemplates $ sendDocumentErrorEmail ctx document
              return ()
        return ()
    -- Pending -> Rejected
    -- main action: sendRejectAuthorEmail
    | oldstatus == Pending && documentstatus == Rejected = do
        _ <- addDocumentRejectStatEvents document
        ctx <- getContext
        customMessage <- getCustomTextField "customtext"
        -- we don't need to forkIO here since we only schedule emails here
        Log.server $ "Sending rejection emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
        sendRejectEmails (fmap BS.toString customMessage) ctx document (fromJust msignalink)
        return ()
    -- Pending -> Canceled
    -- main action: if canceled because of ElegDataMismatch, send out emails
    | oldstatus == Pending &&
        documentstatus == Canceled &&
        isJust documentcancelationreason &&
        isELegDataMismatch (fromJust documentcancelationreason) = do
            _ <- addDocumentCancelStatEvents document
            ctx <- getContext
            Log.server $ "Sending cancelation emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
            sendElegDataMismatchEmails ctx document
            return ()
    --  -> DocumentError
    | DocumentError _msg <- documentstatus = do
        ctx <- getContext
        Log.server $ "Sending error emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
        sendDocumentErrorEmail ctx document
        return ()

    -- transition with no necessary action; do nothing
    -- FIXME: log status change
    | otherwise =
         return ()
    where msignalink = maybe Nothing (getSigLinkFor document) msignalinkid

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
          udoc <- update $ SaveDocumentForUser documentid user signatorylinkid
          return udoc

-- EMAILS

sendElegDataMismatchEmails :: TemplatesMonad m => Context -> Document -> m ()
sendElegDataMismatchEmails ctx document = do
    let signlinks = [sl | sl <- documentsignatorylinks document
                        , isActivatedSignatory (documentcurrentsignorder document) sl
                        , not $ isAuthor sl]
        Just (ELegDataMismatch msg badid _ _ _) = documentcancelationreason document
        badsig = fromJust $ find (\sl -> badid == signatorylinkid sl) (documentsignatorylinks document)
        badname  = BS.toString $ getFullName badsig
        bademail = BS.toString $ getEmail badsig
    forM_ signlinks $ sendDataMismatchEmailSignatory ctx document badid badname msg
    sendDataMismatchEmailAuthor ctx document badname bademail

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

sendDataMismatchEmailAuthor :: TemplatesMonad m => Context -> Document -> String -> String -> m ()
sendDataMismatchEmailAuthor ctx document badname bademail = do
    let authorname = getFullName $ fromJust $ getAuthorSigLink document
        authoremail = getEmail $ fromJust $ getAuthorSigLink document
    mail <- mailMismatchAuthor ctx document (BS.toString authorname) badname bademail
    scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress {fullname = authorname, email = authoremail }]}

{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendDocumentErrorEmail :: TemplatesMonad m => Context -> Document -> m ()
sendDocumentErrorEmail ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendDocumentErrorEmail1 ctx document)

sendDocumentErrorEmailToAuthor :: TemplatesMonad m => Context -> Document -> m ()
sendDocumentErrorEmailToAuthor ctx document = do
  let authorlink = fromJust $ getAuthorSigLink document
  mail <- mailDocumentError ctx document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail 
    { to = [getMailAddress authorlink]
    , from = documentservice document
    }

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendDocumentErrorEmail1 :: TemplatesMonad m => Context -> Document -> SignatoryLink -> m ()
sendDocumentErrorEmail1 ctx document signatorylink = do
  let SignatoryLink { signatorylinkid
                    , signatorydetails } = signatorylink
      Document { documentid } = document
  mail <- mailDocumentError ctx document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        to = [getMailAddress signatorydetails]
      , mailInfo = Invitation documentid  signatorylinkid
      , from = documentservice document
  }

{- |
   Send emails to all of the invited parties.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendInvitationEmails :: TemplatesMonad m => Context -> Document -> m ()
sendInvitationEmails ctx document = do
  Log.debug $ show document
  let signlinks = [sl | sl <- documentsignatorylinks document
                      , isCurrentSignatory (documentcurrentsignorder document) sl
                      , not $ isAuthor sl]
  forM_ signlinks (sendInvitationEmail1 ctx document)

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendInvitationEmail1 :: TemplatesMonad m => Context -> Document -> SignatoryLink -> m ()
sendInvitationEmail1 ctx document signatorylink = do
  let SignatoryLink { signatorylinkid
                    , signatorydetails } = signatorylink
      Document { documentid } = document
      authorsiglink = fromJust $ getAuthorSigLink document
      hasAuthorSigned = isJust $ maybesigninfo authorsiglink
  mail <- case (isSignatory signatorylink, hasAuthorSigned) of
          (True, True)  -> mailInvitation True ctx Sign document (Just signatorylink)
          (True, False) -> mailInvitation True ctx Send document (Just signatorylink)
          (False, _)    -> mailInvitation True ctx View document (Just signatorylink)
  -- ?? Do we need to read in the contents? -EN
  _attachmentcontent <- liftIO $ getFileContents ctx $ head $ documentfiles document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        to = [getMailAddress signatorydetails]
      , mailInfo = Invitation documentid signatorylinkid
      , from = documentservice document
  }

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
sendAwaitingEmail :: TemplatesMonad m => Context -> Document -> m ()
sendAwaitingEmail ctx document = do
  let Just authorsiglink = getAuthorSigLink document
  mail <- mailDocumentAwaitingForAuthor ctx (getFullName authorsiglink) document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress authorsiglink]
                                                  , from = documentservice document  }

makeMailAttachments :: Context -> Document -> IO [(BS.ByteString,BS.ByteString)]
makeMailAttachments ctx document = do
  let mainfile = head $ case documentsealedfiles document of
        [] -> documentfiles document
        fs -> fs
      aattachments = map authorattachmentfile $ documentauthorattachments document
      sattachments = concatMap (maybeToList . signatoryattachmentfile) $ documentsignatoryattachments document
      allfiles = [mainfile] ++ aattachments ++ sattachments
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
    mail <- mailDocumentRejected customMessage ctx (getFullName sl) document signalink
    scheduleEmailSendout (ctxesenforcer ctx) $ mail {   to = [getMailAddress sl]
                                                      , from = documentservice document }

sendMailAPIConfirmEmail :: TemplatesMonad m 
                           => Context -> Document -> m ()
sendMailAPIConfirmEmail ctx document =
  case getAuthorSigLink document of
    Nothing -> error "No author in Document"
    Just authorsl -> do
      mail <-   mailMailAPIConfirm ctx  document authorsl
      Log.debug $ show $ mail { to = [getMailAddress authorsl] }
      scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress authorsl] }







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
  handleAccountRemovalFromSign actionid magichash
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
             -> MagicHash       -- ^ The MagicHash that is in the URL
             -> m KontraLink
signDocument documentid
             signatorylinkid1
             magichash1 = do
  fieldnames <- getAndConcat "fieldname"
  fieldvalues <- getAndConcat "fieldvalue"
  let fields = zip fieldnames fieldvalues
      
  edoc <- signDocumentWithEmail documentid signatorylinkid1 magichash1 fields
  
  case edoc of
    Left (DBActionNotAvailable message) -> do
      addFlash (OperationFailed, message)
      getHomeOrUploadLink
    Left (DBDatabaseNotAvailable message) -> do
      addFlash (OperationFailed, message)
      getHomeOrUploadLink
    Left _ -> mzero
    Right (doc, olddoc) -> do
      postDocumentChangeAction doc olddoc (Just signatorylinkid1)
      handleAfterSigning doc signatorylinkid1

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
          _ <- update $ SaveDocumentForUser documentid user signatorylinkid
          if isClosed document
            then addFlashM $ modalSignedClosedNoAccount document signatorylink actionid magichash
            else addFlashM $ modalSignedNotClosedNoAccount document signatorylink actionid magichash
          return ()
        _ -> return ()
    Just user -> do
     _ <- update $ SaveDocumentForUser documentid user signatorylinkid
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
               -> MagicHash
               -> m KontraLink
rejectDocument documentid
               signatorylinkid1
               magichash = do
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

getDocumentLocale :: MonadIO m => DocumentID -> m (Maybe Locale)
getDocumentLocale documentid = do
  mdoc <- query $ GetDocumentByDocumentID documentid
  return $ fmap getLocale mdoc --TODO: store lang on doc

{- |
   Show the document to be signed
 -}
handleSignShow :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m String
handleSignShow documentid
               signatorylinkid1
               magichash1 = do
  Context { ctxtime
          , ctxipnumber
          , ctxflashmessages } <- getContext
  _ <- markDocumentSeen documentid signatorylinkid1 magichash1 ctxtime ctxipnumber
  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid1 magichash1
  invitedlink <- guardJust $ getSigLinkFor document signatorylinkid1
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
    2. Friend of author in which case they get pageDocumentForViewer
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
            let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
            when (isCanceled document && isJust mMismatchMessage) $
              addFlash (OperationFailed, fromJust mMismatchMessage)
            ctx2 <- getContext -- need to get new context because we may have added flash msg
            step <- getDesignStep (documentid document)
            case (documentstatus document) of
              Preparation -> do
                mattachments <- getDocsByLoggedInUser
                case mattachments of
                  Left _ -> Right <$> pageDocumentDesign ctx2 document step []
                  Right attachments -> Right <$> pageDocumentDesign ctx2 document step (filter isAttachment attachments)
              _ ->  Right <$> pageDocumentForAuthor ctx2 document
          (_, Just invitedlink, _, _) -> Right <$> pageDocumentForSignatory (LinkSignDoc document invitedlink) document ctx invitedlink
          -- friends can just look (but not touch)
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
  guard (isAuthor (document, muser)) -- still need this because friend can read document
  sign              <- isFieldSet "sign"
  send              <- isFieldSet "final"
  template          <- isFieldSet "template"
  contract          <- isFieldSet "contract"
  csvupload         <- isFieldSet "csvupload"
  updateattachments <- isFieldSet "updateattachments"
  switchtoadvanced  <- isFieldSet "changefunctionality"
  sigattachments    <- isFieldSet "sigattachments"
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
    Preparation                     -> handleIssueSave                 document
    AwaitingAuthor                  -> handleIssueSignByAuthor         document
    _ -> return $ LinkContracts

handleIssueSign :: Kontrakcja m => Document -> m KontraLink
handleIssueSign document = do
    Log.debug "handleIssueSign"
    ctx@Context { ctxtime, ctxipnumber} <- getContext
    -- unless (document `allowsIdentification` EmailIdentification) mzero | This need to be refactored | Breaks templates
    mudoc <- updateDocument ctx document
    case mudoc of
        Right udoc-> do
          mdocs <- splitUpDocument udoc
          case mdocs of
            Right docs -> do
              mndocs <- mapM (forIndividual ctxtime ctxipnumber udoc) docs
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
                _ -> mzero
            Left link -> return link
        Left _ -> mzero
    where
      forIndividual ctxtime ctxipnumber udoc doc = do
        mndoc <- authorSignDocument (documentid doc) Nothing
        case mndoc of
          Right newdocument -> do
            markDocumentAuthorReadAndSeen newdocument ctxtime ctxipnumber
            postDocumentChangeAction newdocument udoc Nothing
            return ()
          Left _ -> return ()
        return mndoc

handleIssueSend :: Kontrakcja m => Document -> m KontraLink
handleIssueSend document = do
    Log.debug "handleIssueSend"
    ctx@Context { ctxtime, ctxipnumber} <- getContext
    mudoc <- updateDocument ctx document
    case mudoc of
        Right udoc-> do
          mdocs <- splitUpDocument udoc
          case mdocs of
            Right docs -> do
              mndocs <- mapM (forIndividual ctxtime ctxipnumber udoc) docs
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
                _ -> mzero
            Left link -> return link
        Left _ -> mzero
    where
      forIndividual ctxtime ctxipnumber udoc doc = do
        mndoc <- authorSendDocument (documentid doc) Nothing
        case mndoc of
          Right newdocument -> do
            markDocumentAuthorReadAndSeen newdocument ctxtime ctxipnumber
            postDocumentChangeAction newdocument udoc Nothing
            return ()
          Left _ -> return ()
        return mndoc

markDocumentAuthorReadAndSeen :: Kontrakcja m => Document -> MinutesTime -> Word32 -> m ()
markDocumentAuthorReadAndSeen Document{documentid, documentsignatorylinks} time ipnumber =
  mapM_ mark $ filter isAuthor documentsignatorylinks
  where
    mark SignatoryLink{signatorylinkid, signatorymagichash} = do
      update $ MarkInvitationRead documentid signatorylinkid time
      update $ MarkDocumentSeen documentid signatorylinkid signatorymagichash time ipnumber

handleIssueSaveAsTemplate :: Kontrakcja m => Document -> m KontraLink
handleIssueSaveAsTemplate document = do
  ctx <- getContext
  udoc <- guardRightM $ updateDocument ctx document
  _ndoc <- guardRightM $ update $ TemplateFromDocument $ documentid udoc
  addFlashM flashDocumentTemplateSaved
  return $ LinkTemplates

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
          update $ DocumentFromSignatoryData (documentid udoc) sigindex (item 0) (item 1) (item 2) (item 3) (item 4) (item 5) (drop 6 xs)
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
      ndoc <- guardRightM $ update $ AttachCSVUpload (documentid udoc) csvupload
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

    let existingattachments = map (fileid . authorattachmentfile) (documentauthorattachments udoc)
        idsforremoval = [read $ BS.toString f | (f, r) <- zip attidsnums removeatt
                                              , r] :: [FileID]
    fileinputs <- getDataFnM $ lookInputs "attachment"
    mattachments <- sequence $ map (makeDocumentFromFile Attachment) fileinputs
    -- read in the ids as both FileID and DocumentID
    -- if the FileID exists in the existing author attachments
    -- it's not a DocumentID
    -- otherwise, consider it a DocumentID to add
    let idsforadd = [did | (did, fid) <- [( read $ BS.toString sid
                                          , read $ BS.toString sid) | (sid, r) <- zip attidsnums removeatt
                                                                    , not r]
                         , not $ fid `elem` existingattachments]
                    ++ (map documentid $ catMaybes mattachments) :: [DocumentID]
    ndoc <- guardRightM $ updateDocAuthorAttachments (documentid udoc) idsforadd idsforremoval
    return $ LinkDesignDoc $ DesignStep3 (documentid ndoc) signlast

{- |
    Deals with a switch to the document's functionality.
    This'll also update the user preferences that they would like
    to continue with this functionality by default in the future.
-}
handleIssueChangeFunctionality :: Kontrakcja m => Document -> m KontraLink
handleIssueChangeFunctionality document = do
  guardLoggedIn
  ctx <- getContext
  udoc <- guardRightM $ updateDocument ctx document
  signlast <- isFieldSet "signlast"
  toBasic <- isFieldSet "tobasic"
  toAdvanced <- isFieldSet "toadvanced"
  let newmode = caseOf [(toBasic, Just BasicMode)
                       ,(toAdvanced, Just AdvancedMode)
                       ] Nothing
  SignatoryLink { maybesignatory = Just authorid } <- guardJust $ getAuthorSigLink udoc
  _ <- runDBUpdate $ SetPreferredDesignMode authorid newmode
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
handleIssueSignByAuthor document = do
    unless (document `allowsIdentification` EmailIdentification) mzero
    doc <- guardRightM $ closeDocument (documentid document) Nothing
    postDocumentChangeAction doc document Nothing
    addFlashM flashAuthorSigned
    return $ LinkIssueDoc (documentid document)

{- |
   Show the document with title in the url
   URL: /d/{documentid}/{title}
   Method: GET
 -}
handleIssueShowTitleGet :: Kontrakcja m => DocumentID -> String -> m (Either KontraLink Response)
handleIssueShowTitleGet docid = withAuthorOrFriend docid
    . checkUserTOSGet . handleIssueShowTitleGet' docid

handleIssueShowTitleGetForSignatory :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> String -> m Response
handleIssueShowTitleGetForSignatory docid siglinkid sigmagichash title = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    handleIssueShowTitleGet' docid title

handleIssueShowTitleGet' :: Kontrakcja m => DocumentID -> String -> m Response
handleIssueShowTitleGet' docid _title = do
    ctx <- getContext
    document <- queryOrFail $ GetDocumentByDocumentID docid
    let files = case documentstatus document of
          Closed -> documentsealedfiles document
          _      -> documentfiles document
    when (null files) mzero
    contents <- liftIO $ getFileContents ctx $ head files
    let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
        res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
    return res2

-- | Check if current user is author or friend so he can view the document
withAuthorOrFriend :: Kontrakcja m => DocumentID -> m (Either KontraLink a) -> m (Either KontraLink a)
withAuthorOrFriend docid action = do
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
   document <- guardRightM $ query $ GetDocumentByFileID $ fileid'

   let allfiles = documentsealedfiles document ++ documentfiles document
   case filter (\file -> fileid file == fileid') allfiles of
     [file] -> do
       contents <- liftIO $ getFileContents ctx file
       let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
       let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
       return res2
     _ -> do
       Log.debug $ "We found a document for file id but then there was no such file in there. docid "++
         show (documentid document) ++ ", fileid " ++ show fileid'
       mzero

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

makeAuthorDetails :: [(BS.ByteString, BS.ByteString, FieldPlacement)]
                     -> [(BS.ByteString, SignatoryField)]
                     -> SignatoryDetails
                     -> SignatoryDetails
makeAuthorDetails pls fielddefs sigdetails@SignatoryDetails{signatoryfields = sigfields} =
  sigdetails {
    signatoryfields =
      map f sigfields ++ filterFieldDefsByID fielddefs (BS.fromString "author")
  }
  where
    f sf = case sfType sf of
      EmailFT -> g "email"
      FirstNameFT -> g "fstname"
      LastNameFT -> g "sndname"
      CompanyFT -> g "company"
      PersonalNumberFT -> g "personalnumber"
      CompanyNumberFT -> g "companynumber"
      CustomFT _ _ -> sf
      where
        g ftype = sf { sfPlacements = filterPlacementsByID pls (BS.fromString "author") (BS.fromString ftype) }

asValidDocumentFunctionality :: User -> DocumentFunctionality -> String -> Result DocumentFunctionality
asValidDocumentFunctionality user oldfunc input =
  parseDocFunctionality input
  >>= checkAllowed user oldfunc
  where
    parseDocFunctionality :: String -> Result DocumentFunctionality
    parseDocFunctionality xs
        | xs==(show AdvancedFunctionality) = return AdvancedFunctionality
        | otherwise = return BasicFunctionality
    {- |
        Not much of an implementation yet, but this is a placeholder for somewhere
        to validate that the user is actually allowed to change the document functionality
        in this way.
    -}
    checkAllowed :: User -> DocumentFunctionality -> DocumentFunctionality -> Result DocumentFunctionality
    checkAllowed _ oldfunc1 newfunc1
      | oldfunc1 == newfunc1 = return newfunc1
      | otherwise = return newfunc1 --probably want to check what sort of account the user has here


{- |
   Save a document from data in the post params.

 -}
updateDocument :: Kontrakcja m => Context -> Document -> m (Either String Document)
updateDocument ctx@Context{ ctxtime } document@Document{ documentid, documentfunctionality } = do
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

  currentuser <- maybe mzero return $ ctxmaybeuser ctx
  docfunctionality <- getCriticalField (asValidDocumentFunctionality currentuser documentfunctionality) "docfunctionality"

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

                        -- authornote: we need to store the author info somehow!
  let Just authorsiglink = getAuthorSigLink document
      Just authorid = maybesignatory authorsiglink
      authorcompany = maybecompany authorsiglink
  let authordetails = (makeAuthorDetails placements fielddefs $ signatorydetails authorsiglink) { signatorysignorder = authorsignorder }
  Log.debug $ "set author sign order to " ++ (show authorsignorder)

  let isauthorsig = authorrole == "signatory"
      signatories2 = zip signatories roles2
      authordetails2 = (authordetails, if isauthorsig
                                       then [SignatoryPartner, SignatoryAuthor]
                                       else [SignatoryAuthor],
                                       authorid, authorcompany)
      roles2 = map guessRoles signatoriesroles
      guessRoles x | x == BS.fromString "signatory" = [SignatoryPartner]
                   | otherwise = []
  -- FIXME: tell the user what happened!
  -- when (daystosign<1 || daystosign>99) mzero

  --let emails = zip signatoriesemails
  --              (sequence $ map (query . GetUserByEmail . Email) signatoriesemails)

  -- author is gotten above, no?
  -- Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor documentis

  if docfunctionality == BasicFunctionality
    then do
     --if they are switching to basic we want to lose information
     let basicauthorroles =
           if getValueForProcess document processauthorsend == Just True
             then [SignatoryAuthor]
             else [SignatoryPartner, SignatoryAuthor]
         --basicauthordetails = ((removeFieldsAndPlacements authordetails), basicauthorroles, authoraccount)
         basicauthordetails = ((removeFieldsAndPlacements authordetails), basicauthorroles, authorid, authorcompany)
         basicsignatories = zip
                             (take 1 (map (replaceSignOrder (SignOrder 1) . removeFieldsAndPlacements) signatories)) (repeat [SignatoryPartner])
     Log.debug $ "basic functionality so author roles are " ++ (show basicauthorroles)
     update $ UpdateDocument ctxtime documentid docname
                basicsignatories Nothing invitetext basicauthordetails docallowedidtypes Nothing docfunctionality
    else do
     update $ UpdateDocument ctxtime documentid docname
           signatories2 daystosign invitetext authordetails2 docallowedidtypes mcsvsigindex docfunctionality

getDocumentsForUserByType :: Kontrakcja m => DocumentType -> User -> m [Document]
getDocumentsForUserByType doctype user = do
  mysigdocs <- query $ GetDocumentsBySignatory user
  mydocuments <- if useriscompanyadmin user
                   then do
                     mycompanydocs <- query $ GetDocumentsByCompany user
                     return $ union mysigdocs mycompanydocs
                   else return mysigdocs
  usersICanView <- runDBQuery $ GetUsersByFriendUserID $ userid user
  friends'Documents <- mapM (query . GetDocumentsBySignatory) usersICanView
  
  return . filter ((\d -> documenttype d == doctype)) $ nub $
          mydocuments ++ concat friends'Documents

{- |
   Constructs a list of documents (Arkiv) to show to the user.
 -}
showContractsList :: Kontrakcja m => m (Either KontraLink String)
showContractsList = someArchivePage pageContractsList 

showOfferList :: Kontrakcja m => m (Either KontraLink String)
showOfferList = someArchivePage pageOffersList 

showOrdersList :: Kontrakcja m => m (Either KontraLink String)
showOrdersList = someArchivePage pageOrdersList 

showTemplatesList :: Kontrakcja m => m (Either KontraLink String)
showTemplatesList = someArchivePage pageTemplatesList 

showAttachmentList :: Kontrakcja m => m (Either KontraLink String)
showAttachmentList = someArchivePage pageAttachmentList
  
showRubbishBinList :: Kontrakcja m => m (Either KontraLink String)
showRubbishBinList = someArchivePage pageRubbishBinList 

{- |
    Helper function for showing lists of documents.
-}
someArchivePage :: (Kontrakcja m, TemplatesMonad m) => (User -> m String) -> m (Either KontraLink String)
someArchivePage page = checkUserTOSGet $ do
    user <- fromJust <$> ctxmaybeuser <$> getContext
    page user 
    
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
  let allfiles = (documentfiles doc) ++ (documentsealedfiles doc) ++ (authorattachmentfile <$> documentauthorattachments doc)
  case (find (((==) fid) . fileid) $ allfiles) of
    Nothing -> return $ JSObject $ toJSObject [("error",JSString $ toJSString "No file found")]
    Just file  -> do
      jpages <- maybeScheduleRendering file did
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
  liftIO $ print "Request for docs"
  edoc <- case mtokens of
    Nothing         -> getDocByDocID documentid
    Just (slid, mh) -> getDocByDocIDSigLinkIDAndMagicHash documentid slid mh
  case edoc of
    Left l -> do
      liftIO $ print ("Could not get Document" ++ show l)
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
         [] -> notFound $ toResponse "temporary unavailable (document has no files)"
         f  -> do
             b <- mapM (\file -> maybeScheduleRendering file documentid) f
             if any pending b
                then notFound (toResponse "temporary unavailable (document has files pending for process)")
                else do
                    pages <- Doc.DocView.showFilesImages2 documentid mtokens $ zip f b
                    simpleResponse pages

handleDocumentUpload :: Kontrakcja m => DocumentID -> BS.ByteString -> BS.ByteString -> m ()
handleDocumentUpload docid content1 filename = do
  Log.debug $ "Uploading file for doc " ++ show docid
  Context{ctxdocstore, ctxs3action} <- getContext
  fileresult <- attachFile docid filename content1
  case fileresult of
    Left err -> do
      Log.debug $ "Got an error in handleDocumentUpload: " ++ show err
      return ()
    Right document -> do
        _ <- liftIO $ forkIO $ mapM_ (AWS.uploadFile ctxdocstore ctxs3action) (documentfiles document)
        return ()
  return ()

handleDocumentUploadNoLogin :: Kontrakcja m => DocumentID -> BS.ByteString -> BS.ByteString -> m ()
handleDocumentUploadNoLogin docid content1 filename = do
  Log.debug $ "Uploading file for doc " ++ show docid
  Context{ctxdocstore, ctxs3action} <- getContext
  ctx <- getContext
  content14 <- liftIO $ preprocessPDF ctx content1 docid
  fileresult <- update (AttachFile docid filename content14)
  case fileresult of
    Left err -> do
      Log.debug $ "Got an error in handleDocumentUpload: " ++ show err
      return ()
    Right document -> do
        _ <- liftIO $ forkIO $ mapM_ (AWS.uploadFile ctxdocstore ctxs3action) (documentfiles document)
        return ()
  return ()


basename :: String -> String
basename filename =
    case break (\x -> (x=='\\') || (x=='/')) filename of
      (_,(_:rest)) -> basename rest
      _ -> takeWhile ((/=) '.') filename

handleIssueNewDocument :: Kontrakcja m => m KontraLink
handleIssueNewDocument = withUserPost $ do
    input <- getDataFnM (lookInput "doc")
    mdocprocess <- getDocProcess
    let docprocess = fromMaybe (Contract) mdocprocess
    mdoc <- makeDocumentFromFile (Signable docprocess) input
    liftIO $ print mdoc
    case mdoc of
      Nothing -> return LinkUpload
      Just doc -> do
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
    guardLoggedIn
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
    if BSL.null content
      then do
        Log.debug "No content"
        return Nothing
      else do
          Log.debug "Got the content, creating document"
          let title = BS.fromString (basename filename)
          doc <- guardRightM $ newDocument title doctype
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ Just doc
makeDocumentFromFile _ _ = mzero -- to complete the patterns

handleContractArchive :: Kontrakcja m => m KontraLink
handleContractArchive = do
    _ <- handleSignableArchive (Signable Contract)
    return $ LinkContracts 

handleOffersArchive :: Kontrakcja m => m KontraLink
handleOffersArchive =  do
    _ <- handleSignableArchive (Signable Offer)
    return $ LinkOffers 

handleOrdersArchive :: Kontrakcja m => m KontraLink
handleOrdersArchive =  do
    _ <- handleSignableArchive (Signable Order)
    return $ LinkOrders 

handleSignableArchive :: Kontrakcja m => DocumentType -> m ()
handleSignableArchive doctype =  do
    handleIssueArchive
    addFlashM $ flashMessageSignableArchiveDone doctype
    return ()

handleTemplateArchive :: Kontrakcja m => m KontraLink
handleTemplateArchive = do
    handleIssueArchive
    addFlashM flashMessageTemplateArchiveDone
    return $ LinkTemplates 

handleAttachmentArchive :: Kontrakcja m => m KontraLink
handleAttachmentArchive = do
    handleIssueArchive
    addFlashM flashMessageAttachmentArchiveDone
    return $ LinkAttachments 

handleIssueArchive :: Kontrakcja m => m ()
handleIssueArchive = do
    Context { ctxmaybeuser = Just user } <- getContext
    docids <- getCriticalFieldList asValidDocID "doccheck"
    res <- update . ArchiveDocuments user $ map DocumentID docids
    case res of
      Left msg -> do
        Log.debug $ "Failed to delete docs " ++ (show docids) ++ " : " ++ msg
        mzero
      Right _ -> return ()
      
handleRubbishRestore :: Kontrakcja m => m KontraLink
handleRubbishRestore = do
  Context { ctxmaybeuser = Just user } <- getContext
  docids <- getCriticalFieldList asValidDocID "doccheck"
  _ <- guardRightM . update . RestoreArchivedDocuments user $ map DocumentID docids
  addFlashM flashMessageRubbishRestoreDone
  return $ LinkRubbishBin 
 
handleRubbishReallyDelete :: Kontrakcja m => m KontraLink
handleRubbishReallyDelete = do
  Context { ctxmaybeuser = Just user } <- getContext
  docids <- getCriticalFieldList asValidDocID "doccheck"
  idsAndUsers <- mapM (lookupUsersRelevantToDoc . DocumentID) docids
  _ <- guardRightM . update . ReallyDeleteDocuments user $ idsAndUsers
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
  newname <- getCriticalField (return . BS.fromString) "docname"
  doc <- guardRightM $ update $ SetDocumentTitle docid newname
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
showPage docid fileid = withAuthorOrFriend docid
    . checkUserTOSGet . showPage' fileid

showPageForSignatory :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> FileID -> Int -> m Response
showPageForSignatory docid siglinkid sigmagichash fileid pageno = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    showPage' fileid pageno

showPage' :: Kontrakcja m => FileID -> Int -> m Response
showPage' fileid pageno = do
  Context{ctxnormalizeddocuments} <- getContext
  modminutes <- query $ FileModTime fileid
  docmap <- liftIO $ readMVar ctxnormalizeddocuments
  case Map.lookup fileid docmap of
    Just (JpegPages pages) -> do
      let (contents,_,_) =  pages !! (pageno - 1)
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
      let modtime = toUTCTime modminutes
      rq <- askRq                 -- FIXME: what?
      return $ ifModifiedSince modtime rq res2
    _ -> mzero

handleCancel :: Kontrakcja m => DocumentID -> m KontraLink
handleCancel docid = withUserPost $ do
  doc <- guardRightM $ getDocByDocID docid
  ctx@Context { ctxtime, ctxipnumber } <- getContext
  if isPending doc || isAwaitingAuthor doc
    then do
    customMessage <- getCustomTextField "customtext"
    mdoc' <- update $ CancelDocument (documentid doc) ManualCancel ctxtime ctxipnumber
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
  mdoc <- query $ GetDocumentByDocumentID docid
  case (mdoc) of
    Just doc -> withDocumentAutho doc $ do
                          update $ WithdrawnDocument $ documentid doc
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
          mnewdoc <- update $ ChangeSignatoryEmailWhenUndelivered docid slid muser email
          case mnewdoc of
            Right newdoc -> do
              -- get (updated) siglink from updated document
              sl <- guardJust (getSigLinkFor newdoc slid)
              sendInvitationEmail1 ctx newdoc sl
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

mainPage :: Kontrakcja m => m String
mainPage =  do
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
  Context { ctxmaybeuser } <- getContext
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
                      update $ SignableFromDocumentIDWithUpdatedAuthor user mcompany did
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
  fid == fileid (authorattachmentfile attachment)

{- |
   The FileID matches the SignatoryAttachment.
-}
sigAttachmentHasFileID :: FileID -> SignatoryAttachment -> Bool
sigAttachmentHasFileID fid attachment =
  maybe False ((fid ==) . fileid) (signatoryattachmentfile attachment)

{- |
   Download the attachment with the given fileid
 -}
handleAttachmentDownloadForAuthor :: Kontrakcja m => DocumentID -> FileID -> m Response
handleAttachmentDownloadForAuthor did fid = do
  doc <- guardRightM $ getDocByDocID did
  case find (authorAttachmentHasFileID fid) (documentauthorattachments doc) of
    Just AuthorAttachment{ authorattachmentfile } -> do
      ctx <- getContext
      contents <- liftIO $ getFileContents ctx authorattachmentfile
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
          res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
      return res2
    Nothing -> case find (sigAttachmentHasFileID fid) (documentsignatoryattachments doc) of
      Just SignatoryAttachment{ signatoryattachmentfile = Just file } -> do
        ctx <- getContext
        contents <- liftIO $ getFileContents ctx file
        let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
            res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
        return res2
      _ -> mzero -- attachment with this file ID does not exist

{- |
   Stream the pdf document for the given FileID.
 -}
handleAttachmentDownloadForViewer :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> FileID -> m Response
handleAttachmentDownloadForViewer did sid mh fid = do
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash did sid mh
  ctx <- getContext
  case find (authorAttachmentHasFileID fid) (documentauthorattachments doc) of
      Just AuthorAttachment{ authorattachmentfile } ->
        respondWithPDF =<< liftIO (getFileContents ctx authorattachmentfile)
      Nothing -> case find (sigAttachmentHasFileID fid) (documentsignatoryattachments doc) of
        Just SignatoryAttachment{ signatoryattachmentfile = Just file } ->
          respondWithPDF =<< liftIO (getFileContents ctx file)
        _ -> mzero -- attachment with this file ID does not exist
        
        
handleDownloadFileLogged  :: Kontrakcja m => DocumentID -> FileID -> String -> m Response
handleDownloadFileLogged did fid _nameForBrowser = do
  doc <- guardRightM $ getDocByDocID did
  respondWithFile doc fid 

handleDownloadFileNotLogged  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> FileID -> String -> m Response
handleDownloadFileNotLogged did sid mh fid _nameForBrowser= do
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash did sid mh      
  respondWithFile doc fid 

  
respondWithFile :: Kontrakcja m =>  Document -> FileID -> m Response  
respondWithFile doc fid =  do
    ctx <- getContext
    case find (authorAttachmentHasFileID fid) (documentauthorattachments doc) of
      Just AuthorAttachment{ authorattachmentfile } ->
        respondWithPDF =<< liftIO (getFileContents ctx authorattachmentfile)
      Nothing -> case find (sigAttachmentHasFileID fid) (documentsignatoryattachments doc) of
        Just SignatoryAttachment{ signatoryattachmentfile = Just file } ->
          respondWithPDF =<< liftIO (getFileContents ctx file)
        _ -> case find (((==) fid) . fileid) (documentfiles doc ++ documentsealedfiles doc) of
           Just file ->   respondWithPDF =<< liftIO (getFileContents ctx file)
           Nothing   -> mzero
  
        
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
    mdoc <- query $ GetDocumentByDocumentID docid
    case (mdoc) of
       Nothing -> return LoopBack
       Just doc -> if (isBroken doc)
                    then do
                        _ <- liftIO $ sealDocument ctx doc
                        return LoopBack
                    else return LoopBack

showDocumentsToFix :: Kontrakcja m => m String
showDocumentsToFix = onlySuperUser $ do
    docs <- query $ GetDocuments Nothing
    documentsToFixView $ filter isBroken docs

handleDeleteSigAttach :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
handleDeleteSigAttach docid siglinkid mh = do
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  siglink <- guardJust $ getSigLinkFor doc siglinkid
  fid <- (read . BS.toString) <$> getCriticalField asValidID "deletesigattachment"
  let email = getEmail siglink
  Log.debug $ "delete Sig attachment " ++ (show fid) ++ "  " ++ (BS.toString email)
  _ <- update $ DeleteSigAttachment docid email fid
  return $ LinkSignDoc doc siglink  

handleSigAttach :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
handleSigAttach docid siglinkid mh = do
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
  _ <- update $ SaveSigAttachment docid attachname email content
  return $ LinkSignDoc doc siglink


jsonDocumentsList ::  Kontrakcja m => m JSValue
jsonDocumentsList = do
    Just user <- ctxmaybeuser <$> getContext
    lang <- getLang <$> getContext
    doctype <- getFieldWithDefault "" "documentType"
    allDocs <- case (doctype) of
        "Contract" -> getDocumentsForUserByType (Signable Contract) user
        "Offer" -> getDocumentsForUserByType (Signable Offer) user
        "Order" -> getDocumentsForUserByType (Signable Order) user
        "Template" -> do
            mydocuments <- query $ GetDocumentsByAuthor (userid user)
            return $ filter isTemplate mydocuments 
        "Attachment" -> do
            mydocuments <- query $ GetDocumentsByAuthor (userid user)
            return $ filter ((==) Attachment . documenttype) mydocuments
        "Rubbish" -> do
            if useriscompanyadmin user
                then query $ GetDeletedDocumentsByCompany user
                else query $ GetDeletedDocumentsByUser user
        "Template|Contract" -> do
            let tfilter doc = (Template Contract == documenttype doc)
            userdocs <- liftIO $ query $ GetDocumentsByAuthor (userid user)
            shareddocs <- liftIO $ query $ GetDocumentsSharedInCompany user
            return $ nub . filter tfilter $ userdocs ++ shareddocs
        "Template|Offer" -> do
            let tfilter doc = (Template Offer == documenttype doc)
            userdocs <- liftIO $ query $ GetDocumentsByAuthor (userid user)
            shareddocs <- liftIO $ query $ GetDocumentsSharedInCompany user
            return $ nub . filter tfilter $ userdocs ++ shareddocs
        "Template|Order" -> do
            let tfilter doc = (Template Order == documenttype doc)
            userdocs <- liftIO $ query $ GetDocumentsByAuthor (userid user)
            shareddocs <- liftIO $ query $ GetDocumentsSharedInCompany user
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
                   mdoc <- query $ GetDocumentByDocumentID did
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
  docs <- query $ GetDocuments Nothing
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
    mdoc <- query $ GetDocumentByDocumentID docid
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
               Just sl -> mailRejectMailContent Nothing ctx  BS.empty doc sl
               Nothing -> return ""
         _ -> return ""
    return $ JSObject $ toJSObject [("content",JSString $ toJSString $ content)]
    

handleCSVLandpage :: Kontrakcja m => Int -> m String
handleCSVLandpage c = do
  text <- csvLandPage c
  return text
