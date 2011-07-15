{- |
   DocControl represents the controler (in MVC) of the document.
 -}
module Doc.DocControl where

import ActionSchedulerState
import AppView
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
import InputValidation
import Kontra
import KontraLink
import ListUtil
import Mails.SendMail
import MinutesTime
import Misc
import Redirect
import User.UserControl
import Util.HasSomeUserInfo
import qualified Amazon as AWS
import qualified AppLogger as Log
import Templates.Templates
import Templates.LocalTemplates
import Util.FlashUtil
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Util.MonadUtils

import Codec.Text.IConv
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.CSV
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Data.Word
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update, query)
import Text.ParserCombinators.Parsec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map

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
        ctx <- getContext
        -- we don't need to forkIO here since we only schedule emails here
        Log.server $ "Sending invitation emails for document #" ++ show documentid ++ ": " ++ BS.toString documenttitle
        sendInvitationEmails ctx document
        return ()
    -- Preparation -> Closed (only author signs)
    -- main action: sealDocument and sendClosedEmails
    | oldstatus == Preparation && documentstatus == Closed = do
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
  mail <- if isSignatory signatorylink
          then if hasAuthorSigned
               then mailInvitationToSign ctx document signatorylink
               else mailInvitationToSend ctx document signatorylink
          else mailInvitationToView ctx document signatorylink
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
      return $ LinkMain
    Left (DBDatabaseNotAvailable message) -> do
      addFlash (OperationFailed, message)
      return $ LinkMain
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
handleAfterSigning document@Document{documentid,documenttitle} signatorylinkid = do
  ctx <- getContext
  signatorylink <- guardJust $ getSigLinkFor document signatorylinkid
  maybeuser <- query $ GetUserByEmail (currentServiceID ctx) (Email $ getEmail signatorylink)
  case maybeuser of
    Nothing -> do
      let details = signatorydetails signatorylink
          fullname = (signatoryfstname details, signatorysndname details)
          email = signatoryemail details
          company = signatorycompany details
      muser <- liftIO $ createUserBySigning ctx documenttitle fullname email company (documentid, signatorylinkid)
      case muser of
        Just (user, actionid, magichash) -> do
          _ <- update $ SaveDocumentForSignedUser documentid (getSignatoryAccount user) signatorylinkid
          if isClosed document
            then addFlashM $ modalSignedClosedNoAccount document signatorylink actionid magichash
            else addFlashM $ modalSignedNotClosedNoAccount document signatorylink actionid magichash
          return ()
        _ -> return ()
    Just user -> do
     _ <- update $ SaveDocumentForSignedUser documentid (getSignatoryAccount user) signatorylinkid
     if isClosed document
       then addFlashM $ modalSignedClosedHasAccount document signatorylink (isJust $ ctxmaybeuser ctx)
       else addFlashM $ modalSignedNotClosedHasAccount document signatorylink (isJust $ ctxmaybeuser ctx)
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
      return $ LinkMain
    Left (DBDatabaseNotAvailable message) -> do
      addFlash (OperationFailed, message)
      return $ LinkMain
    Left _ -> mzero
    Right (document, olddocument) -> do
      postDocumentChangeAction document olddocument (Just signatorylinkid1)
      addFlashM $ modalRejectedView document
      return $ LoopBack

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
        -- authors get a view with buttons
        case (isAuthor (document, ctxmaybeuser), isAttachment document, documentstatus document) of
          (True, True, Preparation) -> Right <$> pageAttachmentDesign document
          (_, True, _) -> Right <$> pageAttachmentView document
          (True, _, _) -> do
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
          -- friends can just look (but not touch)
          (False, _, _) -> Right <$> pageDocumentForViewer ctx document Nothing

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
    _ -> return $ LinkContracts emptyListParams

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
                    addFlashM $ flashMessageCSVSent $ length ds
                    Log.debug (show $ map documenttype ds)
                    case documenttype (head ds) of
                      Signable Contract -> return $ LinkContracts emptyListParams
                      Signable Offer    -> return $ LinkOffers emptyListParams                      
                      Signable Order    -> return $ LinkOrders emptyListParams
                      _                 -> return $ LinkMain
                _ -> mzero
            Left link -> return link
        Left _ -> mzero
    where
      forIndividual ctxtime ctxipnumber udoc doc = do
        mndoc <- update $ AuthorSignDocument (documentid doc) ctxtime ctxipnumber Nothing
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
                    addFlashM $ flashMessageCSVSent $ length ds
                    Log.debug (show $ map documenttype ds)
                    case documenttype (head ds) of
                      Signable Contract -> return $ LinkContracts emptyListParams
                      Signable Offer    -> return $ LinkOffers emptyListParams                      
                      Signable Order    -> return $ LinkOrders emptyListParams
                      _ -> return $ LinkMain
                _ -> mzero
            Left link -> return link
        Left _ -> mzero
    where
      forIndividual ctxtime ctxipnumber udoc doc = do
        mndoc <- update $ AuthorSendDocument (documentid doc) ctxtime ctxipnumber Nothing
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
  return $ LinkTemplates emptyListParams

-- TODO | I belive this is dead. Some time ago if you were editing template you could create contract from it.
--        But this probably is gone now.
handleIssueChangeToContract :: Kontrakcja m => Document -> m KontraLink
handleIssueChangeToContract document = do
  ctx <- getContext
  signlast <- isFieldSet "signlast"
  guard (isJust $ ctxmaybeuser ctx)
  contract <- guardRightM $ update $ SignableFromDocumentIDWithUpdatedAuthor (fromJust $ ctxmaybeuser ctx) (documentid document)
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
          udoc <- guardRightM $ if (isTemplate doc)
                                 then return $ Right doc
                                 else update $ TemplateFromDocument $ documentid doc
          mdocs <- mapM (createDocFromRow udoc (csvsignatoryindex csvupload)) csvbody
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

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
    where (w, s'') = break (== c) s'

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
  ndoc <- guardRightM $ update $ UpdateSigAttachments (documentid udoc) sigattachments
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
    ndoc <- guardRightM $ update $ UpdateDocumentAttachments (documentid udoc) idsforadd idsforremoval
    return $ LinkDesignDoc $ DesignStep3 (documentid ndoc) signlast

{- |
    Deals with a switch to the document's functionality.
    This'll also update the user preferences that they would like
    to continue with this functionality by default in the future.
-}
handleIssueChangeFunctionality :: Kontrakcja m => Document -> m KontraLink
handleIssueChangeFunctionality document = do
  ctx <- getContext
  udoc <- guardRightM $ updateDocument ctx document
  signlast <- isFieldSet "signlast"
  SignatoryLink { maybesignatory = Just authorid } <- guardJust $ getAuthorSigLink udoc
  _user <- guardRightM $ handlePreferenceChange authorid
  return $ LinkDesignDoc $ DesignStep2 (documentid udoc) Nothing Nothing signlast

  where
    handlePreferenceChange :: Kontrakcja m => UserID -> m (Either String User)
    handlePreferenceChange userid = do
      toBasic <- isFieldSet "tobasic"
      toAdvanced <- isFieldSet "toadvanced"
      case (toBasic, toAdvanced) of
        (True, _) -> setPreferredMode $ Just BasicMode
        (_, True) -> setPreferredMode $ Just AdvancedMode
        _ -> setPreferredMode Nothing
      where
        setPreferredMode :: Kontrakcja m => Maybe DesignMode -> m (Either String User)
        setPreferredMode designmode = update $ SetPreferredDesignMode userid designmode

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
                  --there's a bug in the Data.CSV library I think, it wants a newline at the end of everything!
                  mcontents = fmap (filter (\r->(not $ isEmptyRow r))) . parse csvFile "" . (++"\n") . decodeByteString $ content
              case mcontents of
                 Left _ -> return $ Bad flashMessageFailedToParseCSV
                 Right contents
                   | length contents > rowlimit -> return $ Bad $ flashMessageCSVHasTooManyRows rowlimit
                   | otherwise -> return $ Good (title, map (map BS.fromString) contents)
        _ -> return Empty
    rowlimit :: Int = 500
    isEmptyRow [] = True
    isEmptyRow [""] = True
    isEmptyRow _ = False
    {- |
        Excel especially will chuck out data in funky char encodings
        so we're going to look to see if some alternative ones "work better"
        than UTF-8.  Otherwise we'll use UTF-8.  The problem is determining
        which "works better" because they will normally all decode without an error,
        it's just it'll be a load of rubbish for a human.
    -}
    decodeByteString :: BSL.ByteString -> String
    decodeByteString bs =
      guessBest . map  (BS.toString . concatChunks) . lefts $ (Left bs) : map (\enc -> convertStrictly enc "UTF-8" bs) alternativeEncodings
    {- |
        I picked these because these seem to be what Excel 2007 is outputting on my Windows machine if you choose to Save As ...
         CSV (Comma delimited) -> ISO8859-1
         CSV (MS-DOS) -> CP437
         CSV (Macintosh) -> MAC
        The MAC encoding seemed to cover the files Viktor sent me from his Mac too.
    -}
    alternativeEncodings = ["ISO8859-1","CP437","MAC"]
    {- |
        Guesses the best string by looking at it, there's not much else you can do really.
        This goes for the one with the most nordic chars in.  This also goes for things
        earlier in the list over those later in the list, because of the way maximumBy works.
    -}
    guessBest :: [String] -> String
    guessBest = maximumBy nordicCharCountOrdering
    nordicCharCountOrdering :: String -> String -> Ordering
    nordicCharCountOrdering a b = compare (nordicCharCount a) (nordicCharCount b)
    nordicCharCount = length . filter (\c -> c `elem` "äÄöÖåÅ")

handleIssueSave :: Kontrakcja m => Document -> m KontraLink
handleIssueSave document = do
    ctx <- getContext
    _ <- updateDocument ctx document
    if (isTemplate document)
     then do
          addFlashM flashDocumentTemplateSaved
          return $ LinkTemplates emptyListParams
     else do
          addFlashM flashDocumentDraftSaved
          return $ LinkContracts emptyListParams

handleIssueSignByAuthor :: Kontrakcja m => Document -> m KontraLink
handleIssueSignByAuthor document = do
    Context{ctxtime, ctxipnumber} <- getContext
    unless (document `allowsIdentification` EmailIdentification) mzero
    doc2 <- update $ CloseDocument (documentid document) ctxtime ctxipnumber  Nothing
    case doc2 of
        Nothing -> return $ LinkIssueDoc (documentid document)
        Just d -> do
            postDocumentChangeAction d document Nothing
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
    let file = safehead "handleIssueShow" (case documentstatus document of
                                                Closed -> documentsealedfiles document
                                                _      -> documentfiles document)
    contents <- liftIO $ getFileContents ctx file
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
                    -> (BS.ByteString, FieldDefinition)
fieldDefAndSigID placements fn fv fid sigid = (sigid,
                                    FieldDefinition { fieldlabel = fn,
                                                        fieldvalue = fv,
                                                        fieldplacements = filterPlacementsByID placements sigid fid,
                                                        fieldfilledbyauthor = (BS.length fv > 0)
                                                    })

makeFieldDefs :: [(BS.ByteString, BS.ByteString, FieldPlacement)]
              -> [BS.ByteString]
              -> [BS.ByteString]
              -> [BS.ByteString]
              -> [BS.ByteString]
              -> [(BS.ByteString, FieldDefinition)]
makeFieldDefs placements = zipWith4 (fieldDefAndSigID placements)

filterFieldDefsByID :: [(BS.ByteString, FieldDefinition)]
                    -> BS.ByteString
                    -> [FieldDefinition]
filterFieldDefsByID fielddefs sigid =
    [x | (s, x) <- fielddefs, s == sigid]

makeSignatoryNoPlacements :: BS.ByteString
                             -> BS.ByteString
                             -> BS.ByteString
                             -> SignOrder
                             -> BS.ByteString
                             -> BS.ByteString
                             -> BS.ByteString
                             -> SignatoryDetails
makeSignatoryNoPlacements sfn ssn se sso sc spn scn =
    SignatoryDetails { signatoryfstname = sfn
                     , signatorysndname = ssn
                     , signatorycompany = sc
                     , signatorypersonalnumber = spn
                     , signatorycompanynumber = scn
                     , signatoryemail = se
                     , signatorysignorder = sso
                     , signatoryfstnameplacements = []
                     , signatorysndnameplacements = []
                     , signatorycompanyplacements = []
                     , signatoryemailplacements = []
                     , signatorypersonalnumberplacements = []
                     , signatorycompanynumberplacements = []
                     , signatoryotherfields = []
                     }

makeSignatory ::[(BS.ByteString, BS.ByteString, FieldPlacement)]
                -> [(BS.ByteString, FieldDefinition)]
                -> BS.ByteString
                -> BS.ByteString
                -> BS.ByteString
                -> BS.ByteString
                -> SignOrder
                -> BS.ByteString
                -> BS.ByteString
                -> BS.ByteString
                -> SignatoryDetails
makeSignatory pls fds sid sfn  ssn  se  sso  sc  spn  scn =
    (makeSignatoryNoPlacements sfn ssn se sso sc spn scn)
    { signatoryfstnameplacements        = filterPlacementsByID pls sid (BS.fromString "fstname")
    , signatorysndnameplacements        = filterPlacementsByID pls sid (BS.fromString "sndname")
    , signatorycompanyplacements        = filterPlacementsByID pls sid (BS.fromString "company")
    , signatoryemailplacements          = filterPlacementsByID pls sid (BS.fromString "email")
    , signatorypersonalnumberplacements = filterPlacementsByID pls sid (BS.fromString "personalnumber")
    , signatorycompanynumberplacements  = filterPlacementsByID pls sid (BS.fromString "companynumber")
    , signatoryotherfields              = filterFieldDefsByID  fds sid
    }

makeSignatories :: [(BS.ByteString, BS.ByteString, FieldPlacement)]
                   -> [(BS.ByteString, FieldDefinition)]
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
    | sigids == [] = zipWith7 makeSignatoryNoPlacements
                        signatoriesfstnames
                        signatoriessndnames
                        signatoriesemails
                        signatoriessignorders
                        signatoriescompanies
                        signatoriespersonalnumbers
                        signatoriescompanynumbers
    | otherwise    = zipWith8 (makeSignatory placements fielddefs)
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
                     -> [(BS.ByteString, FieldDefinition)]
                     -> SignatoryDetails
                     -> SignatoryDetails
makeAuthorDetails pls fielddefs authorsigdetails =
  authorsigdetails
    { signatoryemailplacements          = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "email")
    , signatoryfstnameplacements        = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "fstname")
    , signatorysndnameplacements        = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "sndname")
    , signatorycompanyplacements        = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "company")
    , signatorypersonalnumberplacements = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "personalnumber")
    , signatorycompanynumberplacements  = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "companynumber")
    , signatoryotherfields = filterFieldDefsByID fielddefs (BS.fromString "author")
    }

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
  authorsignorder <- (SignOrder . fromIntegral . fromMaybe 1) <$> getValidateAndHandle asValidNumber asMaybe "authorsignorder"

  currentuser <- maybe mzero return $ ctxmaybeuser ctx
  docfunctionality <- getCriticalField (asValidDocumentFunctionality currentuser documentfunctionality) "docfunctionality"

  validmethods <- getAndConcat "validationmethod"

  let docallowedidtypes = mapJust (idmethodFromString . BS.toString) validmethods

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
      authoraccount = getSignatoryAccount authorsiglink
  let authordetails = (makeAuthorDetails placements fielddefs $ signatorydetails authorsiglink) { signatorysignorder = authorsignorder }
  Log.debug $ "set author sign order to " ++ (show authorsignorder)

  let isauthorsig = authorrole == "signatory"
      signatories2 = zip signatories roles2
      authordetails2 = (authordetails, if isauthorsig
                                       then [SignatoryPartner, SignatoryAuthor]
                                       else [SignatoryAuthor],
                                       authoraccount)
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
     let basicauthordetails = ((removeFieldsAndPlacements authordetails), [SignatoryPartner, SignatoryAuthor], authoraccount)
         basicsignatories = zip
                             (take 1 (map (replaceSignOrder (SignOrder 1) . removeFieldsAndPlacements) signatories)) (repeat [SignatoryPartner])
     update $ UpdateDocument ctxtime documentid docname
                basicsignatories Nothing invitetext basicauthordetails docallowedidtypes Nothing docfunctionality
    else do
     update $ UpdateDocument ctxtime documentid docname
           signatories2 daystosign invitetext authordetails2 docallowedidtypes mcsvsigindex docfunctionality

{- |
    This stuff is deeply messed up.  At the moment maybesignatory and maybesupervisor aren't populated
    until a signatory signs.  This means that to make docs available to signatories, viewers or supervisors
    until a sign happens (which is never in the case of viewers!) is to lookup by email.  We need to change
    so we lookup not by email, but by userid only.  This means linking up and saving the docs far earlier for users.
-}
getDocumentsForUserByType :: Kontrakcja m => DocumentType -> User -> m [Document]
getDocumentsForUserByType doctype user = do
  mydocuments <- query $ GetDocumentsByUser user --docs saved for user, so not included if yet to sign or viewer
  usersICanView <- query $ GetUsersByFriendUserID $ userid user
  usersISupervise <- query $ GetUserSubaccounts $ userid user
  friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
  supervised'Documents <- query $ GetDocumentsBySupervisor user --supervised docs saved for user (required if subaccount is deleted), again just saved ones
  moresupervised'Documents <- mapM (query . GetDocumentsByUser) usersISupervise --all supervised docs for undeleted subaccounts
  return . filter ((\d -> documenttype d == doctype)) $ nub $
          mydocuments ++ concat friends'Documents ++ supervised'Documents ++ concat moresupervised'Documents

{- |
   Constructs a list of documents (Arkiv) to show to the user.
   The list contains all documents the user is an author on or
   is a friend of the author.
   Duplicates are removed.
 -}
showContractsList :: Kontrakcja m => m (Either KontraLink String)
showContractsList =
  showItemList' pageContractsList $ getDocumentsForUserByType (Signable Contract)

showOfferList :: Kontrakcja m => m (Either KontraLink String)
showOfferList =
  showItemList' pageOffersList $ getDocumentsForUserByType (Signable Offer)

showOrdersList :: Kontrakcja m => m (Either KontraLink String)
showOrdersList =
  showItemList' pageOrdersList $ getDocumentsForUserByType (Signable Order)

showTemplatesList :: Kontrakcja m => m (Either KontraLink String)
showTemplatesList =
  let userTemplates user = do
        mydocuments <- query $ GetDocumentsByAuthor (userid user)
        return $ filter isTemplate mydocuments in
  showItemList' pageTemplatesList userTemplates

showAttachmentList :: Kontrakcja m => m (Either KontraLink String)
showAttachmentList =
  let getAttachments user = do
        mydocuments <- query $ GetDocumentsByAuthor (userid user)
        return $ filter ((==) Attachment . documenttype) mydocuments in
  showItemList' pageAttachmentList getAttachments

{- |
    Helper function for showing lists of documents.
-}
showItemList' :: Kontrakcja m
    => (TemplatesMonad m => MinutesTime -> User -> PagedList Document -> m String)
    -> (User -> m [Document])
    -> m (Either KontraLink String)
showItemList' viewPage getDocs = checkUserTOSGet $ do
  Context {ctxmaybeuser = Just user, ctxtime} <- getContext
  docs <- getDocs user
  params <- getListParams
  viewPage ctxtime user (docSortSearchPage params $ prepareDocsForList docs)

handleAttachmentViewForViewer :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m Response
handleAttachmentViewForViewer docid siglinkid mh = do
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  ctx <- getContext
  let pending JpegPagesPending = True
      pending _                = False
      files                    = map authorattachmentfile (documentauthorattachments doc)
  case files of
    [] -> return $ toResponse ""
    f  -> do
      b <- mapM (\file -> liftIO $ maybeScheduleRendering ctx file (documentid doc)) f
      if any pending b
        then notFound (toResponse "temporary unavailable (document has files pending for process)")
        else do
        pages <- Doc.DocView.showFilesImages2 (documentid doc) Nothing $ zip f b
        simpleResponse pages

handleAttachmentViewForAuthor :: Kontrakcja m => DocumentID -> m Response
handleAttachmentViewForAuthor docid = do
  doc <- guardRightM $ getDocByDocID docid
  ctx <- getContext
  let pending JpegPagesPending = True
      pending _                = False
      files                    = map authorattachmentfile (documentauthorattachments doc)
  case files of
    [] -> return $ toResponse ""
    f  -> do
      b <- mapM (\file -> liftIO $ maybeScheduleRendering ctx file (documentid doc)) f
      if any pending b
        then notFound (toResponse "temporary unavailable (document has files pending for process)")
        else do
        pages <- Doc.DocView.showFilesImages2 (documentid doc) Nothing $ zip f b
        simpleResponse pages

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
      ctx <- getContext
      let pending JpegPagesPending = True
          pending _                = False
          files                    = if documentstatus == Closed
                                      then documentsealedfiles
                                      else documentfiles
      case files of
         [] -> notFound $ toResponse "temporary unavailable (document has no files)"
         f  -> do
             b <- mapM (\file -> liftIO $ maybeScheduleRendering ctx file documentid) f
             if any pending b
                then notFound (toResponse "temporary unavailable (document has files pending for process)")
                else do
                    pages <- Doc.DocView.showFilesImages2 documentid mtokens $ zip f b
                    simpleResponse pages

handleDocumentUpload :: Kontrakcja m => DocumentID -> BS.ByteString -> BS.ByteString -> m ()
handleDocumentUpload docid content1 filename = do
  Log.debug "Uploading doc"
  ctx@Context{ctxdocstore, ctxs3action} <- getContext
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  content <- liftIO $ preprocessPDF ctx content1 docid

  fileresult <- update $ AttachFile docid filename content
  case fileresult of
    Left err -> do
      liftIO $ print ("Got an error: " ++ show err)
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
      Nothing -> return LinkMain
      (Just doc) -> return $ LinkIssueDoc $ documentid doc

handleCreateNewTemplate:: Kontrakcja m => m KontraLink
handleCreateNewTemplate = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  mdoc <- makeDocumentFromFile (Template Contract) input
  case mdoc of
    Nothing -> handleTemplateReload
    (Just doc) -> return $ LinkIssueDoc $ documentid doc

handleCreateNewAttachment:: Kontrakcja m => m KontraLink
handleCreateNewAttachment = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  _ <- makeDocumentFromFile Attachment input
  handleAttachmentReload

makeDocumentFromFile :: Kontrakcja m => DocumentType -> Input -> m (Maybe Document)
makeDocumentFromFile doctype (Input contentspec (Just filename) _contentType) = do
    Context { ctxmaybeuser = Just user, ctxtime } <- getContext
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
          doc <- update $ NewDocument user title doctype ctxtime
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ Just doc
makeDocumentFromFile _ _ = mzero -- to complete the patterns

handleContractArchive :: Kontrakcja m => m KontraLink
handleContractArchive = do
    _ <- handleSignableArchive (Signable Contract)
    return $ LinkContracts emptyListParams

handleOffersArchive :: Kontrakcja m => m KontraLink
handleOffersArchive =  do
    _ <- handleSignableArchive (Signable Offer)
    return $ LinkOffers emptyListParams

handleOrdersArchive :: Kontrakcja m => m KontraLink
handleOrdersArchive =  do
    _ <- handleSignableArchive (Signable Order)
    return $ LinkOrders emptyListParams

handleSignableArchive :: Kontrakcja m => DocumentType -> m ()
handleSignableArchive doctype =  do
    handleIssueArchive
    addFlashM $ flashMessageSignableArchiveDone doctype
    return ()

handleTemplateArchive :: Kontrakcja m => m KontraLink
handleTemplateArchive = do
    handleIssueArchive
    addFlashM flashMessageTemplateArchiveDone
    return $ LinkTemplates emptyListParams

handleAttachmentArchive :: Kontrakcja m => m KontraLink
handleAttachmentArchive = do
    handleIssueArchive
    addFlashM flashMessageAttachmentArchiveDone
    return $ LinkAttachments emptyListParams

handleIssueArchive :: Kontrakcja m => m ()
handleIssueArchive = do
    Log.debug "handleIssueArchive"
    Context { ctxmaybeuser = Just user } <- getContext
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    let ids = map DocumentID idnumbers
    idsAndUsers <- mapM lookupUsersRelevantToDoc ids
    let uid = userid user
        uemail = getEmail user
    res <- update $ ArchiveDocuments uid uemail idsAndUsers
    case res of
      Left msg -> do
        Log.debug $ "Failed to delete docs " ++ (show ids) ++ " : " ++ msg
        mzero
      Right _ -> return ()

handleTemplateShare :: Kontrakcja m => m KontraLink
handleTemplateShare = withUserPost $ do
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashM $ flashMessageSingleTemplateShareDone $ documenttitle d
      _ -> addFlashM flashMessageMultipleTemplateShareDone
    return $ LinkTemplates emptyListParams

handleAttachmentShare :: Kontrakcja m => m KontraLink
handleAttachmentShare = withUserPost $ do
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashM $ flashMessageSingleAttachmentShareDone $ documenttitle d
      _ -> addFlashM  flashMessageMultipleAttachmentShareDone
    return $ LinkAttachments emptyListParams

handleIssueShare :: Kontrakcja m => m [Document]
handleIssueShare = do
    Context { ctxmaybeuser = Just user } <- getContext
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    let ids = map DocumentID idnumbers
    guardRightM $ update $ ShareDocuments user ids

handleAttachmentRename :: Kontrakcja m => DocumentID -> m KontraLink
handleAttachmentRename docid = withUserPost $ do
  newname <- getCriticalField (return . BS.fromString) "docname"
  doc <- guardRightM $ update $ SetDocumentTitle docid newname
  return $ LinkIssueDoc $ documentid doc

handleBulkContractRemind :: Kontrakcja m => m KontraLink
handleBulkContractRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Contract)
    return $ LinkContracts emptyListParams

handleBulkOfferRemind :: Kontrakcja m => m KontraLink
handleBulkOfferRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Offer)
    return $ LinkOffers emptyListParams

handleBulkOrderRemind :: Kontrakcja m => m KontraLink
handleBulkOrderRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Order)
    return $ LinkOrders emptyListParams

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

handleContractsReload :: Kontrakcja m => m KontraLink
handleContractsReload  = fmap LinkContracts getListParamsForSearch

handleTemplateReload :: Kontrakcja m => m KontraLink
handleTemplateReload = fmap LinkTemplates getListParamsForSearch

handleAttachmentReload :: Kontrakcja m => m KontraLink
handleAttachmentReload = fmap LinkAttachments getListParamsForSearch

handleOffersReload :: Kontrakcja m => m KontraLink
handleOffersReload = fmap LinkOffers getListParamsForSearch

handleOrdersReload :: Kontrakcja m => m KontraLink
handleOrdersReload = fmap LinkOrders getListParamsForSearch

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
  doc <- guardRightM $  getDocByDocID docid
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
        Left _ -> return LinkMain
        Right doc -> do
          guard $ isAuthor (doc, user)
          mnewdoc <- update $ ChangeSignatoryEmailWhenUndelivered docid slid email
          case mnewdoc of
            Right newdoc -> do
              -- get (updated) siglink from updated document
              sl <- guardJust (getSigLinkFor newdoc slid)
              sendInvitationEmail1 ctx newdoc sl
              return $ LinkIssueDoc $ docid
            _ -> return LinkMain
    _ -> return LinkMain

sendCancelMailsForDocument :: Kontrakcja m => (Maybe BS.ByteString) -> Context -> Document -> m ()
sendCancelMailsForDocument customMessage ctx document = do
  let activated_signatories = filter (isActivatedSignatory $ documentcurrentsignorder document) $ documentsignatorylinks document
  forM_ activated_signatories (scheduleEmailSendout (ctxesenforcer ctx) <=< (mailCancelDocumentByAuthor customMessage ctx document))

failIfNotAuthor :: Kontrakcja m => Document -> User -> m ()
failIfNotAuthor document user = guard (isAuthor (document, user))

checkLinkIDAndMagicHash :: Kontrakcja m => Document -> SignatoryLinkID -> MagicHash -> m ()
checkLinkIDAndMagicHash document linkid magichash1 = do
  siglink <- guardJust $ getSigLinkFor document linkid
  guard $ signatorymagichash siglink == magichash1
  return ()

mainPage :: Kontrakcja m => m String
mainPage =  do
    params <- getListParams
    showTemplates <- isFieldSet "showTemplates"
    tooLarge <- isFieldSet "tooLarge"
    mdocprocess <- getDocProcess
    when tooLarge $ addFlashM modalPdfTooLarge
    uploadPage params mdocprocess showTemplates

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

getTemplatesForAjax :: Kontrakcja m => m Response
getTemplatesForAjax = do
    ctx <- getContext
    params <- getListParams
    mdocprocess <- getDocProcess
    case (ctxmaybeuser ctx,mdocprocess) of
            (Just user, Just docprocess) -> do
                let tfilter doc = (Template docprocess == documenttype doc)
                userdocs <- liftIO $ query $ GetDocumentsByAuthor (userid user)
                relatedusers <- liftIO $ query $ GetUserRelatedAccounts (userid user)
                shareddocs <- liftIO $ query $ GetSharedTemplates (map userid relatedusers)
                let templates = filter tfilter $ nub (userdocs ++ shareddocs)
                content <- templatesForAjax (ctxtime ctx) user docprocess $ docSortSearchPage params templates
                simpleResponse content
            (Nothing, _) -> sendRedirect $ LinkLogin NotLogged
            _ -> mzero

handleCreateFromTemplate :: Kontrakcja m => m KontraLink
handleCreateFromTemplate = withUserPost $ do
  Context { ctxmaybeuser } <- getContext
  docid <- readField "template"
  case docid of
    Just did -> do
      let user = fromJust ctxmaybeuser
      document <- queryOrFail $ GetDocumentByDocumentID $ did
      sharedWithUser <- isShared user document
      enewdoc <- if (isAuthor (document, user) ||  sharedWithUser) 
                    then update $ SignableFromDocumentIDWithUpdatedAuthor user did
                    else mzero
      case enewdoc of
        Right newdoc -> return $ LinkIssueDoc $ documentid newdoc
        Left _ -> mzero
    Nothing -> mzero
  where
    isShared :: Kontrakcja m => User -> Document -> m Bool
    isShared user document = do
      let Just authorsiglink = getAuthorSigLink document
          Just authorid = maybesignatory authorsiglink
      relatedaccounts <- query $ GetUserRelatedAccounts (userid user)
      return $ (documentsharing document == Shared)
        && (authorid `elem` (map userid relatedaccounts))

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
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did sid mh
  case edoc of
    Left _ -> mzero
    Right doc -> case find (authorAttachmentHasFileID fid) (documentauthorattachments doc) of
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

handleMigrateDocumentAuthorAttachments :: Kontrakcja m => m Response
handleMigrateDocumentAuthorAttachments = onlySuperUser $ do
  docs <- query $ GetDocuments Nothing
  forM_ docs (\doc -> do
                         eatts <- query $ GetDocumentsByDocumentID (documentattachments doc)
                         case eatts of
                           Left msg -> do
                             liftIO $ print msg
                             return ()
                           Right atts -> do
                             eres <- update $
                                     MigrateDocumentAuthorAttachments (documentid doc) (map (head . documentfiles) atts)
                             case eres of
                               Left msg2 -> do
                                 liftIO $ print msg2
                                 return ()
                               Right _udoc -> return ())
  addFlash (OperationDone, "All documents migrated!")
  sendRedirect LinkMain

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
