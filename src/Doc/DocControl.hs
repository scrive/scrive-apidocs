{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
{- |
   DocControl represents the controler (in MVC) of the document.
 -}
module Doc.DocControl where

import ActionSchedulerState
import AppView
import Doc.CSVUtils
import Doc.DocSeal
import Doc.DocState
import Doc.DocStateQuery
import Doc.DocStateUpdate
import Doc.DocStorage
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import FlashMessage
import InputValidation
import Kontra
import KontraLink
import ListUtil
import Mails.SendMail
import MinutesTime
import Misc
import Redirect
import User.UserControl
import qualified Amazon as AWS
import qualified AppLogger as Log
import Templates.Templates

import Codec.Text.IConv
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
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

{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}


{- |
   Perform the appropriate action when transitioning between documentstatuses.
   This function should always be called after changing the document.
 -}
postDocumentChangeAction :: Document -> Document -> Maybe SignatoryLinkID -> Kontra ()
postDocumentChangeAction document@Document  { documentstatus
                                            , documentid
                                            , documentcancelationreason
                                            } 
                      olddocument@Document  { documentstatus = oldstatus }
                            msignalinkid
    -- No status change ; 
    | documentstatus == oldstatus =
        -- if sign order has changed, we need to send another invitations
        when (documentcurrentsignorder document /= documentcurrentsignorder olddocument) $ do
            ctx <- get
            Log.forkIOLogWhenError ("error in sending invitation emails for document " ++ show documentid) $ 
              sendInvitationEmails ctx document
    -- Preparation -> Pending
    -- main action: sendInvitationEmails
    | oldstatus == Preparation && documentstatus == Pending = do
        ctx <- get
        liftIO $ print document
        Log.forkIOLogWhenError ("error in sending invitation emails for document " ++ show documentid) $ do
          sendInvitationEmails ctx document
        return ()
    -- Preparation -> Closed (only author signs)
    -- main action: sealDocument and sendClosedEmails
    | oldstatus == Preparation && documentstatus == Closed = do
        ctx <- get
        Log.forkIOLogWhenError ("error sealing document " ++ show documentid)$ do
          enewdoc <- sealDocument ctx document
          case enewdoc of
            Right newdoc -> sendClosedEmails ctx newdoc
            Left errmsg -> Log.error $ "Sealing of document #" ++ show documentid ++ " failed, could not send document confirmations: " ++ errmsg
        return ()
    -- Pending -> AwaitingAuthor
    -- main action: sendAwaitingEmail
    | oldstatus == Pending && documentstatus == AwaitingAuthor = do
        ctx <- get
        Log.forkIOLogWhenError ("error in sending awaiting emails for document " ++ show documentid) $ do
          sendAwaitingEmail ctx document
        return ()
    -- Pending -> Closed OR AwaitingAuthor -> Closed
    -- main action: sendClosedEmails
    | (oldstatus == Pending || oldstatus == AwaitingAuthor) && documentstatus == Closed = do
        ctx <- get
        Log.forkIOLogWhenError ("error sealing document " ++ show documentid) $ do
          enewdoc <- sealDocument ctx document
          case enewdoc of
            Right newdoc -> sendClosedEmails ctx newdoc
            Left errmsg -> do
              _ <- update $ ErrorDocument documentid errmsg
              Log.forkIOLogWhenError ("error in sending seal error emails for document " ++ show documentid) $ do
                sendDocumentErrorEmail ctx document
              return ()
        return ()
    -- Pending -> Rejected
    -- main action: sendRejectAuthorEmail
    | oldstatus == Pending && documentstatus == Rejected = do
        ctx <- get
        customMessage <- getCustomTextField "customtext"
        Log.forkIOLogWhenError ("error in sending rejection emails for document " ++ show documentid) $ do
          sendRejectEmails (fmap BS.toString customMessage) ctx document (fromJust msignalink)
        return ()
    -- Pending -> Canceled
    -- main action: if canceled because of ElegDataMismatch, send out emails
    | oldstatus == Pending && 
        documentstatus == Canceled && 
        isJust documentcancelationreason &&
        isELegDataMismatch (fromJust documentcancelationreason) = do
            ctx <- get
            Log.forkIOLogWhenError ("error sending cancelation emails for document " ++ show documentid) $ do
                sendElegDataMismatchEmails ctx document
            return ()
    --  -> DocumentError
    | DocumentError _msg <- documentstatus = do
        ctx <- get
        Log.forkIOLogWhenError ("error in sending error emails for document " ++ show documentid) $ do
          sendDocumentErrorEmail ctx document
        return ()
                                            
    -- transition with no necessary action; do nothing
    -- FIXME: log status change
    | otherwise = 
         return ()
    where msignalink = maybe Nothing (signlinkFromDocById document) msignalinkid

-- EMAILS

sendElegDataMismatchEmails :: Context -> Document -> IO ()
sendElegDataMismatchEmails ctx document = do
    let signlinks = [sl | sl <- documentsignatorylinks document
                        , isActivatedSignatory (documentcurrentsignorder document) sl
                        , not $ siglinkIsAuthor sl]
        Just (ELegDataMismatch msg badid _ _ _) = documentcancelationreason document
        badsig = fromJust $ find (\sl -> badid == signatorylinkid sl) (documentsignatorylinks document)
        badname  = BS.toString $ signatoryname  $ signatorydetails badsig
        bademail = BS.toString $ signatoryemail $ signatorydetails badsig
    forM_ signlinks $ sendDataMismatchEmailSignatory ctx document badid badname msg
    sendDataMismatchEmailAuthor ctx document badname bademail

sendDataMismatchEmailSignatory :: Context -> Document -> SignatoryLinkID -> String -> String -> SignatoryLink -> IO ()
sendDataMismatchEmailSignatory ctx document badid badname msg signatorylink = do
    let SignatoryLink { signatorylinkid, signatorydetails = sigdets } = signatorylink
        isbad = badid == signatorylinkid
    case getAuthorSigLink document of
      Nothing -> error "No author in Document"
      Just authorsl -> do
        mail <- mailMismatchSignatory 
                  ctx 
                document 
                (BS.toString $ signatoryemail $ signatorydetails authorsl) 
                (BS.toString $ signatoryname  $ signatorydetails authorsl) 
                (ctxhostpart ctx ++ (show $ LinkSignDoc document signatorylink))
                (BS.toString $ signatoryname $ signatorydetails signatorylink) 
                badname
                msg
                isbad
        scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress {fullname = signatoryname sigdets, email = signatoryemail sigdets }]}
          
sendDataMismatchEmailAuthor :: Context -> Document -> String -> String -> IO ()
sendDataMismatchEmailAuthor ctx document badname bademail = do
    let authorname = signatoryname $ signatorydetails $ fromJust $ getAuthorSigLink document
        authoremail = signatoryemail $ signatorydetails $ fromJust $ getAuthorSigLink document
    mail <- mailMismatchAuthor ctx document (BS.toString authorname) badname bademail
    scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress {fullname = authorname, email = authoremail }]}
    
{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendDocumentErrorEmail :: Context -> Document -> IO ()
sendDocumentErrorEmail ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendDocumentErrorEmail1 ctx document)

sendDocumentErrorEmailToAuthor :: Context -> Document -> IO ()
sendDocumentErrorEmailToAuthor ctx document = do
  let authordetails = signatorydetails $ fromJust $ getAuthorSigLink document
  mail <- mailDocumentError (ctxtemplates ctx) ctx document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        to = [MailAddress { fullname = signatoryname authordetails
                          , email = signatoryemail authordetails}]
  }

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendDocumentErrorEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendDocumentErrorEmail1 ctx document signatorylink = do
  let SignatoryLink { signatorylinkid
                    , signatorydetails } = signatorylink
      Document { documentid } = document
  mail <- mailDocumentError (ctxtemplates ctx) ctx document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        to = [MailAddress { fullname = signatoryname signatorydetails
                          , email = signatoryemail signatorydetails}]
      , mailInfo = Invitation documentid  signatorylinkid
  }

{- |
   Send emails to all of the invited parties.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendInvitationEmails :: Context -> Document -> IO ()
sendInvitationEmails ctx document = do
  print document
  let signlinks = [sl | sl <- documentsignatorylinks document
                      , isCurrentSignatory (documentcurrentsignorder document) sl
                      , not $ siglinkIsAuthor sl]
  forM_ signlinks (sendInvitationEmail1 ctx document)

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendInvitationEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendInvitationEmail1 ctx document signatorylink = do
  let SignatoryLink { signatorylinkid
                    , signatorydetails } = signatorylink
      Document { documentid } = document
      authorsiglink = fromJust $ getAuthorSigLink document
      hasAuthorSigned = isJust $ maybesigninfo authorsiglink
  mail <- if isSignatory signatorylink
          then if hasAuthorSigned 
               then mailInvitationToSign (ctxtemplates ctx) ctx document signatorylink
               else mailInvitationToSend (ctxtemplates ctx) ctx document signatorylink
          else mailInvitationToView (ctxtemplates ctx) ctx document signatorylink
  -- ?? Do we need to read in the contents? -EN
  _attachmentcontent <- getFileContents ctx $ head $ documentfiles document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        to = [MailAddress {fullname = signatoryname signatorydetails
                          , email = signatoryemail signatorydetails}]
      , mailInfo = Invitation documentid signatorylinkid
  }

{- |
   Send emails to all parties when a document is closed.
 -}
sendClosedEmails :: Context -> Document -> IO ()
sendClosedEmails ctx document = do
    let signatorylinks = documentsignatorylinks document
    let mailAddressFromSignatoryLink signatorylink = 
            MailAddress { fullname = signatoryname $ signatorydetails $ signatorylink
                        , email = signatoryemail $ signatorydetails $ signatorylink
                        }

    mail <- mailDocumentClosed (ctxtemplates ctx) ctx document
    mailattachments <- makeMailAttachments ctx document
    scheduleEmailSendout (ctxesenforcer ctx) $ 
                         mail { to = map mailAddressFromSignatoryLink signatorylinks
                              , attachments = mailattachments
                              }


{- |
   Send an email to the author when the document is awaiting approval
 -}
sendAwaitingEmail :: Context -> Document -> IO ()
sendAwaitingEmail ctx document = do
  let Just authorsiglink = getAuthorSigLink document
      authoremail = signatoryemail $ signatorydetails authorsiglink
      authorname  = signatoryname  $ signatorydetails authorsiglink
  mail <- mailDocumentAwaitingForAuthor (ctxtemplates ctx) ctx authorname document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress {fullname = authorname, email = authoremail }]}

makeMailAttachments :: Context -> Document -> IO [(BS.ByteString,BS.ByteString)]
makeMailAttachments ctx document = do
  let mainfile = head $ case documentsealedfiles document of
        [] -> documentfiles document
        fs -> fs
      attachments = map authorattachmentfile $ documentauthorattachments document
      allfiles = mainfile : attachments
      filenames = map filename allfiles
  filecontents <- sequence $ map (getFileContents ctx) allfiles
  return $ zip filenames filecontents

{- |
   Send an email to the author and to all signatories who were sent an invitation  when the document is rejected
 -}
sendRejectEmails :: (Maybe String) -> Context -> Document -> SignatoryLink -> IO ()
sendRejectEmails customMessage ctx document signalink = do
  let activatedSignatories = [sl | sl <- documentsignatorylinks document
                                 , isActivatedSignatory (documentcurrentsignorder document) sl || siglinkIsAuthor sl]
  forM_ activatedSignatories $ \sl -> do
    let semail = signatoryemail $ signatorydetails  sl
        sname = signatoryname $ signatorydetails  sl
    mail <- mailDocumentRejected (ctxtemplates ctx) customMessage ctx sname document signalink
    scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress { fullname = sname, email = semail }]}

-- END EMAILS

{- |
   Render a page of Contracts for a user
   URL: /s
   Method: Get
   ??: Is this what it does?
 -}
handleSTable :: Kontra (Either KontraLink Response)
handleSTable = checkUserTOSGet $ do
  Context { ctxmaybeuser = Just user, ctxtime, ctxtemplates } <- get
  edocs <- getDocsByLoggedInUser
  case edocs of
    Left _ -> mzero
    Right documents -> do
      let contracts  = [doc | doc <- documents
                            , (Signable Contract) == documenttype doc]
      params <- getListParams
      content <- liftIO $ pageContractsList ctxtemplates ctxtime user (docSortSearchPage params contracts)
      renderFromBody TopNone kontrakcja content

{- |
    Handles an account setup from within the sign view.
-}
handleAcceptAccountFromSign :: DocumentID -> SignatoryLinkID -> MagicHash -> ActionID -> MagicHash -> Kontra KontraLink
handleAcceptAccountFromSign documentid
                            signatorylinkid
                            signmagichash
                            actionid
                            magichash = do
  Context { ctxtemplates } <- get
  muser <- handleAccountSetupFromSign actionid magichash
  edoc <- getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid signmagichash
  case edoc of
    Left _ -> mzero
    Right document -> case getSigLinkBySigLinkID signatorylinkid document of
      Nothing -> mzero
      Just signatorylink -> do
        case (muser, Closed == documentstatus document) of
          (Nothing, True)  -> addModal $ modalSignedClosedNoAccount document signatorylink actionid magichash
          (Nothing, False) -> addModal $ modalSignedNotClosedNoAccount document signatorylink actionid magichash
          (Just _, _)      -> addFlashMsg =<< (liftIO $ flashMessageAccountActivatedFromSign ctxtemplates)
        return $ LinkSignDoc document signatorylink

{- |
    Handles an account removal from within the sign view.
-}
handleDeclineAccountFromSign :: DocumentID -> SignatoryLinkID -> MagicHash -> ActionID -> MagicHash -> Kontra KontraLink
handleDeclineAccountFromSign documentid
                             signatorylinkid
                             signmagichash
                             actionid
                             magichash = do
  Context{ ctxtemplates } <- get
  edoc <- getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid signmagichash
  case edoc of
    Left _ -> mzero
    Right document -> case getSigLinkBySigLinkID signatorylinkid document of
      Nothing -> mzero
      Just signatorylink -> do
        handleAccountRemovalFromSign actionid magichash
        addFlashMsg =<< (liftIO $ flashMessageAccountRemovedFromSign ctxtemplates)
        return $ LinkSignDoc document signatorylink

{- |
   Control the signing of a document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
   Method: POST
 -}
signDocument :: DocumentID      -- ^ The DocumentID of the document to sign 
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL 
             -> MagicHash       -- ^ The MagicHash that is in the URL
             -> Kontra KontraLink
signDocument documentid
             signatorylinkid1
             magichash1 = do            
  Context { ctxtime, ctxipnumber } <- get
  edoc <- getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid1 magichash1
  case edoc of
    Left _ -> mzero
    Right olddocument -> do
      fieldnames <- getAndConcat "fieldname"
      fieldvalues <- getAndConcat "fieldvalue"
      let fields = zip fieldnames fieldvalues
      let allowedidtypes = documentallowedidtypes olddocument
          allowsEmail = EmailIdentification `elem` allowedidtypes

      guard allowsEmail

      newdocument <- update $ SignDocument documentid signatorylinkid1 ctxtime ctxipnumber Nothing fields
      case newdocument of
        Left message -> do
          addFlashMsg $ toFlashMsg OperationFailed message
          return $ LinkMain
        Right document -> do 
          postDocumentChangeAction document olddocument (Just signatorylinkid1)
          handleAfterSigning document signatorylinkid1

{- |
    Call after signing in order to save the document for any new user,
    put up the appropriate modal, and register the necessary nagging email actions.
    This is factored into it's own function because that way it can be used by eleg too.
-}
handleAfterSigning :: Document -> SignatoryLinkID -> Kontra KontraLink
handleAfterSigning document@Document{documentid,documenttitle} signatorylinkid = do
  ctx <- get
  signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
  maybeuser <- query $ GetUserByEmail (currentServiceID ctx) (Email $ signatoryemail (signatorydetails signatorylink))
  case maybeuser of
    Nothing -> do
      let details = signatorydetails signatorylink
          fullname = (signatoryfstname details, signatorysndname details)
          email = signatoryemail details
          company = signatorycompany details
      muser <- liftIO $ createUserBySigning ctx documenttitle fullname email company (documentid, signatorylinkid)
      case muser of
        (Just (user, actionid, magichash)) -> do
          _ <- update $ SaveDocumentForSignedUser documentid (getSignatoryAccount user) signatorylinkid
          if (Closed == documentstatus document)
            then addModal $ modalSignedClosedNoAccount document signatorylink actionid magichash
            else addModal $ modalSignedNotClosedNoAccount document signatorylink actionid magichash
          return ()
        _ -> return ()
    Just user -> do
     _ <- update $ SaveDocumentForSignedUser documentid (getSignatoryAccount user) signatorylinkid
     if Closed == documentstatus document
       then addModal $ modalSignedClosedHasAccount document signatorylink (isJust $ ctxmaybeuser ctx)
       else addModal $ modalSignedNotClosedHasAccount document signatorylink (isJust $ ctxmaybeuser ctx)
  return $ LinkSignDoc document signatorylink

{- |
   Control rejecting the document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
 -}
rejectDocument :: DocumentID 
               -> SignatoryLinkID 
               -> MagicHash
               -> Kontra KontraLink
rejectDocument documentid 
               signatorylinkid1 
               magichash = withUserPost $ do
  Context{ ctxtime, ctxipnumber } <- get
  edoc <- getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid1 magichash
  case edoc of
    Left _ -> mzero
    Right olddocument -> do
      customtext <- getCustomTextField "customtext"
      mdocument <- update $ RejectDocument documentid signatorylinkid1 ctxtime ctxipnumber customtext
      case (mdocument) of
        Left message -> do
          addFlashMsg $ toFlashMsg OperationFailed message
          return $ LinkMain
        Right document -> do  
          postDocumentChangeAction document olddocument (Just signatorylinkid1)
          addModal $ modalRejectedView document
          return $ LoopBack

{- |
   Get the SignatoryLink associated with a SignatoryLinkID or mzero if not found
 -}
signatoryLinkFromDocumentByID :: Document -> SignatoryLinkID -> Kontra SignatoryLink
signatoryLinkFromDocumentByID Document{ documentsignatorylinks } linkid = do
    let invitedlinks = filter (\x -> signatorylinkid x == linkid
                               {- && signatorymagichash x == magichash1 -})
                              documentsignatorylinks
    case invitedlinks of
      [invitedlink] -> return invitedlink
      _ -> mzero
      
{- |
   Show the document to be signed
 -}
handleSignShow :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra String
handleSignShow documentid 
               signatorylinkid1
               magichash1 = do
  doc1 <- queryOrFail $ GetDocumentByDocumentID documentid
  setCurrentService doc1
  Context { ctxtime
          , ctxipnumber 
          , ctxflashmessages } <- get
  _ <- markDocumentSeen documentid signatorylinkid1 magichash1 ctxtime ctxipnumber
  edocument <- getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid1 magichash1
  case edocument of
    Left _ -> mzero -- not allowed to view
    Right document -> case getSigLinkBySigLinkID signatorylinkid1 document of
      Nothing -> mzero -- signatory link does not exist
      Just invitedlink -> do
        liftIO $ print document
        let isFlashNeeded = Data.List.null ctxflashmessages
                            && (not (isJust $ maybesigninfo invitedlink))
            -- heavens this is a confusing case statement, there must be a better way!
            flashMsg =
              case (isFlashNeeded, 
                    isSignatory invitedlink,
                    document `allowsIdentification` ELegitimationIdentification) of
                (False, _, _) -> Nothing
                (_, False, _) -> Just flashMessageOnlyHaveRightsToViewDoc
                (_, _, True) -> Just flashMessagePleaseSignWithEleg
                _ -> Just $ flashMessagePleaseSign document

        ctx@Context{ctxtemplates} <- get

        when (isJust flashMsg) $
          addFlashMsg =<< (liftIO $ (fromJust flashMsg) ctxtemplates)

        case (isAttachment document, isSignatory invitedlink) of
            (True, _) -> liftIO $ pageAttachmentForSignatory ctx document invitedlink
            (_, True) -> liftIO $ pageDocumentForSignatory (LinkSignDoc document invitedlink) document ctx invitedlink
            _ -> liftIO $ pageDocumentForViewer ctx document (Just invitedlink)

--end

maybeAddDocumentCancelationMessage :: Document -> Kontra ()
maybeAddDocumentCancelationMessage document = do
  let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
  when (documentstatus document == Canceled && isJust mMismatchMessage)
   (addFlashMsg $ toFlashMsg OperationFailed (fromJust mMismatchMessage))
  return ()

{- |
   Handles the request to show a document to a user.
   There are two cases: 
    1. author in which case they get pageDocumentForAuthor
    2. Friend of author in which case they get pageDocumentForViewer
   URL: /d/{documentid}
   Method: GET
 -}
handleIssueShowGet :: DocumentID -> Kontra (Either KontraLink (Either KontraLink String))
handleIssueShowGet docid = do
 checkUserTOSGet $ do 
  edoc <- getDocByDocID docid
  case edoc of
    Left _ -> mzero
    Right document -> do
        ctx@Context { ctxmaybeuser 
                    } <- get
        mdstep <- getDesignStep docid
        case (mdstep, documentfunctionality document) of
          (Just (DesignStep3 _), BasicFunctionality) -> return $ Left $ LinkIssueDoc docid
          _ -> do
            -- authors get a view with buttons
            case (joinB $ isUserAuthor document <$> ctxmaybeuser, isAttachment document, documentstatus document) of
              (True, True, Preparation) -> liftIO $ Right <$> pageAttachmentDesign ctx document
              (_, True, _) -> liftIO $ Right <$> pageAttachmentView ctx document 
              (True, _, _) -> do
                let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
                when ((documentstatus document == Canceled) && (isJust mMismatchMessage)) 
                  (addFlashMsg $ toFlashMsg OperationFailed (fromJust mMismatchMessage))
                ctx2 <- get   -- need to get new context because we may have added flash msg
                step <- getDesignStep (documentid document)
                case (documentstatus document) of
                  Preparation -> do
                    mattachments <- getDocsByLoggedInUser
                    case mattachments of
                      Left _ -> liftIO $ Right <$> pageDocumentDesign ctx2 document step []
                      Right attachments -> liftIO $ Right <$> pageDocumentDesign ctx2 document step (filter isAttachment attachments)
                  _ ->  liftIO $ Right <$> pageDocumentForAuthor ctx2 document            
              -- friends can just look (but not touch)
              (False, _, _) -> liftIO $ Right <$> pageDocumentForViewer ctx document Nothing

getDesignStep::DocumentID -> Kontra (Maybe DesignStep)
getDesignStep docid = do
    step3 <- isFieldSet "step3"
    step2 <- isFieldSet "step2"
    mperson <- getOptionalField asValidNumber "person"
    aftercsvupload <- isFieldSet "aftercsvupload"
    case (step2,step3) of
       (True,_) -> return $ Just $ DesignStep2 docid mperson (if aftercsvupload 
                                                                then (Just AfterCSVUpload) 
                                                                else Nothing)
       (_,True) -> return $ Just $ DesignStep3 docid
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
handleIssueShowPost :: DocumentID -> Kontra KontraLink
handleIssueShowPost docid = withUserPost $ do
  edocument <- getDocByDocID docid
  case edocument of
    Left _ -> mzero
    Right document -> do
      Context { ctxmaybeuser = Just user } <- get
      guard (isUserAuthor document user) -- still need this because friend can read document
      sign <- isFieldSet "sign"
      send <- isFieldSet "final"
      template <- isFieldSet "template"
      contract <- isFieldSet "contract"
      csvupload <- isFieldSet "csvupload"
      updateattachments <- isFieldSet "updateattachments"
      switchtoadvanced <- isFieldSet "changefunctionality"
      sigattachments <- isFieldSet "sigattachments"
      -- is this routing logic or business logic? I say business, but
      -- here it looks mixed.
      -- I'm especially asking about AwaitingAuthor case, because I though
      -- it was covered by SignDocument
      --   Eric
      case (documentstatus document,sign,send,template,contract,csvupload,updateattachments,switchtoadvanced,sigattachments) of 
        (Preparation, True, _,_, _, _ , _, _, _) -> handleIssueSign document
        (Preparation, _ ,  True,_, _, _, _, _, _) -> handleIssueSend document
        (Preparation, _ , _ ,True, _, _, _, _, _) -> handleIssueSaveAsTemplate document
        (Preparation, _ , _ ,_ , True, _, _, _, _) -> handleIssueChangeToContract document
        (Preparation, _ , _ ,_ , _, True, _, _, _) -> handleIssueCSVUpload document
        (Preparation, _ , _ ,_ , _, _, True, _, _) -> handleIssueUpdateAttachments document
        (Preparation, _ , _ ,_ , _, _, _, True, _) -> handleIssueChangeFunctionality document
        (Preparation, _ , _ ,_ , _, _, _, _, True) -> handleIssueUpdateSigAttachments document
        (Preparation, _ , _ ,_, _, _, _, _, _) -> handleIssueSave document
        (AwaitingAuthor, True , _ ,_, _, _, _, _, _) -> handleIssueSignByAuthor document
        _  -> return $ LinkContracts emptyListParams

handleIssueSign :: Document -> Kontra KontraLink
handleIssueSign document = do
    ctx@Context { ctxtime, ctxipnumber} <- get
    -- unless (document `allowsIdentification` EmailIdentification) mzero | This need to be refactored | Breaks templates
    mudoc <- updateDocument ctx document
    case mudoc of 
        Right udoc-> do
          mdocs <- splitUpDocument udoc
          case mdocs of
            Right docs -> do
              mndocs <- mapM (forIndividual ctxtime ctxipnumber udoc) docs
              case (lefts mndocs, rights mndocs) of
                ([],d:[]) -> do
                    addModal $ modalSendConfirmationView d
                    return $ LinkIssueDoc (documentid d)
                ([],ds) -> do 
                    addFlashMsg =<< (liftIO $ flashMessageCSVSent (length ds) (ctxtemplates ctx))
                    return $ LinkContracts emptyListParams
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
        
handleIssueSend :: Document -> Kontra KontraLink
handleIssueSend document = do
    ctx@Context { ctxtime, ctxipnumber} <- get
    mudoc <- updateDocument ctx document
    case mudoc of 
        Right udoc-> do
          mdocs <- splitUpDocument udoc
          case mdocs of
            Right docs -> do
              mndocs <- mapM (forIndividual ctxtime ctxipnumber udoc) docs
              case (lefts mndocs, rights mndocs) of
                ([],d:[]) -> do
                    addModal $ modalSendConfirmationView d
                    return $ LinkIssueDoc (documentid d)
                ([],ds) -> do 
                    addFlashMsg =<< (liftIO $ flashMessageCSVSent (length ds) (ctxtemplates ctx))
                    return $ LinkContracts emptyListParams
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

markDocumentAuthorReadAndSeen :: Document -> MinutesTime -> Word32 -> Kontra ()
markDocumentAuthorReadAndSeen Document{documentid, documentsignatorylinks} time ipnumber =
  mapM_ mark $ filter siglinkIsAuthor documentsignatorylinks
  where
    mark SignatoryLink{signatorylinkid, signatorymagichash} = do
      update $ MarkInvitationRead documentid signatorylinkid time
      update $ MarkDocumentSeen documentid signatorylinkid signatorymagichash time ipnumber

handleIssueSaveAsTemplate :: Document -> Kontra KontraLink
handleIssueSaveAsTemplate document = do
  ctx <- get
  eudoc <- updateDocument ctx document
  case eudoc of 
    Left _ -> mzero
    Right _udoc -> do   
      mndoc <- update $ TemplateFromDocument $ documentid document -- ?? Should be udoc? -EN
      case mndoc of
        Left _ -> mzero
        Right _newdocument -> do
          addFlashMsg =<< (liftIO $ flashDocumentTemplateSaved $ ctxtemplates ctx)
          return $ LinkTemplates emptyListParams       

handleIssueChangeToContract :: Document -> Kontra KontraLink
handleIssueChangeToContract document = do
  ctx <- get
  mcontract <- update $ SignableFromDocumentID $ documentid document 
  case mcontract of 
    Right contract -> do   
      mncontract <- updateDocument ctx contract
      case mncontract of
        Right ncontract -> return $ LinkDesignDoc $ DesignStep3 $ documentid ncontract                        
        Left _ -> mzero
    Left _ -> mzero

{- |
    If the document has a multiple part this will pump csv values through it to create multiple docs, and then
    save the original as a template if it isn't already.  This will make sure to clean the csv data.  It just returns
    a list containing the original doc on it's own, if the doc hasn't got a multiple part.

    I feel like this is quite dangerous to do all at once, maybe need a transaction?!
-}
splitUpDocument :: Document -> Kontra (Either KontraLink [Document])
splitUpDocument doc =
  case (documentcsvupload doc, getCSVCustomFields doc) of
    (Just _, Left _) -> mzero
    (Nothing, _) -> return $ Right [doc]
    (Just csvupload, Right csvcustomfields) ->
      case (cleanCSVContents (documentallowedidtypes doc) (length csvcustomfields) $ csvcontents csvupload) of
        (_prob:_, _) -> do
          Context{ctxtemplates} <- get
          addFlashMsg =<< (liftIO $ flashMessageInvalidCSV ctxtemplates)
          return $ Left $ LinkDesignDoc $ DesignStep2 (documentid doc) (Just (1 + csvsignatoryindex csvupload)) (Just AfterCSVUpload)
        ([], CleanCSVData{csvbody}) -> do
          mudoc <- if (isTemplate doc)
                    then return $ Right doc
                    else update $ TemplateFromDocument $ documentid doc
          case mudoc of
            Left _ -> mzero
            Right udoc -> do
              mdocs <- mapM (createDocFromRow udoc (csvsignatoryindex csvupload)) csvbody
              if Data.List.null (lefts mdocs)
                then return $ Right (rights mdocs) 
                else mzero
  where createDocFromRow :: Document -> Int -> [BS.ByteString] -> Kontra (Either String Document)
        createDocFromRow udoc sigindex xs =
          update $ DocumentFromSignatoryData (documentid udoc) sigindex (item 0) (item 1) (item 2) (item 3) (item 4) (item 5) (drop 6 xs)
          where item n | n<(length xs) = xs !! n
                       | otherwise = BS.empty
{- |
   Handles a csv file upload.  This'll parse the file, and save the info
   on the document and relevant signatory.
-}
handleIssueCSVUpload :: Document -> Kontra KontraLink 
handleIssueCSVUpload document = do
  ctx <- get             
  mudoc <- updateDocument ctx document
  case mudoc of
    Left _ -> mzero
    Right udoc -> do
      mcsvsigindex <- getOptionalField asValidNumber "csvsigindex"
      mcsvfile <- getCSVFile "csv"
      case (mcsvsigindex, mcsvfile) of
        (Nothing, Nothing) -> return $ LinkDesignDoc $ DesignStep2 (documentid udoc) Nothing Nothing
        (Nothing, Just _) ->  do
          Log.error "something weird happened, got csv file but there's no relevant person index"
          mzero
        (Just csvsigindex, Nothing) -> return $ LinkDesignDoc $ DesignStep2 (documentid udoc) (Just $ csvsigindex + 1) Nothing 
        (Just csvsigindex, Just (title, contents)) ->  do
          let csvupload = CSVUpload 
                          { csvtitle = title
                          , csvcontents = contents
                          , csvsignatoryindex = csvsigindex
                          }
          mndoc <- update $ AttachCSVUpload (documentid udoc) csvupload
          case mndoc of
            Left _ -> mzero
            Right ndoc -> return $ LinkDesignDoc $ DesignStep2 (documentid ndoc) (Just $ csvsigindex + 1) (Just AfterCSVUpload)
            
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
   
handleIssueUpdateSigAttachments :: Document -> Kontra KontraLink
handleIssueUpdateSigAttachments doc = do
  ctx <- get
  mudoc <- updateDocument ctx doc
  udoc <- returnRightOrMZero mudoc
  
  sigattachmentnames  <- getAndConcat "sigattachname"
  sigattachmentdescs  <- getAndConcat "sigattachdesc"
  sigattachmentemails <- getAndConcat "sigattachemails"
  
  let sigattachments = concat $ zipWith3 zipSigAttachments sigattachmentnames sigattachmentdescs sigattachmentemails
  endoc <- update $ UpdateSigAttachments (documentid udoc) sigattachments
  case endoc of
    Left _ -> mzero
    Right ndoc -> return (LinkDesignDoc (DesignStep3 (documentid ndoc)))

handleIssueUpdateAttachments :: Document -> Kontra KontraLink
handleIssueUpdateAttachments doc = withUserPost $ do
    ctx <- get
    mudoc <- updateDocument ctx doc
    udoc <- returnRightOrMZero mudoc
    
    attidsnums <- getCriticalFieldList asValidID "attachmentid"
    removeatt <- getCriticalFieldList asValidBool "removeattachment"
    let idsforremoval = [read $ BS.toString f | (f, r) <- zip attidsnums removeatt
                                              , r] :: [FileID]
    fileinputs <- getDataFnM $ lookInputs "attachment"
    mattachments <- sequence $ map (makeDocumentFromFile Attachment) fileinputs
    let idsforadd = [read $ BS.toString did |(did, r)<- zip attidsnums removeatt, not r] 
                    ++ (map documentid $ catMaybes mattachments) :: [DocumentID]

    mndoc <- update $ UpdateDocumentAttachments (documentid udoc) idsforadd idsforremoval
    case mndoc of
        Left _msg -> mzero
        Right ndoc -> return . LinkDesignDoc . DesignStep3 $ documentid ndoc


{- |
    Deals with a switch to the document's functionality.
    This'll also update the user preferences that they would like
    to continue with this functionality by default in the future.
-}
handleIssueChangeFunctionality :: Document -> Kontra KontraLink
handleIssueChangeFunctionality document = do
  ctx <- get
  mudoc <- updateDocument ctx document
  case mudoc of
    Left _ -> mzero
    Right udoc -> case getAuthorSigLink udoc of
      Just SignatoryLink { maybesignatory = Just authorid } -> do
        muser <- handlePreferenceChange authorid
        case muser of
          Left _ -> mzero
          Right _ -> return $ LinkDesignDoc $ DesignStep2 (documentid udoc) Nothing Nothing
      _ -> mzero
  where
    handlePreferenceChange :: UserID -> Kontra (Either String User)
    handlePreferenceChange userid = do
      toBasic <- isFieldSet "tobasic"
      toAdvanced <- isFieldSet "toadvanced"
      case (toBasic, toAdvanced) of
        (True, _) -> setPreferredMode $ Just BasicMode
        (_, True) -> setPreferredMode $ Just AdvancedMode
        _ -> setPreferredMode Nothing
      where 
        setPreferredMode :: Maybe DesignMode -> Kontra (Either String User)
        setPreferredMode designmode = do
          muser <- update $ SetPreferredDesignMode userid designmode
          return muser

{- |
    This will get and parse a csv file.  It
    also deals with any flash messages.  It returns a pair
    of (title, contents).
-}
getCSVFile :: String -> Kontra (Maybe (BS.ByteString, [[BS.ByteString]]))
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
                   | length contents > rowlimit -> return . Bad $ flashMessageCSVHasTooManyRows rowlimit 
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
    
handleIssueSave :: Document -> Kontra KontraLink
handleIssueSave document = do
    ctx <- get
    _ <- updateDocument ctx document
    if (isTemplate document) 
     then do
          addFlashMsg =<< (liftIO . flashDocumentTemplateSaved $ ctxtemplates ctx)
          return $ LinkTemplates emptyListParams
     else do
          addFlashMsg =<< (liftIO . flashDocumentDraftSaved $ ctxtemplates ctx)
          return $ LinkContracts emptyListParams
     
handleIssueSignByAuthor :: Document -> Kontra KontraLink
handleIssueSignByAuthor document = do
    ctx@Context { ctxtime, ctxipnumber} <- get
    unless (document `allowsIdentification` EmailIdentification) mzero
    doc2 <- update $ CloseDocument (documentid document) ctxtime ctxipnumber  Nothing
    case doc2 of
        Nothing -> return $ LinkIssueDoc (documentid document)
        Just d -> do 
            postDocumentChangeAction d document Nothing
            addFlashMsg =<< (liftIO $ flashAuthorSigned $ ctxtemplates ctx)
            return $ LinkIssueDoc (documentid document)
            
{- |
   Show the document with title in the url
   URL: /d/{documentid}/{title}
   Method: GET
 -}
handleIssueShowTitleGet :: DocumentID -> String -> Kontra (Either KontraLink Response)
handleIssueShowTitleGet docid = withAuthorOrFriend docid
    . checkUserTOSGet . handleIssueShowTitleGet' docid

handleIssueShowTitleGetForSignatory :: DocumentID -> SignatoryLinkID -> MagicHash -> String -> Kontra Response
handleIssueShowTitleGetForSignatory docid siglinkid sigmagichash title = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    handleIssueShowTitleGet' docid title

handleIssueShowTitleGet' :: DocumentID -> String -> Kontra Response
handleIssueShowTitleGet' docid _title = do
    ctx <- get
    document <- queryOrFail $ GetDocumentByDocumentID docid
    let file = safehead "handleIssueShow" (case documentstatus document of
                                                Closed -> documentsealedfiles document
                                                _      -> documentfiles document)
    contents <- liftIO $ getFileContents ctx file
    let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
        res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
    return res2

-- | Check if current user is author or friend so he can view the document
withAuthorOrFriend :: DocumentID -> Kontra (Either KontraLink a) -> Kontra (Either KontraLink a)
withAuthorOrFriend docid action = do
  edoc <- getDocByDocID docid
  case edoc of
    Left  _ -> mzero
    Right _ -> action

{- |
   Show the document with title in the url
   URL: /d/{documentid}/{title}
   Method: GET
 -}
handleFileGet :: FileID -> String -> Kontra (Either KontraLink Response)
handleFileGet fileid' _title = do
  withUserGet $ onlySuperUser $ do
   ctx <- get
   mdocument <- query $ GetDocumentByFileID $ fileid'
   document <- case mdocument of
     Right document -> return document
     Left msg -> do
       Log.debug $ "Cannot file a document for fileid " ++ show fileid' ++ ", msg= " ++ msg
       mzero
   
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
getAndConcat :: String -> Kontra [BS.ByteString]
getAndConcat fname = do
  values <- getDataFnM $ lookInputList fname
  return $ map concatChunks values

makePlacements :: [BS.ByteString]
               -> [BS.ByteString]
               -> [Int]
               -> [Int]
               -> [Int]
               -> [Int]
               -> [Int]
               -> Kontra [(BS.ByteString, BS.ByteString, FieldPlacement)]
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
updateDocument :: Context -> Document -> Kontra (Either String Document)
updateDocument ctx@Context{ ctxtime } document@Document{ documentid, documentfunctionality } = do
  -- each signatory has these predefined fields
  signatoriesfstnames        <- getAndConcat "signatoryfstname"
  signatoriessndnames        <- getAndConcat "signatorysndname"
  signatoriescompanies       <- getAndConcat "signatorycompany"
  signatoriespersonalnumbers <- getAndConcat "signatorypersonalnumber"
  signatoriescompanynumbers  <- getAndConcat "signatorycompanynumber"
  signatoriesemails          <- map (BSC.map toLower) <$> getAndConcat "signatoryemail"
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

  authorrole <- getFieldWithDefault "" "authorrole"
  
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
  let authordetails = (makeAuthorDetails placements fielddefs $ signatorydetails authorsiglink) { signatorysignorder = SignOrder 0 } -- author has sign order set to 0 since he is 'the host' of the document
                        
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

              

getDocumentsForUserByType :: User -> (Document -> Bool) -> Kontra ([Document])
getDocumentsForUserByType user docfilter = do
  mydocuments <- query $ GetDocumentsByUser user 
  usersICanView <- query $ GetUsersByFriendUserID $ userid user
  usersISupervise <- query $ GetUserSubaccounts $ userid user
  relatedUsers <- query $ GetUserRelatedAccounts $ userid user
  friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
  supervised'Documents <- mapM (query . GetDocumentsByUser) usersISupervise
  relatedUsersSharedDocuments <- filter ((==) Shared . documentsharing) <$> concat <$> mapM (query . GetDocumentsByAuthor . userid) relatedUsers
  return . filter docfilter $ nub $
          mydocuments ++ concat friends'Documents ++ concat supervised'Documents ++ relatedUsersSharedDocuments

{- |
   Constructs a list of documents (Arkiv) to show to the user.
   The list contains all documents the user is an author on or
   is a friend of the author.
   Duplicates are removed.
 -}
showContractsList :: Kontra (Either KontraLink String)
showContractsList =
  let getContracts user = do
        mydocuments <- query $ GetDocumentsByUser user
        usersICanView <- query $ GetUsersByFriendUserID $ userid user
        friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
        supervised'Documents <- query $ GetDocumentsBySupervisor user
        return . filter ((==) (Signable Contract) . documenttype) $ 
          mydocuments ++ concat friends'Documents ++ supervised'Documents in
  showItemList' pageContractsList getContracts

showTemplatesList :: Kontra (Either KontraLink String)
showTemplatesList = 
  let userTemplates user = do
        mydocuments <- query $ GetDocumentsByUser user
        return $ filter isTemplate mydocuments in
  showItemList' pageTemplatesList userTemplates

showOfferList :: Kontra (Either KontraLink String)
showOfferList = 
  let getOffers user = do
        mydocuments <- query $ GetDocumentsByUser user 
        usersICanView <- query $ GetUsersByFriendUserID $ userid user
        friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
        supervised'Documents <- query $ GetDocumentsBySupervisor user        
        return . filter ((==) (Signable Offer) . documenttype) $
           mydocuments ++ concat friends'Documents ++ supervised'Documents in
  showItemList' pageOffersList getOffers

showOrdersList :: Kontra (Either KontraLink String)
showOrdersList = 
  let getOrders user = do
        mydocuments <- query $ GetDocumentsByUser user 
        usersICanView <- query $ GetUsersByFriendUserID $ userid user
        friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
        supervised'Documents <- query $ GetDocumentsBySupervisor user
        return . filter ((==) (Signable Order) . documenttype) $
           mydocuments ++ concat friends'Documents ++ supervised'Documents in
  showItemList' pageOrdersList getOrders

showAttachmentList :: Kontra (Either KontraLink String)
showAttachmentList = 
  let getAttachments user = do
        mydocuments <- query $ GetDocumentsByAuthor (userid user)
        return $ filter ((==) Attachment . documenttype) mydocuments in
  showItemList' pageAttachmentList getAttachments

{- |
    Helper function for showing lists of documents.
-}
showItemList' :: (KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String)
                 -> (User -> Kontra [Document])
                 -> Kontra (Either KontraLink String)
showItemList' viewPage getDocs = checkUserTOSGet $ do
  Context {ctxmaybeuser = Just user, ctxtime, ctxtemplates} <- get
  docs <- getDocs user
  params <- getListParams
  liftIO $ viewPage ctxtemplates ctxtime user (docSortSearchPage params $ prepareDocsForList docs)
  
handleAttachmentViewForViewer :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra Response
handleAttachmentViewForViewer docid siglinkid mh = do
  edoc <- getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  case edoc of 
    Left _ -> mzero
    Right doc -> do
      ctx <- get
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
            pages <- liftIO $ Doc.DocView.showFilesImages2 (ctxtemplates ctx) (documentid doc) Nothing $ zip f b
            simpleResponse pages

handleAttachmentViewForAuthor :: DocumentID -> Kontra Response
handleAttachmentViewForAuthor docid = do
  edoc <- getDocByDocID docid
  case edoc of 
    Left _ -> mzero
    Right doc -> do
      ctx <- get
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
            pages <- liftIO $ Doc.DocView.showFilesImages2 (ctxtemplates ctx) (documentid doc) Nothing $ zip f b
            simpleResponse pages

-- get rid of duplicates
-- FIXME: nub is very slow
prepareDocsForList :: [Document] -> [Document]
prepareDocsForList = 
  sortBy (\d1 d2 -> compare (documentmtime d2) (documentmtime d1)) . nub

handlePageOfDocument :: DocumentID -> Kontra (Either KontraLink Response)
handlePageOfDocument docid = checkUserTOSGet $ handlePageOfDocument' docid Nothing

handlePageOfDocumentForSignatory :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra Response
handlePageOfDocumentForSignatory docid siglinkid sigmagichash = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    handlePageOfDocument' docid $ Just (siglinkid, sigmagichash)

handlePageOfDocument' :: DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> Kontra Response
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
      ctx <- get
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
                    pages <- liftIO $ Doc.DocView.showFilesImages2 (ctxtemplates ctx) documentid mtokens $ zip f b
                    simpleResponse pages

handleDocumentUpload :: DocumentID -> BS.ByteString -> BS.ByteString -> Kontra ()
handleDocumentUpload docid content1 filename = do
  liftIO $ print "Uploading doc"
  ctx@Context{ctxdocstore, ctxs3action} <- get
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

handleIssueNewDocument :: Kontra KontraLink
handleIssueNewDocument = withUserPost $ do
    input <- getDataFnM (lookInput "doc")
    mdoctype <- getDocType
    let doctype = fromMaybe (Signable Contract) mdoctype
    mdoc <- makeDocumentFromFile doctype input
    liftIO $ print mdoc
    case mdoc of
      Nothing -> return LinkMain
      (Just doc) -> return $ LinkIssueDoc $ documentid doc

handleCreateNewTemplate:: Kontra KontraLink
handleCreateNewTemplate = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  mdoc <- makeDocumentFromFile (Template Contract) input
  case mdoc of
    Nothing -> handleTemplateReload
    (Just doc) -> return $ LinkIssueDoc $ documentid doc

handleCreateNewAttachment:: Kontra KontraLink
handleCreateNewAttachment = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  _ <- makeDocumentFromFile Attachment input
  handleAttachmentReload

makeDocumentFromFile :: DocumentType -> Input -> Kontra (Maybe Document)
makeDocumentFromFile doctype (Input contentspec (Just filename) _contentType) = do
    Context { ctxmaybeuser = Just user, ctxtime } <- get
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
    if BSL.null content
      then do
        liftIO $ print "No content"
        return Nothing
      else do
          liftIO $ print "Got the content, creating document"
          let title = BS.fromString (basename filename)
          doc <- update $ NewDocument user title doctype ctxtime
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ Just doc
makeDocumentFromFile _ _ = mzero -- to complete the patterns

handleContractArchive :: Kontra KontraLink
handleContractArchive = do
    _ <- handleSignableArchive (Signable Contract)
    return $ LinkContracts emptyListParams

handleOffersArchive :: Kontra KontraLink
handleOffersArchive =  do
    _ <- handleSignableArchive (Signable Offer)
    return $ LinkOffers emptyListParams   

handleOrdersArchive :: Kontra KontraLink
handleOrdersArchive =  do
    _ <- handleSignableArchive (Signable Order)
    return $ LinkOrders emptyListParams

handleSignableArchive :: DocumentType -> Kontra ()
handleSignableArchive doctype =  do
    Context { ctxtemplates } <- get
    handleIssueArchive
    addFlashMsg =<< (liftIO $ flashMessageSignableArchiveDone ctxtemplates doctype)
    return ()

handleTemplateArchive :: Kontra KontraLink
handleTemplateArchive = do
    Context { ctxtemplates } <- get
    handleIssueArchive
    addFlashMsg =<< (liftIO $ flashMessageTemplateArchiveDone ctxtemplates)
    return $ LinkTemplates emptyListParams

handleAttachmentArchive :: Kontra KontraLink
handleAttachmentArchive = do
    Context { ctxtemplates } <- get
    handleIssueArchive
    addFlashMsg =<< (liftIO $ flashMessageAttachmentArchiveDone ctxtemplates)
    return $ LinkAttachments emptyListParams
    
handleIssueArchive :: Kontra ()
handleIssueArchive = do
    Context { ctxmaybeuser = Just user } <- get
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    let ids = map DocumentID idnumbers
    idsAndUsers <- mapM lookupUsersRelevantToDoc ids
    let uid = userid user
        uemail = unEmail $ useremail $ userinfo user
    res <- update $ ArchiveDocuments uid uemail idsAndUsers
    case res of
      Left msg -> do
        Log.debug $ "Failed to delete docs " ++ (show ids) ++ " : " ++ msg
        mzero
      Right _ -> return ()

handleTemplateShare :: Kontra KontraLink
handleTemplateShare = withUserPost $ do
    Context { ctxtemplates } <- get
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashMsg =<< (liftIO $ flashMessageSingleTemplateShareDone (documenttitle d) ctxtemplates)
      _ -> addFlashMsg =<< (liftIO $ flashMessageMultipleTemplateShareDone ctxtemplates) 
    return $ LinkTemplates emptyListParams

handleAttachmentShare :: Kontra KontraLink
handleAttachmentShare = withUserPost $ do
    Context { ctxtemplates } <- get
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashMsg =<< (liftIO $ flashMessageSingleAttachmentShareDone (documenttitle d) ctxtemplates)
      _ -> addFlashMsg =<< (liftIO $ flashMessageMultipleAttachmentShareDone ctxtemplates) 
    return $ LinkAttachments emptyListParams

handleIssueShare :: Kontra [Document]
handleIssueShare = do
    Context { ctxmaybeuser = Just user } <- get
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    let ids = map DocumentID idnumbers
    mdocs <- update $ ShareDocuments user ids
    case mdocs of
      Left _msg  -> mzero
      Right docs -> return docs

handleAttachmentRename :: DocumentID -> Kontra KontraLink
handleAttachmentRename docid = withUserPost $ do
  newname <- getCriticalField (return . BS.fromString) "docname"
  mdoc <- update $ SetDocumentTitle docid newname
  case mdoc of
    Left _msg -> mzero
    Right doc -> return $ LinkIssueDoc $ documentid doc
    
handleBulkContractRemind :: Kontra KontraLink
handleBulkContractRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Contract)
    return $ LinkContracts emptyListParams

handleBulkOfferRemind :: Kontra KontraLink
handleBulkOfferRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Offer)
    return $ LinkOffers emptyListParams   

handleBulkOrderRemind :: Kontra KontraLink
handleBulkOrderRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Order)
    return $ LinkOrders emptyListParams   

{- |
    This sends out bulk reminders.  The functionality is offered in the document
    and offers list page.  It will make sure the user is actually the author of everything,
    and send out reminders only to signatories who haven't accepted or signed on those that are 
    pending.  This returns all the signatory links that were reminded.
-}
handleIssueBulkRemind :: DocumentType -> Kontra [SignatoryLink]
handleIssueBulkRemind doctype = do
    ctx@(Context { ctxtemplates, ctxmaybeuser = Just user }) <- get
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    let ids = map DocumentID idnumbers
    remindedsiglinks <- fmap concat . sequence . map (\docid -> docRemind ctx user docid) $ ids
    case (length remindedsiglinks) of
      0 -> addFlashMsg =<< (liftIO $ flashMessageNoBulkRemindsSent ctxtemplates doctype)
      _ -> addFlashMsg =<< (liftIO $ flashMessageBulkRemindsSent ctxtemplates doctype)
    return remindedsiglinks
    where
      docRemind :: Context -> User -> DocumentID -> Kontra [SignatoryLink]
      docRemind ctx user docid = do
        doc <- queryOrFail $ GetDocumentByDocumentID docid
        failIfNotAuthor doc user
        case (documentstatus doc) of
          Pending -> do
            let isElegible = isEligibleForReminder (Just user) doc
                unsignedsiglinks = filter isElegible $ documentsignatorylinks doc
            sequence . map (sigRemind ctx doc) $ unsignedsiglinks
          _ -> return []
      sigRemind :: Context -> Document -> SignatoryLink -> Kontra SignatoryLink
      sigRemind ctx doc signlink = do
        mail <- liftIO $ mailDocumentRemind (ctxtemplates ctx) Nothing ctx doc signlink
        scheduleEmailSendout (ctxesenforcer ctx) $ mail {
              to = [MailAddress { fullname = signatoryname $ signatorydetails signlink
                                , email = signatoryemail $ signatorydetails signlink }]
            , mailInfo = Invitation (documentid doc) (signatorylinkid signlink)
        }
        return signlink

handleContractsReload :: Kontra KontraLink
handleContractsReload  = fmap LinkContracts getListParamsForSearch
    
handleTemplateReload :: Kontra KontraLink
handleTemplateReload = fmap LinkTemplates getListParamsForSearch

handleAttachmentReload :: Kontra KontraLink
handleAttachmentReload = fmap LinkAttachments getListParamsForSearch

handleOffersReload :: Kontra KontraLink
handleOffersReload = fmap LinkOffers getListParamsForSearch

handleOrdersReload :: Kontra KontraLink
handleOrdersReload = fmap LinkOrders getListParamsForSearch

{- |
   Get some html to display the images of the files
   URL: /pagesofdoc/{documentid}
   Method: GET
   FIXME: Should probably check for permissions to view
 -}
showPage :: DocumentID -> FileID -> Int -> Kontra (Either KontraLink Response)
showPage docid fileid = withAuthorOrFriend docid
    . checkUserTOSGet . showPage' fileid

showPageForSignatory :: DocumentID -> SignatoryLinkID -> MagicHash -> FileID -> Int -> Kontra Response
showPageForSignatory docid siglinkid sigmagichash fileid pageno = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    showPage' fileid pageno

showPage' :: FileID -> Int -> Kontra Response
showPage' fileid pageno = do
  Context{ctxnormalizeddocuments} <- get
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

handleCancel:: DocumentID -> Kontra KontraLink
handleCancel docid = withUserPost $ do
  ctx@Context { ctxtime, ctxipnumber } <- get
  edoc <- getDocByDocID docid
  case edoc of
    Left _ -> mzero
    Right doc -> do
      case documentstatus doc `elem` [Pending, AwaitingAuthor] of
        False -> addFlashMsg =<< (liftIO $ flashMessageCannotCancel (ctxtemplates ctx))
        True -> do
          customMessage <- getCustomTextField "customtext"  
          mdoc' <- update $ CancelDocument (documentid doc) ManualCancel ctxtime ctxipnumber
          case mdoc' of 
            Right doc' -> do
              sendCancelMailsForDocument customMessage ctx doc
              addFlashMsg =<< (liftIO $ flashMessageCanceled (ctxtemplates ctx) doc')
            Left errmsg -> addFlashMsg $ toFlashMsg OperationFailed errmsg
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

handleRestart:: DocumentID -> Kontra KontraLink
handleRestart docid = withUserPost $ do
  Context { ctxtemplates } <- get
  edoc  <- getDocByDocID docid
  case edoc of
    Left _ -> mzero
    Right doc -> do
      edoc2 <- restartDocument doc
      case edoc2 of
        Left _ -> mzero
        Right doc2 -> do
          addFlashMsg =<< (liftIO $ flashDocumentRestarted ctxtemplates doc2)
          return $ LinkIssueDoc docid
                    
handleResend:: DocumentID -> SignatoryLinkID -> Kontra KontraLink
handleResend docid signlinkid  = withUserPost $ do
  ctx@Context { ctxmaybeuser = Just user } <- get
  edoc <- getDocByDocID docid
  case edoc of
    Left _ -> mzero
    Right doc -> do
      failIfNotAuthor doc user -- only author can resend
      case getSigLinkBySigLinkID signlinkid doc of
        Nothing -> mzero
        Just signlink -> do
          customMessage <- getCustomTextField "customtext"  
          mail <- liftIO $  mailDocumentRemind (ctxtemplates ctx) customMessage ctx doc signlink
          scheduleEmailSendout (ctxesenforcer ctx) $ mail {
                to = [MailAddress { fullname = signatoryname $ signatorydetails signlink
                                  , email = signatoryemail $ signatorydetails signlink }]
              , mailInfo = Invitation  (documentid doc) (signatorylinkid signlink)
          }
          addFlashMsg =<< (liftIO $ flashRemindMailSent (ctxtemplates ctx) signlink)
          return (LinkIssueDoc docid)

{- |
    If the custom text field is empty then that's okay, but if it's invalid
    then we want to fail.
-}
getCustomTextField :: String -> Kontra (Maybe BS.ByteString)
getCustomTextField = getValidateAndHandle asValidInviteText customTextHandler
  where customTextHandler :: (Maybe String, Result BS.ByteString) -> Kontra (Maybe BS.ByteString)
        customTextHandler textresult =
                logIfBad textresult
                >>= flashValidationMessage
                >>= withFailureIfBad

--This only works for undelivered mails. We shoulkd check if current user is author
handleChangeSignatoryEmail :: DocumentID -> SignatoryLinkID -> Kontra KontraLink
handleChangeSignatoryEmail docid slid = withUserPost $ do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Just email -> do
      ctx@Context { ctxmaybeuser = Just user } <- get
      edoc <- getDocByDocID docid
      case edoc of
        Left _ -> return LinkMain
        Right doc -> do
          guard $ isUserAuthor doc user
          mnewdoc <- update $ ChangeSignatoryEmailWhenUndelivered docid slid email
          case mnewdoc of 
            Right newdoc -> do
              -- get (updated) siglink from updated document
              sl <- signatoryLinkFromDocumentByID newdoc slid
              liftIO $ sendInvitationEmail1 ctx newdoc sl
              return $ LinkIssueDoc $ docid
            _ -> return LinkMain
    _ -> return LinkMain

sendCancelMailsForDocument:: (Maybe BS.ByteString) -> Context -> Document -> Kontra ()
sendCancelMailsForDocument customMessage ctx document = do
  let activated_signatories = filter (isActivatedSignatory $ documentcurrentsignorder document) $ documentsignatorylinks document
  liftIO $ forM_ activated_signatories (scheduleEmailSendout (ctxesenforcer ctx) <=< (mailCancelDocumentByAuthor (ctxtemplates ctx) customMessage ctx document))

failIfNotAuthor :: Document -> User -> Kontra ()
failIfNotAuthor document user = guard (isUserAuthor document user)

checkLinkIDAndMagicHash :: Document -> SignatoryLinkID -> MagicHash -> Kontra ()
checkLinkIDAndMagicHash document linkid magichash1 = do
    case signlinkFromDocById document linkid of
      Just SignatoryLink { signatorymagichash } -> if signatorymagichash == magichash1 
                                                    then return ()
                                                    else mzero
      Nothing -> mzero
        
mainPage:: Kontra String
mainPage =  do
                      ctx <- get
                      params <- getListParams
                      showTemplates <- isFieldSet "showTemplates"
                      mdoctype <- getDocType
                      liftIO $ uploadPage ctx params mdoctype showTemplates

getDocType :: Kontra (Maybe DocumentType)
getDocType = getOptionalField asDocType "doctype"
  where
    asDocType :: String -> Result DocumentType
    asDocType val
      | val == show Offer = Good $ Signable Offer
      | val == show Contract = Good $ Signable Contract
      | val == show Order = Good $ Signable Order
      | otherwise = Empty

idmethodFromString :: String -> Maybe IdentificationType
idmethodFromString idmethod 
  | idmethod == "email" = Just EmailIdentification
  | idmethod == "eleg"  = Just ELegitimationIdentification
  | otherwise           = Nothing

getTemplatesForAjax::Kontra Response                      
getTemplatesForAjax = do
    ctx <- get 
    params <- getListParams
    mdoctype <- getDocType
    case (ctxmaybeuser ctx,mdoctype) of
            (Just user, Just doctype) -> do
                let tfilter doc = isTemplate doc && (matchingType doctype $ documenttype doc)
                documents <- liftIO $ query $ GetDocumentsByUser user
                let templates = filter tfilter documents
                content <- liftIO $ templatesForAjax (ctxtemplates ctx) (ctxtime ctx) user doctype $ docSortSearchPage params templates
                simpleResponse content
            (Nothing, _) -> sendRedirect $ LinkLogin NotLogged
            _ -> mzero
    
handleCreateFromTemplate::Kontra KontraLink
handleCreateFromTemplate = withUserPost $ do
  Context { ctxmaybeuser } <- get
  docid <- readField "template"
  case docid of 
    Just did -> do
      let user = fromJust ctxmaybeuser
      document <- queryOrFail $ GetDocumentByDocumentID $ did
      sharedWithUser <- isShared user document
      enewdoc <- case (isUserAuthor document user, sharedWithUser) of
        (True, _) -> update $ SignableFromDocumentID did
        (_, True) -> update $ SignableFromSharedDocumentID user did
        _ -> mzero
      case enewdoc of
        Right newdoc -> return $ LinkIssueDoc $ documentid newdoc
        Left _ -> mzero
    Nothing -> mzero
  where
    isShared :: User -> Document -> Kontra Bool
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
handleAttachmentDownloadForAuthor :: DocumentID -> FileID -> Kontra Response
handleAttachmentDownloadForAuthor did fid = do
  edoc <- getDocByDocID did
  case edoc of
    Left _ -> mzero
    Right doc -> case find (authorAttachmentHasFileID fid) (documentauthorattachments doc) of
      Just AuthorAttachment{ authorattachmentfile } -> do
        ctx <- get
        contents <- liftIO $ getFileContents ctx authorattachmentfile
        let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
            res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
        return res2
      Nothing -> case find (sigAttachmentHasFileID fid) (documentsignatoryattachments doc) of
        Just SignatoryAttachment{ signatoryattachmentfile = Just file } -> do
          ctx <- get
          contents <- liftIO $ getFileContents ctx file
          let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
              res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
          return res2
        _ -> mzero -- attachment with this file ID does not exist


{- |
   Stream the pdf document for the given FileID.
 -}
handleAttachmentDownloadForViewer :: DocumentID -> SignatoryLinkID -> MagicHash -> FileID -> Kontra Response
handleAttachmentDownloadForViewer did sid mh fid = do
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did sid mh
  case edoc of
    Left _ -> mzero
    Right doc -> case find (authorAttachmentHasFileID fid) (documentauthorattachments doc) of
      Just AuthorAttachment{ authorattachmentfile } -> do
        ctx <- get
        contents <- liftIO $ getFileContents ctx authorattachmentfile
        let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
            res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
        return res2
      Nothing -> case find (sigAttachmentHasFileID fid) (documentsignatoryattachments doc) of
        Just SignatoryAttachment{ signatoryattachmentfile = Just file } -> do
          ctx <- get
          contents <- liftIO $ getFileContents ctx file
          let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
              res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
          return res2
        _ -> mzero -- attachment with this file ID does not exist


{-
-- | temporary for migrating data into the document structure
-- Make sure to only allow superuser
migrateDocSigLinks :: Kontra Response
migrateDocSigLinks = onlySuperUser $ do
  docs <- query $ GetDocuments
  forM_ docs (\doc -> do
                author <- queryOrFail $ GetUserByUserID $ unAuthor $ documentauthor doc
                _ <- update $ MigrateToSigLinks (documentid doc) author
                return ())
  addFlashMsg $ toFlashMsg OperationDone "All documents migrated!"
  sendRedirect LinkMain

-}

handleMigrateDocumentAuthorAttachments :: Kontra Response
handleMigrateDocumentAuthorAttachments = do
  Context { ctxmaybeuser = Just user } <- get
  guard (useremail (userinfo user) == Email (BS.fromString "ericwnormand@gmail.com"))
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
  addFlashMsg $ toFlashMsg OperationDone "All documents migrated!"
  sendRedirect LinkMain

-- Fix for broken production | To be removed after fixing is done
isBroken::Document -> Bool
isBroken doc = documentstatus doc == Closed && (not $ Data.List.null $ documentfiles doc)  && (Data.List.null $ documentsealedfiles doc)

handleFixDocument::DocumentID -> Kontra KontraLink
handleFixDocument docid = onlySuperUser $ do
    ctx <- get
    mdoc <- query $ GetDocumentByDocumentID docid
    case (mdoc) of 
       Nothing -> return LoopBack
       Just doc -> if (isBroken doc)
                    then do
                        _ <- liftIO $ sealDocument ctx doc
                        return LoopBack
                    else return LoopBack


showDocumentsToFix::Kontra  String
showDocumentsToFix = onlySuperUser $ do
    ctx <- get
    docs <- query $ GetDocuments Nothing 
    liftIO $ documentsToFixView (ctxtemplates ctx) $ filter isBroken docs
    
handleSigAttach :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra KontraLink
handleSigAttach docid siglinkid mh = do
  liftIO $ print "handleSigAttach"
  edoc <- getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  case edoc of
    Left _ -> do
      liftIO $ print "Doc doesn't exist."
      mzero
    Right doc -> case signlinkFromDocById doc siglinkid of
      Nothing -> do
        liftIO $ print "No siglink."
        mzero
      Just siglink -> do
        attachname <- getCriticalField asValidFieldValue "attachname"
        let email = signatoryemail (signatorydetails siglink)
        case find (\sa -> signatoryattachmentemail sa == email
                          && signatoryattachmentname sa == attachname) (documentsignatoryattachments doc) of
          Nothing -> do
            liftIO $ print "No email."
            liftIO $ print $ show doc
            mzero
          Just _ -> do
            (Input contentspec _ _) <- getDataFnM (lookInput "sigattach")
            content1 <- case contentspec of
              Left filepath -> liftIO $ BSL.readFile filepath
              Right content -> return content
            -- we need to downgrade the PDF to 1.4 that has uncompressed structure
            -- we use gs to do that of course
            ctx <- get
            content <- liftIO $ preprocessPDF ctx (concatChunks content1) docid
            _ <- update $ SaveSigAttachment docid attachname email content
            return $ LinkSignDoc doc siglink
        

