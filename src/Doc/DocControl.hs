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
import Doc.DocStateUtils
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
import Routing
import User.UserControl
import User.UserState
import User.UserView (prettyName, modalAccountRemoval)
import qualified Amazon as AWS
import qualified AppLogger as Log
import qualified SealSpec as Seal
import qualified TrustWeaver as TW
import Templates.TemplatesLoader

import Codec.Text.IConv
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Bits
import Data.CSV
import Data.Char
import Data.Either
import Data.Functor
import Data.List
import Data.Map ((!))
import Data.Maybe
import Data.Word
import Debug.Trace
import HSP hiding (catch)
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update, query)
import Happstack.Util.Common
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import Text.ParserCombinators.Parsec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified MemCache
import qualified Network.HTTP as HTTP


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
                                            , documentsignatorylinks
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
        Log.forkIOLogWhenError ("error in sending invitation emails for document " ++ show documentid) $ do
          sendInvitationEmails ctx document
        return ()
    -- Preparation -> Closed (only author signs)
    -- main action: sealDocument and sendClosedEmails
    | oldstatus == Preparation && documentstatus == Closed = do
        ctx@Context{ctxnormalizeddocuments,ctxhostpart,ctxtime} <- get
        Log.forkIOLogWhenError ("error sealing document " ++ show documentid)$ do
          enewdoc <- sealDocument ctx document
          case enewdoc of
            Right newdoc -> sendClosedEmailsToSignatories ctx newdoc
            Left errmsg -> do
              update $ ErrorDocument documentid errmsg
              Log.forkIOLogWhenError ("error in sending seal error emails for document " ++ show documentid) $ do
                sendDocumentErrorEmailToAuthor ctx document
              return ()
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
        ctx@Context{ ctxnormalizeddocuments, ctxhostpart, ctxtime} <- get
        Log.forkIOLogWhenError ("error sealing document " ++ show documentid) $ do
          enewdoc <- sealDocument ctx document
          case enewdoc of
            Right newdoc -> sendClosedEmails ctx newdoc
            Left errmsg -> do
              update $ ErrorDocument documentid errmsg
              Log.forkIOLogWhenError ("error in sending seal error emails for document " ++ show documentid) $ do
                sendDocumentErrorEmail ctx document
              return ()
        return ()
    -- Pending -> Rejected
    -- main action: sendRejectAuthorEmail
    | oldstatus == Pending && documentstatus == Rejected = do
        ctx@Context{ ctxnormalizeddocuments, ctxhostpart, ctxtime} <- get
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
    | DocumentError msg <- documentstatus = do
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
        Document { documenttitle, documentid } = document
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
        scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullname = signatoryname sigdets, email = signatoryemail sigdets }
          
sendDataMismatchEmailAuthor :: Context -> Document -> String -> String -> IO ()
sendDataMismatchEmailAuthor ctx document badname bademail = do
    let authorname = signatoryname $ signatorydetails $ fromJust $ getAuthorSigLink document
        authoremail = signatoryemail $ signatorydetails $ fromJust $ getAuthorSigLink document
    mail <- mailMismatchAuthor ctx document (BS.toString authorname) badname bademail
    scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullname = authorname, email = authoremail }
    
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
        fullname = signatoryname authordetails
      , email = signatoryemail authordetails
  }

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendDocumentErrorEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendDocumentErrorEmail1 ctx document signatorylink = do
  let SignatoryLink { signatorylinkid
                    , signatorydetails
                    , signatorymagichash } = signatorylink
      Document {documenttitle, documentid} = document
  mail <- mailDocumentError (ctxtemplates ctx) ctx document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        fullname = signatoryname signatorydetails
      , email = signatoryemail signatorydetails
      , mailInfo = Invitation documentid  signatorylinkid
  }

{- |
   Send emails to all of the invited parties.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendInvitationEmails :: Context -> Document -> IO ()
sendInvitationEmails ctx document = do
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
                    , signatorydetails
                    , signatorymagichash
                    , signatoryroles } = signatorylink
      Document {documenttitle, documentid} = document
      authorsiglink = fromJust $ getAuthorSigLink document
      hasAuthorSigned = isJust $ maybesigninfo authorsiglink
      isSignatory = SignatoryPartner `elem` signatoryroles
  mail <- if isSignatory
          then if hasAuthorSigned 
               then mailInvitationToSign (ctxtemplates ctx) ctx document signatorylink
               else mailInvitationToSend (ctxtemplates ctx) ctx document signatorylink
          else mailInvitationToView (ctxtemplates ctx) ctx document signatorylink

  attachmentcontent <- getFileContents ctx $ head $ documentfiles document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        fullname = signatoryname signatorydetails
      , email = signatoryemail signatorydetails
      , mailInfo = Invitation documentid signatorylinkid
  }

{- |
   Send emails to all parties when a document is closed.
 -}
sendClosedEmails :: Context -> Document -> IO ()
sendClosedEmails ctx document = do
  sendClosedEmailsToSignatories ctx document
  sendClosedAuthorEmail ctx document

{- | Send email only to non-author
-}
sendClosedEmailsToSignatories :: Context -> Document -> IO ()
sendClosedEmailsToSignatories ctx document = do
  let signlinks = [sl | sl <- documentsignatorylinks document
                      , not $ siglinkIsAuthor sl]
  forM_ signlinks $ sendClosedEmail1 ctx document

{- |
   Helper for sendClosedEmails
 -}
sendClosedEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendClosedEmail1 ctx document signatorylink = do
  let SignatoryLink { signatorylinkid
                    , signatorymagichash
                    , signatorydetails } = signatorylink
      Document {documenttitle, documentid} = document
  mail <- mailDocumentClosedForSignatories (ctxtemplates ctx) ctx document signatorylink
  attachmentcontent <- getFileContents ctx $ head $ documentsealedfiles document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        fullname = signatoryname signatorydetails
      , email = signatoryemail signatorydetails
      , attachments = [(documenttitle, attachmentcontent)]
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
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullname = authorname, email = authoremail }

{- |
   Send the email to the Author when the document is closed
 -}
sendClosedAuthorEmail :: Context -> Document -> IO ()
sendClosedAuthorEmail ctx document = do
  let Just authorsiglink = getAuthorSigLink document
      authoremail = signatoryemail $ signatorydetails authorsiglink
      authorname  = signatoryname  $ signatorydetails authorsiglink
  mail <- mailDocumentClosedForAuthor (ctxtemplates ctx) ctx authorname document
  attachmentcontent <- getFileContents ctx $ head $ documentsealedfiles document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail {
        fullname = authorname
      , email = authoremail
      , attachments = [(documenttitle document, attachmentcontent)]
  }

{- |
   Send an email to the author and to all signatories who were sent an invitation  when the document is rejected
 -}
sendRejectEmails :: (Maybe String) -> Context -> Document -> SignatoryLink -> IO ()
sendRejectEmails customMessage ctx document signalink = do
  let activatedSignatories = [sl | sl <- documentsignatorylinks document
                                 , isActivatedSignatory (documentcurrentsignorder document) sl]
  forM_ activatedSignatories $ \sl -> do
    let semail = signatoryemail $ signatorydetails  sl
        sname = signatoryname $ signatorydetails  sl
    mail <- mailDocumentRejected (ctxtemplates ctx) customMessage ctx sname document signalink
    scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullname = sname, email = semail }
  case getAuthorSigLink document of
    Nothing -> return ()
    Just authorsiglink -> do
      let authoremail = signatoryemail $ signatorydetails authorsiglink
          authorname  = signatoryname  $ signatorydetails authorsiglink
      mail <- mailDocumentRejected (ctxtemplates ctx) customMessage ctx authorname document signalink
      scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullname = authorname, email = authoremail }

-- END EMAILS

{- |
   Render a page of Contracts for a user
   URL: /s
   Method: Get
   ??: Is this what it does?
 -}
handleSTable :: Kontra (Either KontraLink Response)
handleSTable = checkUserTOSGet $ do
  ctx@Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime, ctxtemplates } <- get
  documents <- query $ GetDocumentsBySignatory user
  let contracts  = [doc | doc <- documents
                        , Contract == documenttype doc]
  params <- getListParams
  content <- liftIO $ pageContractsList ctxtemplates ctxtime user (docSortSearchPage params contracts)
  renderFromBody TopNone kontrakcja $ cdata content

{- |
    Handles an account setup from within the sign view.
-}
handleAcceptAccountFromSign :: DocumentID -> SignatoryLinkID -> MagicHash -> ActionID -> MagicHash -> Kontra KontraLink
handleAcceptAccountFromSign documentid
                            signatorylinkid
                            signmagichash
                            actionid
                            magichash = do
  ctx@Context { ctxtemplates } <- get
  muser <- handleAccountSetupFromSign actionid magichash
  document <- queryOrFail $ GetDocumentByDocumentID documentid
  signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
  case (muser, isContract document) of
    (Nothing, True) -> addModal $ modalContractSignedNoAccount document signatorylink actionid magichash
    (Nothing, False) -> addModal $ modalOfferSignedNoAccount document signatorylink actionid magichash
    (Just _, _) -> addFlashMsg =<< (liftIO $ flashMessageAccountActivatedFromSign ctxtemplates)
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
  ctx@Context{ctxtemplates} <- get
  document <- queryOrFail $ GetDocumentByDocumentID documentid
  signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
  handleAccountRemovalFromSign actionid magichash
  addFlashMsg =<< (liftIO $ flashMessageAccountRemovedFromSign ctxtemplates)
  return $ LinkSignDoc document signatorylink

{- |
   Control the signing of a document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
   Method: POST
   NOTE: magichash1 is currently ignored! (though it must exist)
 -}
signDocument :: DocumentID -- ^ The DocumentID of the document to sign 
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL 
             -> MagicHash -- ^ The MagicHash that is in the URL (NOTE: This is ignored!)
             -> Kontra KontraLink
signDocument documentid
             signatorylinkid1
             magichash1
                 = do            
  Context { ctxtime, ctxipnumber, ctxtemplates, ctxmaybeuser } <- get
  olddocument@Document{ documentsignatorylinks } <- queryOrFail $ GetDocumentByDocumentID documentid

  checkLinkIDAndMagicHash olddocument signatorylinkid1 magichash1

  fieldnames <- getAndConcat "fieldname"
  fieldvalues <- getAndConcat "fieldvalue"
  let fields = zip fieldnames fieldvalues

  let allowedidtypes = documentallowedidtypes olddocument
      allowsEmail = isJust $ find (== EmailIdentification) allowedidtypes

  when (not allowsEmail) mzero


  newdocument <- update $ SignDocument documentid signatorylinkid1 ctxtime ctxipnumber Nothing fields
  case newdocument of
    Left message -> 
        do
          addFlashMsg $ toFlashMsg OperationFailed message
          return $ LinkMain
    Right document -> 
        do 
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
          update $ SaveDocumentForSignedUser documentid (userid user) signatorylinkid
          if (isContract document)
            then addModal $ modalContractSignedNoAccount document signatorylink actionid magichash
            else addModal $ modalOfferSignedNoAccount document signatorylink actionid magichash
          return ()
        _ -> return ()
    (Just user) | (isContract document) ->
      addModal $ modalContractSignedHasAccount document signatorylink (isJust $ ctxmaybeuser ctx)
    (Just user) -> 
      addModal $ modalOfferSignedHasAccount document
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
               magichash
                   = do
  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxipnumber}) <- get
  olddocument@Document{ documentsignatorylinks } <- queryOrFail $ GetDocumentByDocumentID documentid

  checkLinkIDAndMagicHash olddocument signatorylinkid1 magichash
  customtext <- getCustomTextField "customtext"
  mdocument <- update $ RejectDocument documentid signatorylinkid1 ctxtime ctxipnumber customtext
  case (mdocument) of
    Left message -> 
        do
          addFlashMsg $ toFlashMsg OperationFailed message
          return $ LinkMain
    Right document -> 
        do  
          postDocumentChangeAction document olddocument (Just signatorylinkid1)
          addModal $ modalRejectedView document
          return $ LoopBack

{- |
   Get the SignatoryLink associated with a SignatoryLinkID or mzero if not found
 -}
signatoryLinkFromDocumentByID :: Document -> SignatoryLinkID -> Kontra SignatoryLink
signatoryLinkFromDocumentByID document@Document{documentsignatorylinks} linkid = do
    let invitedlinks = filter (\x -> signatorylinkid x == linkid
                               {- && signatorymagichash x == magichash1 -})
                              documentsignatorylinks
    case invitedlinks of
      [invitedlink] -> return invitedlink
      _ -> mzero
      
{- |
   Show the document to be signed
 -}
handleSignShow :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra Response
handleSignShow documentid 
               signatorylinkid1
               magichash1 = do
  doc1 <- queryOrFail $ GetDocumentByDocumentID documentid
  Context { ctxtemplates
          , ctxmaybeuser
          , ctxhostpart
          , ctxtime
          , ctxipnumber 
          , ctxflashmessages } <- get
  markDocumentSeen documentid signatorylinkid1 magichash1 ctxtime ctxipnumber
  edocument <- getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid1 magichash1
  case edocument of
    Left _ -> mzero
    Right document -> case getSigLinkBySigLinkID signatorylinkid1 document of
      Nothing -> mzero
      Just invitedlink -> case getAuthorSigLink document of
        Nothing -> mzero -- this means there is no author!
        Just authorsiglink -> do
          attachments <- queryOrFailIfLeft $ GetDocumentsByDocumentID $ documentattachments document
          let authorname = personname authorsiglink
              invitedname = signatoryname $ signatorydetails $ invitedlink 
              isSignatory = SignatoryPartner `elem` signatoryroles invitedlink
              isFlashNeeded = Data.List.null ctxflashmessages
                       && (not (isJust $ maybesigninfo invitedlink))
              -- heavens this is a confusing case statement, there must be a better way!
              flashMsg =
                case (isFlashNeeded, 
                      isSignatory, 
                      isContract document, 
                      document `allowsIdentification` ELegitimationIdentification,
                      isOffer document) of
                  (False, _, _, _, _) -> Nothing
                  (_, False, _, True, _) -> Just flashMessageOnlyHaveRightsToViewDoc
                  (_, False, _, _, True) -> Just flashMessageOnlyHaveRightsToViewDoc
                  (_, _, True, True, _) -> Just flashMessagePleaseSignWithEleg
                  (_, _, True, _, _) -> Just flashMessagePleaseSignContract
                  (_, _, _, _, True) -> Just flashMessagePleaseSignOffer
                  _ -> Nothing

          ctx@Context{ctxtemplates} <- get

          when (isJust flashMsg) $
            addFlashMsg =<< (liftIO $ (fromJust flashMsg) ctxtemplates)

          case (isAttachment document, isSignatory) of
            (True, _) -> 
              renderFromBody TopNone kontrakcja (cdata <$> pageAttachmentForSignatory ctx document invitedlink)
            (_, True) ->
              renderFromBody TopNone kontrakcja 
              (cdata <$> pageDocumentForSignatory (LinkSignDoc document invitedlink) 
               document attachments ctx invitedlink)
            _ ->
              renderFromBody TopNone kontrakcja 
              (cdata <$> pageDocumentForViewer ctx document attachments (Just invitedlink))

--end


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
handleIssueShowGet :: DocumentID -> Kontra RedirectOrContent
handleIssueShowGet docid = checkUserTOSGet $ do
  edoc <- getDocByDocID docid
  case edoc of
    Left _ -> mzero
    Right document -> do
        ctx@Context { ctxmaybeuser = Just (user@User{userid})
                    , ctxipnumber
                    , ctxhostpart
                    } <- get
        attachments <- queryOrFailIfLeft $ GetDocumentsByDocumentID $ documentattachments document
        -- authors get a view with buttons
        case (isUserAuthor document user, isAttachment document, documentstatus document) of
          (True, True, Preparation) -> liftIO $ pageAttachmentDesign ctx document
          (_, True, _) -> liftIO $ pageAttachmentView ctx document 
          (True, _, _) -> do
            let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
            when ((documentstatus document == Canceled) && (isJust mMismatchMessage)) 
              (addFlashMsg $ toFlashMsg OperationFailed (fromJust mMismatchMessage))
            ctx2 <- get   -- need to get new context because we may have added flash msg
            step <- getDesignStep (documentid document)
            case (documentstatus document) of
              Preparation -> liftIO $ pageDocumentDesign ctx2 document attachments step
              _ ->  liftIO $ pageDocumentForAuthor ctx2 document attachments               
          -- friends can just look (but not touch)
          (False, _, _) -> liftIO $ pageDocumentForViewer ctx document attachments Nothing

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
      ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
      guard (isUserAuthor document user) -- still need this because friend can read document
      sign <- isFieldSet "sign"
      send <- isFieldSet "final"
      template <- isFieldSet "template"
      contract <- isFieldSet "contract"
      csvupload <- isFieldSet "csvupload"
      updateattachments <- isFieldSet "updateattachments"
      switchtoadvanced <- isFieldSet "changefunctionality"
      -- is this routing logic or business logic? I say business, but
      -- here it looks mixed.
      -- I'm especially asking about AwaitingAuthor case, because I though
      -- it was covered by SignDocument
      --   Eric
      case (documentstatus document,sign,send,template,contract,csvupload,updateattachments,switchtoadvanced) of 
        (Preparation, True, _,_, _, _ , _, _) -> handleIssueSign document
        (Preparation, _ ,  True,_, _, _, _, _) -> handleIssueSend document
        (Preparation, _ , _ ,True, _, _, _, _) -> handleIssueSaveAsTemplate document
        (Preparation, _ , _ ,_ , True, _, _, _) -> handleIssueChangeToContract document
        (Preparation, _ , _ ,_ , _, True, _, _) -> handleIssueCSVUpload document
        (Preparation, _ , _ ,_ , _, _, True, _) -> handleIssueUpdateAttachments document
        (Preparation, _ , _ ,_ , _, _, _, True) -> handleIssueChangeFunctionality document
        (Preparation, _ , _ ,_, _, _, _, _) -> handleIssueSave document
        (AwaitingAuthor, True , _ ,_, _, _, _, _) -> handleIssueSignByAuthor document
        _  -> return $ LinkContracts emptyListParams

handleIssueSign document = do
    ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
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
                    addModal $ modalSignInviteView d
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
            finaliseAttachments newdocument
            postDocumentChangeAction newdocument udoc Nothing
            return ()
          Left _ -> return ()
        return mndoc

handleIssueSend document = do
    ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
    mudoc <- updateDocument ctx document
    case mudoc of 
        Right udoc-> do
          mdocs <- splitUpDocument udoc
          case mdocs of
            Right docs -> do
              mndocs <- mapM (forIndividual ctxtime ctxipnumber udoc) docs
              case (lefts mndocs, rights mndocs) of
                ([],d:[]) -> do
                    if (isOffer d) 
                     then addModal $ modalOfferCreated d
                     else addModal $ modalSignInviteView d
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
            finaliseAttachments newdocument
            postDocumentChangeAction newdocument udoc Nothing
            return ()
          Left _ -> return ()
        return mndoc

markDocumentAuthorReadAndSeen :: Document -> MinutesTime -> Word32 -> Kontra ()
markDocumentAuthorReadAndSeen doc@Document{documentid, documentsignatorylinks} time ipnumber =
  mapM_ mark $ filter siglinkIsAuthor documentsignatorylinks
  where
    mark SignatoryLink{signatorylinkid, signatorymagichash} = do
      update $ MarkInvitationRead documentid signatorylinkid time
      update $ MarkDocumentSeen documentid signatorylinkid signatorymagichash time ipnumber

finaliseAttachments :: Document -> Kontra ()
finaliseAttachments Document{documentattachments,documentsignatorylinks} = do
  _ <- update $ FinaliseAttachments documentattachments documentsignatorylinks
  return () 

handleIssueSaveAsTemplate document = do
  ctx <- get
  eudoc <- updateDocument ctx document
  case eudoc of 
    Left _ -> mzero
    Right udoc -> do   
      mndoc <- update $ TemplateFromDocument $ documentid document -- ?? Should be udoc? -EN
      case mndoc of
        Left _ -> mzero
        Right newdocument -> do
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
    (Just _, Left msg) -> mzero
    (Nothing, _) -> return $ Right [doc]
    (Just csvupload, Right csvcustomfields) ->
      case (cleanCSVContents (documentallowedidtypes doc) (length csvcustomfields) $ csvcontents csvupload) of
        ((prob:_), _) -> do
          Context{ctxtemplates} <- get
          addFlashMsg =<< (liftIO $ flashMessageInvalidCSV ctxtemplates)
          return $ Left $ LinkDesignDoc $ DesignStep2 (documentid doc) (Just (1 + csvsignatoryindex csvupload)) (Just AfterCSVUpload)
        ([], CleanCSVData{csvbody}) -> do
          mudoc <- if (isTemplate doc)
                    then return $ Right doc
                    else update $ TemplateFromDocument $ documentid doc
          case mudoc of
            (Left x) -> mzero
            (Right udoc) -> do
              mdocs <- mapM (createDocFromRow udoc (csvsignatoryindex csvupload)) csvbody
              if Data.List.null (lefts mdocs)
                then return $ Right (rights mdocs) 
                else mzero
  where createDocFromRow :: Document -> Int -> [BS.ByteString] -> Kontra (Either String Document)
        createDocFromRow udoc sigindex xs =
          update $ ContractFromSignatoryData (documentid udoc) sigindex (item 0) (item 1) (item 2) (item 3) (item 4) (item 5) (drop 6 xs)
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

handleIssueUpdateAttachments :: Document -> Kontra KontraLink
handleIssueUpdateAttachments doc = withUserPost $ do
    ctx <- get
    mudoc <- updateDocument ctx doc
    udoc <- returnRightOrMZero mudoc
    
    attidsnums <- getCriticalFieldList asValidDocID "attachmentid"
    removeatt <- getCriticalFieldList asValidBool "removeattachment"
    let idsforremoval = map (DocumentID . fst) . filter snd $ zip attidsnums removeatt
        Just user = ctxmaybeuser ctx
    fileinputs <- getDataFnM $ lookInputs "attachment"
    mattachments <- sequence $ map (makeDocumentFromFile Attachment) fileinputs
    let idsforadd = map documentid $ catMaybes mattachments

    mndoc <- update $ UpdateDocumentAttachments (userid user) (unEmail $ useremail $ userinfo user) (documentid udoc) idsforadd idsforremoval
    case mndoc of
        Left msg -> mzero
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
  result <- liftIO $ asCSVFile input
  flashValidationMessage (Nothing, result) >>= asMaybe
  where
    asCSVFile :: Maybe Input -> IO (Result (BS.ByteString, [[BS.ByteString]]))
    asCSVFile input = do
      case input of
        Nothing -> return Empty
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
    rowlimit = 500
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
    

handleIssueSave document = do
    ctx <- get
    updateDocument ctx document
    if (isTemplate document) 
     then do
          addFlashMsg =<< (liftIO . flashDocumentTemplateSaved $ ctxtemplates ctx)
          return $ LinkTemplates emptyListParams
     else do
          addFlashMsg =<< (liftIO . flashDocumentDraftSaved $ ctxtemplates ctx)
          return $ LinkContracts emptyListParams
     
handleIssueSignByAuthor document = do
    ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
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
getAndConcat field = do
  values <- getDataFnM $ lookInputList field
  return $ map concatChunks values

makePlacements :: [BS.ByteString]
               -> [BS.ByteString]
               -> [BS.ByteString]
               -> [BS.ByteString]
               -> [BS.ByteString]
               -> [BS.ByteString]
               -> [BS.ByteString]
               -> Kontra [(BS.ByteString, BS.ByteString, FieldPlacement)]
makePlacements placedsigids
               placedfieldids
               placedxs
               placedys
               placedpages
               placedwidths
               placedheights = do
    -- safely read to Ints
    placedxsf      <- mapM (readM . BS.toString) placedxs
    placedysf      <- mapM (readM . BS.toString) placedys
    placedpagesf   <- mapM (readM . BS.toString) placedpages
    placedwidthsf  <- mapM (readM . BS.toString) placedwidths
    placedheightsf <- mapM (readM . BS.toString) placedheights
    
    let placements = zipWith5 FieldPlacement 
                        placedxsf
                        placedysf
                        placedpagesf
                        placedwidthsf
                        placedheightsf
                   
    return $ zip3 placedsigids placedfieldids placements

filterPlacementsByID placements sigid fieldid =
    [x | (s, f, x) <- placements, s == sigid, f == fieldid]

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
    checkAllowed _ oldfunc newfunc
      | oldfunc==newfunc = return newfunc
      | otherwise = return newfunc --probably want to check what sort of account the user has here


{- |
   Save a document from data in the post params.
   
 -}
updateDocument :: Context -> Document -> Kontra (Either String Document)
updateDocument ctx@Context{ctxtime,ctxipnumber} document@Document{documentid,documentfunctionality} = do
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
  placedxs       <- getAndConcat "placedx"
  placedys       <- getAndConcat "placedy"
  placedpages    <- getAndConcat "placedpage"
  placedwidths   <- getAndConcat "placedwidth"
  placedheights  <- getAndConcat "placedheight"
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
      Just authorid = maybesignatory authorsiglink
  let authordetails = (makeAuthorDetails placements fielddefs $ signatorydetails authorsiglink) { signatorysignorder = SignOrder 0 } -- author has sign order set to 0 since he is 'the host' of the document
                        
  let isauthorsig = authorrole == "signatory"
      signatories2 = zip signatories roles2
      authordetails2 = (authordetails, if isauthorsig
                                       then [SignatoryPartner, SignatoryAuthor]
                                       else [SignatoryAuthor],
                                       authorid)
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
     let basicauthordetails = ((removeFieldsAndPlacements authordetails), [SignatoryPartner, SignatoryAuthor], authorid)
         basicsignatories = zip 
                             (take 1 (map (replaceSignOrder (SignOrder 1) . removeFieldsAndPlacements) signatories)) (repeat [SignatoryPartner])
     update $ UpdateDocument ctxtime documentid docname
                basicsignatories Nothing invitetext basicauthordetails docallowedidtypes Nothing docfunctionality
    else do
     update $ UpdateDocument ctxtime documentid docname
           signatories2 daystosign invitetext authordetails2 docallowedidtypes mcsvsigindex docfunctionality

              
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
        usersISupervise <- fmap Set.toList $ query $ GetUserSubaccounts $ userid user
        friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
        supervised'Documents <- mapM (query . GetDocumentsByUser) usersISupervise
        return . filter ((==) Contract . documenttype) $ 
          mydocuments ++ concat friends'Documents ++ concat supervised'Documents in
  showItemList' pageContractsList getContracts docSortSearchPage

showTemplatesList:: Kontra (Either KontraLink String)
showTemplatesList = 
  let getTemplates user = do
        mydocuments <- query $ GetDocumentsByUser user
        return $ filter isTemplate mydocuments in
  showItemList' pageTemplatesList getTemplates docSortSearchPage

showOfferList:: Kontra (Either KontraLink String)
showOfferList= checkUserTOSGet $ do
    -- Just user is safe here because we guard for logged in user
    ctx@(Context {ctxmaybeuser = Just user, ctxhostpart, ctxtime, ctxtemplates}) <- get
    mydocuments <- query $ GetDocumentsByUser user 
    usersICanView <- query $ GetUsersByFriendUserID $ userid user
    friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
    let contracts = prepareDocsForList . filter ((==) Offer . documenttype) $ 
                      mydocuments ++ concat friends'Documents
        authornames = map getAuthorName contracts
    params <- getListParams
    liftIO $ pageOffersList ctxtemplates ctxtime user (docAndAuthorSortSearchPage params (zip contracts authornames))

showAttachmentList :: Kontra (Either KontraLink String)
showAttachmentList = 
  let getAttachments user = do
        mydocuments <- query $ GetDocumentsByUser user
        return $ filter ((==) AttachmentTemplate . documenttype) mydocuments in
  showItemList' pageAttachmentList getAttachments docSortSearchPage

{- |
    Helper function for showing lists of documents.
-}
showItemList' :: (KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String)
                 -> (User -> Kontra [Document])
                 -> (ListParams -> [Document] -> PagedList Document)
                 -> Kontra (Either KontraLink String)
showItemList' viewPage getDocs sortSearchPage = checkUserTOSGet $ do
  ctx@(Context {ctxmaybeuser = Just user, ctxhostpart, ctxtime, ctxtemplates}) <- get
  docs <- getDocs user
  params <- getListParams
  liftIO $ viewPage ctxtemplates ctxtime user (sortSearchPage params $ prepareDocsForList docs)

-- get rid of duplicates
  -- FIXME: nub is very slow
prepareDocsForList :: [Document] -> [Document]
prepareDocsForList = 
  let makeunique = nub
      sort = sortBy (\d1 d2 -> compare (documentmtime d2) (documentmtime d1))
  in  sort . makeunique

handlePageOfDocument :: DocumentID -> Kontra (Either KontraLink Response)
handlePageOfDocument docid = checkUserTOSGet $ handlePageOfDocument' docid Nothing

handlePageOfDocumentForSignatory :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra Response
handlePageOfDocumentForSignatory docid siglinkid sigmagichash = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    handlePageOfDocument' docid $ Just (siglinkid, sigmagichash)

handlePageOfDocument' :: DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> Kontra Response
handlePageOfDocument' documentid mtokens = do
    
  edoc <- case mtokens of
    Nothing         -> getDocByDocID documentid
    Just (slid, mh) -> getDocByDocIDSigLinkIDAndMagicHash documentid slid mh
  case edoc of
    Left _ -> mzero
    Right document@Document { documentfiles
                            , documentsealedfiles
                            , documentstatus
                            , documentid
                            } -> do
      ctx@Context{ctxmaybeuser, ctxnormalizeddocuments} <- get
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
                    webHSP $ return $ cdata pages

handleDocumentUpload :: DocumentID -> BS.ByteString -> BS.ByteString -> Kontra ()
handleDocumentUpload docid content1 filename = do
  ctx@Context{ctxdocstore, ctxs3action} <- get
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  content <- liftIO $ preprocessPDF ctx content1 docid

  result <- update $ AttachFile docid filename content
  case result of
    Left err -> return ()
    Right document -> do
        liftIO $ forkIO $ mapM_ (AWS.uploadFile ctxdocstore ctxs3action) (documentfiles document)
        return ()
  return ()

basename :: String -> String
basename filename = 
    case break (\x -> (x=='\\') || (x=='/')) filename of
      (_,(_:rest)) -> basename rest
      _ -> takeWhile ((/=) '.') filename

handleIssueNewDocument :: Kontra KontraLink
handleIssueNewDocument = withUserPost $ do
    ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) <- get
    input <- getDataFnM (lookInput "doc")
    offer <- isFieldSet "offer"
    let doctype = if (offer) then Offer else Contract
    mdoc <- makeDocumentFromFile doctype input
    case mdoc of
      Nothing -> return LinkMain
      (Just doc) -> return $ LinkIssueDoc $ documentid doc

handleCreateNewTemplate:: Kontra KontraLink
handleCreateNewTemplate = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  mdoc <- makeDocumentFromFile ContractTemplate input
  case mdoc of
    Nothing -> handleTemplateReload
    (Just doc) -> return $ LinkIssueDoc $ documentid doc

handleCreateNewAttachment:: Kontra KontraLink
handleCreateNewAttachment = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  _ <- makeDocumentFromFile AttachmentTemplate input
  handleAttachmentReload

makeDocumentFromFile :: DocumentType -> Input -> Kontra (Maybe Document)
makeDocumentFromFile doctype input@(Input contentspec (Just filename) _contentType) = do
    ctx@(Context { ctxmaybeuser = Just user, ctxtime }) <- get
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
    if BSL.null content
        then return Nothing
        else do
          let title = BS.fromString (basename filename)
          doc <- update $ NewDocument user title doctype ctxtime
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ Just doc

handleContractArchive :: Kontra KontraLink
handleContractArchive = do
    ctx@(Context { ctxtemplates }) <- get
    handleIssueArchive
    addFlashMsg =<< (liftIO $ flashMessageContractArchiveDone ctxtemplates)
    return $ LinkContracts emptyListParams

handleOffersArchive :: Kontra KontraLink
handleOffersArchive =  do
    ctx@(Context { ctxtemplates }) <- get
    handleIssueArchive
    addFlashMsg =<< (liftIO $ flashMessageOfferArchiveDone ctxtemplates)
    return $ LinkOffers emptyListParams   

handleTemplateArchive :: Kontra KontraLink
handleTemplateArchive = do
    ctx@(Context { ctxtemplates }) <- get
    handleIssueArchive
    addFlashMsg =<< (liftIO $ flashMessageTemplateArchiveDone ctxtemplates)
    return $ LinkTemplates emptyListParams

handleAttachmentArchive :: Kontra KontraLink
handleAttachmentArchive = do
    ctx@(Context { ctxtemplates }) <- get
    handleIssueArchive
    addFlashMsg =<< (liftIO $ flashMessageAttachmentArchiveDone ctxtemplates)
    return $ LinkAttachments emptyListParams
    
handleIssueArchive :: Kontra ()
handleIssueArchive = do
    ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) <- get
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    liftIO $ putStrLn $ show idnumbers
    let ids = map DocumentID idnumbers
    docs <- mapM (query . GetDocumentByDocumentID) ids
    liftIO $ print docs
    let uid = userid user
        uemail = unEmail $ useremail $ userinfo user
    update $ ArchiveDocuments uid uemail ids

handleTemplateShare :: Kontra KontraLink
handleTemplateShare = withUserPost $ do
    ctx@(Context { ctxtemplates }) <- get
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashMsg =<< (liftIO $ flashMessageSingleTemplateShareDone (documenttitle d) ctxtemplates)
      _ -> addFlashMsg =<< (liftIO $ flashMessageMultipleTemplateShareDone ctxtemplates) 
    return $ LinkTemplates emptyListParams

handleAttachmentShare :: Kontra KontraLink
handleAttachmentShare = withUserPost $ do
    ctx@(Context { ctxtemplates }) <- get
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashMsg =<< (liftIO $ flashMessageSingleAttachmentShareDone (documenttitle d) ctxtemplates)
      _ -> addFlashMsg =<< (liftIO $ flashMessageMultipleAttachmentShareDone ctxtemplates) 
    return $ LinkAttachments emptyListParams

handleIssueShare :: Kontra [Document]
handleIssueShare = do
    ctx@(Context { ctxmaybeuser = Just user }) <- get
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    let ids = map DocumentID idnumbers
    mdocs <- update $ ShareDocuments user ids
    case mdocs of
      Left msg -> mzero
      Right docs -> return docs

handleAttachmentRename :: DocumentID -> Kontra KontraLink
handleAttachmentRename docid = withUserPost $ do
  ctx@(Context { ctxmaybeuser = Just user }) <- get
  newname <- getCriticalField (return . BS.fromString) "docname"
  mdoc <- update $ SetDocumentTitle docid newname
  case mdoc of
    Left msg -> mzero
    Right doc -> return $ LinkIssueDoc $ documentid doc
    
handleBulkContractRemind :: Kontra KontraLink
handleBulkContractRemind = withUserPost $ do
    ctx@(Context { ctxtemplates }) <- get
    remindedsiglinks <- handleIssueBulkRemind
    case (length remindedsiglinks) of
      0 -> addFlashMsg =<< (liftIO $ flashMessageNoBulkContractRemindsSent ctxtemplates)
      n -> addFlashMsg =<< (liftIO $ flashMessageBulkContractRemindsSent ctxtemplates)
    return $ LinkContracts emptyListParams

handleBulkOfferRemind :: Kontra KontraLink
handleBulkOfferRemind =  withUserPost $ do
    ctx@(Context { ctxtemplates }) <- get
    remindedsiglinks <- handleIssueBulkRemind
    case (length remindedsiglinks) of
      0 -> addFlashMsg =<< (liftIO $ flashMessageNoBulkOfferRemindsSent ctxtemplates)
      n -> addFlashMsg =<< (liftIO $ flashMessageBulkOfferRemindsSent ctxtemplates)
    return $ LinkOffers emptyListParams   

{- |
    This sends out bulk reminders.  The functionality is offered in the document
    and offers list page.  It will make sure the user is actually the author of everything,
    and send out reminders only to signatories who haven't accepted or signed on those that are 
    pending.  This returns all the signatory links that were reminded.
-}
handleIssueBulkRemind :: Kontra [SignatoryLink]
handleIssueBulkRemind = do
    ctx@(Context { ctxmaybeuser = Just user }) <- get
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    let ids = map DocumentID idnumbers
    remindedsiglinks <- fmap concat . sequence . map (\docid -> docRemind ctx user docid) $ ids
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
              fullname = signatoryname $ signatorydetails signlink
            , email = signatoryemail $ signatorydetails signlink
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
  ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber } <- get
  edoc <- getDocByDocID docid
  case edoc of
    Left _ -> mzero
    Right doc -> do
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
                fullname = signatoryname $ signatorydetails signlink
              , email = signatoryemail $ signatorydetails signlink
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
        customTextHandler result =
                logIfBad result
                >>= flashValidationMessage
                >>= withFailureIfBad

--This only works for undelivered mails. We shoulkd check if current user is author
-- ?? Should this really accept Strings or DocumentID -> SignatoryLinkID ? -EN
handleChangeSignatoryEmail :: String -> String -> Kontra KontraLink
handleChangeSignatoryEmail did slid = withUserPost $ do
    let mdid = readM did
    memail <- getOptionalField asValidEmail "email"
    let mslid = readM slid
    case (mdid,mslid,memail) of
     (Just docid,Just slid,Just email) -> do
         ctx@Context { ctxmaybeuser = Just user } <- get
         edoc <- getDocByDocID docid
         case edoc of
           Left _ -> return LinkMain
           Right doc -> do
             guard $ isUserAuthor doc user
             mdoc <- update $ ChangeSignatoryEmailWhenUndelivered docid slid email
             case (mdoc, ctxmaybeuser ctx) of 
               (Right doc, Just user) -> do
                   -- get (updated) siglink from updated document
                   sl <- signatoryLinkFromDocumentByID doc slid
                   liftIO $ sendInvitationEmail1 ctx doc sl
                   return $ LinkIssueDoc $ docid
               _ -> return LinkMain
     _ -> return LinkMain

sendCancelMailsForDocument:: (Maybe BS.ByteString) -> Context -> Document -> Kontra ()
sendCancelMailsForDocument customMessage ctx document = do
  let activated_signatories = filter (isActivatedSignatory $ documentcurrentsignorder document) $ documentsignatorylinks document
  liftIO $ forM_ activated_signatories (scheduleEmailSendout (ctxesenforcer ctx) <=< (mailCancelDocumentByAuthor (ctxtemplates ctx) customMessage ctx document))

failIfNoDocument :: DocumentID -> Kontra ()
failIfNoDocument docid = do
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
    Just doc -> return ()
    Nothing  -> mzero

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

getDocType::Kontra (Maybe DocumentType)
getDocType = readField "doctype"

idmethodFromString :: String -> Maybe IdentificationType
idmethodFromString method 
    | method == "email" = Just EmailIdentification
    | method == "eleg"  = Just ELegitimationIdentification
    | otherwise         = Nothing

getTemplatesForAjax::Kontra Response                      
getTemplatesForAjax = do
    ctx <- get 
    params <- getListParams
    mdoctype <- getDocType
    case (ctxmaybeuser ctx,mdoctype) of
            (Just user, Just doctype) -> do
                userTemplates <- liftIO $ query $ GetUserTemplates (userid user)
                relatedUsers <- liftIO $ query $ GetUserRelatedAccounts (userid user)
                sharedTemplates <- liftIO $ query $ GetSharedTemplates (map userid relatedUsers)
                let allTemplates = userTemplates ++ sharedTemplates
                let templates = allTemplates
                let templatesOfGoodType =  filter (matchingType doctype) templates
                content <- liftIO $ templatesForAjax (ctxtemplates ctx) (ctxtime ctx) user doctype $ docSortSearchPage params templatesOfGoodType
                simpleResponse content
            (Nothing, _) -> sendRedirect $ LinkLogin NotLogged
            _ -> mzero
    
handleCreateFromTemplate::Kontra KontraLink
handleCreateFromTemplate = withUserPost $ do
     ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxtemplates}) <- get
     docid <- readField "template"
     case docid of 
         Just did -> do
             let user = fromJust ctxmaybeuser
             document <- queryOrFail $ GetDocumentByDocumentID $ did
             isShared <- isShared user document
             newdoc <- case (isUserAuthor document user, isShared) of
                         (True, _) -> update $ SignableFromDocumentID did
                         (_, True) -> update $ SignableFromSharedDocumentID user did
                         _ -> mzero
             case newdoc of
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
