{-# LANGUAGE CPP #-}
{- |
   DocControl represents the controler (in MVC) of the document.
 -}
module Doc.DocControl where

import ActionSchedulerState
import AppView
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import Data.Functor
import Debug.Trace
import Doc.CSVUtils
import Doc.DocState
import Doc.DocStateUtils
import Doc.DocView
import Doc.DocViewMail
import Doc.DocSeal
import Doc.DocStorage
import Data.Either
import HSP hiding (catch)
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import Happstack.Util.Common
import KontraLink
import Mails.SendMail
import MinutesTime
import Misc
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Process
import Kontra
import User.UserControl
import User.UserState
import User.UserView (prettyName, modalAccountRemoval)
import qualified Amazon as AWS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.HTTP as HTTP
import qualified SealSpec as Seal
import qualified TrustWeaver as TW
import qualified AppLogger as Log
import System.IO.Temp
import qualified MemCache
import Data.Char
import Data.Map ((!))
import InputValidation
import ListUtil
import Redirect
import Data.CSV
import Text.ParserCombinators.Parsec
import Codec.Text.IConv
import Routing

{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}

{- |
   Get the Just SignatoryLink from doc that has sid. Nothing when not found.
 -}
signlinkFromDocById :: Document -> SignatoryLinkID -> Maybe SignatoryLink
signlinkFromDocById doc sid = find ((== sid) . signatorylinkid) (documentsignatorylinks  doc)

{- |
   Perform the appropriate action when transitioning between documentstatuses.
   This function should always be called after changing the document.
 -}
postDocumentChangeAction :: Document -> Document -> Maybe SignatoryLinkID -> Kontra ()
postDocumentChangeAction document@Document  { documentstatus
                                            , documentsignatorylinks
                                            , documentid
                                            , documentauthor 
                                            , documentcancelationreason
                                            } 
                      olddocument@Document  { documentstatus = oldstatus }
                            msignalinkid
    -- No status change ; 
    | documentstatus == oldstatus =
        -- if sign order has changed, we need to send another invitations
        when (documentcurrentsignorder document /= documentcurrentsignorder olddocument) $ do
            ctx <- get
            Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor
            liftIO $ sendInvitationEmails ctx document author
    -- Preparation -> Pending
    -- main action: sendInvitationEmails
    | oldstatus == Preparation && documentstatus == Pending = do
        ctx <- get
        Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor
        Log.forkIOLogWhenError ("error in sending invitation emails for document " ++ show documentid) $ do
          sendInvitationEmails ctx document author
        return ()
    -- Preparation -> Closed (only author signs)
    -- main action: sealDocument and sendClosedEmails
    | oldstatus == Preparation && documentstatus == Closed = do
        liftIO $ print "Prep -> Closed"
        ctx@Context{ctxnormalizeddocuments,ctxhostpart,ctxtime} <- get
        Log.forkIOLogWhenError ("error sealing document " ++ show documentid)$ do
          Just user <- query $ GetUserByUserID (unAuthor (documentauthor))
          enewdoc <- sealDocument ctx user document
          case enewdoc of
               Right newdoc -> sendClosedEmailsToSignatories ctx newdoc
               Left errmsg -> do
                 update $ ErrorDocument documentid errmsg
                 Log.forkIOLogWhenError ("error in sending seal error emails for document " ++ show documentid) $ do
                       sendDocumentErrorEmailToAuthor ctx user document
                 return ()
        return ()
    -- Pending -> AwaitingAuthor
    -- main action: sendAwaitingEmail
    | oldstatus == Pending && documentstatus == AwaitingAuthor = do
        ctx <- get
        Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor
        Log.forkIOLogWhenError ("error in sending awaiting emails for document " ++ show documentid) $ do
          sendAwaitingEmail ctx document author
        return ()
    -- Pending -> Closed OR AwaitingAuthor -> Closed
    -- main action: sendClosedEmails
    | (oldstatus == Pending || oldstatus == AwaitingAuthor) && documentstatus == Closed = do
        ctx@Context{ctxnormalizeddocuments,ctxhostpart,ctxtime} <- get
        Log.forkIOLogWhenError ("error sealing document " ++ show documentid)$ do
          Just user <- query $ GetUserByUserID (unAuthor (documentauthor))
          enewdoc <- sealDocument ctx user document
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
        ctx@Context{ctxnormalizeddocuments,ctxhostpart,ctxtime} <- get
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
            author <- queryOrFail $ GetUserByUserID $ unAuthor $ documentauthor
            Log.forkIOLogWhenError ("error sending cancelation emails for document " ++ show documentid) $ do
                sendElegDataMismatchEmails ctx document author
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

sendElegDataMismatchEmails :: Context -> Document -> User -> IO ()
sendElegDataMismatchEmails ctx document author = do
    let authorid = unAuthor $ documentauthor document
        activatedbutauthor =
            (&&) <$> activatedSignatories (documentcurrentsignorder document)
                 <*> (maybe True (/= authorid)) . maybesignatory
        signlinks = filter activatedbutauthor (documentsignatorylinks document)
        Just (ELegDataMismatch msg badid _ _ _) = documentcancelationreason document
        badsig = fromJust $ find (\sl -> badid == signatorylinkid sl) (documentsignatorylinks document)
        badname  = BS.toString $ signatoryname $ signatorydetails badsig
        bademail = BS.toString $ signatoryemail $ signatorydetails badsig
    forM_ signlinks $ sendDataMismatchEmailSignatory ctx document author badid badname msg
    sendDataMismatchEmailAuthor ctx document author badname bademail

sendDataMismatchEmailSignatory :: Context -> Document -> User -> SignatoryLinkID -> String -> String -> SignatoryLink -> IO ()
sendDataMismatchEmailSignatory ctx document author badid badname msg signatorylink = do
    let SignatoryLink { signatorydetails, signatorylinkid } = signatorylink
        Document { documenttitle, documentid } = document
        isbad = badid == signatorylinkid
        
    mail <- mailMismatchSignatory 
                ctx 
                document 
                (BS.toString $ unEmail $ useremail $ userinfo author) 
                (BS.toString $ prettyName author) 
                (ctxhostpart ctx ++ (show $ LinkSignDoc document signatorylink))
                (BS.toString $ signatoryname signatorydetails) 
                badname 
                msg 
                isbad
    scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(signatoryname signatorydetails, signatoryemail signatorydetails)] }
          
sendDataMismatchEmailAuthor :: Context -> Document -> User -> String -> String -> IO ()
sendDataMismatchEmailAuthor ctx document author badname bademail = do
    let authorname = BS.toString $ prettyName author
    mail <- mailMismatchAuthor ctx document authorname badname bademail
    scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(userfullname author
                                                        , unEmail $ useremail $ userinfo author)] 
                                    }
    
{- |
   Send emails to all of the invited parties saying that we fucked up the process.
   Say sorry about this to them.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendDocumentErrorEmail :: Context -> Document -> IO ()
sendDocumentErrorEmail ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendDocumentErrorEmail1 ctx document)

sendDocumentErrorEmailToAuthor :: Context -> User -> Document -> IO ()
sendDocumentErrorEmailToAuthor ctx author document = do
  mail <- mailDocumentError (ctxtemplates ctx) ctx document
  scheduleEmailSendout (ctxesenforcer ctx) $
      mail { fullnameemails = [(userfullname author, unEmail $ useremail $ userinfo author)] }

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendDocumentErrorEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendDocumentErrorEmail1 ctx document signatorylink = do
  let SignatoryLink{ signatorylinkid
                   , signatorydetails
                   , signatorymagichash } = signatorylink
      Document{documenttitle,documentid} = document
  mail <- mailDocumentError (ctxtemplates ctx) ctx document
  scheduleEmailSendout (ctxesenforcer ctx) $ 
           mail { fullnameemails = [(signatoryname signatorydetails,signatoryemail signatorydetails)]
                , mailInfo = Invitation documentid  signatorylinkid
                }

{- |
   Send emails to all of the invited parties.
   ??: Should this be in DocControl or in an email-sepecific file?
 -}
sendInvitationEmails :: Context -> Document -> User -> IO ()
sendInvitationEmails ctx document author = do
  let signlinks = filter currentSigs $ documentsignatorylinks document
  forM_ signlinks (sendInvitationEmail1 ctx document author)
  where
      signorder = signatorysignorder . signatorydetails
      currentSigs =
          (&&) <$> (==) (documentcurrentsignorder document) . signorder
               <*> isNotLinkForUserID (userid author)

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendInvitationEmail1 :: Context -> Document -> User -> SignatoryLink -> IO ()
sendInvitationEmail1 ctx document author signatorylink = do
  let SignatoryLink{ signatorylinkid
                   , signatorydetails
                   , signatorymagichash
                   , signatoryroles } = signatorylink
      Document{documenttitle,documentid} = document
      authorid = unAuthor $ documentauthor document
      hasAuthorSigned = any (not . isNotLinkForUserID authorid) (documentsignatorylinks document)
      isSignatory = SignatoryPartner `elem` signatoryroles
  mail <- if isSignatory
          then if hasAuthorSigned 
               then mailInvitationToSign (ctxtemplates ctx) ctx document signatorylink author
               else mailInvitationToSend (ctxtemplates ctx) ctx document signatorylink author
          else mailInvitationToView (ctxtemplates ctx) ctx document signatorylink author

  attachmentcontent <- getFileContents ctx $ head $ documentfiles document
  scheduleEmailSendout (ctxesenforcer ctx) $ 
           mail { fullnameemails = [(signatoryname signatorydetails,signatoryemail signatorydetails)]
                , mailInfo = Invitation documentid signatorylinkid
                }

-- | Get signatories that has been 'activated',
-- i.e. link to sign view was sent to them
activatedSignatories :: SignOrder -> SignatoryLink -> Bool
activatedSignatories signorder siglink = 
    signorder >= (signatorysignorder $ signatorydetails siglink)

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
  let signlinks = documentsignatorylinks document
  forM_ signlinks $ \sl -> do 
                            when (not $ isAuthor document sl) $
                              sendClosedEmail1 ctx document sl

{- |
   Helper for sendClosedEmails
 -}
sendClosedEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendClosedEmail1 ctx document signatorylink = do
  let SignatoryLink{ signatorylinkid
                   , signatorymagichash
                   , signatorydetails } = signatorylink
      Document{documenttitle,documentid} = document
  mail <- mailDocumentClosedForSignatories (ctxtemplates ctx) ctx document signatorylink
  attachmentcontent <- getFileContents ctx $ head $ documentsealedfiles document
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails =  [(signatoryname signatorydetails,signatoryemail signatorydetails)],attachments = [(documenttitle,attachmentcontent)]}

{- |
   Send an email to the author when the document is awaiting approval
 -}
sendAwaitingEmail :: Context -> Document -> User -> IO ()
sendAwaitingEmail ctx document author = do
  let authorid = unAuthor $ documentauthor document
  Just authoruser <- query $ GetUserByUserID authorid
  mail <- mailDocumentAwaitingForAuthor (ctxtemplates ctx) ctx (userfullname authoruser)
             document
  let email1 = unEmail $ useremail $ userinfo authoruser
      name1 = userfullname authoruser
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(name1,email1)] }

{- |
   Send the email to the Author when the document is closed
 -}
sendClosedAuthorEmail :: Context -> Document -> IO ()
sendClosedAuthorEmail ctx document = do
  let authorid = unAuthor $ documentauthor document
  Just authoruser <- query $ GetUserByUserID authorid
  mail<- mailDocumentClosedForAuthor (ctxtemplates ctx) ctx (userfullname authoruser)
             document
  attachmentcontent <- getFileContents ctx $ head $ documentsealedfiles document
  let Document{documenttitle,documentid} = document
      email1 = unEmail $ useremail $ userinfo authoruser
      name1 = userfullname authoruser
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(name1,email1)], attachments = [(documenttitle,attachmentcontent)]}

{- |
   Send an email to the author when the document is rejected
 -}
sendRejectEmails :: (Maybe String) -> Context -> Document -> SignatoryLink -> IO ()
sendRejectEmails customMessage ctx document signalink = do
  let activated_signatories = filter (activatedSignatories $ documentcurrentsignorder document) $ documentsignatorylinks document
  forM_ activated_signatories $ \sl ->  
                     do
                      let semail = signatoryemail $ signatorydetails  sl
                      let sname = signatoryname $ signatorydetails  sl
                      mail <- mailDocumentRejected (ctxtemplates ctx) customMessage ctx sname document signalink
                      scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(sname,semail)]}
  when (not $ any (isAuthor document) $ documentsignatorylinks document) $ 
        do
          mauthor <- query $ GetUserByUserID (unAuthor $ documentauthor document) 
          case (mauthor) of
            Just user ->  do 
                      let aemail =  unEmail $ useremail $ userinfo $ user
                      let aname = userfullname user
                      mail <- mailDocumentRejected (ctxtemplates ctx) customMessage ctx aname document signalink
                      scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(aname,aemail)]}
            Nothing -> return ()
{- |
   Render a page of documents that a user has signed
   URL: /s
   Method: Get
   ??: Is this what it does?
 -}
handleSTable :: Kontra (Either KontraLink Response)
handleSTable = checkUserTOSGet $ do
      ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxtemplates}) <- get
      let user = fromJust ctxmaybeuser
      documents <- query $ GetDocumentsBySignatory user
      let notdeleted = filter (not . documentdeleted) documents
      let contracts = filter ((==) Contract . documenttype) notdeleted
      params <- getListParams
      content <- liftIO $ pageContractsList ctxtemplates ctxtime user (docSortSearchPage params contracts)
      renderFromBody ctx TopNone kontrakcja $ cdata content

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
          signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid1
          maybeuser <- query $ GetUserByEmail (Email $ signatoryemail (signatorydetails signatorylink))
          when_ (isNothing maybeuser) $ do
              ctx <- get
              let details = signatorydetails signatorylink
                  fullname = (signatoryfstname details, signatorysndname details)
                  email = signatoryemail details
                  company = signatorycompany details
              muser <- liftIO $ createUserBySigning ctx (documenttitle document) fullname email company (documentid, signatorylinkid1)
              when_ (isJust muser) $
                  update $ SaveDocumentForSignedUser documentid (userid $ fromJust muser) signatorylinkid1
          if (isContract document)
            then addModal $ modalSignedView document signatorylink (isJust maybeuser) (isJust ctxmaybeuser)
            else addModal $ modalOfferSigned document
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
  checkLinkIDAndMagicHash doc1 signatorylinkid1 magichash1
  Context { ctxtemplates
          , ctxmaybeuser
          , ctxhostpart
          , ctxtime
          , ctxipnumber 
          , ctxflashmessages } <- get
  
  update $ MarkDocumentSeen documentid signatorylinkid1 ctxtime ctxipnumber
  -- Structure of this needs to be changed. MR
  document <- queryOrFail $ GetDocumentByDocumentID documentid
  invitedlink <- signatoryLinkFromDocumentByID document signatorylinkid1
  let authoruserid = unAuthor $ documentauthor document
  author <- queryOrFail $ GetUserByUserID authoruserid
  let authorname = prettyName author
  let invitedname = signatoryname $ signatorydetails $ invitedlink 
  let isSignatory = SignatoryPartner `elem` signatoryroles invitedlink

  when (Data.List.null ctxflashmessages
        && (not (isJust $ maybesigninfo invitedlink))) $ do

    let message = if isSignatory
                  then if (isContract document ) -- Move all to flash messages in docView !!!!!!!!
                       then 
                           if document `allowsIdentification` ELegitimationIdentification
                           then "Du undertecknar dokumentet längst ned. Det krävs e-legitimation för att underteckna."
                           else "Underteckna dokumentet längst ned på sidan."
                       else  "Du kan acceptera offerten längst ned på sidan."
                  else "Du har endast rättigheter att se detta dokument"

    addFlashMsg $ toFlashMsg OperationDone message
  
  ctx <- get

  if isSignatory
     then renderFromBody ctx TopNone kontrakcja 
              (cdata <$> pageDocumentForSignatory (LinkSignDoc document invitedlink) 
                    document ctx invitedlink author)
     else renderFromBody ctx TopNone kontrakcja 
              (cdata <$> pageDocumentForViewer ctx document author (Just invitedlink))


{- |
   The user with id uid is a friend of user.
   Should be moved to User and imported
 -}
isFriendOf :: UserID -> User -> Bool
isFriendOf uid user = (unUserID uid `elem` map unFriend (userfriends user))

isFriendOf' :: UserID -> Maybe User -> Bool
isFriendOf' uid muser = fromMaybe False $ fmap (isFriendOf uid) muser

isFriendWithSignatory :: UserID -> Document -> IO Bool
isFriendWithSignatory uid document = do
                                   areFriends <- sequence $ map (isFriendWithSignatoryLink uid) $ documentsignatorylinks document
                                   return $ or areFriends
                                   
isFriendWithSignatoryLink :: UserID -> SignatoryLink -> IO Bool
isFriendWithSignatoryLink uid sl = do
                                     muser1 <- query $ GetUserByEmail $ Email $ signatoryemail $ signatorydetails $  sl
                                     muser2 <- sequenceMM $ fmap (query . GetUserByUserID) $ maybesignatory sl
                                     return $ (isFriendOf' uid muser1) || (isFriendOf' uid muser2)
{- |
   Handles the request to show a document to a user.
   User must be logged in.
   User must have signed TOS agreement.
   Document must exist.
   User must be authorized to view the document.
   There are two cases: 
    1. author in which case they get pageDocumentForAuthor
    2. Friend of author in which case they get pageDocumentForViewer
   URL: /d/{documentid}
   Method: GET
 -}
handleIssueShowGet :: DocumentID -> Kontra RedirectOrContent 
handleIssueShowGet docid = checkUserTOSGet $ do
  document@Document{ documentauthor } <- queryOrFail $ GetDocumentByDocumentID $ docid
  ctx@Context {
        ctxmaybeuser = Just (user@User{userid})
      , ctxipnumber
      , ctxhostpart
  } <- get
  author <- queryOrFail $ GetUserByUserID $ unAuthor documentauthor
  let toptab = if documentstatus document == Closed
                then TopDocument
                else TopNone
  -- authors get a view with buttons
  if isAuthor document user
   then do
        let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
        when ((documentstatus document == Canceled) && (isJust mMismatchMessage)) 
           (addFlashMsg $ toFlashMsg OperationFailed (fromJust mMismatchMessage))
        ctx2 <- get   -- need to get new context because we may have added flash msg
        step <- getDesignStep (documentid document)
        case (documentstatus document) of
           Preparation -> liftIO $ pageDocumentDesign ctx2 document author step
           _ ->  liftIO $ pageDocumentForAuthor ctx2 document author                
   -- friends can just look (but not touch)
   else do
        friendWithSignatory <- liftIO $ isFriendWithSignatory userid document 
        if (isFriendOf userid author || friendWithSignatory )
         then liftIO $ pageDocumentForViewer ctx document author Nothing
         -- not allowed
         else mzero

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
  document <- queryOrFail $ GetDocumentByDocumentID $ docid
  ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
  failIfNotAuthor document user
  let authorid = unAuthor $ documentauthor document
  Just author <- query $ GetUserByUserID authorid
  sign <- isFieldSet "sign"
  send <- isFieldSet "final"
  template <- isFieldSet "template"
  contract <- isFieldSet "contract"
  csvupload <- isFieldSet "csvupload"
  switchtoadvanced <- isFieldSet "changefunctionality"
  -- is this routing logic or business logic? I say business, but
  -- here it looks mixed.
  -- I'm especially asking about AwaitingAuthor case, because I though
  -- it was covered by SignDocument
  --   Eric
  case (documentstatus document,sign,send,template,contract,csvupload,switchtoadvanced) of 
      (Preparation, True, _,_, _, _ , _) -> handleIssueSign document author
      (Preparation, _ ,  True,_, _, _, _) -> handleIssueSend document author
      (Preparation, _ , _ ,True, _, _, _) -> handleIssueSaveAsTemplate document author             
      (Preparation, _ , _ ,_ , True, _, _) -> handleIssueChangeToContract document author
      (Preparation, _ , _ ,_ , _, True, _) -> handleIssueCSVUpload document author
      (Preparation, _ , _ ,_ , _, _, True) -> handleIssueChangeFunctionality document author                    
      (Preparation, _ , _ ,_, _, _, _) -> handleIssueSave document author
      (AwaitingAuthor, True , _ ,_, _, _, _) -> handleIssueSignByAuthor document author
      _  -> return $ LinkContracts emptyListParams

handleIssueSign document author = do
    ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
    -- unless (document `allowsIdentification` EmailIdentification) mzero | This need to be refactored | Breaks templates
    mudoc <- updateDocument ctx author document
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
        mndoc <- update $ AuthorSignDocument (documentid doc) ctxtime ctxipnumber author Nothing
        case mndoc of
          Right newdocument -> do
            postDocumentChangeAction newdocument udoc Nothing
            return ()
          Left _ -> return ()
        return mndoc

handleIssueSend document author = do
    ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
    mudoc <- updateDocument ctx author document
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
        mndoc <- update $ AuthorSendDocument (documentid doc) ctxtime ctxipnumber author Nothing
        case mndoc of
          Right newdocument -> do
            postDocumentChangeAction newdocument udoc Nothing
            return ()
          Left _ -> return ()
        return mndoc
    
handleIssueSaveAsTemplate document author = do
    ctx <- get
    mudoc <- updateDocument ctx author document
    case mudoc of 
        Right udoc -> do   
            mndoc <- update $ TemplateFromDocument $ documentid document
            case mndoc of
                Right newdocument -> do
                    addFlashMsg =<< (liftIO $ flashDocumentTemplateSaved $ ctxtemplates ctx)
                    return $ LinkTemplates emptyListParams       
                Left _ -> mzero
        Left _ -> mzero            

handleIssueChangeToContract document author = do
    ctx <- get
    mcontract <- update $ SignableFromDocumentID $ documentid document 
    case mcontract of 
        Right contract -> do   
            mncontract <- updateDocument ctx author contract
            case mncontract of
                Right ncontract ->  return $ LinkDesignDoc $ DesignStep3 $ documentid ncontract                        
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
          return $ Left $ LinkDesignDoc $ DesignStep2 (documentid doc) (fmap (+1) (csvPersonIndex doc)) (Just AfterCSVUpload)
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
handleIssueCSVUpload :: Document -> User -> Kontra KontraLink 
handleIssueCSVUpload document author = do
  ctx <- get             
  mudoc <- updateDocument ctx author document
  case mudoc of
    Left _ -> mzero
    Right udoc -> do
      mcsvpersonindex <- getOptionalField asValidNumber "csvpersonindex"
      mcsvfile <- getCSVFile "csv"
      case (mcsvpersonindex, mcsvfile) of
        (Nothing, Nothing) -> return $ LinkDesignDoc $ DesignStep2 (documentid udoc) Nothing Nothing
        (Nothing, Just _) ->  do
          Log.error "something weird happened, got csv file but there's no relevant person index"
          mzero
        (Just personindex, Nothing) -> return $ LinkDesignDoc $ DesignStep2 (documentid udoc) (Just $ personindex + 1) Nothing 
        (Just personindex, Just (title, contents)) ->  do
          let csvupload = CSVUpload 
                          { csvtitle = title
                          , csvcontents = contents
                          , csvsignatoryindex = personToSigIndexForDoc udoc personindex
                          }
          mndoc <- update $ AttachCSVUpload (documentid udoc) csvupload
          case mndoc of
            Left _ -> mzero
            Right ndoc -> return $ LinkDesignDoc $ DesignStep2 (documentid ndoc) (Just $ personindex + 1) (Just AfterCSVUpload)

{- |
    Deals with a switch to the document's functionality.
    This'll also update the user preferences that they set up
    in the switch confirmation dialog.
-}
handleIssueChangeFunctionality :: Document -> User -> Kontra KontraLink
handleIssueChangeFunctionality document author = do
  ctx <- get
  mudoc <- updateDocument ctx author document
  case mudoc of
    Left _ -> mzero
    Right udoc -> do
      muser <- handlePreferenceChange $ userid author
      case muser of
        Left _ -> mzero
        Right _ -> return $ LinkDesignDoc $ DesignStep2 (documentid udoc) Nothing Nothing
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
    

handleIssueSave document author = do
    ctx <- get
    updateDocument ctx author document
    if (isTemplate document) 
     then do
          addFlashMsg =<< (liftIO . flashDocumentTemplateSaved $ ctxtemplates ctx)
          return $ LinkTemplates emptyListParams
     else do
          addFlashMsg =<< (liftIO . flashDocumentDraftSaved $ ctxtemplates ctx)
          return $ LinkContracts emptyListParams
     
handleIssueSignByAuthor document author = do
    ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
    unless (document `allowsIdentification` EmailIdentification) mzero
    doc2 <- update $ CloseDocument (documentid document) ctxtime ctxipnumber user Nothing
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
    document@Document {
          documentauthor
        , documentid
    } <- queryOrFail $ GetDocumentByDocumentID $ docid
    let file = safehead "handleIssueShow" (case documentstatus document of
                                                Closed -> documentsealedfiles document
                                                _      -> documentfiles document)
    contents <- liftIO $ getFileContents ctx file
    let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
        res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
    return res2

-- | Check if current user is author or friend so he can view the document
withAuthorOrFriend :: DocumentID -> Kontra (Either KontraLink a) -> Kontra (Either KontraLink a)
withAuthorOrFriend docid action = (fmap join) $ withUserGet $ do
    doc@Document{documentauthor} <- queryOrFail $ GetDocumentByDocumentID $ docid
    ctx@Context {
          ctxmaybeuser = Just user@User{userid}
        , ctxipnumber
        , ctxhostpart
    } <- get
    author <- queryOrFail $ GetUserByUserID $ unAuthor documentauthor
    friendWithSignatory <- liftIO $ isFriendWithSignatory userid doc
    if isAuthor doc user || userid `isFriendOf` author || friendWithSignatory
       then action
       else mzero

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

mapJust :: (a -> Maybe b) -> [a] -> [b]
--mapJust = map fromJust . filter isJust . map
mapJust f ls = [l | Just l <- map f ls]

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


makeAuthorDetails pls fielddefs author = 
    (signatoryDetailsFromUser author)
    { signatoryemailplacements = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "email")
    , signatoryfstnameplacements = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "fstname")
    , signatorysndnameplacements = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "sndname")
    , signatorycompanyplacements = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "company")
    , signatorypersonalnumberplacements = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "personalnumber")
    , signatorycompanynumberplacements = filterPlacementsByID pls (BS.fromString "author") (BS.fromString "companynumber")
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
   Save a document.
   
   NOTE: This function is getting long. I am refactoring it --Eric
 -}
updateDocument :: Context -> User -> Document -> Kontra (Either String Document)
updateDocument ctx@Context{ctxtime,ctxipnumber} author document@Document{documentid,documentfunctionality} = do
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
  
  mcsvpersonindex <- getOptionalField asValidNumber "csvpersonindex"

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
  
  docfunctionality <- getCriticalField (asValidDocumentFunctionality author documentfunctionality) "docfunctionality"
  
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
                        
  let authordetails = (makeAuthorDetails placements fielddefs author) { signatorysignorder = SignOrder 0 } -- author has sign order set to 0 since he is 'the host' of the document
                        
  let isauthorsig = authorrole == "signatory"
      signatories2 = if isauthorsig
                       then (authordetails,[SignatoryPartner]) : zip signatories roles2
                       else zip signatories roles2
      roles2 = map guessRoles signatoriesroles
      guessRoles x | x == BS.fromString "signatory" = [SignatoryPartner]
                   | otherwise = []
      mcsvsigindex = fmap (personToSigIndex isauthorsig) mcsvpersonindex
  -- FIXME: tell the user what happened!
  -- when (daystosign<1 || daystosign>99) mzero

  --let emails = zip signatoriesemails 
  --              (sequence $ map (query . GetUserByEmail . Email) signatoriesemails)

  -- author is gotten above, no?
  -- Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor documentis

  if docfunctionality == BasicFunctionality
    then do
     --if they are switching to basic we want to lose information
     let basicauthordetails = removeFieldsAndPlacements authordetails
         basicsignatories = zip 
                             (basicauthordetails : 
                              take 1 (map (replaceSignOrder (SignOrder 1) . removeFieldsAndPlacements) signatories)) (repeat [SignatoryPartner])
     update $ UpdateDocument ctxtime documentid 
                basicsignatories Nothing invitetext author basicauthordetails docallowedidtypes Nothing docfunctionality
    else do
     update $ UpdateDocument ctxtime documentid
           signatories2  daystosign invitetext author authordetails docallowedidtypes mcsvsigindex docfunctionality

              
{- |
   Constructs a list of documents (Arkiv) to show to the user.
   The list contains all documents the user is an author on or
   is a friend of the author.
   Duplicates are removed.
 -}
showContractsList:: Kontra (Either KontraLink Response)
showContractsList= checkUserTOSGet $ do
  -- Just user is safe here because we guard for logged in user
  ctx@(Context {ctxmaybeuser = Just user, ctxhostpart, ctxtime, ctxtemplates}) <- get
  mydocuments <- query $ GetDocumentsByUser user 
  usersICanView <- query $ GetUsersByFriendUserID $ userid user
  friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
  -- get rid of duplicates
  let documents = nub $ mydocuments ++ concat friends'Documents
  let sorteddocuments = sortBy (\d1 d2 -> compare (documentmtime d2) (documentmtime d1)) documents
  let notdeleted = filter (not . documentdeleted) sorteddocuments
  let contracts  = filter ((==) Contract . documenttype) notdeleted
  params <- getListParams
  liftIO $ putStrLn $ show params
  content <- liftIO $ pageContractsList ctxtemplates ctxtime user (docSortSearchPage params contracts)
  renderFromBody ctx TopDocument kontrakcja $ cdata content

showTemplatesList:: Kontra (Either KontraLink Response)
showTemplatesList = checkUserTOSGet $ do
  -- Just user is safe here because we guard for logged in user
  ctx@(Context {ctxmaybeuser = Just user, ctxhostpart, ctxtime, ctxtemplates}) <- get
  mydocuments <- query $ GetDocumentsByUser user 
  let documents = nub $ mydocuments
  let sorteddocuments = sortBy (\d1 d2 -> compare (documentmtime d2) (documentmtime d1)) documents
  let notdeleted = filter (not . documentdeleted) sorteddocuments
  let templates = filter isTemplate notdeleted
  params <- getListParams
  content <- liftIO $ pageTemplatesList ctxtemplates ctxtime user (docSortSearchPage params templates)
  renderFromBody ctx TopDocument kontrakcja $ cdata content

handlePageOfDocument :: DocumentID -> Kontra (Either KontraLink Response)
handlePageOfDocument docid = withAuthorOrFriend docid
    . checkUserTOSGet $ handlePageOfDocument' docid Nothing

handlePageOfDocumentForSignatory :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra Response
handlePageOfDocumentForSignatory docid siglinkid sigmagichash = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    handlePageOfDocument' docid $ Just (siglinkid, sigmagichash)

handlePageOfDocument' :: DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> Kontra Response
handlePageOfDocument' documentid mtokens = do
    document@Document {
          documentfiles
        , documentsealedfiles
        , documentstatus
        , documentid
    } <- queryOrFail $ GetDocumentByDocumentID documentid
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

showOfferList:: Kontra (Either KontraLink Response)
showOfferList= checkUserTOSGet $ do
    -- Just user is safe here because we guard for logged in user
    ctx@(Context {ctxmaybeuser = Just user, ctxhostpart, ctxtime, ctxtemplates}) <- get
    mydocuments <- query $ GetDocumentsByUser user 
    usersICanView <- query $ GetUsersByFriendUserID $ userid user
    friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
    -- get rid of duplicates
    let documents = nub $ mydocuments ++ concat friends'Documents
    let sorteddocuments = sortBy (\d1 d2 -> compare (documentmtime d2) (documentmtime d1)) documents
    let notdeleted = filter (not . documentdeleted) sorteddocuments
    let contracts  = filter ((==) Offer . documenttype) notdeleted
    mauthors <- mapM (query . GetUserByUserID . unAuthor . documentauthor) contracts
    case sequence mauthors of
      Nothing -> mzero
      (Just authors) -> do
        params <- getListParams
        liftIO $ putStrLn $ show params
        content <- liftIO $ pageOffersList ctxtemplates ctxtime user (docAndAuthorSortSearchPage params (zip contracts authors))
        renderFromBody ctx TopDocument kontrakcja $ cdata content

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
    input@(Input contentspec (Just filename) _contentType) <- getDataFnM (lookInput "doc")
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content

    -- see if we have empty input, then there was no file selected
    if BSL.null content
       then do
         return LinkMain
        else do
          -- FIXME: here we have encoding issue
          -- Happstack gives use String done by BS.unpack, so BS.pack it here
          -- in our case it should be utf-8 as this is what we use everywhere
          offer <- isFieldSet "offer"               
          let title = BS.fromString (basename filename) 
          let doctype = if (offer) then Offer else Contract
          doc <- update $ NewDocument user title doctype ctxtime
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ LinkIssueDoc $ documentid doc

handleCreateNewTemplate:: Kontra KontraLink
handleCreateNewTemplate = withUserPost $ do
    ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) <- get
    input@(Input contentspec (Just filename) _contentType) <- getDataFnM (lookInput "doc")
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
    if BSL.null content
       then do
         handleTemplateReload
        else do
          let title = BS.fromString (basename filename) 
          let doctype = ContractTemplate
          doc <- update $ NewDocument user title doctype ctxtime
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ LinkIssueDoc $ documentid doc


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
    
handleIssueArchive :: Kontra ()
handleIssueArchive = do
    ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) <- get
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    liftIO $ putStrLn $ show idnumbers
    let ids = map DocumentID idnumbers
    docs <- mapM (query . GetDocumentByDocumentID) ids
    liftIO $ print docs
    update $ ArchiveDocuments user ids

handleTemplateShare :: Kontra KontraLink
handleTemplateShare = withUserPost $ do
    ctx@(Context { ctxtemplates }) <- get
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashMsg =<< (liftIO $ flashMessageSingleTemplateShareDone (documenttitle d) ctxtemplates)
      _ -> addFlashMsg =<< (liftIO $ flashMessageMultipleTemplateShareDone ctxtemplates) 
    return $ LinkTemplates emptyListParams

handleIssueShare :: Kontra [Document]
handleIssueShare = do
    ctx@(Context { ctxmaybeuser = Just user }) <- get
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    let ids = map DocumentID idnumbers
    mdocs <- update $ ShareDocuments user ids
    case mdocs of
      Left msg -> mzero
      Right docs -> return docs
    
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
            sequence . map (sigRemind ctx user doc) $ unsignedsiglinks
          _ -> return []
      sigRemind :: Context -> User -> Document -> SignatoryLink -> Kontra SignatoryLink
      sigRemind ctx author doc signlink = do
        mail <- liftIO $ mailDocumentRemind (ctxtemplates ctx) Nothing ctx doc signlink author
        scheduleEmailSendout (ctxesenforcer ctx) 
                         (mail {fullnameemails = [(signatoryname $ signatorydetails signlink,signatoryemail $ signatorydetails signlink )],
                          mailInfo = Invitation  (documentid doc) (signatorylinkid signlink) })
        return signlink

handleContractsReload :: Kontra KontraLink
handleContractsReload  = fmap LinkContracts getListParamsForSearch
    
handleTemplateReload :: Kontra KontraLink
handleTemplateReload = fmap LinkTemplates getListParamsForSearch

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
  doc <- queryOrFail $ GetDocumentByDocumentID docid
  failIfNotAuthor doc user
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
handleRestart docid = do
  ctx <- get
  mdoc <- query $ GetDocumentByDocumentID docid
  case (ctxmaybeuser ctx,mdoc) of
    (Just user,Just doc) -> do
      update $ RestartDocument docid user (ctxtime ctx) (ctxipnumber ctx)
      addFlashMsg =<< (liftIO $ flashDocumentRestarted (ctxtemplates ctx) doc)
      return $ LinkIssueDoc docid
    (Nothing,Just doc) -> return $ LinkLogin NotLogged
    _ -> mzero
                    
handleResend:: DocumentID -> SignatoryLinkID -> Kontra KontraLink
handleResend docid signlinkid  = withUserPost $ do
  ctx@Context { ctxmaybeuser = Just user } <- get
  doc <- queryOrFail $ GetDocumentByDocumentID docid
  failIfNotAuthor doc user
  signlink <- signatoryLinkFromDocumentByID doc signlinkid
  author <- queryOrFail $ GetUserByUserID $ unAuthor $ documentauthor doc
  customMessage <- getCustomTextField "customtext"  
  mail <- liftIO $  mailDocumentRemind (ctxtemplates ctx) customMessage ctx doc signlink author
  scheduleEmailSendout (ctxesenforcer ctx) (mail {fullnameemails = [(signatoryname $ signatorydetails signlink,signatoryemail $ signatorydetails signlink )],
                                                mailInfo = Invitation  (documentid doc) (signatorylinkid signlink) })
  addFlashMsg =<< (liftIO $ flashRemindMailSent (ctxtemplates ctx) signlink)
  return (LinkIssueDoc $ documentid doc)

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
handleChangeSignatoryEmail::String -> String -> Kontra KontraLink
handleChangeSignatoryEmail did slid = do
    let mdid = readM did
    memail <- getOptionalField asValidEmail "email"
    let mslid = readM slid
    case (mdid,mslid,memail) of
     (Just docid,Just slid,Just email) -> do
         ctx <- get
         md <- query $ GetDocumentByDocumentID docid
         when ((liftM2 isAuthor md $ ctxmaybeuser ctx) /= Just True) mzero
         mdoc <- update $ ChangeSignatoryEmailWhenUndelivered docid slid email
         let msl =  do 
                    doc <- either (const Nothing) Just mdoc
                    find ((== slid) . signatorylinkid) $ documentsignatorylinks doc 
         case (mdoc,msl,ctxmaybeuser ctx) of 
              (Right doc,Just sl,Just user) -> do
                    liftIO $ sendInvitationEmail1 ctx doc user sl
                    return $ LinkIssueDoc $ docid                                           
              _ -> return LinkMain                                                               
     _ -> return LinkMain  
                                     

sendCancelMailsForDocument:: (Maybe BS.ByteString) -> Context -> Document -> Kontra ()
sendCancelMailsForDocument customMessage ctx document = do
  let activated_signatories = filter (activatedSignatories $ documentcurrentsignorder document) $ documentsignatorylinks document
  Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor document
  liftIO $ forM_ activated_signatories (scheduleEmailSendout (ctxesenforcer ctx) <=< (mailCancelDocumentByAuthor (ctxtemplates ctx) customMessage ctx document author))

failIfNoDocument :: DocumentID -> Kontra ()
failIfNoDocument docid = do
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
    Just doc -> return ()
    Nothing  -> mzero

failIfNotAuthor :: Document -> User -> Kontra ()
failIfNotAuthor document user = when (not $ isAuthor document user) mzero

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
                let templates = filter (not . documentdeleted) allTemplates
                let templatesOfGoodType =  filter (matchingType doctype) templates
                content <- liftIO $ templatesForAjax (ctxtemplates ctx) (ctxtime ctx) user doctype $ docSortSearchPage params templatesOfGoodType
                simpleResponse content
            (Nothing,_) ->  sendRedirect $ LinkLogin NotLogged
            _ -> mzero
    
handleCreateFromTemplate::Kontra KontraLink
handleCreateFromTemplate = withUserPost $ do
     ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxtemplates}) <- get
     docid <- readField "template"
     case docid of 
         Just did -> do
             let user = fromJust ctxmaybeuser
             document@Document{ documentauthor } <- queryOrFail $ GetDocumentByDocumentID $ did
             isShared <- isShared user document
             newdoc <- case (isAuthor document user, isShared) of
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
         relatedaccounts <- query $ GetUserRelatedAccounts (userid user)
         return $ ((documentsharing document) == Shared)
                  && ((unAuthor $ documentauthor document) `elem` (map userid relatedaccounts))
