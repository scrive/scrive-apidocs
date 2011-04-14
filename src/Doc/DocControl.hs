{-# LANGUAGE CPP #-}
{- |
   DocControl represents the controler (in MVC) of the document.
 -}
module Doc.DocControl where

import ActionSchedulerState
import AppView
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
postDocumentChangeAction :: Document -> DocumentStatus -> Maybe SignatoryLinkID -> Kontra ()
postDocumentChangeAction document@Document  { documentstatus
                                            , documentsignatorylinks
                                            , documentid
                                            , documentauthor 
                                            , documentcancelationreason
                                            } 
                            oldstatus 
                            msignalinkid
    -- No status change ; no action
    | documentstatus == oldstatus = return ()
    -- Preparation -> Pending
    -- main action: sendInvitationEmails
    | oldstatus == Preparation && documentstatus == Pending = do
        ctx <- get
        Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor
        Log.forkIOLogWhenError ("error in sending invitation emails for document " ++ show documentid) $ do
          sendInvitationEmails ctx document author
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
        allbutauthor = filter ((maybe True (/= authorid)) . maybesignatory) 
                            (documentsignatorylinks document)
        Just (ELegDataMismatch msg badid _ _ _) = documentcancelationreason document
        badsig = fromJust $ find (\sl -> badid == signatorylinkid sl) (documentsignatorylinks document)
        badname  = BS.toString $ signatoryname $ signatorydetails badsig
        bademail = BS.toString $ signatoryemail $ signatorydetails badsig
    forM_ allbutauthor $ sendDataMismatchEmailSignatory ctx document author badid badname msg
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
  let signlinks = filter (isNotLinkForUserID $ userid author) $ documentsignatorylinks document
  forM_ signlinks (sendInvitationEmail1 ctx document author)

{- |
   Helper function to send emails to invited parties
   ??: Should this be in DocControl or in an email-specific file?
 -}
sendInvitationEmail1 :: Context -> Document -> User -> SignatoryLink -> IO ()
sendInvitationEmail1 ctx document author signatorylink = do
  let SignatoryLink{ signatorylinkid
                   , signatorydetails
                   , signatorymagichash } = signatorylink
      Document{documenttitle,documentid} = document
      authorid = unAuthor $ documentauthor document
      hasAuthorSigned = not $ Data.List.null $ filter (not . isNotLinkForUserID authorid) (documentsignatorylinks document)
  mail <- if hasAuthorSigned 
           then mailInvitationToSign (ctxtemplates ctx) ctx document signatorylink author
           else mailInvitationToSend (ctxtemplates ctx) ctx document signatorylink author
  attachmentcontent <- getFileContents ctx $ head $ documentfiles document
  scheduleEmailSendout (ctxesenforcer ctx) $ 
           mail { fullnameemails = [(signatoryname signatorydetails,signatoryemail signatorydetails)]
                , mailInfo = Invitation documentid signatorylinkid
                }

{- |
   Send emails to all parties when a document is closed.
 -}
sendClosedEmails :: Context -> Document -> IO ()
sendClosedEmails ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks $ \sl -> do 
                            when (not $ isAuthor document sl) $
                              sendClosedEmail1 ctx document sl
  sendClosedAuthorEmail ctx document

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
  forM_ (documentsignatorylinks document) $ \sl ->  
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
handleSTable :: Kontra Response
handleSTable = withUserGet $ checkUserTOSGet $ do
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
  document@Document{ documentstatus = olddocumentstatus, documentsignatorylinks } <- queryOrFail $ GetDocumentByDocumentID documentid

  checkLinkIDAndMagicHash document signatorylinkid1 magichash1

  fieldnames <- getAndConcat "fieldname"
  fieldvalues <- getAndConcat "fieldvalue"
  let fields = zip fieldnames fieldvalues

  let allowedidtypes = documentallowedidtypes document
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
          postDocumentChangeAction document olddocumentstatus (Just signatorylinkid1)
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
               -> MagicHash -- ^ The MagicHash that is in the URL (NOTE: This is ignored!)
               -> Kontra KontraLink
rejectDocument documentid 
               signatorylinkid1 
               magichash
                   = do
  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxipnumber}) <- get
  document@Document{ documentsignatorylinks } <- queryOrFail $ GetDocumentByDocumentID documentid

  checkLinkIDAndMagicHash document signatorylinkid1 magichash

  mdocument <- update $ RejectDocument documentid signatorylinkid1 ctxtime ctxipnumber
  case (mdocument) of
    Left message -> 
        do
          addFlashMsg $ toFlashMsg OperationFailed message
          return $ LinkMain
    Right document -> 
        do  
          postDocumentChangeAction document Pending (Just signatorylinkid1)
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

  tokens <- getDocumentTokens ctxipnumber (userid <$> ctxmaybeuser)
  when (Data.List.null ctxflashmessages
        && (not (isJust $ maybesigninfo invitedlink))) $ do
    let message = if document `allowsIdentification` ELegitimationIdentification
                    then "Du undertecknar dokumentet längst ned. Det krävs e-legitimation för att underteckna."
                    else "Underteckna dokumentet längst ned på sidan."
    addFlashMsg $ toFlashMsg OperationDone message
  
  ctx <- get
  renderFromBody ctx TopNone kontrakcja 
                       (cdata <$> pageDocumentForSignatory (LinkSignDoc document invitedlink) 
                                            document ctx invitedlink author tokens)

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
handleIssueShowGet :: DocumentID -> Kontra Response
handleIssueShowGet docid = withUserGet $ checkUserTOSGet $ do
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
  tokens <- getDocumentTokens ctxipnumber $ Just userid
  if isAuthor document user
   then do
        let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
        when ((documentstatus document == Canceled) && (isJust mMismatchMessage)) 
           (addFlashMsg $ toFlashMsg OperationFailed (fromJust mMismatchMessage))
        ctx2 <- get   
        step <- getDesignStep (documentid document)
        case (documentstatus document) of
           Preparation -> renderFromBody ctx2 toptab kontrakcja 
                            (cdata <$> pageDocumentDesign ctx2 document author tokens step)
           _ ->  renderFromBody ctx2 toptab kontrakcja 
                            (cdata <$> pageDocumentForAuthor ctx2 document author tokens)                
   -- friends can just look (but not touch)
   else do
        friendWithSignatory <- liftIO $ isFriendWithSignatory userid document 
        if (isFriendOf userid author || friendWithSignatory )
         then renderFromBody ctx toptab kontrakcja
                  (cdata <$> pageDocumentForViewer ctx document author tokens)
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
  -- is this routing logic or business logic? I say business, but
  -- here it looks mixed.
  -- I'm especially asking about AwaitingAuthor case, because I though
  -- it was covered by SignDocument
  --   Eric
  case (documentstatus document,sign,send,template,contract,csvupload) of 
      (Preparation, True, _,_, _, _ ) -> handleIssueSign document author
      (Preparation, _ ,  True,_, _, _) -> handleIssueSend document author
      (Preparation, _ , _ ,True, _, _) -> handleIssueSaveAsTemplate document author             
      (Preparation, _ , _ ,_ , True, _) -> handleIssueChangeToContract document author
      (Preparation, _ , _ ,_ , _, True) -> handleIssueCSVUpload document author                    
      (Preparation, _ , _ ,_, _, _) -> handleIssueSave document author
      (AwaitingAuthor, True , _ ,_, _, _) -> handleIssueSignByAuthor document author
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
                ([],ds) -> return $ LinkContracts emptyListParams
                _ -> mzero
            Left _ -> mzero
        Left _ -> mzero
    where
      forIndividual ctxtime ctxipnumber udoc doc = do
        mndoc <- update $ AuthorSignDocument (documentid doc) ctxtime ctxipnumber author Nothing
        case mndoc of
          Right newdocument -> do
            postDocumentChangeAction newdocument (documentstatus udoc) Nothing
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
                ([],ds) -> return $ LinkContracts emptyListParams
                _ -> mzero
            Left _ -> mzero
        Left _ -> mzero
    where
      forIndividual ctxtime ctxipnumber udoc doc = do
        mndoc <- update $ AuthorSendDocument (documentid doc) ctxtime ctxipnumber author Nothing
        case mndoc of
          Right newdocument -> do
            postDocumentChangeAction newdocument (documentstatus udoc) Nothing
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
    mcontract <- update $ SignableFromDocument $ documentid document 
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
splitUpDocument :: Document -> Kontra (Either String [Document])
splitUpDocument doc =
  case (documentcsvupload doc, getCSVCustomFields doc) of
    (Just _, Left msg) -> return $ Left msg
    (Nothing, _) -> return $ Right [doc]
    (Just csvupload, Right csvcustomfields) ->
      case (cleanCSVContents (documentallowedidtypes doc) (length csvcustomfields) $ csvcontents csvupload) of
        ((prob:_), _) -> return $ Left "data is not valid"
        ([], CleanCSVData{csvbody}) -> do
          mudoc <- if (isTemplate doc)
                    then return $ Right doc
                    else update $ TemplateFromDocument $ documentid doc
          case mudoc of
            (Left x) -> return $ Left x
            (Right udoc) -> do
              mdocs <- mapM (createDocFromRow udoc (csvsignatoryindex csvupload)) csvbody
              return $ if Data.List.null (lefts mdocs) then Right (rights mdocs) else Left . head . lefts $ mdocs
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
                  mcontents = fmap (filter (\r->(not $ isEmptyRow r))) . parse csvFile "" . (++"\n") . BS.toString . concatChunks $ content
              case mcontents of
                 Left _ -> return $ Bad flashMessageFailedToParseCSV
                 Right contents 
                   | length contents > rowlimit -> return . Bad $ flashMessageCSVHasTooManyRows rowlimit 
                   | otherwise -> return $ Good (title, map (map BS.fromString) contents)
    rowlimit = 500
    isEmptyRow [] = True
    isEmptyRow [""] = True
    isEmptyRow _ = False

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
            postDocumentChangeAction d AwaitingAuthor Nothing
            addFlashMsg =<< (liftIO $ flashAuthorSigned $ ctxtemplates ctx)
            return $ LinkIssueDoc (documentid document)
            
{- |
   Show the document with title in the url
   URL: /d/{documentid}/{title}
   Method: GET
 -}
handleIssueShowTitleGet :: ActionID -> MagicHash -> DocumentID -> String -> Kontra Response
handleIssueShowTitleGet aid hash docid _title = do
    verifyDocumentTokens aid hash
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

{- |
   Show the document with title in the url
   URL: /d/{documentid}/{title}
   Method: GET
 -}
handleFileGet :: FileID -> String -> Kontra Response
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
    
makeSignatoryNoPlacements sfn ssn se sc spn scn = 
    SignatoryDetails { signatoryfstname = sfn
                     , signatorysndname = ssn
                     , signatorycompany = sc
                     , signatorypersonalnumber = spn
                     , signatorycompanynumber = scn
                     , signatoryemail = se
                     , signatoryfstnameplacements = []
                     , signatorysndnameplacements = []
                     , signatorycompanyplacements = []
                     , signatoryemailplacements = []
                     , signatorypersonalnumberplacements = []
                     , signatorycompanynumberplacements = []
                     , signatoryotherfields = []
                     }
                     
makeSignatory pls fds sid sfn  ssn  se  sc  spn  scn =
    (makeSignatoryNoPlacements sfn ssn se sc spn scn)
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
                signatoriescompanies
                signatoriespersonalnumbers
                signatoriescompanynumbers
                signatoriesfstnames
                signatoriessndnames
    | sigids == [] = zipWith6 makeSignatoryNoPlacements 
                        signatoriesfstnames
                        signatoriessndnames
                        signatoriesemails
                        signatoriescompanies
                        signatoriespersonalnumbers
                        signatoriescompanynumbers
    | otherwise    = zipWith7 (makeSignatory placements fielddefs)
                        sigids
                        signatoriesfstnames
                        signatoriessndnames
                        signatoriesemails
                        signatoriescompanies
                        signatoriespersonalnumbers
                        signatoriescompanynumbers


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

{- |
   Save a document.
   
   NOTE: This function is getting long. I am refactoring it --Eric
 -}
updateDocument :: Context -> User -> Document -> Kontra (Either String Document)
updateDocument ctx@Context{ctxtime,ctxipnumber} author document@Document{documentid} = do
  -- each signatory has these predefined fields
  signatoriesfstnames        <- getAndConcat "signatoryfstname"
  signatoriessndnames        <- getAndConcat "signatorysndname"
  signatoriescompanies       <- getAndConcat "signatorycompany"
  signatoriespersonalnumbers <- getAndConcat "signatorypersonalnumber"
  signatoriescompanynumbers  <- getAndConcat "signatorycompanynumber"
  signatoriesemails'         <- getAndConcat "signatoryemail"
  let signatoriesemails = map (BSC.map toLower) signatoriesemails'
  signatoriesroles           <- getAndConcat "signatoryrole"
  liftIO $ print signatoriesroles

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
                        signatoriescompanies
                        signatoriespersonalnumbers
                        signatoriescompanynumbers
                        signatoriesfstnames
                        signatoriessndnames
                        
  let authordetails = makeAuthorDetails placements fielddefs author
                        
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
  -- Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor document

  update $ UpdateDocument ctxtime documentid
           signatories2 daystosign invitetext author authordetails docallowedidtypes mcsvsigindex
              
{- |
   Constructs a list of documents (Arkiv) to show to the user.
   The list contains all documents the user is an author on or
   is a friend of the author.
   Duplicates are removed.
 -}
showContractsList:: Kontra Response
showContractsList= withUserGet $ checkUserTOSGet $ do
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

showTemplatesList:: Kontra Response
showTemplatesList = withUserGet $ checkUserTOSGet $ do
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

handlePageOfDocument :: ActionID -> MagicHash -> DocumentID -> Kontra Response
handlePageOfDocument aid hash documentid = do
    verifyDocumentTokens aid hash
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
                    pages <- liftIO $ Doc.DocView.showFilesImages2 (ctxtemplates ctx) (aid, hash) $ zip f b
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
    handleIssueArchive
    return $ LinkContracts emptyListParams

{- |
    This sends out bulk contract reminders.  The functionality is offered in the document
    list page.  It will make sure the user is actually the author of all the documents,
    and send out reminders only to signatories who haven't signed on documents that are 
    pending.
-}
handleBulkContractRemind :: Kontra KontraLink
handleBulkContractRemind = withUserPost $ do
    ctx@(Context { ctxmaybeuser = Just user, ctxtemplates }) <- get
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    let ids = map DocumentID idnumbers
    remindedsiglinks <- fmap concat . sequence . map (\docid -> docRemind ctx user docid) $ ids
    case (length remindedsiglinks) of
      0 -> addFlashMsg =<< (liftIO $ flashMessageNoBulkRemindsSent ctxtemplates)
      n -> addFlashMsg =<< (liftIO $ flashMessageBulkRemindsSent ctxtemplates)
    return $ LinkContracts emptyListParams
    where
      docRemind :: Context -> User -> DocumentID -> Kontra [SignatoryLink]
      docRemind ctx user docid = do
        doc <- queryOrFail $ GetDocumentByDocumentID docid
        failIfNotAuthor doc user
        case (documentstatus doc) of
          Pending -> do
            let isElegible siglink = (isNothing $ maybesigninfo siglink) && (not $ isAuthor doc siglink)
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

handleTemplateArchive :: Kontra KontraLink
handleTemplateArchive = do
    handleIssueArchive
    return $ LinkTemplates emptyListParams
    
handleIssueArchive :: Kontra ()
handleIssueArchive = do
    ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) <- get
    idnumbers <- getCriticalFieldList asValidDocID "doccheck"
    liftIO $ putStrLn $ show idnumbers
    let ids = map DocumentID idnumbers
    update $ ArchiveDocuments user ids


handleContractsReload :: Kontra KontraLink
handleContractsReload  = fmap LinkContracts getListParamsForSearch
    
handleTemplateReload :: Kontra KontraLink
handleTemplateReload = fmap LinkTemplates getListParamsForSearch
    
{- |
   Get some html to display the images of the files
   URL: /pagesofdoc/{documentid}
   Method: GET
   FIXME: Should probably check for permissions to view
 -}
showPage :: ActionID -> MagicHash -> FileID -> Int -> Kontra Response
showPage aid hash fileid pageno = do
  verifyDocumentTokens aid hash
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
          addFlashMsg =<< (liftIO $ flashMessageCanceled (ctxtemplates ctx))
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
  case ctxmaybeuser ctx of
    Just user -> do
      update $ RestartDocument docid user (ctxtime ctx) (ctxipnumber ctx)
      addFlashMsg =<< (liftIO $ flashDocumentRestarted (ctxtemplates ctx))
      return $ LinkIssueDoc docid
    Nothing -> return $ LinkLogin NotLogged
                    
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
  Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor document
  liftIO $ forM_ (documentsignatorylinks document) (scheduleEmailSendout (ctxesenforcer ctx) <=< (mailCancelDocumentByAuthor (ctxtemplates ctx) customMessage ctx document author))

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

showMainPage::User -> Kontra Response
showMainPage user =  do
                      ctx <- get
                      params <- getListParams
                      showTemplates <- isFieldSet "showTemplates"
                      content <- liftIO $ uploadPage (ctxtemplates ctx) params showTemplates
                      renderFromBody ctx TopDocument kontrakcja $ cdata content 
                      
idmethodFromString :: String -> Maybe IdentificationType
idmethodFromString method 
    | method == "email" = Just EmailIdentification
    | method == "eleg"  = Just ELegitimationIdentification
    | otherwise         = Nothing

getTemplatesForAjax::Kontra Response                      
getTemplatesForAjax = do
    ctx <- get 
    params <- getListParams
    case (ctxmaybeuser ctx) of
            Just user -> do
                allTemplates <- liftIO $ query $ GetUserTemplates (userid user)
                let templates = filter (not . documentdeleted) allTemplates
                content <- liftIO $ templatesForAjax (ctxtemplates ctx) (ctxtime ctx) user $ docSortSearchPage params templates            
                simpleResponse content
            Nothing ->  sendRedirect $ LinkLogin NotLogged
    
handleCreateFromTemplate::Kontra KontraLink
handleCreateFromTemplate = do
     docid <- readField "template"
     case docid of 
         Just did -> do
             newdoc <- update $ SignableFromDocument did
             case newdoc of 
                 Right newdoc -> return $ LinkIssueDoc $ documentid newdoc   
                 Left _ -> mzero
         Nothing -> mzero

-- | Get existing document tokens for given user or create new and return them
getDocumentTokens :: MonadIO m => Word32 -> Maybe UserID -> m (ActionID, MagicHash)
getDocumentTokens ip muid = maybe createNewTicket (\uid -> do
    now <- liftIO getMinutesTime
    query (GetDocViewTicket now uid)
        >>= maybe createNewTicket (useExistingTicket now)
    ) muid
    where
        createNewTicket = do
            action <- liftIO $ newDocViewTicketAction ip muid
            return (actionID action, dvtToken $ actionType action)
        useExistingTicket now Action{actionID, actionType} = do
            update $ UpdateActionEvalTime actionID $ 10 `minutesAfter` now
            update $ UpdateActionType actionID $ actionType { dvtIP = ip }
            return (actionID, dvtToken actionType)

-- | Verify if given document token is valid
verifyDocumentTokens :: ActionID -> MagicHash -> Kontra ()
verifyDocumentTokens aid hash = do
    now <- liftIO getMinutesTime
    (checkValidity now <$> (query $ GetAction aid)) >>= maybe mzero checkAction
    where
        checkAction action@Action{actionType = DocViewTicket ip muid token} = do
            Context{ctxmaybeuser = muser, ctxipnumber = ctxip} <- get
            when ((userid <$> muser) /= muid || ctxip /= ip || hash /= token) mzero
        checkAction _ = mzero
