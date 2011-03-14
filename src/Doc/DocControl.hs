{-# LANGUAGE CPP #-}
{- |
   DocControl represents the controler (in MVC) of the document.
 -}
module Doc.DocControl where

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
import Debug.Trace
import Doc.DocState
import Doc.DocView
import Doc.DocViewMail
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
import User.UserView (prettyName)
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

getFileContents :: Context -> File -> IO (BS.ByteString)
getFileContents ctx file = do
  result <- MemCache.get (fileid file) (ctxfilecache ctx)
  case result of
    Just result -> return result
    Nothing -> do
                result <- AWS.getFileContents (ctxs3action ctx) file
                MemCache.put (fileid file) result (ctxfilecache ctx)
                return result
                          
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
postDocumentChangeAction document@Document{documentstatus, documentsignatorylinks, documentid, documentauthor} oldstatus msignalinkid
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
          enewdoc <- sealDocument ctx ctxnormalizeddocuments ctxhostpart ctxtime user document
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
        customMessage <- getField "customtext"
        Log.forkIOLogWhenError ("error in sending rejection emails for document " ++ show documentid) $ do
          sendRejectEmails customMessage ctx document (fromJust msignalink)
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
  sendMail (ctxmailer ctx) $ 
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
  sendMail (ctxmailer ctx) $ 
           mail { fullnameemails = [(signatoryname signatorydetails,signatoryemail signatorydetails)]
                , attachments = [(documenttitle,attachmentcontent)]
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
  sendMail  (ctxmailer ctx) $ mail { fullnameemails =  [(signatoryname signatorydetails,signatoryemail signatorydetails)]
                                     , attachments = [(documenttitle,attachmentcontent)]}

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
  sendMail (ctxmailer ctx) $ mail { fullnameemails = [(name1,email1)] }

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
  sendMail (ctxmailer ctx) $ mail { fullnameemails = [(name1,email1)], attachments = [(documenttitle,attachmentcontent)]}

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
                      sendMail  (ctxmailer ctx) $ mail { fullnameemails = [(sname,semail)]}
  when (not $ any (isAuthor document) $ documentsignatorylinks document) $ 
        do
          mauthor <- query $ GetUserByUserID (unAuthor $ documentauthor document) 
          case (mauthor) of
            Just user ->  do 
                      let aemail =  unEmail $ useremail $ userinfo $ user
                      let aname = userfullname user
                      mail <- mailDocumentRejected (ctxtemplates ctx) customMessage ctx aname document signalink
                      sendMail (ctxmailer ctx) $ mail { fullnameemails = [(aname,aemail)]}
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
      content <- liftIO $ pageDocumentList ctxtemplates ctxtime user documents
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
  Context { ctxtime, ctxipnumber, ctxtemplates } <- get
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
          return $ LinkSigned documentid signatorylinkid1

{- |
   Control rejecting the document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
   NOTE: magichash1 is currently ignored! (though it must exist)
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
          return $ LinkRejected documentid signatorylinkid1

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
   The page the author sees after sending a document
   URL: /landpage/signinvite/{documentid}
   Method: GET

   User must be author
 -}
landpageSignInvite :: DocumentID -> Kontra Response
landpageSignInvite documentid = withUserGet $ do
  ctx@Context { ctxmaybeuser = Just user } <- get
  document <- queryOrFail $ GetDocumentByDocumentID documentid

  failIfNotAuthor document user

  let authorid = unAuthor $ documentauthor document
      hasAuthorSigned = authorHasSigned authorid document

  content <- liftIO $ if hasAuthorSigned 
                       then landpageSignInviteView (ctxtemplates ctx) document
                       else landpageSendInviteView (ctxtemplates ctx) document
  renderFromBody ctx TopNone kontrakcja $ cdata content

{- |
   The author has signed the document
 -}
authorHasSigned :: UserID -> Document -> Bool
authorHasSigned authorid document = not $ Data.List.null $ 
                                    filter (\x -> not (isNotLinkForUserID authorid x) && isJust (maybesigninfo x))
                                               (documentsignatorylinks document)

{- |
   When someone has signed a document
   URL: /landpage/signed
   Method: GET
 -}
landpageSigned :: DocumentID -> SignatoryLinkID -> Kontra Response
landpageSigned documentid signatorylinkid = do
  ctx <- get
  document <- queryOrFail $ GetDocumentByDocumentID documentid
  signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
  maybeuser <- query $ GetUserByEmail (Email $ signatoryemail (signatorydetails signatorylink))
  content <- liftIO $ landpageSignedView (ctxtemplates ctx) document signatorylink (isJust maybeuser)
  renderFromBody ctx TopEmpty kontrakcja $ cdata content

{- |
   When someone rejects a document
   URL: /landpage/rejected
   Method: GET
 -}
landpageRejected :: DocumentID -> SignatoryLinkID -> Kontra Response
landpageRejected documentid signatorylinkid = do
  ctx <- get
  document <- queryOrFail $ GetDocumentByDocumentID documentid
  signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
  content <- liftIO $ landpageRejectedView (ctxtemplates ctx) document
  renderFromBody ctx TopEmpty kontrakcja $ cdata content

{- |
   Here we need to save the document either under existing account or create a new account
   send invitation email and put the document in that account
   URL: /landpage/signedsave
   Method: GET
-}
landpageSignedSave :: DocumentID -> SignatoryLinkID -> Kontra Response
landpageSignedSave documentid signatorylinkid = do
  ctx@Context{ctxhostpart} <- get
  document <- queryOrFail $ GetDocumentByDocumentID documentid
  signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
  let details = signatorydetails signatorylink
      fullname = signatoryname details
  maybeuser <- query $ GetUserByEmail (Email $ signatoryemail details)
  muser <- case maybeuser of
             Nothing -> do 
               let email = signatoryemail details
               liftIO $ createUser ctx ctxhostpart fullname email Nothing True Nothing False
             Just user -> return maybeuser
  when_ (isJust muser) $ update $ SaveDocumentForSignedUser documentid (userid $ fromJust muser) signatorylinkid
  -- should redirect
  content <- liftIO $ landpageLoginForSaveView (ctxtemplates ctx)
  renderFromBody ctx TopEmpty kontrakcja $ cdata content


{- |
   Show the document to be signed
 -}
handleSignShow :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra Response
handleSignShow documentid 
               signatorylinkid1
               magichash1 = do
  doc1 <- queryOrFail $ GetDocumentByDocumentID documentid
  checkLinkIDAndMagicHash doc1 signatorylinkid1 magichash1

  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxipnumber}) <- get
  
  mdocument <- update $ MarkDocumentSeen documentid signatorylinkid1 ctxtime ctxipnumber
  -- I believe this is redundant - Eric
  document <- either (const mzero) return mdocument
  invitedlink <- signatoryLinkFromDocumentByID document signatorylinkid1
  let authoruserid = unAuthor $ documentauthor document
  author <- queryOrFail $ GetUserByUserID authoruserid
  let authorname = prettyName author
  let invitedname = signatoryname $ signatorydetails $ invitedlink 
  renderFromBody ctx TopNone kontrakcja 
                       (fmap cdata $ pageDocumentForSignatory (LinkSignDoc document invitedlink) 
                                            document ctx invitedlink author)

{- |
   How many documents does the free user have left?
 -}
freeLeftForUser :: User -> Kontra Int
freeLeftForUser user = do
  numdoc <- query $ GetNumberOfDocumentsOfUser user
  let freeleft = if numdoc >= 5 then 0 else 5 - numdoc
  return freeleft

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
                                     muser2 <- sequenceMM $ fmap (query . GetUserByUserID . unSignatory) $ maybesignatory sl
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
  ctx@(Context {ctxmaybeuser = Just (user@User{userid}), ctxhostpart}) <- get
  author <- queryOrFail $ GetUserByUserID $ unAuthor documentauthor
  let toptab = if documentstatus document == Closed
                then TopDocument
                else TopNone
  -- authors get a view with buttons
  if isAuthor document user
   then do
       step <- getDesignStep (documentid document)
       renderFromBody ctx toptab kontrakcja 
            (fmap cdata $ pageDocumentForAuthor ctx document author step)
   -- friends can just look (but not touch)
   else do
        friendWithSignatory <- liftIO $ isFriendWithSignatory userid document 
        if (isFriendOf userid author || friendWithSignatory )
         then renderFromBody ctx toptab kontrakcja
                  (fmap cdata $ pageDocumentForViewer ctx document author)
         -- not allowed
         else mzero

getDesignStep::DocumentID -> Kontra (Maybe DesignStep)
getDesignStep docid = do
    step3 <- isFieldSet "step3"
    step2 <- isFieldSet "step2"
    case (step2,step3) of
       (True,_) -> return $ Just $ DesignStep2 docid
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
  case (documentstatus document,sign,send,template,contract) of 
      (Preparation, True, _,_, _ ) -> handleIssueSign document author
      (Preparation, _ ,  True,_, _) -> handleIssueSend document author
      (Preparation, _ , _ ,True, _) -> handleIssueSaveAsTemplate document author             
      (Preparation, _ , _ ,_ , True) -> handleIssueChangeToContract document author                    
      (Preparation, _ , _ ,_, _) -> handleIssueSave document author
      (AwaitingAuthor, _ , _ ,_, _) -> handleIssueSignByAuthor document author
      _  -> return LinkIssue 
     

handleIssueSign document author = do
    ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
    mudoc <- updateDocument ctx author document Nothing
    case mudoc of 
        Right udoc-> do    
            mndoc <- update $ AuthorSignDocument (documentid document) ctxtime ctxipnumber author Nothing
            case mndoc of
                Right newdocument -> do
                    postDocumentChangeAction newdocument (documentstatus udoc) Nothing
                    return $ LinkSignInvite (documentid document)
                Left _ -> mzero
        Left _ -> mzero               
                  
handleIssueSend document author = do
    ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
    mudoc <- updateDocument ctx author document Nothing
    case mudoc of 
        Right udoc-> do
            mndoc <- update $ AuthorSendDocument (documentid document) ctxtime ctxipnumber author Nothing
            case mndoc of
                Right newdocument -> do
                    postDocumentChangeAction newdocument (documentstatus udoc) Nothing
                    return $ LinkSignInvite (documentid document)
                Left _ -> mzero
        Left _ -> mzero          
    
handleIssueSaveAsTemplate document author = do
    ctx <- get
    mudoc <- updateDocument ctx author document Nothing
    case mudoc of 
        Right udoc -> do   
            mndoc <- update $ TemplateFromDocument $ documentid document
            case mndoc of
                Right newdocument -> do
                    return LinkIssue        
                Left _ -> mzero
        Left _ -> mzero            

handleIssueChangeToContract document author = do
    ctx <- get
    mcontract <- update $ ContractFromDocument $ documentid document 
    case mcontract of 
        Right contract -> do   
            mncontract <- updateDocument ctx author contract Nothing
            case mncontract of
                Right ncontract ->  return $ LinkDesignDoc $ DesignStep3 $ documentid ncontract                        
                Left _ -> mzero
        Left _ -> mzero             

handleIssueSave document author = do
    ctx <- get
    updateDocument ctx author document Nothing
    addFlashMsg =<< (liftIO . flashDocumentDraftSaved $ ctxtemplates ctx)
    return LinkIssue

handleIssueSignByAuthor document author = do
    ctx@Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- get
    doc2 <- update $ CloseDocument (documentid document) ctxtime ctxipnumber user Nothing
    case doc2 of
        Nothing -> return $ LinkIssueDoc (documentid document)
        Just d -> do 
            postDocumentChangeAction d AwaitingAuthor Nothing
            addFlashMsg =<< (liftIO $ flashAuthorSigned $ ctxtemplates ctx)
            return LinkIssue
{- |
   Show the document with title in the url
   URL: /d/{documentid}/{title}
   Method: GET
 -}
handleIssueShowTitleGet :: DocumentID -> String -> Kontra Response
handleIssueShowTitleGet docid _title = 
   do
   ctx <- get
   document@Document{ documentauthor
                    , documentid
                    } <- queryOrFail $ GetDocumentByDocumentID $ docid
   let file = safehead "handleIssueShow" (case documentstatus document of
                                            Closed -> documentsealedfiles document
                                            _      -> documentfiles document)
   contents <- liftIO $ getFileContents ctx file
   let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
   let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
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

{- |
   do the work necessary for saving a document being authored
 -}
updateDocument :: Context -> User -> Document -> Maybe SignatureInfo -> Kontra (Either String Document)
updateDocument ctx@Context{ctxtime,ctxipnumber} author document@Document{documentid} msiginfo = do
  -- each signatory has these predefined fields
  signatoriesfstnames <- getAndConcat "signatoryfstname"
  signatoriessndnames <- getAndConcat "signatorysndname"
  signatoriescompanies <- getAndConcat "signatorycompany"
  signatoriesnumbers <- getAndConcat "signatorynumber"
  signatoriesemails' <- getAndConcat "signatoryemail"
  let signatoriesemails = map (BSC.map toLower) signatoriesemails'

  -- if the post doesn't contain this one, we parse the old way
  sigids <- getAndConcat "sigid"

  daystosign <- readField "daystosign"
  
  invitetext <- getFieldUTFWithDefault defaultInviteMessage "invitetext"
  
  -- each custom field must have this
  fieldnames <- getAndConcat "fieldname"
  fieldids <- getAndConcat "fieldid"
  fieldsigids <- getAndConcat "fieldsigid"
  fieldvalues <- getAndConcat "fieldvalue"

  -- each placed field must have these values
  placedxs <- getAndConcat "placedx"
  placedys <- getAndConcat "placedy"
  placedpages <- getAndConcat "placedpage"
  placedwidths <- getAndConcat "placedwidth"
  placedheights <- getAndConcat "placedheight"
  placedsigids <- getAndConcat "placedsigid"
  placedfieldids <- getAndConcat "placedfieldid"

  authorrole <- getFieldWithDefault "" "authorrole"

  -- which type of identifications are allowed
  allowedidtypes <- getFieldWithDefault "" "allowedsignaturetypes"

  let emailallowed = if "Email" `isInfixOf` allowedidtypes
                      then [EmailIdentification]
                      else []
      elegitimationallowed = if "ELeg" `isInfixOf` allowedidtypes
                              then [ELegitimationIdentification]
                              else []
      
  let docallowedidtypes = emailallowed ++ elegitimationallowed

  let placements = zipWith5 FieldPlacement 
                   (map (read . BS.toString) placedxs)
                   (map (read . BS.toString) placedys)
                   (map (read . BS.toString) placedpages)
                   (map (read . BS.toString) placedwidths)
                   (map (read . BS.toString) placedheights)
                   
  let pls = zipWith3 m placedsigids placedfieldids placements where
          m s f p = (s, f, p)

  let placementsByID sigid fieldid = 
          map plac (filter sameids pls) where
              sameids (sigid2, fieldid2, _)
                  | sigid == sigid2 && fieldid == fieldid2 = True
                  | otherwise                              = False
              plac (_, _, x) = x

  let fielddefs = zipWith4 fd fieldnames fieldvalues fieldids fieldsigids where
          
          fd fn fv id sigid = (sigid,
                                 FieldDefinition { fieldlabel = fn, 
                                                   fieldvalue = fv,
                                                   fieldplacements = placementsByID sigid id,
                                                   fieldfilledbyauthor = (BS.length fv > 0)
                                                 })

  let defsByID sigid = 
          map snd (filter (sid sigid) fielddefs) where
              sid id (id2, _) 
                  | id == id2 = True
                  | otherwise = False


  let signatories = sigs where
          sigs = if sigids == [] 
                 then zipWith5 sdsimple signatoriesfstnames signatoriessndnames signatoriescompanies signatoriesnumbers signatoriesemails
                      
                 else zipWith6 sd sigids signatoriesfstnames signatoriessndnames signatoriescompanies signatoriesnumbers signatoriesemails 
          sdsimple n sn c no e = SignatoryDetails n
                                               sn
                                               c
                                               no
                                               e
                                               []
                                               []
                                               []
                                               []
                                               []
                                               []
          sd id n sn c no e = SignatoryDetails n 
                                            sn
                                            c 
                                            no 
                                            e 
                                            (placementsByID id (BS.fromString "fstname"))
                                            (placementsByID id (BS.fromString "sndname"))
                                            (placementsByID id (BS.fromString "company"))
                                            (placementsByID id (BS.fromString "email"))
                                            (placementsByID id (BS.fromString "number"))
                                            (defsByID id)
                                                                      
  let authordetails = (signatoryDetailsFromUser author)
                      {
                        signatoryemailplacements = placementsByID (BS.fromString "author") (BS.fromString "email")
                      , signatoryfstnameplacements = placementsByID (BS.fromString "author") (BS.fromString "fstname")
                      , signatorysndnameplacements = placementsByID (BS.fromString "author") (BS.fromString "sndname")
                      , signatorycompanyplacements = placementsByID (BS.fromString "author") (BS.fromString "company")
                      , signatorynumberplacements = placementsByID (BS.fromString "author") (BS.fromString "number")
                      , signatoryotherfields = defsByID (BS.fromString "author")
                      }
      signatories2 = if authorrole == "signatory"
                     then authordetails : signatories
                     else signatories
  -- FIXME: tell the user what happened!
  -- when (daystosign<1 || daystosign>99) mzero

  --let emails = zip signatoriesemails 
  --              (sequence $ map (query . GetUserByEmail . Email) signatoriesemails)

  -- author is gotten above, no?
  -- Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor document

  update $ UpdateDocument ctxtime documentid
           signatories2 daystosign invitetext author authordetails docallowedidtypes
              
{- |
   Constructs a list of documents (Arkiv) to show to the user.
   The list contains all documents the user is an author on or
   is a friend of the author.
   Duplicates are removed.
 -}
handleIssueGet :: Kontra Response
handleIssueGet = withUserGet $ checkUserTOSGet $ do
  -- Just user is safe here because we guard for logged in user
  ctx@(Context {ctxmaybeuser = Just user, ctxhostpart, ctxtime, ctxtemplates}) <- get
  mydocuments <- query $ GetDocumentsByUser user 
  usersICanView <- query $ GetUsersByFriendUserID $ userid user
  friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
  -- get rid of duplicates
  let documents = nub $ mydocuments ++ concat friends'Documents
  let sorteddocuments = sortBy (\d1 d2 -> compare (documentmtime d2) (documentmtime d1)) documents
  content <- liftIO $ pageDocumentList ctxtemplates ctxtime user sorteddocuments
  renderFromBody ctx TopDocument kontrakcja $ cdata content

{- |
   The command line for calling ghostscript
 -}
gs :: String
#ifdef WINDOWS
gs = "c:\\Program Files\\gs\\gs8.60\\bin\\gswin32c.exe" 
#else
gs = "gs"
#endif

{- |
   Convert PDF to jpeg images of pages
 -}
convertPdfToJpgPages :: Context
                     -> File
                     -> IO JpegPages
convertPdfToJpgPages ctx@Context{ctxs3action} file@File{fileid,filename} = do
  tmppath1 <- getTemporaryDirectory
  let tmppath = tmppath1 ++ "/" ++ show fileid
  createDirectoryIfMissing True tmppath
  let sourcepath = tmppath ++ "/source.pdf"

  content <- getFileContents ctx file

  BS.writeFile sourcepath content

  let gsproc = (proc gs [ "-sDEVICE=jpeg" 
                        , "-sOutputFile=" ++ tmppath ++ "/output-%d.jpg"
                        , "-dSAFER"
                        , "-dBATCH"
                        , "-dNOPAUSE"
                        , "-dTextAlphaBits=4"
                        , "-dGraphicsAlphaBits=4"
                        --, "-r91.361344537815126050420168067227"
                        , "-r190"
                        , sourcepath
                        ]) { std_out = CreatePipe
                           , std_err = CreatePipe
                           }
  (_, Just outhandle, Just errhandle, gsProcHandle) <- createProcess gsproc
  errcontent <- BS.hGetContents errhandle
  outcontent <- BS.hGetContents outhandle
                 
  exitcode <- waitForProcess gsProcHandle

  result <- case exitcode of
    ExitFailure _ -> return $ JpegPagesError (errcontent `BS.append` outcontent)
    ExitSuccess -> do
                  let pathofx x = tmppath ++ "/output-" ++ show x ++ ".jpg"
                  let exists1 x = doesFileExist (pathofx x)
                  let w (x:xs) = do
                                 g <- exists1 x 
                                 if g 
                                  then do
                                   h <- w xs
                                   return (x:h)
                                  else return []

                  listofpages <- w [1..]
                  x<-forM listofpages $ \x -> (do 
                                        (_,Just outhandle,_, sizechecker) <- createProcess $ ( proc "gm" ["-identify", pathofx x])  { std_out = CreatePipe}
                                        out <-  hGetContents outhandle
                                        let (w,h) = readSize out
                                        exitcode <- waitForProcess sizechecker
                                        case exitcode of
                                         ExitFailure _ -> return ()
                                         ExitSuccess -> return ()
                                        (_,_,_, resizer) <- createProcess $  proc "gm" ["convert", "-scale","943x1335!",pathofx x,pathofx x]
                                        exitcode <- waitForProcess resizer
                                        case exitcode of
                                         ExitFailure _ -> return ()
                                         ExitSuccess -> return ()
                                        content <- BS.readFile (pathofx x)                                         
                                        return (content,w,h)) `catch` (\_ -> do
                                                                           content <- BS.readFile (pathofx x)
                                                                           return (content,943,1335))

                  return (JpegPages x)
  -- remove the directory with all the files now
  -- everything as been collected, process has ended, we are done!
  removeDirectoryRecursive tmppath
  return result
  where       
   readSize::[Char] -> (Int,Int) --Ugly and unsafe but I can't get info about output format so writing nicer parser is useless
   readSize ('J':'P':'E':'G':' ':rest) = let 
                                          (w,hs) = span (isDigit) rest 
                                          h = takeWhile (isDigit) (tail hs)
                                         in (read w,read h) 
   readSize (r:rest) = readSize rest
   readSize [] = (943,1335)
{- |
   
 -}
maybeScheduleRendering :: Context 
                       -> File
                       -> IO JpegPages
maybeScheduleRendering ctx@Context{ ctxnormalizeddocuments = mvar }
                       (file@File { fileid }) = do
  modifyMVar mvar $ \setoffilesrenderednow ->
      case Map.lookup fileid setoffilesrenderednow of
         Just pages -> return (setoffilesrenderednow, pages)
         Nothing -> do
           Log.forkIOLogWhenError ("error rendering file " ++ show fileid) $ do
                jpegpages <- convertPdfToJpgPages ctx file
                modifyMVar_ mvar (\setoffilesrenderednow -> return (Map.insert fileid jpegpages setoffilesrenderednow))
           return (Map.insert fileid JpegPagesPending setoffilesrenderednow, JpegPagesPending)

{- |
   Get some html to display the images of the files
   URL: /pagesofdoc/{documentid}
   Method: GET
   FIXME: Should probably check for permissions to view
 -}
handlePageOfDocument :: DocumentID -> Kontra Response
handlePageOfDocument documentid = do
  document@Document {documentfiles
                    ,documentsealedfiles
                    ,documentstatus
                    ,documentid} <- queryOrFail $ GetDocumentByDocumentID documentid
  ctx@Context{ctxnormalizeddocuments} <- get
  let pending JpegPagesPending = True
      pending _                = False
      files                    = if documentstatus == Closed
                                   then documentsealedfiles
                                   else documentfiles                             
  case files of
    [] -> notFound (toResponse "temporary unavailable (document has no files)")
    f  -> do
      b <- mapM (\file -> liftIO $ maybeScheduleRendering ctx file) f
      if any pending b
       then notFound (toResponse "temporary unavailable (document has files pending for process)")
       else do
              pages <- liftIO $ Doc.DocView.showFilesImages2 (ctxtemplates ctx) $ zip f b
              webHSP $ return $ cdata pages

{- |
   Convert PDF to jpeg images of pages
 -}
preprocessPDF :: BS.ByteString
              -> IO BS.ByteString
preprocessPDF content = withSystemTempDirectory "gs" $ \tmppath -> do
  let sourcepath = tmppath ++ "/source.pdf"
  let outputpath = tmppath ++ "/output.pdf"

  BS.writeFile sourcepath content

  let gsproc = (proc gs [ "-sDEVICE=pdfwrite" 
                        , "-sOutputFile=" ++ outputpath 
                        , "-dSAFER"
                        , "-dBATCH"
                        , "-dNOPAUSE"
                        , sourcepath
                        ]) { std_out = CreatePipe
                           , std_err = CreatePipe
                           }
  (_, Just outhandle, Just errhandle, gsProcHandle) <- createProcess gsproc
  errcontent <- BSL.hGetContents errhandle
  outcontent <- BSL.hGetContents outhandle
                 
  exitcode <- waitForProcess gsProcHandle

  result <- case exitcode of
    ExitFailure _ -> do
       Log.debug $ "preprocess PDF error code " ++ show exitcode
       Log.debug $ "stdout: " ++ BSL.toString outcontent
       Log.debug $ "stderr: " ++ BSL.toString errcontent
       return content
    ExitSuccess -> BS.readFile outputpath

  return result
    
handleDocumentUpload :: DocumentID -> BS.ByteString -> BS.ByteString -> Kontra ()
handleDocumentUpload docid content1 filename = do
  ctx@Context{ctxs3action} <- get
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  content <- liftIO $ preprocessPDF content1

  result <- update $ AttachFile docid filename content
  case result of
    Left err -> return ()
    Right document -> do
        liftIO $ forkIO $ mapM_ (AWS.uploadFile ctxs3action) (documentfiles document)
        return ()
  return ()

personFromSignatoryDetails :: SignatoryDetails -> Seal.Person
personFromSignatoryDetails details =
    Seal.Person { Seal.fullname = BS.toString $ signatoryname details 
                , Seal.company = BS.toString $ signatorycompany details
                , Seal.email = BS.toString $ signatoryemail details
                , Seal.number = BS.toString $ signatorynumber details
                }

personsFromDocument :: Document -> [(Seal.Person, (MinutesTime,Word32), (MinutesTime,Word32))]
personsFromDocument document = 
    let
        links = documentsignatorylinks document
        unSignInfo (si@SignInfo { signtime, signipnumber }) = (signtime,signipnumber)
        x (SignatoryLink{ signatorydetails
                        , maybesigninfo = Just (si@SignInfo { signtime, signipnumber })
                        , maybeseeninfo
                        })
             -- FIXME: this one should really have seentime always...
             = (personFromSignatoryDetails signatorydetails, unSignInfo $ maybe si id maybeseeninfo, (signtime,signipnumber))
        x link = trace (show link) $ error "SignatoryLink does not have all the necessary data"
    in map x links

fieldsFromPlacement value placement  =
    let toPtt x = (x * 72 `div` 190) - 5 -- scalling and some replacing
        w = placementpagewidth placement
        h = placementpageheight placement 
    in    
    Seal.Field { Seal.value = value
               , Seal.x =  toPtt $ (placementx placement * w) `div` 943
               , Seal.y =  toPtt $ h - ((placementy placement * h) `div` 1335)
               , Seal.page = placementpage placement
               , Seal.w =  w
               , Seal.h = h
               }

fieldsFromDefinition def =
    map (fieldsFromPlacement (BS.toString (fieldvalue def))) (fieldplacements def)

fieldsFromSignatory sig = 
    (map (fieldsFromPlacement (BS.toString (signatoryfstname sig))) (signatoryfstnameplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatorysndname sig))) (signatorysndnameplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatoryemail sig))) (signatoryemailplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatorycompany sig))) (signatorycompanyplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatorynumber sig))) (signatorynumberplacements sig))
    ++
    (foldl (++) [] (map fieldsFromDefinition (signatoryotherfields sig)))    

sealSpecFromDocument :: String -> Document -> User ->  String -> String -> Seal.SealSpec
sealSpecFromDocument hostpart document author inputpath outputpath =
  let docid = unDocumentID (documentid document)
      authorHasSigned = (any ((maybe False ((== (userid author)) . unSignatory)) . maybesignatory) (documentsignatorylinks document))
      signatoriesdetails = map signatorydetails $ documentsignatorylinks document
      authordetails = (signatoryDetailsFromUser author) 
                      {
                        signatoryfstnameplacements = authorfstnameplacements document
                      , signatorysndnameplacements = authorsndnameplacements document
                      , signatorynumberplacements = authornumberplacements document
                      , signatoryemailplacements = authoremailplacements document
                      , signatorycompanyplacements = authorcompanyplacements document
                      , signatoryotherfields = authorotherfields document
                      }
      signatories = personsFromDocument document
      secretaries = if authorHasSigned then [] else [personFromSignatoryDetails authordetails]

      -- oh boy, this is really network byte order!
      formatIP :: Word32 -> String
      formatIP 0 = ""
      -- formatIP 0x7f000001 = ""
      formatIP x = " (IP: " ++ show ((x `shiftR` 0) .&. 255) ++
                   "." ++ show ((x `shiftR` 8) .&. 255) ++
                   "." ++ show ((x `shiftR` 16) .&. 255) ++
                   "." ++ show ((x `shiftR` 24) .&. 255) ++ ")"
      fst3 (a,b,c) = a
      snd3 (a,b,c) = b
      thd3 (a,b,c) = c
      persons = (map fst3 signatories)
      -- persons = authorperson : (map fst3 signatories)
      paddeddocid = reverse $ take 20 $ (reverse (show docid) ++ repeat '0')
      initials = concatComma (map initialsOfPerson persons)
      initialsOfPerson (Seal.Person {Seal.fullname}) = map head (words fullname)
      authorfullname = signatoryname authordetails
      -- 2. "Name of invited" granskar dokumentet online
      makeHistoryEntryFromSignatory (Seal.Person {Seal.fullname},(seentime2,seenipnumber2),(signtime2,signipnumber2)) = 
          [   Seal.HistEntry
            { Seal.histdate = show seentime2
            , Seal.histcomment = fullname ++ " granskar dokumentet online" ++ formatIP seenipnumber2
            } 
            , Seal.HistEntry
            { Seal.histdate = show signtime2
            , Seal.histcomment = fullname ++ " undertecknar dokumentet online" ++ formatIP signipnumber2
            }
      
          ]
      makeHistoryEntryFromEvent (DocumentHistoryInvitationSent time ipnumber) =
          [ Seal.HistEntry
            { Seal.histdate = show time 
            , Seal.histcomment = 
                if length signatories>1
                   then BS.toString authorfullname ++ " skickar en inbjudan att underteckna till parterna" ++ formatIP ipnumber
                   else BS.toString authorfullname ++ " skickar en inbjudan att underteckna till parten" ++ formatIP ipnumber
            }
          ]
      makeHistoryEntry = either makeHistoryEntryFromEvent makeHistoryEntryFromSignatory  
      maxsigntime = maximum (map (fst . thd3) signatories)
      concatComma = concat . intersperse ", "
      -- Hack to switch the order of events, so we put invitation send after author signing
      makeHistory (fst@(Left (DocumentHistoryInvitationSent time _)):(snd@(Right (_,_,(signtime2,_)))):rest) = 
          if (signtime2 == time)
           then (makeHistoryEntry snd) ++ (makeHistoryEntry fst) ++ (makeHistory rest)
           else (makeHistoryEntry fst) ++ (makeHistoryEntry snd) ++ (makeHistory rest)
      makeHistory (e:es) = (makeHistoryEntry e) ++ (makeHistory es)
      makeHistory [] = []
      
      lastHistEntry = Seal.HistEntry
                      { Seal.histdate = show maxsigntime
                      , Seal.histcomment = "Samtliga parter har undertecknat dokumentet och avtalet är nu juridiskt bindande."
                      }

      history = (makeHistory $ ((map Left (documenthistory document)) ++ (map Right signatories))) ++ [lastHistEntry]
      
      -- document fields
      fields = if authorHasSigned
               then (concat (map fieldsFromSignatory signatoriesdetails))
               else (concat (map fieldsFromSignatory $ authordetails : signatoriesdetails))
                    
      config = Seal.SealSpec 
            { Seal.input = inputpath
            , Seal.output = outputpath
            , Seal.documentNumber = paddeddocid
            , Seal.persons = persons
            , Seal.secretaries = secretaries
            , Seal.history = history
            , Seal.initials = initials
            , Seal.hostpart = hostpart
            , Seal.fields = fields
            }
      in config

eitherLog action = do
  value <- action
  case value of
    Left errmsg -> do
               putStrLn errmsg
               error errmsg
    Right value -> return value

uploadDocumentFileToAmazon ctxs3action docid fileid1 = do
  Just doc <- query $ GetDocumentByDocumentID docid
  let files = documentfiles doc ++ documentsealedfiles doc
  case filter (\x -> fileid x == fileid1) files  of
    [file] -> do
      AWS.uploadFile ctxs3action file
      return ()
    _ -> return ()
  return ()


uploadDocumentFilesToTrustWeaver :: TW.TrustWeaverConf 
                                 -> String 
                                 -> DocumentID 
                                 -> IO ()
uploadDocumentFilesToTrustWeaver ctxtwconf twownername documentid = do
  Just document <- query $ GetDocumentByDocumentID documentid
  let twdocumentid = show documentid
  let twdocumentdate = showDateOnly (documentmtime document)
  let File{filestorage = FileStorageMemory pdfdata} = head $ documentsealedfiles document 
          
  -- FIXME: we should retry here if the following fails
  -- because of external reasons 
  reference <- eitherLog $ TW.storeInvoice ctxtwconf twdocumentid twdocumentdate twownername pdfdata
  update $ SetDocumentTrustWeaverReference documentid reference
  return ()

sealDocument :: Context 
             -> MVar (Map.Map FileID JpegPages)
             -> String
             -> MinutesTime
             -> User
             -> Document
             -> IO (Either String Document)
sealDocument ctx@Context{ctxs3action,ctxtwconf}
             normalizemap 
             hostpart
             signtime1
             author
             document = do
  let (file@File {fileid,filename}) = 
           safehead "sealDocument" $ documentfiles document
  let docid = documentid document

  tmppath <- getTemporaryDirectory
  let tmpin = tmppath ++ "/in_" ++ show docid ++ ".pdf"
  let tmpout = tmppath ++ "/out_" ++ show docid ++ ".pdf"
  contents <- getFileContents ctx file
  BS.writeFile tmpin contents
  let config = sealSpecFromDocument hostpart document author tmpin tmpout
  (code,stdout,stderr) <- readProcessWithExitCode' "dist/build/pdfseal/pdfseal" [] (BSL.fromString (show config))
  Log.debug $ "seal exit code " ++ show code
  Log.debug $ "seal stdout: " ++ BSL.toString stdout
  Log.debug $ "seal stderr: " ++ BSL.toString stderr

  if code == ExitSuccess 
     then do

       newfilepdf1 <- BS.readFile tmpout
       removeFile tmpout

       newfilepdf <- 
        if TW.signcert ctxtwconf == ""
         then return newfilepdf1
         else do
           x <- TW.signDocument ctxtwconf newfilepdf1
           case x of
                   Left errmsg -> do
                     -- FIXME: handle the error in a better way here
                     putStrLn errmsg
                     return newfilepdf1
                   Right result -> return result

       mdocument <- update $ AttachSealedFile docid filename newfilepdf
       case mdocument of
         Right document -> do
          liftIO $ forkIO $ mapM_ (AWS.uploadFile ctxs3action) (documentsealedfiles document)
          case signeddocstorage (usersettings author) of
           Nothing -> return ()
           Just twsettings -> do
                        forkIO $ uploadDocumentFilesToTrustWeaver ctxtwconf (BS.toString $ storagetwname twsettings) (documentid document)
                        return ()
          return $ Right document
         Left msg -> error msg
     else do
        -- error handling
        Log.error $ "seal: cannot seal document " ++ show docid ++ ", fileid " ++ show fileid
        update $ ErrorDocument docid "could not seal document"
        return $ Left "could not seal document"


basename :: String -> String
basename filename = 
    case break (\x -> (x=='\\') || (x=='/')) filename of
      (_,(_:rest)) -> basename rest
      _ -> takeWhile ((/=) '.') filename

handleIssueNewDocument :: Kontra KontraLink
handleIssueNewDocument = withUserPost $ do
    ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) <- get
    input@(Input content (Just filename) _contentType) <- getDataFnM (lookInput "doc")
    -- see if we have empty input, then there was no file selected
    if BSL.null content
       then do
         return LinkMain
        else do
          -- FIXME: here we have encoding issue
          -- Happstack gives use String done by BS.unpack, so BS.pack it here
          -- in our case it should be utf-8 as this is what we use everywhere
          let title = BSC.pack (basename filename) 
          freeleft <- freeLeftForUser user
          template <- isFieldSet "template"
          let doctype = if (template) then Template else Contract
          doc <- update $ NewDocument user title doctype ctxtime (freeleft>0) 
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ LinkIssueDoc $ documentid doc


handleIssueArchive :: Kontra KontraLink
handleIssueArchive = do
    ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) <- get
    something <- getDataFnM (lookInput "archive")
    idstrings <- getDataFnM (lookInputList "doccheck")
    let Just ids = sequence $ map (readM . BSL.toString) idstrings
    update $ ArchiveDocuments user ids
    return LinkIssue


showPage :: FileID -> Int -> Kontra Response
showPage fileid pageno = do
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
  ctx@Context { ctxmaybeuser = Just user } <- get
  doc <- queryOrFail $ GetDocumentByDocumentID docid
  failIfNotAuthor doc user
  customMessage <- fmap (fmap concatChunks) $ getDataFn' (lookBS "customtext")  
  mdoc' <- update $ CancelDocument(documentid doc) 
  case mdoc' of 
    Just doc' -> do
          sendCancelMailsForDocument customMessage ctx doc
          addFlashMsg =<< (liftIO $ flashMessageCanceled (ctxtemplates ctx))
    Nothing -> addFlashMsg $ toFlashMsg OperationFailed "Could not cancel"
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
      update $ RestartDocument docid user
      addFlashMsg =<< (liftIO $ flashDocumentRestarted (ctxtemplates ctx))
      return $ LinkIssueDoc docid
    Nothing -> return $ LinkLogin NotLoggedAsSuperUser
                    
handleResend:: DocumentID -> SignatoryLinkID -> Kontra KontraLink
handleResend docid signlinkid  = withUserPost $ do
  ctx@Context { ctxmaybeuser = Just user } <- get
  doc <- queryOrFail $ GetDocumentByDocumentID docid
  failIfNotAuthor doc user
  signlink <- signatoryLinkFromDocumentByID doc signlinkid
  author <- queryOrFail $ GetUserByUserID $ unAuthor $ documentauthor doc
  customMessage <- fmap (fmap concatChunks) $ getDataFn' (lookBS "customtext")  
  mail <- liftIO $  mailDocumentRemind (ctxtemplates ctx) customMessage ctx doc signlink author
  liftIO $ sendMail (ctxmailer ctx) (mail {fullnameemails = [(signatoryname $ signatorydetails signlink,signatoryemail $ signatorydetails signlink )],
                                                mailInfo = Invitation  (documentid doc) (signatorylinkid signlink) })
  addFlashMsg =<< (liftIO $ flashRemindMailSent (ctxtemplates ctx) signlink)
  return (LinkIssueDoc $ documentid doc)

--This only works for undelivered mails. We shoulkd check if current user is author
handleChangeSignatoryEmail::String -> String -> Kontra KontraLink
handleChangeSignatoryEmail did slid = do
                                     let mdid = readM did
                                     memail' <- getField "email"
                                     let memail = fmap (fmap toLower) memail'
                                     let mslid = readM slid
                                     case (mdid,mslid,memail) of
                                       (Just docid,Just slid,Just email) -> do
                                                                           ctx <- get
                                                                           md <- query $ GetDocumentByDocumentID docid
                                                                           when ((liftM2 isAuthor md $ ctxmaybeuser ctx) /= Just True) mzero
                                                                           mdoc <- update $ ChangeSignatoryEmailWhenUndelivered docid slid (BS.fromString email)
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
  liftIO $ forM_ (documentsignatorylinks document) (sendMail  (ctxmailer ctx) <=< (mailCancelDocumentByAuthor (ctxtemplates ctx) customMessage ctx document author))

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
                      content <- liftIO $ uploadPage (ctxtemplates ctx) 
                      renderFromBody ctx TopDocument kontrakcja $ cdata content 
                      
getAllTemplates::Kontra Response                      
getAllTemplates = do
    ctx <- get 
    allTemplates <- 
        case (ctxmaybeuser ctx) of
            Just user -> liftIO $ query $ GetUserTemplates (userid user)
            Nothing ->  return []
    content <- liftIO $ templatesForAjax (ctxtemplates ctx)  allTemplates            
    simpleResponse content
    
handleCreateFromTemplate::Kontra KontraLink
handleCreateFromTemplate = do
     docid <- readField "template"
     case docid of 
         Just did -> do
             newdoc <- update $ ContractFromDocument did
             case newdoc of 
                 Right newdoc -> return $ LinkIssueDoc $ documentid newdoc   
                 Left _ -> mzero
         Nothing -> mzero
     