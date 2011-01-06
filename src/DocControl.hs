{-# LANGUAGE CPP, ScopedTypeVariables #-}

{- |
   DocControl represents the controler (in MVC) of the document.
 -}
module DocControl where

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
import DocState
import DocView
import HSP
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
import User
import UserControl
import UserState
import UserView (prettyName)
import qualified Amazon as AWS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.HTTP as HTTP
import qualified Seal as Seal
import qualified TrustWeaver as TW

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
postDocumentChangeAction document@Document{documentstatus, documentsignatorylinks} oldstatus msignalinkid
    -- No status change ; no action
    | documentstatus == oldstatus = return ()
    -- Preparation -> Pending
    -- main action: sendInvitationEmails
    | oldstatus == Preparation && documentstatus == Pending = do
        ctx <- get
        Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor document
        liftIO $ forkIO $ do
          -- this is here to postpone email send a couple of seconds
          -- so our service has a chance to give answer first
          -- is GHC using blocking calls or what?
          threadDelay 5000 
          sendInvitationEmails ctx document author
        return ()
    -- Pending -> AwaitingAuthor
    -- main action: sendAwaitingEmail
    | oldstatus == Pending && documentstatus == AwaitingAuthor = do
        ctx <- get
        Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor document
        liftIO $ forkIO $ do
          threadDelay 5000 
          sendAwaitingEmail ctx document author
        return ()
    -- Pending -> Closed OR AwaitingAuthor -> Closed
    -- main action: sendClosedEmails
    | (oldstatus == Pending || oldstatus == AwaitingAuthor) && documentstatus == Closed = do
        ctx@Context{ctxnormalizeddocuments,ctxhostpart,ctxtime} <- get
        liftIO $ forkIO $ do
          Just user <- query $ GetUserByUserID (unAuthor (documentauthor document))
          newdoc <- sealDocument ctx ctxnormalizeddocuments ctxhostpart ctxtime user document
          sendClosedEmails ctx newdoc
        return ()
    -- Pending -> Rejected
    -- main action: sendRejectAuthorEmail
    | oldstatus == Pending && documentstatus == Rejected = do
        ctx@Context{ctxnormalizeddocuments,ctxhostpart,ctxtime} <- get
        customMessage <- fmap (fmap concatChunks) $ getDataFn' (lookBS "customtext")  
        liftIO $ forkIO $ do
          threadDelay 5000 
          sendRejectAuthorEmail customMessage ctx document (fromJust msignalink)
        return ()
    -- transition with no necessary action; do nothing
    -- FIXME: log status change
    | otherwise = 
         return ()
    where msignalink = maybe Nothing (signlinkFromDocById document) msignalinkid
          
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
                   , signatorydetails = SignatoryDetails { signatoryname
                                                         , signatorycompany
                                                         , signatoryemail
                                                         }
                   , signatorymagichash } = signatorylink
      Document{documenttitle,documentid} = document
      authorid = unAuthor $ documentauthor document
      hasAuthorSigned = not $ Data.List.null $ filter (not . isNotLinkForUserID authorid) (documentsignatorylinks document)
  mail <- if hasAuthorSigned 
           then mailInvitationToSign (ctxtemplates ctx) ctx document signatorylink author
           else mailInvitationToSend (ctxtemplates ctx) ctx document signatorylink author
  attachmentcontent <- AWS.getFileContents (ctxs3action ctx) $ head $ documentfiles document
  sendMail (ctxmailsconfig ctx) $ 
           mail { fullnameemails = [(signatoryname,signatoryemail)]
                , attachments = [(documenttitle,attachmentcontent)]
                , mailInfo = Invitation signatorylinkid
                } 

{- |
   Send emails to all parties when a document is closed.
 -}
sendClosedEmails :: Context -> Document -> IO ()
sendClosedEmails ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendClosedEmail1 ctx document)
  sendClosedAuthorEmail ctx document

{- |
   Helper for sendClosedEmails
 -}
sendClosedEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendClosedEmail1 ctx document signatorylink = do
  let SignatoryLink{ signatorylinkid
                   , signatorymagichash
                   , signatorydetails = SignatoryDetails { signatoryname
                                                         , signatorycompany
                                                         , signatoryemail }} = signatorylink
      Document{documenttitle,documentid} = document
  mail <- mailDocumentClosedForSignatories (ctxtemplates ctx) ctx document signatorylink
  attachmentcontent <- AWS.getFileContents (ctxs3action ctx) $ head $ documentsealedfiles document
  sendMail  (ctxmailsconfig ctx) $ mail { fullnameemails =  [(signatoryname,signatoryemail)] , attachments = [(documenttitle,attachmentcontent)]}

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
  sendMail (ctxmailsconfig ctx) $ mail { fullnameemails = [(name1,email1)] }

{- |
   Send the email to the Author when the document is closed
 -}
sendClosedAuthorEmail :: Context -> Document -> IO ()
sendClosedAuthorEmail ctx document = do
  let authorid = unAuthor $ documentauthor document
  Just authoruser <- query $ GetUserByUserID authorid
  mail<- mailDocumentClosedForAuthor (ctxtemplates ctx) ctx (userfullname authoruser)
             document
  attachmentcontent <- AWS.getFileContents (ctxs3action ctx) $ head $ documentsealedfiles document
  let Document{documenttitle,documentid} = document
      email1 = unEmail $ useremail $ userinfo authoruser
      name1 = userfullname authoruser
  sendMail (ctxmailsconfig ctx) $ mail { fullnameemails = [(name1,email1)], attachments = [(documenttitle,attachmentcontent)]}

{- |
   Send an email to the author when the document is rejected
 -}
sendRejectAuthorEmail :: (Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> IO ()
sendRejectAuthorEmail customMessage ctx document signalink = do
  let authorid = unAuthor $ documentauthor document
  Just authoruser <- query $ GetUserByUserID authorid
  let rejectorName = signatoryname (signatorydetails signalink)
  mail <- mailDocumentRejectedForAuthor (ctxtemplates ctx) customMessage ctx (userfullname authoruser)
             document rejectorName
  let email1 = unEmail $ useremail $ userinfo authoruser
      name1 = userfullname authoruser
  sendMail  (ctxmailsconfig ctx) $ mail { fullnameemails = [(name1,email1)]}

{- |
   Render a page of documents that a user has signed
   URL: /s
   Method: Get
   ??: Is this what it does?
 -}
handleSTable :: Kontra Response
handleSTable = withUserGet $ checkUserTOSGet $ do
      ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime}) <- get
      let user = fromJust ctxmaybeuser
      documents <- query $ GetDocumentsBySignatory user
      renderFromBody ctx TopNone kontrakcja (pageDocumentList ctxtime user documents)

{- |
   Control the signing of a document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
   Method: POST
   NOTE: magichash1 is currently ignored! (though it must exist)
 -}
signDocument :: DocumentID 
             -> SignatoryLinkID 
             -> MagicHash
             -> Kontra KontraLink
signDocument documentid -- ^ The DocumentID of the document to sign
             signatorylinkid1 -- ^ The SignatoryLinkID that is in the URL
             magichash1 -- ^ The MagicHash that is in the URL (NOTE: This is ignored!)
                 = do
  Context { ctxtime, ctxipnumber } <- get
  document@Document{ documentstatus = olddocumentstatus, documentsignatorylinks } <- queryOrFail $ GetDocumentByDocumentID documentid

  checkLinkIDAndMagicHash document signatorylinkid1 magichash1

  fieldnames <- getAndConcat "fieldname"
  fieldvalues <- getAndConcat "fieldvalue"
  let fields = zip fieldnames fieldvalues

  newdocument <- update $ SignDocument documentid signatorylinkid1 ctxtime ctxipnumber fields
  case newdocument of
    Left message -> 
        do
          addFlashMsgText message
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
               -> MagicHash
               -> Kontra KontraLink
rejectDocument documentid 
               signatorylinkid1 
               magichash -- ^ The MagicHash that is in the URL (NOTE: This is ignored!)
                   = do
  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxipnumber}) <- get
  document@Document{ documentsignatorylinks } <- queryOrFail $ GetDocumentByDocumentID documentid

  checkLinkIDAndMagicHash document signatorylinkid1 magichash

  mdocument <- update $ RejectDocument documentid signatorylinkid1 ctxtime ctxipnumber
  case (mdocument) of
    Left message -> 
        do
          addFlashMsgText message
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
authorHasSigned authorid document = not $ Data.List.null $ filter (not . isNotLinkForUserID authorid) (documentsignatorylinks document)

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
               liftIO $ createUser ctx ctxhostpart fullname email Nothing True Nothing
             Just user -> return maybeuser
  when_ (isJust muser) $ update $ SaveDocumentForSignedUser documentid (userid $ fromJust muser) signatorylinkid
  -- should redirect
  content <- liftIO $ landpageLoginForSaveView (ctxtemplates ctx)
  renderFromBody ctx TopEmpty kontrakcja $ cdata content

{- |
   Landpage when a document is saved
   Perhapse this is not used
   URL: /landpage/saved
   Method: GET
 -}
landpageSaved :: DocumentID -> SignatoryLinkID -> Kontra Response
landpageSaved documentid signatorylinkid = do
  (ctx@Context { ctxmaybeuser = Just user@User{userid} }) <- get
  document <- queryOrFail $ GetDocumentByDocumentID documentid
  -- ignore output
  update $ SaveDocumentForSignedUser documentid userid signatorylinkid
  signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
  content <- liftIO $ landpageDocumentSavedView (ctxtemplates ctx)
  renderFromBody ctx TopDocument kontrakcja $ cdata content

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
  document <- maybe mzero return mdocument

  invitedlink <- signatoryLinkFromDocumentByID document signatorylinkid1
  let wassigned = isJust $ maybesigninfo invitedlink
      authoruserid = unAuthor $ documentauthor document

  author <- queryOrFail $ GetUserByUserID authoruserid

  let authorname = prettyName author
      invitedname = signatoryname $ signatorydetails $ invitedlink 
  if wassigned
   then renderFromBody ctx TopNone kontrakcja
                       (pageDocumentForViewer ctx document author)
   else renderFromBody ctx TopNone kontrakcja 
                       (pageDocumentForSign (LinkSignDoc document invitedlink) 
                                            document ctx invitedlink wassigned author)

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
   then renderFromBody ctx toptab kontrakcja 
            (pageDocumentForAuthor ctx document author)
   -- friends can just look (but not touch)
   else if isFriendOf userid author
         then renderFromBody ctx toptab kontrakcja
                  (pageDocumentForViewer ctx document author)
         -- not allowed
         else mzero

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
  ctx@Context { ctxmaybeuser = Just user } <- get
  failIfNotAuthor document user
     
  -- something has to change here
  case documentstatus document of
       Preparation -> do
                       doc2 <- updateDocument ctx document
                       if documentstatus doc2 == Pending
                        -- It went to pending, so it's been sent to signatories
                        then return $ LinkSignInvite docid
                        -- otherwise it was just a save
                        else do
                          addFlashMsgText =<< (liftIO $ flashDocumentDraftSaved $ ctxtemplates ctx)
                          return LinkIssue
       AwaitingAuthor -> do 
                          doc2 <- update $ CloseDocument docid 
                          case doc2 of
                            Nothing -> return $ LinkIssueDoc docid
                            Just d -> do 
                                       postDocumentChangeAction d AwaitingAuthor Nothing
                                       return LinkIssue
       _ -> return $ LinkIssueDoc docid

{- |
   Show the document with title in the url
   URL: /d/{documentid}/{title}
   Method: GET
 -}
handleIssueShowTitleGet :: DocumentID -> String -> Kontra Response
handleIssueShowTitleGet docid _title = withUserGet $ checkUserTOSGet $ do
   ctx <- get
   document@Document{ documentauthor
                    , documentid
                    } <- queryOrFail $ GetDocumentByDocumentID $ docid
   ctx@(Context {ctxmaybeuser = Just (user@User{userid}), ctxhostpart}) <- get
   failIfNotAuthor document user
   let file = safehead "handleIssueShow" (case documentstatus document of
                                            Closed -> documentsealedfiles document
                                            _      -> documentfiles document)
   contents <- liftIO $ AWS.getFileContents (ctxs3action ctx) file
   let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
   let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
   return res2

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
updateDocument :: Context -> Document -> Kontra Document  
updateDocument ctx@Context{ctxtime,ctxipnumber} document@Document{documentid} = do
  -- each signatory has these predefined fields
  signatoriesnames <- getAndConcat "signatoryname"
  signatoriescompanies <- getAndConcat "signatorycompany"
  signatoriesnumbers <- getAndConcat "signatorynumber"
  signatoriesemails <- getAndConcat "signatoryemail"
  -- if the post doesn't contain this one, we parse the old way
  sigids <- getAndConcat "sigid"

  daystosignstring <- getDataFn' (look "daystosign")
  daystosign <- maybe (return Nothing) ((fmap Just). readM) daystosignstring
  invitetext <- fmap (maybe defaultInviteMessage concatChunks) (getDataFn' $ lookBS "invitetext")
  
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
                 then zipWith4 sdsimple signatoriesnames signatoriescompanies signatoriesnumbers signatoriesemails
                      
                 else zipWith5 sd sigids signatoriesnames signatoriescompanies signatoriesnumbers signatoriesemails 
          sdsimple n c no e = SignatoryDetails n
                                               c
                                               no
                                               e
                                               []
                                               []
                                               []
                                               []
                                               []
          sd id n c no e = SignatoryDetails n 
                                            c 
                                            no 
                                            e 
                                            (placementsByID id (BS.fromString "name"))
                                            (placementsByID id (BS.fromString "company"))
                                            (placementsByID id (BS.fromString "email"))
                                            (placementsByID id (BS.fromString "number"))
                                            (defsByID id)

  -- FIXME: tell the user what happened!
  -- when (daystosign<1 || daystosign>99) mzero

  --let emails = zip signatoriesemails 
  --              (sequence $ map (query . GetUserByEmail . Email) signatoriesemails)

  Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor document

  doc2 <- update $ UpdateDocument ctxtime documentid
           signatories daystosign invitetext author

  msum 
     [ do getDataFnM (look "final" `mplus` look "sign")
          mdocument <- update $ AuthorSignDocument documentid ctxtime ctxipnumber author
          case mdocument of
            Left msg -> return doc2
            Right newdocument -> do
                postDocumentChangeAction newdocument (documentstatus doc2) Nothing
                return newdocument
     , return doc2
     ]
    
{- |
   Constructs a list of documents (Arkiv) to show to the user.
   The list contains all documents the user is an author on or
   is a friend of the author.
   Duplicates are removed.
 -}
handleIssueGet :: Kontra Response
handleIssueGet = withUserGet $ checkUserTOSGet $ do
  -- Just user is safe here because we guard for logged in user
  ctx@(Context {ctxmaybeuser = Just user, ctxhostpart, ctxtime}) <- get
  mydocuments <- query $ GetDocumentsByUser user 
  usersICanView <- query $ GetUsersByFriendUserID $ userid user
  friends'Documents <- mapM (query . GetDocumentsByUser) usersICanView
  -- get rid of duplicates
  let documents = nub $ mydocuments ++ concat friends'Documents
  let sorteddocuments = sortBy (\d1 d2 -> compare (documentmtime d2) (documentmtime d1)) documents
  renderFromBody ctx TopDocument kontrakcja (pageDocumentList ctxtime user sorteddocuments) 

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
convertPdfToJpgPages Context{ctxs3action} file@File{fileid,filename} = do
  tmppath1 <- getTemporaryDirectory
  let tmppath = tmppath1 ++ "/" ++ show fileid
  createDirectoryIfMissing True tmppath
  let sourcepath = tmppath ++ "/source.pdf"

  content <- AWS.getFileContents ctxs3action file

  BS.writeFile sourcepath content

  let gsproc = (proc gs [ "-sDEVICE=jpeg" 
                        , "-sOutputFile=" ++ tmppath ++ "/output-%d.jpg"
                        , "-dSAFER"
                        , "-dBATCH"
                        , "-dNOPAUSE"
                        , "-dTextAlphaBits=4"
                        , "-dGraphicsAlphaBits=4"
                        , "-r91.361344537815126050420168067227"
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
                  x <- mapM (\x -> BS.readFile (pathofx x)) listofpages
                  return (JpegPages x)
  -- remove the directory with all the files now
  -- everything as been collected, process has ended, we are done!
  removeDirectoryRecursive tmppath
  return result
       
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
           forkIO $ do
                -- putStrLn $ "Rendering " ++ show fileid
                -- FIXME: handle exceptions gracefully
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
       else webHSP (DocView.showFilesImages2 $ zip f b)
    
handleDocumentUpload :: DocumentID -> BS.ByteString -> BS.ByteString -> Kontra ()
handleDocumentUpload docid content filename = do
  ctx@Context{ctxs3action} <- get
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

fieldsFromPlacement value placement =
    Seal.Field { Seal.value = value
               , Seal.x = placementx placement
               , Seal.y = placementy placement
               , Seal.page = placementpage placement
               , Seal.w = placementpagewidth placement
               , Seal.h = placementpageheight placement
               }

fieldsFromDefinition def =
    map (fieldsFromPlacement (BS.toString (fieldvalue def))) (fieldplacements def)

fieldsFromSignatory sig = 
    (map (fieldsFromPlacement (BS.toString (signatoryname sig))) (signatorynameplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatoryemail sig))) (signatoryemailplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatorycompany sig))) (signatorycompanyplacements sig))
    ++
    (map (fieldsFromPlacement (BS.toString (signatorynumber sig))) (signatorynumberplacements sig))
    ++
    (foldl (++) [] (map fieldsFromDefinition (signatoryotherfields sig)))    

sealSpecFromDocument :: String -> Document -> User -> String -> String -> Seal.SealSpec
sealSpecFromDocument hostpart document author inputpath outputpath =
  let docid = unDocumentID (documentid document)
      {-
        (authorsigntime,authorsignipnumber) = 
          case documentmaybesigninfo document of
            Nothing -> error "signtime1"
            Just (SignInfo {signtime,signipnumber}) -> (signtime,signipnumber)
      authordetails = documentauthordetails document
      authorperson = if authordetails /= emptyDetails 
                     then personFromSignatoryDetails authordetails
                     else personFromSignatoryDetails (SignatoryDetails 
                                  { signatoryname = userfullname author
                                  , signatorycompany = usercompanyname $ userinfo author
                                  , signatorynumber = usercompanynumber $ userinfo author
                                  , signatoryemail = unEmail $ useremail $ userinfo author
                                  , signatorynameplacements = []
                                  , signatorycompanyplacements = []
                                  , signatorynumberplacements = []
                                  , signatoryemailplacements = []
                                  , signatoryotherfields = []
                                  })
      -}
      signatoriesdetails = map signatorydetails $ documentsignatorylinks document

      signatories = personsFromDocument document

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
      -- 2. "Name of invited" granskar dokumentet online
      makeHistoryEntry (Seal.Person {Seal.fullname},(seentime2,seenipnumber2),(signtime2,signipnumber2)) = 
          [ Seal.HistEntry
            { Seal.histdate = show signtime2
            , Seal.histcomment = fullname ++ " undertecknar dokumentet online" ++ formatIP signipnumber2
            }
          , Seal.HistEntry
            { Seal.histdate = show seentime2
            , Seal.histcomment = fullname ++ " granskar dokumentet online" ++ formatIP seenipnumber2
            } 
          ]
      maxsigntime = maximum (map (fst . thd3) signatories)
      firstHistEntries = []
      {-
          [ Seal.HistEntry
            { Seal.histdate = show authorsigntime
            , Seal.histcomment = Seal.fullname authorperson ++ 
                                 " undertecknar dokumentet." ++ formatIP authorsignipnumber
            }
          , Seal.HistEntry
            { Seal.histdate = show authorsigntime
            , Seal.histcomment = "En automatisk inbjudan skickas via e-post till samtliga parter."
            }
          ]
      -}
      lastHistEntry = Seal.HistEntry
                      { Seal.histdate = show maxsigntime
                      , Seal.histcomment = "Samtliga parter har undertecknat dokumentet och avtalet är nu juridiskt bindande."
                      }

      concatComma = concat . intersperse ", "

      history = firstHistEntries ++ sort (concatMap makeHistoryEntry signatories) ++ [lastHistEntry]
      
      -- document fields

      fields = (concat (map fieldsFromSignatory signatoriesdetails))

      config = Seal.SealSpec 
            { Seal.input = inputpath
            , Seal.output = outputpath
            , Seal.documentNumber = paddeddocid
            , Seal.persons = persons
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

uploadDocumentFilesToTrustWeaver :: TW.TrustWeaverConf 
                                 -> TrustWeaverStorage 
                                 -> Document 
                                 -> IO ()
uploadDocumentFilesToTrustWeaver ctxtwconf twsettings document = do
  let twdocumentid = show (documentid document)
  let twdocumentdate = showDateOnly (documentmtime document)
  let twownername = BS.toString (storagetwname twsettings)
  let File{filestorage = FileStorageMemory pdfdata} = head $ documentsealedfiles document 
          
  -- FIXME: we should retry here if the following fails
  -- because of external reasons 
  reference <- eitherLog $ TW.storeInvoice ctxtwconf twdocumentid twdocumentdate twownername pdfdata
  update $ SetDocumentTrustWeaverReference (documentid document) reference
  return ()

sealDocument :: Context 
             -> MVar (Map.Map FileID JpegPages)
             -> String
             -> MinutesTime
             -> User
             -> Document
             -> IO Document
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
  contents <- AWS.getFileContents ctxs3action file
  BS.writeFile tmpin contents
  let config = sealSpecFromDocument hostpart document author tmpin tmpout

  (code,stdout,stderr) <- readProcessWithExitCode' "dist/build/pdfseal/pdfseal" [] (BSL.fromString (show config))

  when (code /= ExitSuccess) $
       putStrLn "Cannot execute dist/build/pdfseal/pdfseal"

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
        forkIO $ mapM_ (AWS.uploadFile ctxs3action) (documentsealedfiles document)
        case signeddocstorage (usersettings author) of
          Nothing -> return ()
          Just twsettings -> do
                        forkIO $ uploadDocumentFilesToTrustWeaver ctxtwconf twsettings document
                        return ()
        return document
    Left msg -> error msg
  -- let newfile = file {filepdf = newfilepdf}
  -- modifyMVar_ normalizemap (\themap -> return $ Map.delete fileid themap)
  -- update $ ReplaceFile (documentid document) newfile
  


basename :: String -> String
basename filename = 
    case span (\x -> x/='\\' || x/='/') filename of
      (_,(_:rest)) -> basename rest
      _ -> fst (span ((/=) '.') filename) -- FIXME: take care of many dots in file name

handleIssuePost :: Kontra KontraLink
handleIssuePost = handleIssueNewDocument `mplus` handleIssueArchive --This work just by accident, HAVE to be with cleaner flow  MR.

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
          doc <- update $ NewDocument user title ctxtime (freeleft>0)
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ LinkIssueDoc $ documentid doc


handleIssueArchive :: Kontra KontraLink
handleIssueArchive = do
    ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) <- get
    something <- getDataFnM (lookInput "archive")
    idstrings <- getDataFnM (lookInputList "doccheck")
    let Just ids = sequence $ map (readM . BSL.toString) idstrings
    update $ ArchiveDocuments ids
    return LinkIssue


showPage :: FileID -> Int -> Kontra Response
showPage fileid pageno = do
  Context{ctxnormalizeddocuments} <- get
  modminutes <- query $ FileModTime fileid
  docmap <- liftIO $ readMVar ctxnormalizeddocuments
  case Map.lookup fileid docmap of
    Just (JpegPages pages) -> do
      let contents = pages !! (pageno - 1)
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
      let modtime = toUTCTime modminutes
      rq <- askRq                 -- FIXME: what?
      return $ ifModifiedSince modtime rq res2
    _ -> mzero

--In this and next check if current is author MR
handleCancel:: String -> Kontra KontraLink
handleCancel docid =  do
                       ctx <- get
                       mdoc <- query $ GetDocumentByDocumentID (read docid)
                       customMessage <- fmap (fmap concatChunks) $ getDataFn' (lookBS "customtext")  
                       case (mdoc) of
                          Just doc -> withDocumentAuthor doc $
                                      do 
                                       mdoc' <- update $ CancelDocument(documentid doc) 
                                       let success = isJust mdoc
                                       case mdoc' of 
                                        Just doc' ->
                                             do
                                              sendCancelMailsForDocument customMessage ctx doc
                                              fm <-liftIO $ flashMessageCanceled (ctxtemplates ctx)
                                              addFlashMsgHtmlFromTemplate fm
                                        Nothing -> addFlashMsgText "Could not cancel"
                                       return (LinkIssueDoc $ documentid doc)
                          Nothing -> mzero  

handleWithdrawn:: String -> Kontra KontraLink
handleWithdrawn docid = do
                          mdoc <- query $ GetDocumentByDocumentID (read docid)  
                          case (mdoc) of
                            Just doc -> withDocumentAuthor doc $ 
                                         do
                                           update $ WithdrawnDocument $ documentid doc
                                           return (LinkIssueDoc $ documentid doc)
                            Nothing -> return LinkMain          
 

handleRestart:: String -> Kontra KontraLink
handleRestart docid = do
                       ctx <- get
                       case ctxmaybeuser ctx of
                        Just user -> do
                                      update $ RestartDocument (read docid) user
                                      addFlashMsgText =<< (liftIO $ flashDocumentRestarted (ctxtemplates ctx))
                                      return $ LinkIssueDoc (read docid)             
                        Nothing -> return LinkLogin          
                    
handleResend:: String -> String -> Kontra KontraLink
handleResend docid signlinkid  = do
  ctx <- get
  mdoc <- query $ GetDocumentByDocumentID (read docid)
  case (mdoc) of
    Just doc -> withDocumentAuthor doc $
                 case (signlinkFromDocById doc (read signlinkid)) of
                   Just signlink -> do 
                     Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor doc
                     customMessage <- fmap (fmap concatChunks) $ getDataFn' (lookBS "customtext")  
                     mail <- liftIO $  mailDocumentRemind (ctxtemplates ctx) customMessage ctx doc signlink author
                     liftIO $ sendMail (ctxmailsconfig ctx) (mail {fullnameemails = [(signatoryname $ signatorydetails signlink,signatoryemail $ signatorydetails signlink )],
                                                                   from=ctxmaybeuser ctx,
                                                                   mailInfo = Invitation $ signatorylinkid signlink })
                     addFlashMsgText =<< (liftIO $ flashRemindMailSent (ctxtemplates ctx) signlink)
                     return (LinkIssueDoc $ documentid doc)
                   Nothing -> mzero           
    Nothing -> mzero               

--This only works for undelivered mails. We shoulkd check if current user is author
handleChangeSignatoryEmail::String -> String -> Kontra KontraLink
handleChangeSignatoryEmail did slid = do
                                     let mdid = maybeRead did
                                     memail <- getField "email"
                                     let mslid = maybeRead slid
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
  liftIO $ forM_ (documentsignatorylinks document) (sendMail  (ctxmailsconfig ctx) <=< (mailCancelDocumentByAuthor (ctxtemplates ctx) customMessage ctx document author))

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

