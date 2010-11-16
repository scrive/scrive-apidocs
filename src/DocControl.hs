{-# LANGUAGE CPP, ScopedTypeVariables #-}

module DocControl where
import "base" Control.Monad
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Trans
import AppView
import Control.Concurrent
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
import MinutesTime
import Misc
import SendMail(Mail,sendMail,fullnameemails,attachments)
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Process
import User
import UserControl
import UserState
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Seal as Seal
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP


{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}

signlinkFromDocById doc sid = find (((==) sid) . signatorylinkid) (documentsignatorylinks  doc)

postDocumentChangeAction :: Document -> DocumentStatus -> Maybe SignatoryLinkID -> Kontra ()
postDocumentChangeAction document@Document{documentstatus, documentsignatorylinks} oldstatus msignalinkid
    | documentstatus == oldstatus = return ()
    | oldstatus == Preparation && documentstatus == Pending = do
        ctx <- get
        liftIO $ forkIO $ do
          -- this is here to postpone email send a couple of seconds
          -- so our service has a chance to give answer first
          -- is GHC using blocking calls or what?
          threadDelay 5000 
          sendInvitationEmails ctx document
        return ()
    | oldstatus == Pending && documentstatus == AwaitingAuthor = do
        ctx <- get
        liftIO $ forkIO $ do
          threadDelay 5000 
          sendAwaitingEmail ctx document
        return ()
    | (oldstatus == Pending || oldstatus == AwaitingAuthor) && documentstatus == Closed = do
        ctx@Context{ctxnormalizeddocuments,ctxhostpart,ctxtime} <- get
        liftIO $ forkIO $ do
          Just user <- query $ GetUserByUserID (unAuthor (documentauthor document))
          newdoc <- sealDocument ctx ctxnormalizeddocuments ctxhostpart ctxtime user document
          sendClosedEmails ctx newdoc
        return ()
    | oldstatus == Pending && documentstatus == Rejected = do
        ctx@Context{ctxnormalizeddocuments,ctxhostpart,ctxtime} <- get
        customMessage <- fmap (fmap concatChunks) $ getDataFn' (lookBS "customtext")  
        liftIO $ forkIO $ do
          threadDelay 5000 
          sendRejectAuthorEmail customMessage ctx document (fromJust msignalink)
        return ()
    | otherwise = -- do nothing. FIXME: log status change
         return ()
    where msignalink = maybe Nothing (signlinkFromDocById document) msignalinkid
          

sendInvitationEmails :: Context -> Document -> IO ()
sendInvitationEmails ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendInvitationEmail1 ctx document)

sendInvitationEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendInvitationEmail1 ctx document signatorylink = do
  let SignatoryLink{ signatorylinkid
                   , signatorydetails = SignatoryDetails { signatoryname
                                                         , signatorycompany
                                                         , signatoryemail
                                                         }
                   , signatorymagichash } = signatorylink
      Document{documenttitle,documentid} = document
  mail <- mailInvitationToSign ctx document signatorylink
  let attachmentcontent = filepdf $ head $ documentfiles document
  sendMail $ mail { fullnameemails =  [(signatoryname,signatoryemail)]  , attachments = [(documenttitle,attachmentcontent)]} 

sendClosedEmails :: Context -> Document -> IO ()
sendClosedEmails ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendClosedEmail1 ctx document)
  sendClosedAuthorEmail ctx document

sendClosedEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendClosedEmail1 ctx document signatorylink = do
  let SignatoryLink{ signatorylinkid
                   , signatorymagichash
                   , signatorydetails = SignatoryDetails { signatoryname
                                                         , signatorycompany
                                                         , signatoryemail }} = signatorylink
      Document{documenttitle,documentid} = document
  mail <- mailDocumentClosedForSignatories ctx document signatorylink
  let attachmentcontent = filepdf $ head $ documentsealedfiles document
  sendMail $ mail { fullnameemails =  [(signatoryname,signatoryemail)] , attachments = [(documenttitle,attachmentcontent)]}

sendAwaitingEmail :: Context -> Document -> IO ()
sendAwaitingEmail ctx document = do
  let authorid = unAuthor $ documentauthor document
  Just authoruser <- query $ GetUserByUserID authorid
  mail<- mailDocumentAwaitingForAuthor ctx (userfullname authoruser)
             document
  let email2 = signatoryemail $ documentauthordetails document
      email1 = unEmail $ useremail authoruser
      em = if email2/=BS.empty && email2/=email1
           then [(name2,email2)]
           else []
      name1 = userfullname authoruser
      name2 = signatoryname $ documentauthordetails document
  sendMail $ mail { fullnameemails = ([(name1,email1)] ++ em) }

sendClosedAuthorEmail :: Context -> Document -> IO ()
sendClosedAuthorEmail ctx document = do
  let authorid = unAuthor $ documentauthor document
  Just authoruser <- query $ GetUserByUserID authorid
  mail<- mailDocumentClosedForAuthor ctx (userfullname authoruser)
             document
  let attachmentcontent = filepdf $ head $ documentsealedfiles document
      Document{documenttitle,documentid} = document
  let email2 = signatoryemail $ documentauthordetails document
      email1 = unEmail $ useremail authoruser
      em = if email2/=BS.empty && email2/=email1
           then [(name2,email2)]
           else []
      name1 = userfullname authoruser
      name2 = signatoryname $ documentauthordetails document
  sendMail $ mail { fullnameemails = ([(name1,email1)] ++ em), attachments = [(documenttitle,attachmentcontent)]}

sendRejectAuthorEmail :: (Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> IO ()
sendRejectAuthorEmail customMessage ctx document signalink = do
  let authorid = unAuthor $ documentauthor document
  Just authoruser <- query $ GetUserByUserID authorid
  let rejectorName = signatoryname (signatorydetails signalink)
  mail <- mailDocumentRejectedForAuthor customMessage ctx (userfullname authoruser)
             document rejectorName
  let email2 = signatoryemail $ documentauthordetails document
      email1 = unEmail $ useremail authoruser
      em = if email2/=BS.empty && email2/=email1
           then [(name2,email2)]
           else []
      name1 = userfullname authoruser
      name2 = signatoryname $ documentauthordetails document
  sendMail $ mail { fullnameemails = ([(name1,email1)] ++ em)}

handleSTable = withUserGet $ checkUserTOSGet $
    do
      ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime}) <- get
      let u = userid $ fromJust ctxmaybeuser
      documents <- query $ GetDocumentsBySignatory u
      renderFromBody ctx TopNone kontrakcja (pageDocumentList ctxtime u documents)

signDocument :: DocumentID 
             -> SignatoryLinkID 
             -> Kontra KontraLink
signDocument documentid 
             signatorylinkid1 = do
  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxipnumber}) <- get
  getDataFnM (look "sign")
  fieldnames <- getAndConcat "fieldname"
  fieldvalues <- getAndConcat "fieldvalue"
  let fields = zipWith (\x y -> (x, y)) fieldnames fieldvalues
  do
     Just olddocument@Document{documentstatus=olddocumentstatus} <- query $ GetDocumentByDocumentID documentid
     newdocument <- update $ SignDocument documentid signatorylinkid1 ctxtime ctxipnumber fields
     case newdocument of
       Left message -> 
           do
             addFlashMsgText $ BS.fromString message
             return $ LinkMain
       Right document -> 
           do 
             postDocumentChangeAction document olddocumentstatus (Just signatorylinkid1)
             return $ LinkSigned documentid signatorylinkid1

rejectDocument :: DocumentID 
               -> SignatoryLinkID 
               -> Kontra KontraLink
rejectDocument documentid 
               signatorylinkid1 = do
  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxipnumber}) <- get
  getDataFnM (look "cancel")
  do
     mdocument <- update $ RejectDocument documentid signatorylinkid1 ctxtime ctxipnumber
     case (mdocument) of
      Left message -> 
          do
            addFlashMsgText $ BS.fromString message
            return $ LinkMain
      Right document -> 
          do  
            postDocumentChangeAction document Pending (Just signatorylinkid1)
            return $ LinkRejected documentid signatorylinkid1
  
signatoryLinkFromDocumentByID document@Document{documentsignatorylinks} linkid = do
    let invitedlinks = filter (\x -> signatorylinkid x == linkid
                               {- && signatorymagichash x == magichash1 -})
                              documentsignatorylinks
    case invitedlinks of
      [invitedlink] -> return invitedlink
      _ -> mzero
    

landpageSignInvite documentid = do
  ctx <- get
  mdocument <- query $ GetDocumentByDocumentID documentid
  case mdocument of
    Nothing -> mzero
    Just document -> renderFromBody ctx TopNone kontrakcja $ landpageSignInviteView document


landpageSigned documentid signatorylinkid = do
  ctx <- get
  mdocument <- query $ GetDocumentByDocumentID documentid
  case mdocument of
    Nothing -> mzero
    Just document -> do
                     signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
                     maybeuser <- query $ GetUserByEmail (Email $ signatoryemail (signatorydetails signatorylink))
                     renderFromBody ctx TopEmpty kontrakcja $ landpageSignedView document signatorylink (isJust maybeuser)

landpageRejected documentid signatorylinkid = do
  ctx <- get
  mdocument <- query $ GetDocumentByDocumentID documentid
  case mdocument of
    Nothing -> mzero
    Just document -> do
                     signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
                     maybeuser <- query $ GetUserByEmail (Email $ signatoryemail (signatorydetails signatorylink))
                     renderFromBody ctx TopEmpty kontrakcja $ landpageRejectedView document
{-
 Here we need to save the document either under existing account or create a new account
 send invitation email and put the document in that account
-}
landpageSignedSave documentid signatorylinkid = do
  ctx@Context{ctxhostpart} <- get
  mdocument <- query $ GetDocumentByDocumentID documentid
  case mdocument of
    Nothing -> mzero
    Just document -> do
     signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
     let details = signatorydetails signatorylink
     maybeuser <- query $ GetUserByEmail (Email $ signatoryemail details)
     let fullname = signatoryname details
     user <- case maybeuser of
            Nothing -> do 
              let email = signatoryemail details
              user <- liftIO $ createUser ctxhostpart fullname email Nothing Nothing
              return user
            Just user -> return user
     Just document2 <- update $ SaveDocumentForSignedUser documentid (userid user) signatorylinkid
     -- should redirect
     renderFromBody ctx TopEmpty kontrakcja $ landpageLoginForSaveView

landpageSaved documentid signatorylinkid = do
  (ctx@Context { ctxmaybeuser = Just user@User{userid} }) <- get
  mdocument <- query $ GetDocumentByDocumentID documentid
  case mdocument of
    Nothing -> mzero
    Just document -> do
                   Just document2 <- update $ SaveDocumentForSignedUser documentid userid signatorylinkid
                   signatorylink <- signatoryLinkFromDocumentByID document signatorylinkid
                   renderFromBody ctx TopDocument kontrakcja $ landpageDocumentSavedView

handleSignPost :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra KontraLink
handleSignPost documentid 
               signatorylinkid1
               magichash1 = do
  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxipnumber}) <- get
               
  msum [ DocControl.signDocument documentid signatorylinkid1
       , DocControl.rejectDocument documentid signatorylinkid1
       ]

handleSignShow :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra Response
handleSignShow documentid 
               signatorylinkid1
               magichash1 = do
  ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime, ctxipnumber}) <- get
               
  -- FIXME: this is so wrong on so many different levels...
  mdocument <- update $ MarkDocumentSeen documentid signatorylinkid1 ctxtime ctxipnumber
  document <- maybe mzero return mdocument
             
  let invitedlinks = filter (\x -> signatorylinkid x == signatorylinkid1
                                   && signatorymagichash x == magichash1)
                     (documentsignatorylinks document)
  invitedlink <- case invitedlinks of
                   [invitedlink] -> return invitedlink
                   _ -> mzero
  let wassigned = f invitedlink
      f (SignatoryLink {maybesigninfo}) = isJust maybesigninfo 
      authoruserid = unAuthor $ documentauthor document
  Just author <- query $ GetUserByUserID authoruserid
  let authorname = signatoryname $ documentauthordetails document
      invitedname = signatoryname $ signatorydetails $ invitedlink 
  renderFromBody ctx TopNone kontrakcja 
                     (pageDocumentForSign (LinkSignDoc document invitedlink) 
                      document ctx invitedlink wassigned)

freeLeftForUser :: User -> Kontra Int
freeLeftForUser user = do
  numdoc <- query $ GetNumberOfDocumentsOfUser user
  let freeleft = if numdoc >= 5 then 0 else 5 - numdoc
  return freeleft

handleIssueShowGet :: DocumentID -> Kontra Response
handleIssueShowGet docid = withUserGet $ checkUserTOSGet $
 do
  maybedocument <- query $ GetDocumentByDocumentID $ docid
  case maybedocument of
    -- Q: We know what they want, shouldn't we send 404 here?
    Nothing -> mzero
    Just document@Document{ documentauthor
                          , documentid
                          } -> do
                       ctx@(Context {ctxmaybeuser = Just (user@User{userid}), ctxhostpart}) <- get
                       when (userid/=unAuthor documentauthor) mzero
                       let toptab = if documentstatus document == Closed
                                     then TopDocument
                                     else TopNone
                       renderFromBody ctx toptab kontrakcja 
                                          (pageDocumentForAuthor ctx document)



handleIssueShowPost :: DocumentID -> Kontra KontraLink
handleIssueShowPost docid = withUserPost $
 do
  maybedocument <- query $ GetDocumentByDocumentID $ docid
  case maybedocument of
    -- Q: We know what they want, shouldn't we send 404 here?
    Nothing -> mzero
    Just document@Document{ documentauthor
                          , documentid
                          } -> do
  
     ctx@(Context {ctxmaybeuser = Just (user@User{userid}), ctxhostpart}) <- get
 
     when (userid/=unAuthor documentauthor) mzero

     
     -- something has to change here
     case documentstatus document of
       Preparation -> do
                       doc2 <- updateDocument ctx document
                       if documentstatus doc2 == Pending
                        -- It went to pending, so it's been sent to signatories
                        then return $ LinkSignInvite documentid
                        -- otherwise it was just a save
                        else do
                          addFlashMsgHtml $ flashDocumentDraftSaved
                          return LinkIssue
       AwaitingAuthor -> do 
                          doc2 <- update $ CloseDocument documentid 
                          case doc2 of
                            -- this should be impossible, but what should we do in this case?
                            Nothing -> return LinkIssue
                            Just d -> do 
                                       postDocumentChangeAction d AwaitingAuthor Nothing
                                       return LinkIssue
       -- FIXME
       _ -> mzero
{-
     if (documentstatus doc2 == Pending &&
         documentstatus document /= Pending) 
      then return $ LinkSignInvite documentid
      else do 
        addFlashMsgHtml $ flashDocumentDraftSaved
        return LinkIssue
-}

handleIssueShowTitleGet :: DocumentID -> String -> Kontra Response
handleIssueShowTitleGet docid _title = withUserGet $ checkUserTOSGet $
  do
   maybedocument <- query $ GetDocumentByDocumentID $ docid
   case maybedocument of
     -- Q: We know what they want, shouldn't we send 404 here?
     Nothing -> mzero
     Just document@Document{ documentauthor
                           , documentid
                           } -> do

                        ctx@(Context {ctxmaybeuser = Just (user@User{userid}), ctxhostpart}) <- get
                        when (userid/=unAuthor documentauthor) mzero
                        let file = safehead "handleIssueShow" (case documentstatus document of
                                                                 Closed -> documentsealedfiles document
                                                                 _      -> documentfiles document)
                        let contents = filepdf file
                        let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
                        let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
                        return res2

getAndConcat :: String -> Kontra [BS.ByteString]
getAndConcat field = do
  values <- getDataFnM $ lookInputList field
  return $ map concatChunks values

updateDocument :: Context -> Document -> Kontra Document  
updateDocument ctx@Context{ctxtime,ctxipnumber} document@Document{documentid, documentauthordetails} = do
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

  let author2 = documentauthordetails { signatorynameplacements = placementsByID (BS.fromString "author") (BS.fromString "name"),
                                        signatorycompanyplacements = placementsByID (BS.fromString "author") (BS.fromString "company"),
                                        signatoryemailplacements = placementsByID (BS.fromString "author") (BS.fromString "email"),
                                        signatorynumberplacements = placementsByID (BS.fromString "author") (BS.fromString "number"),
                                        signatoryotherfields = defsByID (BS.fromString "author") }
 
  -- FIXME: tell the user what happened!
  -- when (daystosign<1 || daystosign>99) mzero
  
  doc2 <- update $ UpdateDocument ctxtime documentid
          signatories author2 daystosign invitetext

  msum 
     [ do getDataFnM (look "final" `mplus` look "sign")
          mdocument <- update $ AuthorSignDocument documentid ctxtime ctxipnumber
          case mdocument of
            Left msg -> return doc2
            Right newdocument -> do
                postDocumentChangeAction newdocument (documentstatus doc2) Nothing
                return newdocument
     , return doc2
     ]
    
handleIssueGet :: Kontra Response
handleIssueGet = withUserGet $ checkUserTOSGet $ 
 do
  ctx@(Context {ctxmaybeuser = Just user, ctxhostpart, ctxtime}) <- get
  documents <- query $ GetDocumentsByUser (userid user) 
  renderFromBody ctx TopDocument kontrakcja (pageDocumentList ctxtime (userid user) documents)

gs :: String
#ifdef WINDOWS
gs = "c:\\Program Files\\gs\\gs8.60\\bin\\gswin32c.exe" 
#else
gs = "gs"
#endif

awsUrlFromFile :: File -> String
awsUrlFromFile File{filename,fileid} =
    "/" ++ show fileid ++ "/" ++ BS.toString filename

convertPdfToJpgPages :: Context
                     -> File
                     -> IO JpegPages
convertPdfToJpgPages Context{ctxs3action} file@File{fileid,filestoragemode,filename} = do
  tmppath1 <- getTemporaryDirectory
  let tmppath = tmppath1 ++ "/" ++ show fileid
  createDirectoryIfMissing True tmppath
  let sourcepath = tmppath ++ "/source.pdf"
  content <- case filestoragemode of
               FileStorageLocal -> return $ filepdf file
               FileStorageAWS -> do
                 let url = awsUrlFromFile file
                 result <- AWS.runAction (ctxs3action { AWS.s3object = url })
                 case result of
                   Right rsp -> return (concatChunks (HTTP.rspBody rsp))
                   _ -> error (show result)


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
  -- removeDirectoryRecursive tmppath
  return result
       

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

handlePageOfDocument :: DocumentID -> Kontra Response
handlePageOfDocument documentid = do
  mdocument <- query $ GetDocumentByDocumentID documentid
  case mdocument of
    Nothing -> mzero
    Just document@Document {documentfiles,documentsealedfiles,documentstatus,documentid} -> do
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
  ctx <- get
  result <- update $ AttachFile docid filename content
  case result of
    Left err -> return ()
    Right document -> do
        liftIO $ forkIO $ mapM_ (amazonUploadFile ctx) (documentfiles document)
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
sealSpecFromDocument hostpart document author@(User {userfullname,usercompanyname,usercompanynumber,useremail}) inputpath outputpath =
  let docid = unDocumentID (documentid document)
      (authorsigntime,authorsignipnumber) = 
          case documentmaybesigninfo document of
            Nothing -> error "signtime1"
            Just (SignInfo {signtime,signipnumber}) -> (signtime,signipnumber)
      authordetails = documentauthordetails document
      authorperson = if authordetails /= emptyDetails 
                     then personFromSignatoryDetails authordetails
                     else personFromSignatoryDetails (SignatoryDetails 
                                  { signatoryname = userfullname
                                  , signatorycompany = usercompanyname
                                  , signatorynumber = usercompanynumber
                                  , signatoryemail = unEmail $ useremail
                                  , signatorynameplacements = []
                                  , signatorycompanyplacements = []
                                  , signatorynumberplacements = []
                                  , signatoryemailplacements = []
                                  , signatoryotherfields = []
                                  })
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
      persons = authorperson : (map fst3 signatories)
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
      maxsigntime = maximum (authorsigntime : map (fst . thd3) signatories)
      firstHistEntries = 
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
      lastHistEntry = Seal.HistEntry
                      { Seal.histdate = show maxsigntime
                      , Seal.histcomment = "Samtliga parter har undertecknat dokumentet och avtalet är nu juridiskt bindande."
                      }

      concatComma = concat . intersperse ", "

      history = firstHistEntries ++ sort (concatMap makeHistoryEntry signatories) ++ [lastHistEntry]
      
      -- document fields

      fields = (foldl (++) (fieldsFromSignatory authordetails) (map fieldsFromSignatory signatoriesdetails))

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


sealDocument :: Context 
             -> MVar (Map.Map FileID JpegPages)
             -> String
             -> MinutesTime
             -> User
             -> Document
             -> IO Document
sealDocument ctx
             normalizemap 
             hostpart
             signtime1
             author@(User {userfullname,usercompanyname,usercompanynumber,useremail})
             document = do
  let (file@File {fileid,filename,filepdf}) = 
           safehead "sealDocument" $ documentfiles document
  let docid = documentid document

  tmppath <- getTemporaryDirectory
  let tmpin = tmppath ++ "/in_" ++ show docid ++ ".pdf"
  let tmpout = tmppath ++ "/out_" ++ show docid ++ ".pdf"
  BS.writeFile tmpin filepdf
  let config = sealSpecFromDocument hostpart document author tmpin tmpout

  (code,stdout,stderr) <- readProcessWithExitCode' "dist/build/pdfseal/pdfseal" [] (BSL.fromString (show config))

  when (code /= ExitSuccess) $
       putStrLn "Cannot execute dist/build/pdfseal/pdfseal"

  newfilepdf <- BS.readFile tmpout
  mdocument <- update $ AttachSealedFile docid filename newfilepdf
  case mdocument of
    Right document -> do
        forkIO $ mapM_ (amazonUploadFile ctx) (documentsealedfiles document)
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
          liftIO $ print (useremail user, documentid doc,title)
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ LinkIssueDoc doc


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
                       case (mdoc) of
                          Just doc -> withDocumentAuthor doc $
                                      do 
                                       mdoc' <- update $ CancelDocument(documentid doc) 
                                       let info = if (isJust mdoc') then "Document canceled"  else "Document dould not be canceled"   --Move texts to view 
                                       addFlashMsgText (BS.fromString info)
                                       return (LinkIssueDoc doc)
                          Nothing -> mzero  
                         
handleResend:: String -> String -> Kontra KontraLink
handleResend docid signlinkid  =
                    do
                      ctx <- get
                      mdoc <- query $ GetDocumentByDocumentID (read docid)
                      case (mdoc) of
                       Just doc -> withDocumentAuthor doc $
                               case (signlinkFromDocById doc (read signlinkid)) of
                                 Just signlink -> 
                                  do 
                                   customMessage <- fmap (fmap concatChunks) $ getDataFn' (lookBS "customtext")  
                                   mail <- liftIO $ remindMail customMessage ctx doc signlink
                                   liftIO $ sendMail (mail {fullnameemails = [(signatoryname $ signatorydetails signlink,signatoryemail $ signatorydetails signlink )]})
                                   addFlashMsgText ( flashRemindMailSent signlink)
                                   return (LinkIssueDoc doc)
                                 Nothing -> mzero           
                       Nothing -> mzero               

amazonUploadFile ctx@Context{ctxs3action} file = do
  let action = ctxs3action { AWS.s3object = awsUrlFromFile file
                           , AWS.s3operation = HTTP.PUT
                           , AWS.s3body = BSL.fromChunks [filepdf file]
                           }
  print ("Uploading AWS", filename file)
  result <- AWS.runAction action
  case result of
    Right _ -> return ()
    -- FIXME: do much better error handling
    Left err -> print err >> return ()
  