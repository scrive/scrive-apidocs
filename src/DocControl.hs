{-# LANGUAGE CPP, ScopedTypeVariables #-}

module DocControl where
import DocView
import DocState hiding (updateDocument)
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import AppView
import UserState
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.Lazy as BSL
import Control.Monad
import Control.Monad.Reader
import HSP
import Data.Maybe
import Control.Monad.Trans
import Misc
import Debug.Trace
import User
import System.Cmd
import System.Directory
import qualified Data.Map as M
import Data.List
import MinutesTime
import Control.Concurrent
import SendMail
import System.Process
import System.IO
import Seal
import Happstack.Util.Common
import KontraLink
import System.Exit
import qualified Data.Set as Set

{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}

doctransPreparation2Pending :: Context -> Document -> IO Document
doctransPreparation2Pending ctx@Context{ctxtime = MinutesTime m } doc = do
  -- FIXME: check if the status was really changed
  newdoc <- update $ UpdateDocumentStatus (ctxtime ctx) doc Pending
  let timeout = TimeoutTime (MinutesTime (m + documentdaystosign doc * 24 * 60))
  newdoc2 <- update $ SetDocumentTimeoutTime newdoc timeout
  liftIO $ sendInvitationEmails ctx newdoc2
  return newdoc2

doctransPending2Closed :: Context -> Document -> IO Document
doctransPending2Closed ctx@Context{ctxtime} doc = do
  update $ UpdateDocumentStatus ctxtime doc Closed
  newdoc <- update $ RemoveFileFromDoc (documentid doc)
  Just user <- query $ GetUserByUserID (unAuthor (documentauthor doc))
  liftIO $ forkIO $ do
    newdoc <- sealDocument ctxtime user doc
    sendClosedEmails ctx newdoc
  return newdoc

doctransPending2Canceled :: Context -> Document -> IO Document
doctransPending2Canceled ctx@Context{ctxtime} doc = do
  update $ UpdateDocumentStatus ctxtime doc Canceled

doctransPending2Timedout :: Context -> Document -> IO Document
doctransPending2Timedout ctx@Context{ctxtime} doc = do
  update $ UpdateDocumentStatus ctxtime doc Timedout

sendInvitationEmails :: Context -> Document -> IO ()
sendInvitationEmails ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendInvitationEmail1 ctx document)

sendInvitationEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendInvitationEmail1 ctx document signlink = do
  let SignatoryLink{ signatorylinkid
                   , signatorydetails = SignatoryDetails { signatoryname
                                                         , signatorycompany
                                                         , signatoryemail
                                                         }
                   , signatorymagichash } = signlink
      Document{documenttitle,documentid} = document
  content <- invitationMail ctx signatoryemail signatoryname
             document signatorylinkid signatorymagichash

  sendMail [(signatoryname,signatoryemail)] documenttitle content 
           (filepdf $ head $ documentfiles document)

sendClosedEmails :: Context -> Document -> IO ()
sendClosedEmails ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendClosedEmail1 ctx document)
  sendClosedAuthorEmail ctx document

sendClosedEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendClosedEmail1 ctx document signlink = do
  let SignatoryLink{ signatorylinkid
                   , signatorymagichash
                   , signatorydetails = SignatoryDetails { signatoryname
                                                         , signatorycompany
                                                         , signatoryemail }} = signlink
      Document{documenttitle,documentid} = document
  content <- closedMail ctx signatoryemail signatoryname
             document signatorylinkid signatorymagichash
  let attachmentcontent = filepdf $ head $ documentfiles document
  sendMail [(signatoryname,signatoryemail)] documenttitle content attachmentcontent

sendClosedAuthorEmail :: Context -> Document -> IO ()
sendClosedAuthorEmail ctx document = do
  let authorid = unAuthor $ documentauthor document
  Just authoruser <- query $ GetUserByUserID authorid
  content <- closedMailAuthor ctx (unEmail $ useremail authoruser) (userfullname authoruser)
             document
  let attachmentcontent = filepdf $ head $ documentfiles document
  let email2 = signatoryemail $ documentauthordetails document
      email1 = unEmail $ useremail authoruser
      em = if email2/=BS.empty && email2/=email1
           then [(name2,email2)]
           else []
      name1 = userfullname authoruser
      name2 = signatoryname $ documentauthordetails document
  sendMail ([(name1,email1)] ++ em)
           (documenttitle document) content attachmentcontent
  
  
handleSign :: Context -> Kontra Response
handleSign ctx@(Context {ctxmaybeuser, ctxhostpart}) = 
    msum [ path $ \documentid -> 
               path $ \signatoryid -> 
                   path $ \magichash -> 
                       handleSignShow ctx documentid signatoryid magichash
         , withUser ctxmaybeuser $ do
             documents <- query $ GetDocumentsBySignatory (userid $ fromJust ctxmaybeuser) 
             webHSP (pageFromBody ctx TopNone kontrakcja (listDocuments documents))
         ]

signDoc :: Context -> DocumentID -> SignatoryLinkID -> Kontra  Response
signDoc ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime}) documentid 
               signatorylinkid1 = do
  getDataFnM (look "sign")
  
  Just document <- update $ SignDocument documentid signatorylinkid1 ctxtime
  let isallsigned = all f (documentsignatorylinks document)
      f (SignatoryLink {maybesigninfo}) = isJust maybesigninfo
  when isallsigned ((liftIO $ doctransPending2Closed ctx document) >> return ())

  let link = "/landpage/signed/" ++ show documentid ++ "/" ++ show signatorylinkid1
  response <- webHSP (seeOtherXML link)
  seeOther link response

landpageSignInvite ctx document = do
  webHSP $ pageFromBody ctx TopNone kontrakcja $ landpageSignInviteView ctx document

landpageSigned ctx document signatorylinkid = do
  webHSP $ pageFromBody ctx TopEmpty kontrakcja $ landpageSignedView ctx document signatorylinkid

landpageSignedSave ctx document signatorylinkid = do
  webHSP $ pageFromBody ctx TopEmpty kontrakcja $ landpageLoginForSaveView ctx document signatorylinkid

landpageSaved (ctx@Context { ctxmaybeuser = Just user }) document signatorylinkid = do
  Just document2 <- update $ SaveDocumentForSignedUser (documentid document) (userid user) signatorylinkid
  webHSP $ pageFromBody ctx TopDocument kontrakcja $ landpageDocumentSavedView ctx document signatorylinkid


handleSignShow :: Context -> DocumentID -> SignatoryLinkID -> MagicHash -> Kontra Response
handleSignShow ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime})
               documentid 
               signatorylinkid1
               magichash1 = do
  msum [ signDoc ctx documentid signatorylinkid1
       , do 
           -- FIXME: this is so wrong on so many different levels...
          mdocument <- update $ MarkDocumentSeen documentid signatorylinkid1 ctxtime
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
          webHSP (pageFromBody ctx TopNone kontrakcja 
                 (showDocumentForSign (LinkSignDoc document signatorylinkid1 magichash1) 
                       document authorname invitedname wassigned))
       ]

handleIssue :: Context -> Kontra Response
handleIssue ctx@(Context {ctxmaybeuser = Just user, ctxhostpart}) = 
    msum [ pathdb GetDocumentByDocumentID (handleIssueShow ctx)
         , methodM GET >> handleIssueGet ctx
         , methodM POST >> handleIssuePost ctx
         ]

freeLeftForUser :: User -> Kontra Int
freeLeftForUser user = do
  numdoc <- query $ GetNumberOfDocumentsOfUser user
  let freeleft = if numdoc >= 5 then 0 else 5 - numdoc
  return freeleft

handleIssueShow :: Context -> Document -> Kontra Response
handleIssueShow ctx@(Context {ctxmaybeuser = Just (user@User{userid}), ctxhostpart}) document = do
  msum [ do
           methodM GET 
           freeleft <- freeLeftForUser user
           webHSP (pageFromBody ctx TopDocument kontrakcja 
                                    (showDocument user document False freeleft))
       , do
           methodM POST
           doc2 <- updateDocument ctx document
           
           if (documentstatus doc2 == Pending &&
               documentstatus document /= Pending) 
             then do
              let link = "/landpage/signinvite/" ++ show (documentid document)
              response <- webHSP (seeOtherXML link)
              seeOther link response
            else do 
             let link = ctxhostpart ++ "/issue"
             flashmsg <- documentSavedForLaterFlashMessage doc2
             liftIO $ update $ AddUserFlashMessage userid flashmsg
             response <- webHSP (seeOtherXML link)
             seeOther link response

       , path $ \(_title::String) -> methodM GET >> do
           let file = safehead "handleIssueShow" (documentfiles document)
           let contents = filepdf file
           let res = Response 200 M.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
           let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
           return res2
       ]

-- | Useful inside the RqData monad.  Gets the named input parameter
-- (either from a POST or a GET)
lookInputList :: String -> RqData [BSL.ByteString]
lookInputList name
    = do 
#if MIN_VERSION_happstack_server(0,5,1)
         inputs <- asks (\(a,b,c) -> a ++ b)
#else
         inputs <- asks fst
#endif
         let isname (xname,(Input value _ _)) | xname == name = [value]
             isname _ = []
         return [value | k <- inputs, value <- isname k]

getAndConcat :: String -> Kontra [BS.ByteString]
getAndConcat field = do
  values <- getDataFnM $ lookInputList field
  return $ map concatChunks values

updateDocument :: Context -> Document -> Kontra Document  
updateDocument ctx@Context{ctxtime} document = do
  signatoriesnames <- getAndConcat "signatoryname"
  signatoriescompanies <- getAndConcat "signatorycompany"
  signatoriesnumbers <- getAndConcat "signatorynumber"
  signatoriesemails <- getAndConcat "signatoryemail"
  daystosignstring <- getDataFnM (look "daystosign")
  daystosign <- readM daystosignstring
  authorname <- getDataFnM (look "authorname")
  authorcompany <- getDataFnM (look "authorcompany")
  authornumber <- getDataFnM (look "authornumber")
  authoremail <- getDataFnM (look "authoremail")

  let signatories = zipWith4 SignatoryDetails signatoriesnames signatoriescompanies signatoriesnumbers signatoriesemails
  let authordetails = SignatoryDetails (BS.fromString authorname) 
                                       (BS.fromString authorcompany) 
                                       (BS.fromString authornumber) 
                                       (BS.fromString authoremail)

  -- FIXME: tell the user what happened!
  when (daystosign<1 || daystosign>99) mzero
  
  doc2 <- update $ UpdateDocument ctxtime document 
          authordetails signatories daystosign

  msum 
     [ do getDataFnM (look "final")
          liftIO $ doctransPreparation2Pending ctx doc2
     , return doc2
     ]
    

handleIssueGet :: (MonadIO m) => Context -> m Response
handleIssueGet ctx@(Context {ctxmaybeuser = Just user, ctxhostpart}) = do
  documents <- query $ GetDocumentsByUser (userid user) 
  webHSP (pageFromBody ctx TopDocument kontrakcja (listDocuments documents))

gs :: String
#ifdef WINDOWS
gs = "c:\\Program Files\\gs\\gs8.60\\bin\\gswin32c.exe" 
#else
gs = "gs"
#endif

convertPdfToJpgPages :: FileID ->BS.ByteString ->IO JpegPages
convertPdfToJpgPages fileid content = do
  tmppath1 <- getTemporaryDirectory
  let tmppath = tmppath1 ++ "/" ++ show fileid
  createDirectoryIfMissing True tmppath
  let sourcepath = tmppath ++ "/source.pdf"
  BS.writeFile sourcepath content

  let gsproc = (proc gs [ "-sDEVICE=jpeg" 
                        , "-sOutputFile=" ++ tmppath ++ "/output-%d.jpg"
                        , "-dSAFER"
                        , "-dBATCH"
                        , "-dNOPAUSE"
                        , "-dTextAlphaBits=4"
                        , "-r144"
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
       

maybeScheduleRendering :: MVar (Set.Set FileID) -> DocumentID -> File -> IO Bool
maybeScheduleRendering mvar 
                       documentid 
                       (file@File { filejpgpages = JpegPagesPending
                                  , fileid
                                  , filepdf 
                                  }) = do
  modifyMVar mvar $ \setoffilesrenderednow ->
      if Set.member fileid setoffilesrenderednow
         then return (setoffilesrenderednow, True)
         else do
           forkIO $ do
                -- putStrLn $ "Rendering " ++ show fileid
                -- FIXME: handle exceptions gracefully
                jpegpages <- convertPdfToJpgPages fileid filepdf
                update $ ReplaceFile documentid (file { filejpgpages = jpegpages })
                modifyMVar_ mvar (\setoffilesrenderednow -> return (Set.delete fileid setoffilesrenderednow))
           return (Set.insert fileid setoffilesrenderednow, True)
maybeScheduleRendering _ _ _ = return False

handlePageOfDocument mvar document@Document {documentfiles,documentid} =
    case documentfiles of
      [] -> notFound (toResponse "temporary unavailable (document has no files)")
      f -> do
        b <- mapM (\file -> liftIO $ maybeScheduleRendering mvar documentid file) f
        if any id b
           then notFound (toResponse "temporary unavailable (document has files pending for process)")
           else webHSP (DocView.showFilesImages2 f)
    
handleDocumentUpload :: DocumentID -> BS.ByteString -> BS.ByteString -> IO ()
handleDocumentUpload docid content filename = do
  update $ AttachFile docid filename content

sealLine :: BS.ByteString -> [BS.ByteString] -> MinutesTime -> SealPerson
sealLine fullname details signtime =
    SealPerson (BS.toString fullname ++ ", ") 
                   ((concat $ intersperse ", " (map BS.toString (filter (not . BS.null) details))) ++ 
                                "  |  " ++ showDateOnly signtime)

personsFromDocument :: Document -> [SealPerson]
personsFromDocument document = 
    let
        links = documentsignatorylinks document
        x (SignatoryLink{ signatorydetails = SignatoryDetails{ signatoryname
                                                             , signatorycompany
                                                             , signatorynumber 
                                                             }
                        , maybesigninfo = Just (SignInfo { signtime })})
             = sealLine signatoryname [signatorycompany, signatorynumber] signtime

    in map x links

sealDocument :: MinutesTime -> User -> Document -> IO Document
sealDocument signtime1 author@(User {userfullname,usercompanyname,usercompanynumber}) document = do
  let (file@File {fileid,filename,filepdf,filejpgpages}) = 
           safehead "sealDocument" $ documentfiles document
  let docid = unDocumentID (documentid document)

  -- FIXME: use the time when author clicked sign
  let signtime = case documentmaybesigninfo document of
                   Nothing -> signtime1
                   Just (SignInfo t) -> t
  let authordetails = documentauthordetails document
  let authorline = if authordetails == emptyDetails 
                   then sealLine userfullname [usercompanyname, usercompanynumber] signtime
                   else sealLine (signatoryname authordetails)
                            [signatorycompany authordetails, signatorynumber authordetails] signtime

  let persons = authorline : personsFromDocument document
  tmppath <- getTemporaryDirectory
  let tmpin = tmppath ++ "/in_" ++ show docid ++ ".pdf"
  let tmpout = tmppath ++ "/out_" ++ show docid ++ ".pdf"
  BS.writeFile tmpin filepdf
  let sealproc = (proc "dist/build/pdfseal/pdfseal" []) {std_in = CreatePipe}
  (Just inx, _, _, sealProcHandle) <- createProcess sealproc
  let config = SealSpec 
            { sealInput = tmpin
            , sealOutput = tmpout
            , sealDocumentNumber = docid
            , sealPersons = persons
            }
  -- print config
  hPutStr inx (show config)
  hClose inx
  
  waitForProcess sealProcHandle
  newfilepdf <- BS.readFile tmpout
  let newfile = file {filepdf = newfilepdf, filejpgpages = JpegPagesPending}
  update $ ReplaceFile (documentid document) newfile
  


basename :: String -> String
basename filename = 
    case span (\x -> x/='\\' || x/='/') filename of
      (_,(_:rest)) -> basename rest
      _ -> fst (span ((/=) '.') filename) -- FIXME: tak care of many dots in file name

handleIssuePost :: Context -> Kontra Response
handleIssuePost ctx = handleIssueNewDocument ctx `mplus` handleIssueArchive ctx

handleIssueNewDocument :: Context -> Kontra Response
handleIssueNewDocument ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) = do
    input@(Input content (Just filename) _contentType) <- getDataFnM (lookInput "doc")
    -- FIXME: here we have encoding issue
    -- Happstack gives use String done by BS.unpack, so BS.pack it here
    -- in our case it should be utf-8 as this is what we use everywhere
    let title = BSC.pack (basename filename) 
    freeleft <- freeLeftForUser user
    doc <- update $ NewDocument user title ctxtime (freeleft>0)
    liftIO $ forkIO $ handleDocumentUpload (documentid doc) (concatChunks content) title
    let link = ctxhostpart ++ "/issue/" ++ show (documentid doc)
    response <- webHSP (seeOtherXML link)
    seeOther link response


handleIssueArchive :: Context -> Kontra Response
handleIssueArchive ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) = do
    something <- getDataFnM (lookInput "archive")
    idstrings <- getDataFnM (lookInputList "doccheck")
    let Just ids = sequence $ map (readM . BSL.toString) idstrings
    update $ ArchiveDocuments ids

    let link = "/issue"
    response <- webHSP (seeOtherXML link)
    seeOther link response


showPage :: Context -> MinutesTime -> FileID -> Int -> Kontra Response
showPage ctx modminutes fileid pageno = do
  maybecontents <- query $ GetFilePageJpg fileid pageno
  case maybecontents of
    Nothing -> mzero
    Just contents -> do
      let res = Response 200 M.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
      let modtime = toUTCTime modminutes
      rq <- askRq                 -- FIXME: what?
      return $ ifModifiedSince modtime rq res2
