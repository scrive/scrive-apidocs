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
  liftIO $ sendInvitationEmails ctx doc
  return newdoc

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
                                                         , signatoryemail }} = signlink
      Document{documenttitle,documentid} = document
  content <- invitationMail ctx signatoryemail signatoryname
             documenttitle documentid signatorylinkid

  sendMail signatoryname signatoryemail documenttitle content 
           (filepdf $ head $ documentfiles document)

sendClosedEmails :: Context -> Document -> IO ()
sendClosedEmails ctx document = do
  let signlinks = documentsignatorylinks document
  forM_ signlinks (sendClosedEmail1 ctx document)
  sendClosedAuthorEmail ctx document

sendClosedEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendClosedEmail1 ctx document signlink = do
  let SignatoryLink{ signatorylinkid
                   , signatorydetails = SignatoryDetails {signatoryname
                                                         , signatorycompany
                                                         , signatoryemail }} = signlink
      Document{documenttitle,documentid} = document
  content <- closedMail ctx signatoryemail signatoryname
             documenttitle documentid signatorylinkid
  let attachmentcontent = filepdf $ head $ documentfiles document
  sendMail signatoryname signatoryemail documenttitle content attachmentcontent

sendClosedAuthorEmail :: Context -> Document -> IO ()
sendClosedAuthorEmail ctx document = do
  let authorid = unAuthor $ documentauthor document
  Just authoruser <- query $ GetUserByUserID authorid
  content <- closedMailAuthor ctx (unEmail $ useremail authoruser) (userfullname authoruser)
             (documenttitle document) (documentid document) 
  let attachmentcontent = filepdf $ head $ documentfiles document
  sendMail (userfullname authoruser) (unEmail $ useremail authoruser) 
           (documenttitle document) content attachmentcontent
  
  
handleSign :: Context -> Kontra Response
handleSign ctx@(Context {ctxmaybeuser, ctxhostpart}) = 
    path (\documentid -> path $ handleSignShow ctx documentid) `mplus` (withUser ctxmaybeuser $ do
    documents <- query $ GetDocumentsBySignatory (userid $ fromJust ctxmaybeuser) 
    webHSP (pageFromBody ctx TopNone kontrakcja (listDocuments documents)))

signDoc :: Context -> DocumentID -> SignatoryLinkID -> Kontra  Response
signDoc ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime}) documentid 
               signatorylinkid1 = do
  maybepressed <- getDataFn (look "sign" `mplus` look "sign2" `mplus` return "")
  when (Prelude.null (fromJust maybepressed)) mzero -- quit here
  
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


handleSignShow :: Context -> DocumentID -> SignatoryLinkID -> Kontra Response
handleSignShow ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime}) documentid 
               signatorylinkid1 = do
  msum [ signDoc ctx documentid signatorylinkid1
       , do 
          Just document <- update $ MarkDocumentSeen documentid signatorylinkid1 ctxtime
                       
          let wassigned = any f (documentsignatorylinks document)
              f (SignatoryLink {signatorylinkid,maybesigninfo}) = 
                  isJust maybesigninfo && signatorylinkid == signatorylinkid1
              authoruserid = unAuthor $ documentauthor document
          Just author <- query $ GetUserByUserID authoruserid
          let authorname = userfullname author
              invitedname = signatoryname $ signatorydetails $ head $ filter (\x -> signatorylinkid x == signatorylinkid1) 
                            (documentsignatorylinks document)
          webHSP (pageFromBody ctx TopNone kontrakcja 
                 (showDocumentForSign (LinkSignDoc document signatorylinkid1) 
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
           maybeissuedone <- getDataFn (look "issuedone")
           let issuedone = isJust maybeissuedone
           freeleft <- freeLeftForUser user
           webHSP (pageFromBody ctx TopDocument kontrakcja 
                                    (showDocument user document issuedone freeleft))
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
    = do inputs <- asks fst
         let isname (xname,(Input value _ _)) | xname == name = [value]
             isname _ = []
         return [value | k <- inputs, value <- isname k]

getAndConcat :: String -> Kontra [BS.ByteString]
getAndConcat field = do
  Just values <- getDataFn $ lookInputList field
  return $ map concatChunks values

updateDocument :: Context -> Document -> Kontra Document  
updateDocument ctx@Context{ctxtime} document = do
  signatories <- getAndConcat "signatoryname"
  signatoriescompanies <- getAndConcat "signatorycompany"
  signatoriesnumbers <- getAndConcat "signatorynumber"
  signatoriesemails <- getAndConcat "signatoryemail"
  daystosignstring <- getDataFnM (look "daystosign")
  daystosign <- readM daystosignstring

  -- FIXME: tell the user what happened!
  when (daystosign<1 || daystosign>99) mzero
  
  doc2 <- update $ UpdateDocument ctxtime document 
          signatories signatoriescompanies signatoriesnumbers signatoriesemails
          daystosign

  final <- getDataFn $ (look "final" `mplus` look "final2" `mplus` return "")
  maybeshowvars <- getDataFn $ look "showvars"
  when (isJust maybeshowvars) $ mzero
  if not (Data.List.null (fromJust final))
     then do
          doc3 <- liftIO $ doctransPreparation2Pending ctx doc2
          return doc3
     else return doc2
    

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

convertPdfToJpgPages :: BS.ByteString ->IO [BS.ByteString]
convertPdfToJpgPages content = do
  tmppath <- getTemporaryDirectory
  allfiles <- getDirectoryContents tmppath
  forM_ allfiles $ \file -> 
      when (".jpg" `isSuffixOf` file) 
               (removeFile (tmppath ++ "/" ++ file))

  let sourcepath = tmppath ++ "/source.pdf"
  BS.writeFile sourcepath content

  rawSystem gs [ "-sDEVICE=jpeg" 
               , "-sOutputFile=" ++ tmppath ++ "/output-%d.jpg"
               , "-dSAFER"
               , "-dBATCH"
               , "-dNOPAUSE"
               , "-dTextAlphaBits=4"
               , sourcepath
               ]
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
  return x
       

handleDocumentUploadX docid content filename = do
  jpgpages <- convertPdfToJpgPages content
  let filename2 = BS.fromString filename
  -- FIXME: take care of case when it does not parse
  update $ AttachFile docid filename2 content jpgpages


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
sealDocument signtime author@(User {userfullname,usercompanyname,usercompanynumber}) document = do
  let (file@File {fileid,filename,filepdf,filejpgpages}) = 
           safehead "sealDocument" $ documentfiles document
  let docid = unDocumentID (documentid document)
  -- FIXME: use the time when author clicked sign
  let persons = sealLine userfullname [usercompanyname, usercompanynumber] signtime
                : personsFromDocument document
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
  print config
  hPutStr inx (show config)
  hClose inx
  
  waitForProcess sealProcHandle
  newfilepdf <- BS.readFile tmpout
  -- FIXME: delete old files
  newfilejpegpages <- convertPdfToJpgPages newfilepdf
  let newfile = file {filepdf = newfilepdf, filejpgpages = newfilejpegpages}
  update $ ReplaceFile document newfile
  


basename :: String -> String
basename filename = 
    case span (\x -> x/='\\' || x/='/') filename of
      (_,(_:rest)) -> basename rest
      _ -> fst (span ((/=) '.') filename) -- FIXME: tak care of many dots in file name

handleIssuePost :: Context -> Kontra Response
handleIssuePost ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) = do
  maybeupload <- getDataFn (lookInput "doc")
  case maybeupload of
    Just input@(Input content (Just filename) _contentType) -> 
        do 
          -- FIXME: here we have encoding issue
          -- Happstack gives use String done by BS.unpack, so BS.pack it here
          -- in our case it should be utf-8 as this is what we use everywhere
          let title = BSC.pack (basename filename) 
          freeleft <- freeLeftForUser user
          doc <- update $ NewDocument (userid user) title ctxtime (freeleft>0)
          liftIO $ forkIO $ handleDocumentUploadX (documentid doc) (concatChunks content) filename
          let link = ctxhostpart ++ "/issue/" ++ show (documentid doc)
          response <- webHSP (seeOtherXML link)
          seeOther link response
    _ -> do
      let link = ctxhostpart ++ "/issue"
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
