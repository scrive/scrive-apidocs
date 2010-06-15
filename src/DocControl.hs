{-# LANGUAGE CPP, ScopedTypeVariables #-}

module DocControl where
import DocView
import DocState
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import AppView
import UserState
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
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

{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}

doctransPreparation2ReadyToSign :: Context -> Document -> IO Document
doctransPreparation2ReadyToSign ctx doc = do
  -- FIXME: check if the status was really changed
  newdoc <- update $ UpdateDocumentStatus doc ReadyToSign
  liftIO $ sendInvitationEmails ctx doc
  return newdoc

doctransReadyToSign2Closed :: Context -> Document -> IO Document
doctransReadyToSign2Closed ctx doc = do
  update $ UpdateDocumentStatus doc Closed
  newdoc <- update $ RemoveFileFromDoc (documentid doc)
  Just user <- query $ FindUserByUserID (unAuthor (author doc))
  liftIO $ forkIO $ do
    newdoc <- sealDocument user doc
    sendClosedEmails ctx newdoc
  return newdoc

doctransReadyToSign2Canceled :: Context -> Document -> IO Document
doctransReadyToSign2Canceled ctx doc = do
  update $ UpdateDocumentStatus doc Canceled

doctransReadyToSign2Timedout :: Context -> Document -> IO Document
doctransReadyToSign2Timedout ctx doc = do
  update $ UpdateDocumentStatus doc Timedout

sendInvitationEmails :: Context -> Document -> IO ()
sendInvitationEmails ctx document = do
  let signlinks = signatorylinks document
  forM_ signlinks (sendInvitationEmail1 ctx document)

sendInvitationEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendInvitationEmail1 ctx document signlink = do
  let SignatoryLink{ signatorylinkid
                   , signatoryname
                   , signatorycompany
                   , signatoryemail } = signlink
      Document{title,documentid} = document
  content <- invitationMail ctx signatoryemail signatoryname
             title documentid signatorylinkid

  sendMail signatoryname signatoryemail title content (filepdf $ head $ files document)

sendClosedEmails :: Context -> Document -> IO ()
sendClosedEmails ctx document = do
  let signlinks = signatorylinks document
  forM_ signlinks (sendClosedEmail1 ctx document)

sendClosedEmail1 :: Context -> Document -> SignatoryLink -> IO ()
sendClosedEmail1 ctx document signlink = do
  let SignatoryLink{ signatorylinkid
                   , signatoryname
                   , signatorycompany
                   , signatoryemail } = signlink
      Document{title,documentid} = document
  content <- closedMail ctx signatoryemail signatoryname
             title documentid signatorylinkid
  let attachmentcontent = filepdf $ head $ files document
  sendMail signatoryname signatoryemail title content attachmentcontent
  
handleSign
  :: Context -> Kontra Response
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
  let isallsigned = all f (signatorylinks document)
      f (SignatoryLink {maybesigninfo}) = isJust maybesigninfo
  when isallsigned ((liftIO $ doctransReadyToSign2Closed ctx document) >> return ())

  -- let link = mkSignDocLink ctxhostpart documentid signatorylinkid1
  let link = "/landpage/signed/" ++ show documentid ++ "/" ++ show signatorylinkid1
  response <- webHSP (seeOtherXML link)
  seeOther link response

landpageSignInvite ctx documentid = do
  Just document <- query $ GetDocumentByDocumentID documentid
  webHSP $ pageFromBody ctx TopNone kontrakcja $ landpageSignInviteView ctx document

landpageSigned ctx documentid signatorylinkid = do
  Just document <- query $ GetDocumentByDocumentID documentid
  webHSP $ pageFromBody ctx TopEmpty kontrakcja $ landpageSignedView ctx document signatorylinkid

landpageSignedSave ctx documentid signatorylinkid = do
  Just document <- query $ GetDocumentByDocumentID documentid
  webHSP $ pageFromBody ctx TopEmpty kontrakcja $ landpageLoginForSaveView ctx document signatorylinkid

landpageSaved (ctx@Context { ctxmaybeuser = Just user }) documentid signatorylinkid = do
  Just document <- query $ GetDocumentByDocumentID documentid
  Just document2 <- update $ SaveDocumentForSignedUser documentid (userid user) signatorylinkid
  webHSP $ pageFromBody ctx TopDocument kontrakcja $ landpageDocumentSavedView ctx document signatorylinkid


handleSignShow
  :: Context -> DocumentID -> SignatoryLinkID -> Kontra Response
handleSignShow ctx@(Context {ctxmaybeuser, ctxhostpart, ctxtime}) documentid 
               signatorylinkid1 = do
  msum [ signDoc ctx documentid signatorylinkid1
       , do 
          Just document <- update $ MarkDocumentSeen documentid signatorylinkid1 ctxtime
                       
          let wassigned = any f (signatorylinks document)
              f (SignatoryLink {signatorylinkid,maybesigninfo}) = 
                  isJust maybesigninfo && signatorylinkid == signatorylinkid1
              authoruserid = unAuthor $ author document
          Just author <- query $ FindUserByUserID authoruserid
          let authorname = userfullname author
              invitedname = signatoryname $ head $ filter (\x -> signatorylinkid x == signatorylinkid1) 
                            (signatorylinks document)
          webHSP (pageFromBody ctx TopNone kontrakcja (showDocumentForSign ("/sign/" ++ show documentid ++ "/" ++ show signatorylinkid1) document authorname invitedname wassigned))
       ]

handleIssue :: Context -> Kontra Response
handleIssue ctx@(Context {ctxmaybeuser = Just user, ctxhostpart}) = 
    msum [ path (handleIssueShow ctx)
         , methodM GET >> handleIssueGet ctx
         , methodM POST >> handleIssuePost ctx
         ]

handleIssueShow
  :: Context -> DocumentID -> Kontra Response
handleIssueShow ctx@(Context {ctxmaybeuser = Just (user@User{userid}), ctxhostpart}) documentid = do
  Just (document::Document) <- query (GetDocumentByDocumentID documentid)
  msum [ do
           methodM GET 
           maybeissuedone <- getDataFn (look "issuedone")
           let issuedone = isJust maybeissuedone
           webHSP (pageFromBody ctx TopDocument kontrakcja (showDocument user document issuedone))
       , do
           methodM POST
           doc2 <- updateDocument ctx document
           
           if (status doc2 == ReadyToSign &&
               status document /= ReadyToSign) 
             then do
              let link = "/landpage/signinvite/" ++ show documentid
              response <- webHSP (seeOtherXML link)
              seeOther link response
            else do 
             let link = ctxhostpart ++ "/issue"
             flashmsg <- documentSavedForLaterFlashMessage doc2
             liftIO $ update $ AddUserFlashMessage userid flashmsg
             response <- webHSP (seeOtherXML link)
             seeOther link response

       , path $ \(_title::String) -> methodM GET >> do
           let file = safehead "handleIssueShow" (files document)
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
updateDocument ctx document = do
  signatories <- getAndConcat "signatoryname"
  signatoriescompanies <- getAndConcat "signatorycompany"
  signatoriesemails <- getAndConcat "signatoryemail"

  doc2 <- update $ UpdateDocumentSignatories document 
          signatories signatoriescompanies signatoriesemails
  final <- getDataFn $ (look "final" `mplus` look "final2" `mplus` return "")
  maybeshowvars <- getDataFn $ look "showvars"
  when (isJust maybeshowvars) $ mzero
  if not (Data.List.null (fromJust final))
     then do
          doc3 <- liftIO $ doctransPreparation2ReadyToSign ctx doc2
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


personsFromDocument document = 
    let
        links = signatorylinks document
        x (SignatoryLink{signatoryname,signatorycompany,maybesigninfo = Just (SignInfo { signtime })})
            | BS.null signatorycompany =
                SealPerson (BS.toString signatoryname ++ ", ") (showDateOnly signtime)
            | otherwise =
                SealPerson (BS.toString signatoryname ++ ", ") 
                               (BS.toString signatorycompany ++ ", " ++ showDateOnly signtime)
    in map x links

sealDocument :: User -> Document -> IO Document
sealDocument author@(User {userfullname,usercompanyname}) document = do
  let (file@File {fileid,filename,filepdf,filejpgpages}) = safehead "sealDocument" $ files document
  let docid = unDocumentID (documentid document)
  let persons = SealPerson (BS.toString userfullname ++ ", ") (BS.toString usercompanyname) : personsFromDocument document
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

handleIssuePost
  :: (ServerMonad m, MonadIO m,FilterMonad Response m) => Context -> m Response
handleIssuePost ctx@(Context { ctxmaybeuser = Just user, ctxhostpart, ctxtime }) = do
  maybeupload <- getDataFn (lookInput "doc")
  case maybeupload of
    Just input@(Input content (Just filename) _contentType) -> 
        do 
          let title = BS.fromString (basename filename) 
          doc <- update $ NewDocument (userid user) title ctxtime
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
