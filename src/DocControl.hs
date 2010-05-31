{-# LANGUAGE CPP #-}

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

doctransPreparation2ReadyToSign :: Context -> Document -> IO ()
doctransPreparation2ReadyToSign ctx doc = do
  -- FIXME: check if the status was really changed
  update $ UpdateDocumentStatus doc ReadyToSign
  liftIO $ sendInvitationEmails ctx doc
  return ()

doctransReadyToSign2Closed :: Context -> Document -> IO ()
doctransReadyToSign2Closed ctx doc = do
  update $ UpdateDocumentStatus doc Closed
  liftIO $ forkIO $ do
    mapM_ forkedSealFile (files doc) 
    sendClosedEmails ctx doc
  return ()

doctransReadyToSign2Canceled :: Context -> Document -> IO ()
doctransReadyToSign2Canceled ctx doc = do
  update $ UpdateDocumentStatus doc Canceled
  return ()

doctransReadyToSign2Timedout :: Context -> Document -> IO ()
doctransReadyToSign2Timedout ctx doc = do
  update $ UpdateDocumentStatus doc Timedout
  return ()

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

  sendMail signatoryname signatoryemail title content

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

  sendMail signatoryname signatoryemail title content
  
handleSign
  :: (MonadIO m, MonadPlus m, ServerMonad m, FilterMonad Response m) =>
     Context -> m Response
handleSign ctx@(Context (Just user) hostpart) = 
    path (\documentid -> path $ handleSignShow ctx documentid) `mplus` do
    documents <- query $ GetDocumentsBySignatory (userid user) 
    webHSP (pageFromBody ctx TopNone kontrakcja (listDocuments documents))

signDoc :: (ServerMonad m, MonadPlus m, MonadIO m, FilterMonad Response m) =>
           Context -> DocumentID -> SignatoryLinkID -> m Response

signDoc ctx@(Context (Just user@User{userid}) hostpart) documentid 
               signatorylinkid1 = do
  time <- liftIO $ getMinutesTime
  maybepressed <- getDataFn (look "sign" `mplus` look "sign2" `mplus` return "")
  when (Prelude.null (fromJust maybepressed)) mzero -- quit here
  
  Just document <- update $ SignDocument documentid userid signatorylinkid1 time
  let isallsigned = all f (signatorylinks document)
      f (SignatoryLink {maybesigninfo}) = isJust maybesigninfo
  when isallsigned (liftIO $ doctransReadyToSign2Closed ctx document)
  let link = mkSignDocLink hostpart documentid signatorylinkid1
  response <- webHSP (seeOtherXML link)
  seeOther link response

handleSignShow
  :: (ServerMonad m, MonadPlus m, MonadIO m,FilterMonad Response m) =>
     Context -> DocumentID -> SignatoryLinkID -> m Response
handleSignShow ctx@(Context (Just user@User{userid}) hostpart) documentid 
               signatorylinkid1 = do
  time <- liftIO $ getMinutesTime

  msum [ signDoc ctx documentid signatorylinkid1
       , do 
          Just document <- update $ MarkDocumentSeen documentid userid signatorylinkid1 time
                       
          let wassigned = any f (signatorylinks document)
              f (SignatoryLink {signatorylinkid,maybesigninfo}) = 
                  isJust maybesigninfo && signatorylinkid == signatorylinkid1
          webHSP (pageFromBody ctx TopNone kontrakcja (showDocumentForSign ("/sign/" ++ show documentid ++ "/" ++ show signatorylinkid1) document wassigned))
       ]

handleIssue :: Context -> ServerPartT IO Response
handleIssue ctx@(Context (Just user) hostpart) = 
    msum [ path (handleIssueShow ctx)
         , methodM GET >> handleIssueGet ctx
         , methodM POST >> handleIssuePost ctx
         ]

handleIssueShow
  :: Context -> DocumentID -> ServerPartT IO Response
handleIssueShow ctx@(Context (Just user) hostpart) documentid = do
  Just document <- query $ GetDocumentByDocumentID documentid
  msum [ methodM GET >> webHSP (pageFromBody ctx TopDocument kontrakcja (showDocument document))
       , do
           methodM POST
           doc2 <- updateDocument ctx document
           let link = hostpart ++ "/issue/" ++ show documentid
           response <- webHSP (seeOtherXML link)
           seeOther link response
       ]

-- | Useful inside the RqData monad.  Gets the named input parameter
-- (either from a POST or a GET)
lookInputList :: String -> RqData [BSL.ByteString]
lookInputList name
    = do inputs <- asks fst
         let isname (xname,(Input value _ _)) | xname == name = [value]
             isname _ = []
         return [value | k <- inputs, value <- isname k]

getAndConcat :: String -> ServerPartT IO [BS.ByteString]
getAndConcat field = do
  Just values <- getDataFn $ lookInputList field
  return $ map concatChunks values

updateDocument :: Context -> Document -> ServerPartT IO Document  
updateDocument ctx document = do
  liftIO $ print "zonk"
  signatories <- getAndConcat "signatoryname"
  signatoriescompanies <- getAndConcat "signatorycompany"
  signatoriesemails <- getAndConcat "signatoryemail"

  doc2 <- update $ UpdateDocumentSignatories document 
          signatories signatoriescompanies signatoriesemails
  final <- getDataFn $ (look "final" `mplus` look "final2" `mplus` return "")
  maybeshowvars <- getDataFn $ look "showvars"
  when (isJust maybeshowvars) $ mzero
  liftIO $ print final
  if not (Data.List.null (fromJust final))
     then do
          liftIO $ doctransPreparation2ReadyToSign ctx doc2
          return doc2
     else return doc2
         
         
    

handleIssueGet :: (MonadIO m) => Context -> m Response
handleIssueGet ctx@(Context (Just user) hostpart) = do
    documents <- query $ GetDocumentsByAuthor (userid user) 
    webHSP (pageFromBody ctx TopDocument kontrakcja (listDocuments documents))

gs :: String
#ifdef WINDOWS
gs = "c:\\Program Files\\gs\\gs8.60\\bin\\gswin32c.exe" 
#else
gs = "gs"
#endif

convertPdfToJpgPages content = do
  tmppath <- getTemporaryDirectory
  allfiles <- getDirectoryContents tmppath
  forM_ allfiles $ \file -> 
      when (".jpg" `isSuffixOf` file) 
               (removeFile (tmppath ++ "/" ++ file))

  let sourcepath = tmppath ++ "/source.pdf"
  BSL.writeFile sourcepath content

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
       

forkedHandleDocumentUpload docid content filename = do
    forkIO $ do
      jpgpages <- liftIO $ convertPdfToJpgPages content
      let filename2 = BS.fromString filename
      let contentx = concatChunks content
      -- FIXME: take care of case when it does not parse
      update $ AttachFile docid filename2 contentx jpgpages


sealFile :: BS.ByteString -> Int -> [()] -> IO BS.ByteString
sealFile content docid persons = do
  tmppath <- getTemporaryDirectory
  let tmpin = tmppath ++ "/in_" ++ show docid ++ ".pdf"
  let tmpout = tmppath ++ "/out_" ++ show docid ++ ".pdf"
  BS.writeFile tmpin content
  let sealproc = (proc "dist/build/pdfseal/pdfseal" []) {std_in = CreatePipe}
  (Just inx, _, _, sealProcHandle) <- createProcess sealproc
  let config = SealSpec 
            { sealInput = tmpin
            , sealOutput = tmpout
            , sealDocumentNumber = docid
            , sealPersons = []
            }
  hPutStr inx (show config)
  hClose inx
  
  waitForProcess sealProcHandle
  newcontent <- BS.readFile tmpout
  -- FIXME: delete old files
  return newcontent
  

forkedSealFile (file@File {fileid,filename,filepdf,filejpgpages}) = do
      newfilepdf <- sealFile filepdf 1345646 []
      newfilejpegpages <- convertPdfToJpgPages (BSL.fromChunks [newfilepdf])
      let newfile = file {filepdf = newfilepdf, filejpgpages = newfilejpegpages}
      update $ ReplaceFile newfile


basename :: String -> String
basename filename = 
    case span (\x -> x/='\\' || x/='/') filename of
      (_,(_:rest)) -> basename rest
      _ -> fst (span ((/=) '.') filename) -- FIXME: tak care of many dots in file name

handleIssuePost
  :: (ServerMonad m, MonadIO m) => Context -> m Response
handleIssuePost ctx@(Context (Just user) hostpart) = do
  maybeupload <- getDataFn (lookInput "doc")
  case maybeupload of
    Just input@(Input content (Just filename) _contentType) -> 
        do 
          ctime <- liftIO $ getMinutesTime
          let title = BS.fromString (basename filename) 
          doc <- update $ NewDocument (userid user) title ctime
          liftIO $ forkedHandleDocumentUpload (documentid doc) content filename
          return ()
    _ -> return ()
  handleIssueGet ctx


showPage :: Context -> MinutesTime -> FileID -> Int -> ServerPartT IO Response
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
