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

handleSign
  :: (MonadIO m, MonadPlus m, ServerMonad m) =>
     Context -> m Response
handleSign ctx@(Context (Just user) hostpart) = 
    path (\documentid -> path $ handleSignShow ctx documentid) `mplus` do
    documents <- query $ GetDocumentsBySignatory (userid user) 
    webHSP (pageFromBody ctx kontrakcja (listDocuments documents))

handleSignShow
  :: (ServerMonad m, MonadPlus m, MonadIO m) =>
     Context -> DocumentID -> SignatoryLinkID -> m Response
handleSignShow ctx@(Context (Just user) hostpart) documentid signatorylinkid1 = do
  time <- liftIO $ getMinutesTime
  Just document <- selectFormAction 
                   [("sign", update $ SignDocument documentid (userid user) signatorylinkid1 time)] 
                   `mplus`
                   (update $ MarkDocumentSeen documentid (userid user) signatorylinkid1 time)
                       
  let wassigned = any f (signatorylinks document)
      f (SignatoryLink {signatorylinkid,maybesigninfo}) = 
          isJust maybesigninfo && signatorylinkid == signatorylinkid1
  webHSP (pageFromBody ctx kontrakcja (showDocumentForSign document wassigned))

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
  msum 
     [ methodM GET >> webHSP (pageFromBody ctx kontrakcja (showDocument document))
     , do
         methodM POST
         doc2 <- updateDocument ctx document 
         webHSP (pageFromBody ctx kontrakcja (showDocument doc2))
     ]

-- | Useful inside the RqData monad.  Gets the named input parameter (either
-- from a POST or a GET)
lookInputList :: String -> RqData [BSL.ByteString]
lookInputList name
    = do inputs <- asks fst
         let isname (xname,(Input value _ _)) | xname == name = [value]
             isname _ = []
         return [value | k <- inputs, value <- isname k]

updateDocument :: Context -> Document -> ServerPartT IO Document  
updateDocument ctx document = do
  Just signatoriesinputs <- getDataFn $ lookInputList "signatoryname"
  let signatories = map concatChunks signatoriesinputs
  Just signatoriesemailsx <- getDataFn $ lookInputList "signatoryemail"
  let signatoriesemails = map concatChunks signatoriesemailsx
  doc2 <- update $ UpdateDocumentSignatories document signatories signatoriesemails
  maybefinal <- getDataFn $ look "final"
  maybeshowvars <- getDataFn $ look "showvars"
  when (isJust maybeshowvars) $ mzero
  if isJust maybefinal
     then
          finalize doc2
     else return doc2
  where
     finalize doc2 = do
         liftIO $ mapM_ (writeemail doc2) (signatorylinks doc2)
         update $ MarkDocumentAsFinal doc2
     writeemail doc2 (SignatoryLink linkid name email maybeuser _ _) = do
         (_,content) <- liftIO $ evalHSP Nothing 
                        (mailToPerson ctx email name 
                                          (title doc2) 
                                          (documentid doc2)
                                          linkid)
         let filename = "Email-" ++ BS.toString email ++ ".html"
         writeFile filename $ renderAsHTML content
         openDocument filename
         
         
    

handleIssueGet :: (MonadIO m) => Context -> m Response
handleIssueGet ctx@(Context (Just user) hostpart) = do
    documents <- query $ GetDocumentsByAuthor (userid user) 
    webHSP (pageFromBody ctx kontrakcja (listDocuments documents))

gs :: String
#ifdef WINDOWS
gs = "c:\\Program Files\\gs\\gs8.60\\bin\\gswin32c.exe" 
#else
gs = "gs"
#endif

convertPdfToJpgPages content = do
  tmppath <- getTemporaryDirectory
  allfiles <- getDirectoryContents tmppath
  mapM_ (\file -> 
             when (".jpg" `isSuffixOf` file) (removeFile (tmppath ++ "/" ++ file))) 
                               allfiles
  let sourcepath = tmppath ++ "/source.pdf"
  BSL.writeFile sourcepath content

  rawSystem gs
                [ "-sDEVICE=jpeg" 
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
      putStrLn "Attaching files to document"
      update $ AttachFile docid filename2 contentx jpgpages

handleIssuePost
  :: (ServerMonad m, MonadIO m) => Context -> m Response
handleIssuePost ctx@(Context (Just user) hostpart) = do
  maybeupload <- getDataFn (lookInput "doc")
  case maybeupload of
    Just input@(Input content (Just filename) _contentType) -> 
        do 
          ctime <- liftIO $ getMinutesTime
          doc <- update $ NewDocument (userid user) (BS.fromString filename) ctime
          liftIO $ forkedHandleDocumentUpload (documentid doc) content filename
          return ()
    _ -> return ()
  handleIssueGet ctx


showPage :: Context -> FileID -> Int -> ServerPartT IO Response
showPage ctx fileid pageno = do
  maybecontents <- query $ GetFilePageJpg fileid pageno
  case maybecontents of
    Nothing -> mzero
    Just contents -> do
      let res = Response 200 M.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
