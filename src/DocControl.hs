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
  Just document <- selectFormAction 
                   [("sign",update $ SignDocument documentid (userid user) signatorylinkid1)] 
                   `mplus`
                   (query $ GetDocumentByDocumentID documentid)
                       
  let wassigned = any f (signatorylinks document)
      f (SignatoryLink {signatorylinkid,signed}) = 
          signed && signatorylinkid == signatorylinkid1
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
  if isJust maybefinal
     then
          finalize doc2
     else return doc2
  where
     finalize doc2 = do
         liftIO $ mapM_ (writeemail doc2) (signatorylinks doc2)
         update $ MarkDocumentAsFinal doc2
     writeemail doc2 (SignatoryLink linkid name email maybeuser _) = do
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

convertPdfToJpgPages content = do
    createDirectoryIfMissing False "Tmp"
    BSL.writeFile "Tmp/source.pdf" content
    system "\"c:\\Program Files\\gs\\gs8.60\\bin\\gswin32c.exe\" -sDEVICE=jpeg -sOutputFile=Tmp/output-%d.jpg -dSAFER -dBATCH -dNOPAUSE -dTextAlphaBits=4 Tmp/source.pdf"
    x <- BS.readFile "Tmp/output-1.jpg"
    return [x]
       

handleDocumentUpload (Input content (Just uploadfilename) contentType) pages = do
  return $ File 
          { fileid = error "put it here later"
          , filename = BS.fromString uploadfilename
          , filepdf = concatChunks content
          , filejpgpages = pages
          }

{-
 FIXME: we get POST request either because we upload file
 or because we log in. How to know which is which?
-}
handleIssuePost
  :: (ServerMonad m, MonadIO m) => Context -> m Response
handleIssuePost ctx@(Context (Just user) hostpart) = do
  maybeupload <- getDataFn (lookInput "doc")
  case maybeupload of
    Just input@(Input content (Just filename) _contentType) -> 
        do 
          pages <- liftIO $ convertPdfToJpgPages content
          file <- handleDocumentUpload input pages
          update $ NewDocument (userid user) (BS.fromString filename) file
    _ -> return ()
  handleIssueGet ctx


showPage :: Context -> FileID -> Int -> ServerPartT IO Response
showPage ctx fileid pageno = do
  maybecontents <- query $ GetFilePageJpg fileid pageno
  case maybecontents of
    Nothing -> mzero
    Just contents -> do
      -- setHeaderM "content-type" "image/jpg"
      --trace "was here" $ return (toResponse contents)
      let res = Response 200 M.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
