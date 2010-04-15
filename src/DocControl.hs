{-# LANGUAGE ForeignFunctionInterface #-}

module DocControl where
import DocView
import DocState
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import AppView
import UserState
import qualified Data.ByteString.UTF8 as BSC
import qualified Data.ByteString as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSCL
import qualified Data.ByteString.Lazy as BSCL
import Control.Monad
import HSP
import Data.Maybe
import Control.Monad.Trans
import Misc
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Debug.Trace

handleSign
  :: (MonadIO m, MonadPlus m, ServerMonad m) =>
     String -> User -> m Response
handleSign hostpart user = path (\documentid -> path $ handleSignShow hostpart user documentid) `mplus` do
    documents <- query $ GetDocumentsBySignatory (userid user) 
    webHSP (pageFromBody (Just user) hostpart kontrakcja (listDocuments documents))

handleSignShow
  :: (ServerMonad m, MonadPlus m, MonadIO m) =>
     String -> User -> DocumentID -> SignatoryLinkID -> m Response
handleSignShow hostpart user documentid signatorylinkid1 = do
  Just document <- selectFormAction 
                   [("sign",update $ SignDocument documentid (userid user) signatorylinkid1)] 
                   `mplus`
                   (query $ GetDocumentByDocumentID documentid)
                       
  let wassigned = any f (signatorylinks document)
      f (SignatoryLink {signatorylinkid,signed}) = 
          signed && signatorylinkid == signatorylinkid1
  webHSP (pageFromBody (Just user) hostpart kontrakcja (showDocumentForSign document wassigned))

handleIssue :: String -> User -> ServerPartT IO Response
handleIssue hostpart user = 
    msum [ path (handleIssueShow hostpart user)
         , methodM GET >> handleIssueGet hostpart user
         , methodM POST >> handleIssuePost hostpart user
         ]

handleIssueShow
  :: String -> User -> DocumentID -> ServerPartT IO Response
handleIssueShow hostpart user documentid = do
  Just document <- query $ GetDocumentByDocumentID documentid
  msum 
     [ methodM GET >> webHSP (pageFromBody (Just user) hostpart kontrakcja (showDocument document))
     , do
         methodM POST
         doc2 <- updateDocument document 
         webHSP (pageFromBody (Just user) hostpart kontrakcja (showDocument doc2))
     ]

updateDocument :: Document -> ServerPartT IO Document  
updateDocument document = do
  Just signatories <- getDataFn $ look "signatories"
  let sign = words signatories
  doc2 <- trace "update document signatories" $
          update $ UpdateDocumentSignatories document sign
  maybefinal <- getDataFn $ look "final"
  if isJust maybefinal
     then
          finalize sign doc2
     else return doc2
  where
     finalize sign doc2 = do
         liftIO $ mapM_ (writeemail doc2) (signatorylinks doc2)
         update $ MarkDocumentAsFinal doc2
     writeemail doc2 (SignatoryLink linkid email maybeuser _) = do
         (_,content) <- liftIO $ evalHSP Nothing 
                        (mailToPerson email "John Doe" 
                                          (title doc2) 
                                          (documentid doc2)
                                          linkid)
         let filename = "Email-" ++ email ++ ".html"
         writeFile filename $ renderAsHTML content
         openDocument filename
         
         
openDocument :: String -> IO ()
openDocument filename = do
  withCString filename $ \filename -> do
                          withCString "open" $ \open -> do
                                        shellExecute nullPtr open filename nullPtr nullPtr 1
             
foreign import stdcall "ShellExecuteA" shellExecute :: Ptr () -> Ptr CChar -> Ptr CChar -> Ptr () -> Ptr () -> CInt -> IO ()

    

handleIssueGet :: (MonadIO m) => String -> User -> m Response
handleIssueGet hostpart user = do
    documents <- query $ GetDocumentsByAuthor (userid user) 
    webHSP (pageFromBody (Just user) hostpart kontrakcja (listDocuments documents))

{-
 FIXME: we get POST request either because we upload file
 or because we log in. How to know which is which?
-}
handleIssuePost
  :: (ServerMonad m, MonadIO m) => String -> User -> m Response
handleIssuePost hostpart user = do
  maybeupload <- getDataFn (lookInput "doc")
  case maybeupload of
    Just (Input _content (Just filename) _contentType) ->
        update $ NewDocument (userid user) (BSC.fromString filename)
    _ -> return ()
  handleIssueGet hostpart user
