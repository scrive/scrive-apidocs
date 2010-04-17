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
import Control.Monad.Reader
import HSP
import Data.Maybe
import Control.Monad.Trans
import Misc
import Debug.Trace

o :: ((t -> t3) -> t4)
     -> ((t1 -> t2) -> t3)
     -> (t -> t1 -> t2)
     -> t4
o path1 path2 rest = path1 (\a -> path2 (\b -> rest a b))

handleSign
  :: (MonadIO m, MonadPlus m, ServerMonad m) =>
     String -> User -> m Response
handleSign hostpart user = 
    -- path (\documentid -> path $ handleSignShow hostpart user documentid) `mplus` do
    (path `o` path) (handleSignShow hostpart user) `mplus` do
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

-- | Useful inside the RqData monad.  Gets the named input parameter (either
-- from a POST or a GET)
lookInputList :: String -> RqData [BSCL.ByteString]
lookInputList name
    = do inputs <- asks fst
         let isname (xname,(Input value _ _)) | xname == name = [value]
             isname _ = []
         return [value | k <- inputs, value <- isname k]

updateDocument :: Document -> ServerPartT IO Document  
updateDocument document = do
  Just signatoriesinputs <- getDataFn $ lookInputList "signatoryname"
  let signatories = map BSCL.toString signatoriesinputs
  Just signatoriesemailsx <- getDataFn $ lookInputList "signatoryemail"
  let signatoriesemails = map BSCL.toString signatoriesemailsx
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
                        (mailToPerson email name 
                                          (title doc2) 
                                          (documentid doc2)
                                          linkid)
         let filename = "Email-" ++ email ++ ".html"
         writeFile filename $ renderAsHTML content
         openDocument filename
         
         
    

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
