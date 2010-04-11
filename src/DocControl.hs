
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

handleSign hostpart user = path (handleSignShow hostpart user) `mplus` do
    documents <- query $ GetDocumentsBySignatory (userid user) 
    webHSP (pageFromBody (Just user) hostpart kontrakcja (listDocuments documents))

handleSignShow hostpart user documentid = do
  Just document <- selectFormAction [("sign",update $ SignDocument documentid (userid user) "email")] `mplus`
                                          (query $ GetDocumentByDocumentID documentid)
                       
  webHSP (pageFromBody (Just user) hostpart kontrakcja (showDocumentForSign document))

handleIssue hostpart user = 
    msum [ path (handleIssueShow hostpart user)
         , methodM GET >> handleIssueGet hostpart user
         , methodM POST >> handleIssuePost hostpart user
         ]

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
  doc2 <- update $ UpdateDocumentSignatories document (map EmailOnly sign)
  maybefinal <- getDataFn $ look "final"
  if isJust maybefinal
     then
          finalize sign doc2
     else return doc2
  where
     finalize sign doc2 = do
         liftIO $ mapM_ (writeemail doc2) sign
         update $ MarkDocumentAsFinal document
     writeemail doc2 email = do
         (_,content) <- liftIO $ evalHSP Nothing 
                        (mailToPerson email "John Doe" "The Important Document" (documentid doc2))
         writeFile ("Email-" ++ email ++ ".html") $ renderAsHTML content
             
    

handleIssueGet hostpart user = do
    documents <- query $ GetDocumentsByAuthor (userid user) 
    webHSP (pageFromBody (Just user) hostpart kontrakcja (listDocuments documents))

{-
 FIXME: we get POST request either because we upload file
 or because we log in. How to know which is which?
-}
handleIssuePost hostpart user = do
  maybeupload <- getDataFn (lookInput "doc")
  case maybeupload of
    Just (Input _content (Just filename) _contentType) ->
        update $ NewDocument (userid user) (BSC.fromString filename)
    _ -> return ()
  handleIssueGet hostpart user
