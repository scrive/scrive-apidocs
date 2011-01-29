
module ActionQueue ()
where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Control.Monad.Trans
import UserState
import Happstack.Data.IxSet as IxSet
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Control.Applicative ((<$>))
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Debug.Trace
import Misc
import Control.Monad
import Data.List (find)
import MinutesTime
import Data.List (zipWith4,partition)
import System.Random
import Data.Word
import Data.Int
import System.Log.Logger (errorM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Mails.MailsUtil
import Control.Concurrent
import ActionQueueState
import DocControl
import User
import Control.Exception as E

runActionLoop :: Context -> IO ()
runActionLoop ctx = do
  maction <- query $ PeekAction
  case maction of
    Nothing -> threadDelay 1000
    Just action -> do
                  {- 
                     Here we need to protect from all possible exceptions.
                     -}
                  -- FIXME: protect
                  success <- (do
                               runSingleAction ctx action
                               return True
                             ) `E.catch` \(ex :: SomeException) -> return False
                  update $ DequeueAction
                  when (not success) $ do
                                -- we want to repeat the action
                                -- lets put it at the end of the queue
                                update $ EnqueueAction action
                                threadDelay 10000
  runActionLoop ctx

runSingleAction :: Context -> Action -> IO ()
runSingleAction Context{ctxtwconf} (TrustWeaverUpload ownertwname docid) = do
  print "doing uploadDocumentFilesToTrustWeaver"
  uploadDocumentFilesToTrustWeaver ctxtwconf ownertwname docid

runSingleAction Context{ctxs3action} (AmazonUpload docid fileid1) = do
  print "doing uploadDocumentFileToAmazon"
  uploadDocumentFileToAmazon ctxs3action docid fileid1
