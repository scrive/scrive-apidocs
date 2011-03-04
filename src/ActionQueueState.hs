
module ActionQueueState
where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Control.Monad.Trans
import User.UserState
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
import qualified TrustWeaver as TW
import Doc.DocState
import Data.Data (Data)

data Action = TrustWeaverUpload String DocumentID 
            | AmazonUpload DocumentID FileID
              deriving (Eq, Ord, Show, Typeable, Data)

data Actions = Actions [Action]
              deriving (Eq, Ord, Show, Data)

instance Typeable Actions where typeOf _ = mkTypeOf "Actions"

$(deriveSerialize ''Action)
instance Version Action where

$(deriveSerialize ''Actions)
instance Version Actions where


instance Component Actions where
  type Dependencies Actions = End
  initialValue = Actions []
                  
enqueueAction :: Action -> Update Actions ()
enqueueAction action = do
  -- FIXME: need to use balanced queue, priority queue maybe
  -- or at least trict with ([],[]). Gracjan knows.
  modify $ \(Actions list) -> Actions (list ++ [action])
  return ()

dequeueAction :: Update Actions ()
dequeueAction = do
  modify $ \(Actions (first:list)) -> Actions list
  return ()


peekAction :: Query Actions (Maybe Action)
peekAction = do
  Actions list <- ask
  case list of
    (action:xs) -> return $ Just action
    _ -> return Nothing


-- create types for event serialization
$(mkMethods ''Actions [ 'peekAction
                      , 'enqueueAction
                      , 'dequeueAction
                      ])
