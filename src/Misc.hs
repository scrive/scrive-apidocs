
module Misc where
import Control.Monad(msum,liftM,mzero,guard,MonadPlus(..))
import Control.Monad.Reader (ask)
import Control.Monad.Trans(liftIO, MonadIO,lift)
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import qualified Data.ByteString as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BSC
import qualified Data.Object.Json as Json
import qualified Network.Curl as Curl
import Data.Maybe

{-

Dump bin for things that do not fit anywhere else

-}

selectFormAction :: (MonadPlus m,ServerMonad m) => [(String,m a)] -> m a
selectFormAction [] = mzero
selectFormAction ((button,action):rest) = do
  maybepressed <- getDataFn (look button)
  if isJust maybepressed
     then action
     else selectFormAction rest

guardFormAction :: (ServerMonad m, MonadPlus m) => String -> m ()
guardFormAction button = do
  maybepressed <- getDataFn (look button)
  guard (isJust maybepressed)
