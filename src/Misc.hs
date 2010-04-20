{-# LANGUAGE ForeignFunctionInterface #-}


module Misc where
import Control.Monad(msum,liftM,mzero,guard,MonadPlus(..))
import Control.Monad.Reader (ask)
import Control.Monad.Trans(liftIO, MonadIO,lift)
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query,getRandomR)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Object.Json as Json
import qualified Network.Curl as Curl
import Data.Maybe
import Happstack.Data.IxSet as IxSet
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import HSX.XMLGenerator
import HSP

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

instance (EmbedAsChild m String) => (EmbedAsChild m BSL.ByteString) where
    asChild string = asChild (BSL.toString string)

instance (EmbedAsChild m String) => (EmbedAsChild m BS.ByteString) where
    asChild string = asChild (BS.toString string)

instance (EmbedAsAttr m String) => (EmbedAsAttr m BSL.ByteString) where
    asAttr string = asAttr (BSL.toString string)

instance (EmbedAsAttr m String) => (EmbedAsAttr m BS.ByteString) where
    asAttr string = asAttr (BS.toString string)

instance Monad m => IsAttrValue m BS.ByteString where
    toAttrValue = toAttrValue . BS.toString

instance Monad m => IsAttrValue m BSL.ByteString where
    toAttrValue = toAttrValue . BSL.toString

concatChunks = BS.concat . BSL.toChunks

{-
getUnique
  :: (Indexable a b,
      Data.Data.Data a,
      Ord a,
      Data.Typeable.Typeable k,
      Monad (t GHC.Conc.STM),
      Control.Monad.Trans.MonadTrans t) =>
     IxSet a
     -> (Int -> k)
     -> happstack-state-0.4.3:Happstack.State.Types.Ev
          (t GHC.Conc.STM) k
-}
getUnique ixset constr = do
  r <- getRandomR (0,0x7fffffff::Int)
  let v = constr r
  if IxSet.null (ixset @= v)
     then return v
     else getUnique ixset constr


openDocument :: String -> IO ()
openDocument filename = do
  withCString filename $ \filename -> do
                          withCString "open" $ \open -> do
                                        shellExecute nullPtr open filename nullPtr nullPtr 1
             
foreign import stdcall "ShellExecuteA" shellExecute :: Ptr () -> Ptr CChar -> Ptr CChar -> Ptr () -> Ptr () -> CInt -> IO ()
