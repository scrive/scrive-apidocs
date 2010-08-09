{-# LANGUAGE ForeignFunctionInterface, CPP, ScopedTypeVariables #-}


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
import Data.Either
import Happstack.Data.IxSet as IxSet
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import HSX.XMLGenerator
import HSP
import Control.Monad.State
import Control.Monad.Error
import Data.Monoid
import Data.List
import Data.Char

{-

Dump bin for things that do not fit anywhere else

-}

selectFormAction :: (MonadPlus m,ServerMonad m) => [(String,m a)] -> m a

selectFormAction [] = mzero
selectFormAction ((button,action):rest) = do
  maybepressed <- getDataFn (look button)
#if MIN_VERSION_happstack_server(0,5,1)
  either (\_ -> selectFormAction rest) (\_ -> action) maybepressed
#else
  if isJust maybepressed
     then action
     else selectFormAction rest
#endif

guardFormAction :: (ServerMonad m, MonadPlus m) => String -> m ()
guardFormAction button = do
  maybepressed <- getDataFn (look button)
#if MIN_VERSION_happstack_server(0,5,1)
  either (\_ -> mzero) (\_ -> return ()) maybepressed
#else
  guard (isJust maybepressed)
#endif

instance (EmbedAsChild m String) => (EmbedAsChild m BSL.ByteString) where
    asChild = asChild . BSL.toString

instance (EmbedAsChild m String) => (EmbedAsChild m BS.ByteString) where
    asChild = asChild . BS.toString

instance (EmbedAsAttr m String) => (EmbedAsAttr m BSL.ByteString) where
    asAttr = asAttr . BSL.toString

instance (EmbedAsAttr m String) => (EmbedAsAttr m BS.ByteString) where
    asAttr = asAttr . BS.toString

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


#ifdef WINDOWS
openDocument :: String -> IO ()
openDocument filename = do
  withCString filename $ \filename -> do
                          withCString "open" $ \open -> do
                                        shellExecute nullPtr open filename nullPtr nullPtr 1
             
foreign import stdcall "ShellExecuteA" shellExecute :: Ptr () -> Ptr CChar -> Ptr CChar -> Ptr () -> Ptr () -> CInt -> IO ()
#else
-- just do nothing on unix
openDocument :: String -> IO ()
openDocument filename = return ()
#endif

{-
newtype ConfigT m a = ConfigT { runConfigT :: ReaderT Config m a }
    deriving (Functor, Applicative, Alternative, MonadPlus, Monad, MonadIO, MonadTrans)

mapConfigT :: (m a -> n b) -> ConfigT m a -> ConfigT n b
mapConfigT f (ConfigT r) = ConfigT (mapReaderT f r)

class MonadConfig m where
    askConfig   :: m Config
    localConfig :: (Config -> Config) -> m a -> m a

instance (Monad m) => MonadConfig (ConfigT m) where
    askConfig                 = ConfigT ask
    localConfig f (ConfigT r) = ConfigT (local f r)

instance (Monad m, MonadConfig m) => MonadConfig (URLT url m) where
    askConfig     = lift askConfig
    localConfig f = mapURLT (localConfig f)

instance (Monad m, MonadConfig m) => MonadConfig (ServerPartT m) where
    askConfig     = lift askConfig
    localConfig f = mapServerPartT (localConfig f)

instance (Monad m, MonadConfig m) => MonadConfig (XMLGenT m) where
    askConfig                 = lift askConfig
    localConfig f (XMLGenT m) = XMLGenT (localConfig f m)
-}

{-
unpackErrorT:: (Monad m, Show e) => UnWebT (ErrorT e m) a -> UnWebT m a
unpackErrorT et = do
      eitherV <- runErrorT et
      case eitherV of
          Left e -> Just (Left e
                           , Set $ Dual $ Endo $ \r -> r{rsCode = 500})
          Right x -> x
-}

unpackErrorT:: (Monad m, Show e) => UnWebT (ErrorT e m) a -> UnWebT m a
unpackErrorT handler = do
      eitherV <- runErrorT handler
      return $ case eitherV of
          Left err -> Just ( Left (toResponse ("Catastrophic failure " ++ show err))
                           , Set $ Dual $ Endo $ \r -> r{rsCode = 500})
          Right x -> x


mapUnWebT :: (Functor m) => (a -> b) -> UnWebT m a -> UnWebT m b
mapUnWebT f x = fmap (fmap (mapFst (fmap f))) x
            where
              mapFst f (a,b) = (f a,b)
{-
toIO :: forall s m a. (Monad m) => s -> ServerPartT (StateT s m) a -> ServerPartT m (a,s)
toIO state = mapServerPartT f
    where
      f :: UnWebT (StateT s m) a -> UnWebT m (a,s)
      f m = runStateT m state
-}

toIO :: forall s m a . (Monad m) => s -> ServerPartT (StateT s m) a -> ServerPartT m a
toIO state = mapServerPartT f
    where
      f :: StateT s m (Maybe (Either Response a, FilterFun Response)) -> m (Maybe (Either Response a, FilterFun Response))
      f m = evalStateT m state

toIO2 :: forall s m a. (Monad m, Functor m) => s -> ServerPartT (StateT s m) a -> ServerPartT m (a, s)
toIO2 state = mapServerPartT evalStateT'
    where
      evalStateT' :: UnWebT (StateT s m) a -> UnWebT m (a, s)
      evalStateT' unwebt =
          do (m, s) <- runStateT unwebt state
             return $ fmap (mapFst (fmap (\a -> (a, s)))) m
      mapFst f (a,b) = (f a,b)


-- this is cheating
-- FIXME: font encoding
winAnsiChars = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~?€\201‚ƒ„…†‡ˆ‰Š‹Œ\215Ž\217\220‘’“”•–—˜™š›œ\235žŸ ¡¢£¤¥¦§¨©ª«¬?®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"

unicodeToWinAnsi x = 
    case findIndex (==x) winAnsiChars of
      Just i -> chr (i + 33)
      Nothing -> x
      

safehead s [] = error s
safehead _ (x:_) = x


getDataFnM fun = do
  m <- getDataFn fun
#if MIN_VERSION_happstack_server(0,5,1)
  either (\_ -> mzero) (return) m
#else
  case m of
    Just x -> return x
    Nothing -> mzero
#endif


pathdb get action = path $ \id -> do
    m <- query $ get id
    case m of
        Nothing -> mzero
        Just obj -> action obj

-- g :: String -> Kontra BS.ByteString 
g name = do
  k <- getDataFnM (look name)
  return (BS.fromString k)

renderHSPToByteString xml = do
  (meta,content) <- evalHSP Nothing xml
  return $ case meta of
             Just (XMLMetaData (showDt, dt) _ pr) -> 
                     BS.fromString ((if showDt then (dt ++) else id) (pr content))
             Nothing -> BS.fromString (renderAsHTML content)

