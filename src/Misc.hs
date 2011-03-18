{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-| Dump bin for things that do not fit anywhere else

I do not mind people sticking stuff in here. From time to time just
please go over this file, reorganize, pull better parts to other
modules.

Keep this one as unorganized dump.
-}
module Misc where
import Control.Monad(msum,liftM,mzero,guard,MonadPlus(..))
import Control.Monad.Reader (ask,asks)
import Control.Monad.Trans(liftIO, MonadIO,lift)
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query,getRandomR)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import Data.Maybe
import Data.Either
import Happstack.Data.IxSet as IxSet
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import HSX.XMLGenerator
import HSP (evalHSP, XMLMetaData(..), renderAsHTML, IsAttrValue, toAttrValue)
import qualified HSP.XML
import qualified HSP
import Control.Monad.State
import Control.Monad.Error
import Data.Monoid
import Data.List
import Data.Char
import Data.Word
import System.Random
import Data.Typeable
import Numeric -- use new module
import Happstack.State
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Data.Data
import Happstack.Data
import Data.Int
import qualified Control.Exception as C
import System.Cmd
import Control.Concurrent
import System.Process
import System.IO
import System.Exit
import System.Log.Logger (errorM)
import Data.Traversable (sequenceA)
import Control.Applicative
import System.Directory
import System.IO.Temp
import Data.Typeable
import Data.Data
import qualified GHC.Conc

selectFormAction :: (HasRqData m, MonadIO m,MonadPlus m,ServerMonad m) => [(String,m a)] -> m a
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

guardFormAction :: (HasRqData m, MonadIO m,ServerMonad m, MonadPlus m) => String -> m ()
guardFormAction button = do
  maybepressed <- getDataFn (look button)
#if MIN_VERSION_happstack_server(0,5,1)
  either (\_ -> mzero) (\_ -> return ()) maybepressed
#else
  guard (isJust maybepressed)
#endif

instance (EmbedAsChild m String) => (EmbedAsChild m BSL.ByteString) 
    where
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

concatChunks :: BSL.ByteString -> BS.ByteString
concatChunks = BS.concat . BSL.toChunks

-- | Get a unique index value in a set. Unique number is 31 bit in
-- this function.  First argument is set, second is index constructor.
--
-- See also 'getUnique64'.
getUnique
  :: (Indexable a,
      Data a,
      Ord a,
      Typeable k,
      Monad (t GHC.Conc.STM),
      MonadTrans t) =>
     IxSet a -> (Int -> k) -> Ev (t GHC.Conc.STM) k
getUnique ixset constr = do
  r <- getRandomR (0,0x7fffffff::Int)
  let v = constr r
  if IxSet.null (ixset @= v)
     then return v
     else getUnique ixset constr

-- | Get a unique index value in a set. Unique number is 31 bit in
-- this function.  First argument is set, second is index constructor.
--
-- See also 'getUnique64'.
getUnique64
  :: (Indexable a,
      Data a,
      Ord a,
      Typeable k,
      Monad (t GHC.Conc.STM),
      MonadTrans t) =>
     IxSet a -> (Int64 -> k) -> Ev (t GHC.Conc.STM) k
getUnique64 ixset constr = do
  r <- getRandomR (0,0x7fffffffffffffff::Int64)
  let v = constr r
  if IxSet.null (ixset @= v)
     then return v
     else getUnique64 ixset constr


#ifdef WINDOWS
-- | Open external document in default application. Useful to open
-- *.eml in email program for example.
openDocument :: String -> IO ()
openDocument filename = do
  withCString filename $ \filename -> do
                          withCString "open" $ \open -> do
                                        shellExecute nullPtr open filename nullPtr nullPtr 1
             
foreign import stdcall "ShellExecuteA" shellExecute :: Ptr () -> Ptr CChar -> Ptr CChar -> Ptr () -> Ptr () -> CInt -> IO ()
#else
-- | Open external document in default application. Useful to open
-- *.eml in email program for example.
openDocument :: String -> IO ()
openDocument filename = return ()
#endif

toIO :: forall s m a . (Monad m) => s -> ServerPartT (StateT s m) a -> ServerPartT m a
toIO state = mapServerPartT f
    where
      f m = evalStateT m state


-- | Oh boy, invent something better.
--
-- FIXME: this is so wrong on so many different levels
safehead :: [Char] -> [t] -> t
safehead s [] = error s
safehead _ (x:_) = x

-- | Extract data from GET or POST request. Fail with 'mzero' if param
-- variable not present or when it cannot be read.
getDataFnM :: (HasRqData m, MonadIO m, ServerMonad m, MonadPlus m) => RqData a -> m a
getDataFnM fun = do
  m <- getDataFn fun
#if MIN_VERSION_happstack_server(0,5,1)
  either (\_ -> mzero) (return) m
#else
  case m of
    Just x -> return x
    Nothing -> mzero
#endif

-- | Since we sometimes want to get 'Maybe' and also we wont work with
-- newer versions of happstack here is.  This should be droped when
-- new version is globaly established.
getDataFn' :: (HasRqData m, MonadIO m, ServerMonad m) => RqData a -> m (Maybe a)
getDataFn' fun = do
  m <- getDataFn fun
#if MIN_VERSION_happstack_server(0,5,1)
  either (\_ -> return Nothing) (return . Just ) m
#else
  return m
#endif

-- | This is a nice attempt at generating database queries directly
-- from URL parts.
pathdb
  :: (FromReqURI a,
      MonadPlus m,
      ServerMonad m,
      MonadIO m,
      QueryEvent a1 (Maybe t)) =>
     (a -> a1) -> (t -> m b) -> m b
pathdb get action = path $ \id -> do
    m <- query $ get id
    case m of
        Nothing -> mzero
        Just obj -> action obj

-- | Get param as strict ByteString instead of a lazy one.
g :: (HasRqData f, MonadIO f, ServerMonad f, MonadPlus f, Functor f) =>
     String -> f BS.ByteString
g name = fmap concatChunks (getDataFnM (lookBS name))

-- | Useful inside the 'RqData' monad.  Gets the named input parameter
-- (either from a @POST@ or a @GET@)
lookInputList :: String -> RqData [BSL.ByteString]
lookInputList name
    = do 
#if MIN_VERSION_happstack_server(0,5,1)
         inputs <- asks (\(a,b,c) -> a ++ b)
#else
         inputs <- asks fst 
#endif
         let isname (xname,(Input value _ _)) | xname == name = [value]
             isname _ = []
         return [value | k <- inputs, eithervalue <- isname k, Right value <- [eithervalue]]

-- | Render XML as a 'String' properly, i. e. with <?xml?> in the beginning.
renderXMLAsStringHTML :: (Maybe XMLMetaData, HSP.XML.XML) -> [Char]
renderXMLAsStringHTML (meta,content) = 
    case meta of
      Just (XMLMetaData (showDt, dt) _ pr) -> 
          (if showDt then (dt ++) else id) (pr content)
      Nothing -> renderAsHTML content

-- | Render XML as a 'ByteString' properly, i. e. with <?xml?> in the beginning.
renderXMLAsBSHTML
  :: (Maybe XMLMetaData, HSP.XML.XML) -> BS.ByteString
renderXMLAsBSHTML = BS.fromString . renderXMLAsStringHTML


-- | Render HSP as a 'ByteString' properly, i. e. with <?xml?> in the beginning.
renderHSPToByteString
  :: HSP.HSP HSP.XML.XML -> IO BS.ByteString
renderHSPToByteString xml = do
  fmap renderXMLAsBSHTML $ evalHSP Nothing xml

-- | Render HSP as a 'String' properly, i. e. with <?xml?> in the beginning.
renderHSPToString
  :: HSP.HSP HSP.XML.XML -> IO String
renderHSPToString xml = do
  fmap renderXMLAsStringHTML $ evalHSP Nothing xml


-- | Opaque 'Word64' type. Used as authentication token. Useful is the 'Random' instance.
newtype MagicHash = MagicHash { unMagicHash :: Word64 }
    deriving (Eq, Ord, Typeable, Data)

deriving instance Random MagicHash
deriving instance Serialize MagicHash

instance Version MagicHash

instance Show MagicHash where
    showsPrec prec (MagicHash x) = (++) (pad0 16 (showHex x ""))
    

instance Read MagicHash where
    readsPrec prec = let make (i,v) = (MagicHash i,v) 
                     in map make . readHex


instance FromReqURI MagicHash where
    fromReqURI = readM
 

-- | Create an external process with arguments. Feed it input, collect
-- exit code, stdout and stderr.
--
-- Standard input is first written to a temporary file. GHC 6.12.1
-- seemed to have trouble doing multitasking when writing to a slow
-- process like curl upload.
readProcessWithExitCode'
    :: FilePath                                    -- ^ command to run
    -> [String]                                    -- ^ any arguments
    -> BSL.ByteString                              -- ^ standard input
    -> IO (ExitCode,BSL.ByteString,BSL.ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' cmd args input = 
  withSystemTempFile "process" $ \inputname inputhandle -> do
    BSL.hPutStr inputhandle input
    hFlush inputhandle
    hSeek inputhandle AbsoluteSeek 0

    (_, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = UseHandle inputhandle,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    outMVar <- newEmptyMVar

    outM <- newEmptyMVar
    errM <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    forkIO $ do
        out <- BSL.hGetContents outh
        C.evaluate (BSL.length out)
        putMVar outM out
        putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    forkIO $ do
        err  <- BSL.hGetContents errh
        C.evaluate (BSL.length err)
        putMVar errM err
        putMVar outMVar ()

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    C.handle ((\e -> return ()) :: (C.IOException -> IO ())) $ hClose outh
    C.handle ((\e -> return ()) :: (C.IOException -> IO ())) $ hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    out <- readMVar outM
    err <- readMVar errM

    return (ex, out, err)

#ifdef WINDOWS
curl_exe = "curl.exe"
#else
curl_exe = "./curl"
#endif

{-| This function executes curl as external program. Args are args.
-}
readCurl :: [String]                 -- ^ any arguments
         -> BSL.ByteString           -- ^ standard input
         -> IO (ExitCode,BSL.ByteString,BSL.ByteString) -- ^ exitcode, stdout, stderr
readCurl args input = readProcessWithExitCode' curl_exe args input
  

-- | Run action, record failure if any. 
logErrorWithDefault :: IO (Either String a)  -- ^ action to run
                    -> b                     -- ^ default value in case action failed
                    -> (a -> IO b)           -- ^ action that uses value
                    -> IO b                  -- ^ result
logErrorWithDefault c d f = do
    c' <- c
    case c' of
        Right c'' ->  f c''
        Left err  ->  do 
                errorM "Happstack.Server" err
                return d

-- | Select first alternative from a list of options.
--
-- Remeber LISP and its cond!
caseOf :: [(Bool, t)] -> t -> t
caseOf ((True,a):_) _ = a
caseOf (_:r) d = caseOf r d
caseOf [] d = d

-- | Enumerate all values of a bounded type.
allValues::(Bounded a, Enum a) => [a]
allValues = enumFrom minBound

-- | Just @flip map@.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | 'sequenceA' says that if we maybe have @(Maybe (m a))@ a computation
-- that gives a then we can get real computation that may fail m
-- @(Maybe a)@ 'sequenceMM' doest the same, but is aware that first
-- computation can also fail, and so it joins two posible fails.
sequenceMM :: (Applicative m) => Maybe (m (Maybe a)) -> m (Maybe a)
sequenceMM = (fmap join) . sequenceA 


when_::(Monad m) => Bool -> m a -> m ()
when_ b c =  when b $ c >> return () 

maybe' :: a -> Maybe a -> a
maybe' a ma = maybe a id ma   

isFieldSet :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) => String -> f Bool
isFieldSet name = isJust <$> getField name


getFields :: (HasRqData m, MonadIO m, ServerMonad m,Functor m) => String -> m [String]
getFields name = (map BSL.toString)  <$> (fromMaybe []) <$> getDataFn' (lookInputList name)

getField :: (HasRqData m, MonadIO m, ServerMonad m,Functor m) => String -> m (Maybe String)
getField name = listToMaybe . reverse <$> getFields name

getFieldBS :: (HasRqData m, MonadIO m, ServerMonad m,Functor m) => String -> m (Maybe BSL.ByteString)
getFieldBS name = getDataFn' (lookBS name)

getFieldUTF
  :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) => String -> f (Maybe BS.ByteString)
getFieldUTF name = (fmap BS.fromString) <$> getField name

getFieldWithDefault
  :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) => String -> String -> f String
getFieldWithDefault d name =   (fromMaybe d) <$> getField name

getFieldBSWithDefault
  :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) =>
     BSL.ByteString -> String -> f BSL.ByteString
getFieldBSWithDefault  d name = (fromMaybe d) <$> getFieldBS name

getFieldUTFWithDefault
  :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) =>
     BS.ByteString -> String -> f BS.ByteString
getFieldUTFWithDefault  d name = (fromMaybe d) <$> getFieldUTF name

readField
  :: (HasRqData f, MonadIO f, Read a, Functor f, ServerMonad f) => String -> f (Maybe a)
readField name =  (join . (fmap readM)) <$> getField name

whenMaybe::(Functor m,Monad m) => Bool -> m a -> m (Maybe a)
whenMaybe True  c = fmap Just c
whenMaybe False _ = return Nothing

-- | Pack value to just unless we have 'mzero'.  Since we can not check
-- emptyness of string in templates we want to pack it in maybe.
nothingIfEmpty::(Eq a, Monoid a) => a -> Maybe a
nothingIfEmpty a = if mempty == a then Nothing else Just a

-- | Failing if inner value is empty
joinEmpty::(MonadPlus m, Monoid a, Ord a) => m a -> m a
joinEmpty m = do 
                mv <- m 
                if mv == mempty
                 then mzero
                 else return mv

{-| This function is useful when creating 'Typeable' instance when we
want a specific name for type.  Example of use:

  > instance Typeable Author where typeOf _ = mkTypeOf "XX_Author" 

-}
mkTypeOf :: String -> TypeRep
mkTypeOf name = mkTyConApp (mkTyCon name) []

-- | Pad string with zeros at the beginning.
pad0 :: Int         -- ^ how long should be the number
     -> String      -- ^ the number as string
     -> String      -- ^ zero padded number
pad0 len str = take missing (repeat '0') ++ str
    where
        diff = len - length str
        missing = if diff >= 0 then diff else 0
