module Log
  ( forkIOLogWhenError
  , teardownLogger
  , withLogger
  , setupLogger

  , MonadLog(..)

  , mixlog
  , mixlogt
  , mixlogjs
  , mixlog_

  ) where

import Control.Exception.Extensible (bracket)
import Control.Monad.Trans
import Prelude hiding (error)
import qualified Control.Concurrent as C
import qualified Control.Exception as C

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Control.Monad.State.Lazy as LS
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy as LW
import qualified Control.Monad.Writer.Strict as SW
import qualified Control.Monad.RWS.Lazy as LRWS
import qualified Control.Monad.RWS.Strict as SRWS
import System.IO.Unsafe
import Data.Char
import Numeric
import Data.Ratio
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Crypto.RNG
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity

import Text.JSON.Gen
import Text.JSON

-- | MonadLog is for situations when you want to have access to
-- logging, but to not expose whole IO functionality. It is a safe
-- entry to a restricted IO monad.
--
-- Should be used together with other IO based monads that do not
-- expose MonadIO or MonadBase IO.
class (Monad m) => MonadLog m where
  logM :: String -> m ()

instance (MonadLog m) => MonadLog (LS.StateT s m) where
  logM msg = lift $ logM msg

instance (MonadLog m) => MonadLog (SS.StateT s m) where
  logM msg = lift $ logM msg

instance (MonadLog m, LW.Monoid w) => MonadLog (LW.WriterT w m) where
  logM msg = lift $ logM msg

instance (MonadLog m, SW.Monoid w) => MonadLog (SW.WriterT w m) where
  logM msg = lift $ logM msg

instance (MonadLog m) => MonadLog (MaybeT m) where
  logM msg = lift $ logM msg

instance (MonadLog m) => MonadLog (ListT m) where
  logM msg = lift $ logM msg

instance (MonadLog m) => MonadLog (ContT r m) where
  logM msg = lift $ logM msg

instance (MonadLog m) => MonadLog (IdentityT m) where
  logM msg = lift $ logM msg

instance (MonadLog m, Error e) => MonadLog (ErrorT e m) where
  logM msg = lift $ logM msg

instance (MonadLog m) => MonadLog (ReaderT r m) where
  logM msg = lift $ logM msg

instance (MonadLog m) => MonadLog (CryptoRNGT m) where
  logM msg = lift $ logM msg

instance (MonadLog m, SRWS.Monoid w) => MonadLog (SRWS.RWST r w s m) where
  logM msg = lift $ logM msg

instance (MonadLog m, LRWS.Monoid w) => MonadLog (LRWS.RWST r w s m) where
  logM msg = lift $ logM msg



-- Here we use 'ByteString.putStrLn' because 'Prelude.putStrLn' prints
-- character by character and in case there are many thread it will
-- interleave messages. Using ByteStrings prevents interleaving at
-- least on page boundary (4096). (I'm not sure if ByteString.putStrLn
-- guarantees that a whole message will go though at once, but we have
-- that one handled on channel level).
--
-- We have one global output channel that should serialize output
-- directly, but there is always the risk that somebody will print
-- things directly.
--
-- Output channel should have a list of unevaluated chunks. Those will
-- be evaluated in the thread outputing text. This is optimization as
-- it reduces latency.
--
-- FIXME: there is a risk that evaluating the string will cause
-- exceptions. We need to catch and ignore them here although not
-- everything should be ignored (notably ThreadKilled should not be
-- ignored).

{-# NOINLINE outputChannel #-}
outputChannel :: C.Chan String
outputChannel = unsafePerformIO $ do
  chan <- C.newChan
  let loop = do
        msg <- C.readChan chan
        BSC.putStrLn (BSU.fromString msg) `C.catch` \(e :: C.SomeException) -> mixlog_ $ "Exception caught while logging: " ++ show e
        loop
  _ <- C.forkIO loop
  return chan


instance MonadLog IO where
  logM = C.writeChan outputChannel

setupLogger :: IO ()
setupLogger = do
    return ()

-- | Tear down the application logger; i.e. close all associated log handlers.
teardownLogger :: () -> IO ()
teardownLogger () = do
    return ()

-- | Bracket an IO action which denotes the whole scope where the loggers of
-- the application are needed to installed. Sets them up before running the action
-- and tears them down afterwards. Even in case of an exception.
withLogger :: IO a -> IO a
withLogger = bracket setupLogger teardownLogger . const


-- | FIXME: use forkAction
forkIOLogWhenError :: (MonadIO m) => String -> IO () -> m ()
forkIOLogWhenError errmsg action =
  liftIO $ do
    _ <- C.forkIO (action `C.catch` \(e :: C.SomeException) -> mixlog_ $ errmsg ++ " " ++ show e)
    return ()

-- | Log a line of text with possibly non-empty set of properties attached to the text.
--
-- In the textual output the line will be prefixed in the output with
-- a timestamp. Properties will follow indented by 4 spaces and
-- generally following restricted yaml format. Example:
--
-- > 2022-12-12 12:34 Something important happened
-- >    "userid": 12344
-- >    "username": "Eric Ericsson"
-- >    "fields":
-- >      - "first_name"
-- >      - "last_name"
-- >      - "email"
--
-- Properties data is restricted to JSON object model and can easily
-- be stored in some external logging database.w
mixlog :: (MonadLog m) => String -> JSONGen () -> m ()
mixlog title jsgen = mixlogjs title (runJSONGen jsgen)

-- | A transformer version of 'mixlog'. Can be used to fetch data from
-- the underlying monad should it be needed sometimes. Example:
--
-- > mixlogt "Frobnicated" $ do
-- >     x <- lift (get_something_from_upper_monad)
-- >     value "x" x
--
mixlogt :: (MonadLog m) => String -> JSONGenT m () -> m ()
mixlogt title jsgent = runJSONGenT jsgent >>= mixlogjs title

-- | This is a variation on 'mixlog' that takes a premade version of
-- properties object. Useful for logging data directly from API calls
-- for example.
mixlogjs :: (ToJSValue js, MonadLog m) => String -> js -> m ()
mixlogjs title js = logM (title ++ "\n" ++ unlines (map ("    " ++) (jsonShowYamlLn (toJSValue js))))

-- | Log a line without any additional properties.
mixlog_ :: (MonadLog m) => String -> m ()
mixlog_ title = logM title

-- | Cannonicalize a string for yaml output. Yaml has a lot of different
-- ways to encode strings, we choose double quoted style as canonical.
showStringYaml :: String -> String
showStringYaml str = "\"" ++ concatMap escape str ++ "\""
  where escape '"' = "\\\""
        escape '\\' = "\\\\"
        escape c | ord c < 32 = "\\x" ++ showHex (ord c `div` 16) (showHex (ord c `mod` 16) "")
        escape c = [c]

-- | Show JSON as Yaml in defined by us canonical form. Important is
-- that it does not introduce spurious newlines so it can be reliably
-- grepped.
jsonShowYamlLn :: JSValue -> [String]
jsonShowYamlLn JSNull = ["null"]
jsonShowYamlLn (JSBool True) = ["true"]
jsonShowYamlLn (JSBool False) = ["false"]
jsonShowYamlLn (JSRational _asFloat val) | denominator val == 1 = [show (numerator val)]
jsonShowYamlLn (JSRational _asFloat val) = [showFFloat Nothing (fromRational val) ""]
jsonShowYamlLn (JSString val) = [showStringYaml (fromJSString val)]
jsonShowYamlLn (JSArray []) = ["[]"]
jsonShowYamlLn (JSArray vals) = concatMap p vals
  where p val = zipWith (\prep line -> prep ++ line) ("- " : repeat "  ") (jsonShowYamlLn val)
jsonShowYamlLn (JSObject vals) | null (fromJSObject vals) = ["{}"]
jsonShowYamlLn (JSObject vals) = concatMap p (fromJSObject vals)
  where p (key,val@(JSObject con)) | not (null (fromJSObject con)) = (showStringYaml key ++ ": ") : map ("  " ++) (jsonShowYamlLn val)
        p (key,val@(JSArray con)) | not (null con) = (showStringYaml key ++ ": ") : map ("  " ++) (jsonShowYamlLn val)
        p (key,val) = zipWith (\prep line -> prep ++ line) ((showStringYaml key ++ ": ") : repeat "  ") (jsonShowYamlLn val)
