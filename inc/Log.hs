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



-- Here we do use Prelude.putStrLn because it prints character
-- by character and in case there are many thread it will interleave
-- messages. Using ByteStrings prevents interleaving at least on
-- page boundary (4096). Usually it is whole message boundary but as
-- far as I know it is not guaranteed.
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

mixlog :: (MonadLog m) => String -> JSONGen () -> m ()
mixlog title jsgen = mixlogjs title (runJSONGen jsgen)

mixlogt :: (MonadLog m) => String -> JSONGenT m () -> m ()
mixlogt title jsgent = runJSONGenT jsgent >>= mixlogjs title

mixlogjs :: (ToJSValue js, MonadLog m) => String -> js -> m ()
mixlogjs title js = logM (title ++ "\n" ++ jsonShowYamlLn False 4 (toJSValue js))

mixlog_ :: (MonadLog m) => String -> m ()
mixlog_ title = logM title

showStringYaml :: String -> String
showStringYaml str = "\"" ++ concatMap escape str ++ "\""
  where escape '"' = "\\\""
        escape '\\' = "\\\\"
        escape c | ord c < 32 = "\\x" ++ showHex (ord c `div` 16) (showHex (ord c `mod` 16) "")
        escape c = [c]


jsonShowYamlLn :: Bool -> Int -> JSValue -> String
jsonShowYamlLn _neednl _indent JSNull = "null\n"
jsonShowYamlLn _neednl _indent (JSBool val) = if val then "true\n" else "false\n"
jsonShowYamlLn _neednl _indent (JSRational _asFloat val) | denominator val == 1 = show (numerator val) ++ "\n"
jsonShowYamlLn _neednl _indent (JSRational _asFloat val) = showFloat (fromRational val :: Double) "\n"
jsonShowYamlLn _neednl _indent (JSString val) = showStringYaml (fromJSString val) ++ "\n"
jsonShowYamlLn _neednl _indent (JSArray []) = "[]\n"
jsonShowYamlLn neednl indent (JSArray vals) = (if neednl then "\n" else "") ++ concatMap p vals
  where p val = take indent (repeat ' ') ++ "- " ++ jsonShowYamlLn True (indent+1) val
jsonShowYamlLn _neednl _indent (JSObject vals) | null (fromJSObject vals) = "{}\n"
jsonShowYamlLn neednl indent (JSObject vals) = (if neednl then "\n" else "") ++ concatMap p (fromJSObject vals)
  where p (key,val) = take indent (repeat ' ') ++ showStringYaml key ++ ": " ++ jsonShowYamlLn True (indent+1) val
