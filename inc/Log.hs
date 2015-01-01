{-# OPTIONS_GHC -fno-warn-orphans #-}
module Log
  ( withLogger
  , withLogger'

  , module Log.Class

  , mixlogjsIO
  , mixlogIO
  , attentionIO

  , mixlog
  , mixlogt
  , mixlog_

  , attention
  , attentiont
  , attention_

  , LogT(..)
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Data.Char
import Data.List
import Data.Ratio
import Data.Time
import Numeric
import Prelude hiding (error)
import System.IO.Unsafe
import Text.JSON
import Text.JSON.Gen
import qualified Control.Concurrent as C
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU

import Log.Class

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
        BSC.putStrLn (BSU.fromString msg) `E.catch` \(e :: E.SomeException) -> do
          mixlogjsIO "Exception caught while logging exception (ATTENTION!):" $
            runJSONGen (value "exception" (show e))
        loop
  _ <- C.forkIO loop
  return chan

instance MonadLog IO where
  mixlogjs = mixlogjsIO

newtype LogT m a = LogT { unLogT :: IdentityT m a }
  deriving (Monad, Functor, Applicative, Alternative, MonadBase b, MonadIO, MonadTrans, MonadMask, MonadCatch, MonadThrow)

instance MonadTransControl LogT where
  newtype StT LogT m = StLogT { unStLogT :: StT (IdentityT) m }
  liftWith = defaultLiftWith LogT unLogT StLogT
  restoreT = defaultRestoreT LogT unStLogT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl IO m => MonadBaseControl IO (LogT m) where
  newtype StM (LogT m) a = StMLogT { unStMLogT :: ComposeSt LogT m a }
  liftBaseWith = defaultLiftBaseWith StMLogT
  restoreM     = defaultRestoreM unStMLogT
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance (MonadBase IO m) => MonadLog (LogT m) where
  mixlogjs a b = liftBase (mixlogjsIO a b)

mixlogjsIO :: (ToJSValue js) => String -> js -> IO ()
mixlogjsIO title js = do
      -- FIXME: asking got time on every log line is actually a heavy task
      -- Find in the internet how to get around this limitation
      currentTime <-getCurrentTime
      -- show instance for UTCTime looks like this:
      -- "2014-01-23 22:08:14.682469 UTC"
      -- this is exactly what we want here
      let datedTitle = takeWhile (/='.') (show currentTime) ++ " " ++ title
      C.writeChan outputChannel (text datedTitle)
    where
      jsx = toJSValue js
      text datedTitle = case jsx of
               JSObject vals | null (fromJSObject vals) -> datedTitle
               JSObject _vals ->
                     intercalate "\n" (datedTitle : map ("    " ++) (jsonShowYamlLn jsx))
               JSArray vals | not (null vals) ->
                     intercalate "\n" (datedTitle : map ("    " ++) (jsonShowYamlLn jsx))
               _ -> intercalate " " (datedTitle : jsonShowYamlLn jsx)

-- | Bracket an IO action which denotes the whole scope where the loggers of
-- the application are needed to installed. Sets them up before running the action
-- and tears them down afterwards. Even in case of an exception.
withLogger :: IO a -> IO a
withLogger = id

withLogger' :: LogT IO a -> IO a
withLogger' = runIdentityT . unLogT

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


-- | Log a line without any additional properties.
mixlog_ :: (MonadLog m) => String -> m ()
mixlog_ title = mixlog title (return ())

mixlogIO :: String -> JSONGen () -> IO ()
mixlogIO title jsgen = mixlogjsIO title (runJSONGen jsgen)

attentionIO :: String -> JSONGen () -> IO ()
attentionIO title jsgen = mixlogjsIO (title ++ " (ATTENTION!)") (runJSONGen jsgen)

attention :: (MonadLog m) => String -> JSONGen () -> m ()
attention title = mixlog (title ++ " (ATTENTION!)")

attentiont :: (MonadLog m) => String -> JSONGenT m () -> m ()
attentiont title = mixlogt (title ++ " (ATTENTION!)")

attention_ :: (MonadLog m) => String -> m ()
attention_ title = mixlog_ (title ++ " (ATTENTION!)")

-- | Cannonicalize a string for yaml output. Yaml has a lot of different
-- ways to encode strings, we choose double quoted style as canonical.
--
-- This function outputs 'undefined' when anything in the string is
-- not defined.
showStringYaml :: String -> String
showStringYaml str | isBottom (goOverString str) = "undefined"
  where
        goOverString [] = ()
        goOverString (c:r) = c `seq` goOverString r
showStringYaml str = "\"" ++ concatMap escape str ++ "\""
  where escape '"' = "\\\""
        escape '\\' = "\\\\"
        escape '\n' = "\\n"
        escape '\r' = "\\r"
        escape '\t' = "\\t"
        escape c | ord c < 32 = "\\x" ++ showHex (ord c `div` 16) (showHex (ord c `mod` 16) "")
        escape c = [c]

-- | @'isBottomTimeOut' timeOutLimit@ works like 'isBottom', but if
-- @timeOutLimit@ is @'Just' lim@, then computations taking more than
-- @lim@ seconds are also considered to be equal to bottom. Note that
-- this is a very crude approximation of what a bottom is. Also note
-- that this \"function\" may return different answers upon different
-- invocations. Take it for what it is worth.
--
-- 'isBottomTimeOut' is subject to all the same vagaries as
-- 'T.timeOut'.

{-# NOINLINE isBottom #-}
isBottom :: a -> Bool
isBottom f = unsafePerformIO $
  (E.evaluate f >> return False) `E.catches`
    [ E.Handler (\(_ :: E.ArrayException)   -> return True)
    , E.Handler (\(_ :: E.ErrorCall)        -> return True)
    , E.Handler (\(_ :: E.NoMethodError)    -> return True)
    , E.Handler (\(_ :: E.NonTermination)   -> return True)
    , E.Handler (\(_ :: E.PatternMatchFail) -> return True)
    , E.Handler (\(_ :: E.RecConError)      -> return True)
    , E.Handler (\(_ :: E.RecSelError)      -> return True)
    , E.Handler (\(_ :: E.RecUpdError)      -> return True)
    ]

-- | Show JSON as Yaml in defined by us canonical form. It does not
-- introduce spurious newlines so it can be reliably grepped.
--
-- This function tries very hard to extract as much info as possible
-- even if there are some bottom values present here and there.
jsonShowYamlLn :: JSValue -> [String]
jsonShowYamlLn ev | isBottom ev = ["undefined"]
jsonShowYamlLn JSNull = ["null"]
jsonShowYamlLn (JSBool True) = ["true"]
jsonShowYamlLn (JSBool False) = ["false"]
jsonShowYamlLn (JSRational _asFloat val) | isBottom (denominator val) || isBottom (numerator val) = ["undefined"]
jsonShowYamlLn (JSRational _asFloat val) | denominator val == 1 = [show (numerator val)]
jsonShowYamlLn (JSRational _asFloat val) = [showFFloat Nothing (fromRational val) ""]
jsonShowYamlLn (JSString val) = [showStringYaml (fromJSString val)]
jsonShowYamlLn (JSArray ev) | isBottom (length ev) = ["undefined"]
jsonShowYamlLn (JSArray []) = ["[]"]
jsonShowYamlLn (JSArray vals) = concatMap p vals
  where p val = zipWith (\prep line -> prep ++ line) ("- " : repeat "  ") (jsonShowYamlLn val)
jsonShowYamlLn (JSObject vals) | isBottom (length (fromJSObject vals)) = ["undefined"]
jsonShowYamlLn (JSObject vals) | null (fromJSObject vals) = ["{}"]
jsonShowYamlLn (JSObject vals) = concatMap p (fromJSObject vals)
  where p (key,val) | isNonEmptyContainer val = (showStringYaml key ++ ": ") : map ("  " ++) (jsonShowYamlLn val)
        p (key,val) = zipWith (\prep line -> prep ++ line) ((showStringYaml key ++ ": ") : repeat "  ") (jsonShowYamlLn val)
        isNonEmptyContainer val | isBottom val = False
        isNonEmptyContainer (JSObject con) | isBottom (length (fromJSObject con)) = False
        isNonEmptyContainer (JSObject con) = not (null (fromJSObject con))
        isNonEmptyContainer (JSArray con) | isBottom (length con) = False
        isNonEmptyContainer (JSArray con) = not (null con)
        isNonEmptyContainer _ = False
