module Log
  ( forkIOLogWhenError
  , teardownLogger
  , withLogger
  , setupLogger

  , MonadLog(..)

  , mixlog
  , mixlogjs
  , mixlog_

  ) where

import Control.Exception.Extensible (bracket)
import Control.Monad.Trans
import Prelude hiding (error)
import System.Directory
import System.IO (stdout, Handle, hSetEncoding, utf8, IOMode(..), hClose)
import System.Log.Formatter
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (streamHandler, GenericHandler(..))
import System.Log.Logger (Priority(..), rootLoggerName, setLevel, setHandlers, updateGlobalLogger)
import qualified System.Log.Logger
import qualified Control.Concurrent as C
import qualified Control.Exception as C
import OpenFileShared

import qualified Control.Monad.State.Lazy as LS
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy as LW
import qualified Control.Monad.Writer.Strict as SW
import qualified Control.Monad.RWS.Lazy as LRWS
import qualified Control.Monad.RWS.Strict as SRWS
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
  logM :: String -> Priority -> String -> m ()

instance (MonadLog m) => MonadLog (LS.StateT s m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m) => MonadLog (SS.StateT s m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m, LW.Monoid w) => MonadLog (LW.WriterT w m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m, SW.Monoid w) => MonadLog (SW.WriterT w m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m) => MonadLog (MaybeT m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m) => MonadLog (ListT m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m) => MonadLog (ContT r m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m) => MonadLog (IdentityT m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m, Error e) => MonadLog (ErrorT e m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m) => MonadLog (ReaderT r m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m) => MonadLog (CryptoRNGT m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m, SRWS.Monoid w) => MonadLog (SRWS.RWST r w s m) where
  logM a b c = lift $ logM a b c

instance (MonadLog m, LRWS.Monoid w) => MonadLog (LRWS.RWST r w s m) where
  logM a b c = lift $ logM a b c


instance MonadLog IO where
  logM = System.Log.Logger.logM

fileHandler' :: FilePath -> Priority -> IO (GenericHandler Handle)
fileHandler' fp pri = do
                     h <- openFileShared fp AppendMode
                     sh <- streamHandler h pri
                     return (sh{closeFunc = hClose})

-- | Opaque type covering all information needed to teardown the logger.
data LoggerHandle = LoggerHandle [GenericHandler Handle]

setupLogger :: IO LoggerHandle
setupLogger = do
    -- under Windows createDirectoryIfMissing throws an exception if
    -- the directory already exists, pretty ugly bug, lets just catch
    -- and ignore the exception
    createDirectoryIfMissing False "log" `C.catch` (\(_ :: C.SomeException) -> return ())

    let fmt = tfLogFormatter "%F %T" "$time $msg"

    appLog         <- fileHandler' "log/app.log"         INFO >>= \lh -> return $ setFormatter lh fmt
    accessLog      <- fileHandler' "log/access.log"      INFO
    cronLog        <- fileHandler' "log/cron.log"        INFO >>= \lh -> return $ setFormatter lh fmt
    mailLog        <- fileHandler' "log/mail.log"        INFO >>= \lh -> return $ setFormatter lh fmt
    debugLog       <- fileHandler' "log/debug.log"       INFO >>= \lh -> return $ setFormatter lh fmt
    errorLog       <- fileHandler' "log/error.log"       INFO >>= \lh -> return $ setFormatter lh fmt
    amazonLog      <- fileHandler' "log/amazon.log"      INFO >>= \lh -> return $ setFormatter lh fmt
    securityLog    <- fileHandler' "log/security.log"    INFO >>= \lh -> return $ setFormatter lh fmt
    elegLog        <- fileHandler' "log/eleg.log"        INFO >>= \lh -> return $ setFormatter lh fmt
    statsLog       <- fileHandler' "log/stats.log"       INFO >>= \lh -> return $ setFormatter lh fmt
    mailContentLog <- fileHandler' "log/mailcontent.log" INFO >>= \lh -> return $ setFormatter lh fmt
    mailingServerLog <- fileHandler' "log/mailingserver.log" INFO >>= \lh -> return $ setFormatter lh fmt
    messengerServerLog <- fileHandler' "log/messengerserver.log" INFO >>= \lh -> return $ setFormatter lh fmt
    docConverterLog <- fileHandler' "log/docconverter.log" INFO >>= \lh -> return $ setFormatter lh fmt
    integrationLog <- fileHandler' "log/integrationapi.log" INFO >>= \lh -> return $ setFormatter lh fmt
    scriveByMailLog<- fileHandler' "log/scrivebymail.log" INFO >>= \lh -> return $ setFormatter lh fmt
    jsonMailAPILog <- fileHandler' "log/jsonmailapi.log" INFO >>= \lh -> return $ setFormatter lh fmt
    mailAPILog     <- fileHandler' "log/mailapi.log" INFO >>= \lh -> return $ setFormatter lh fmt
    scriveByMailFailuresLog <- fileHandler' "log/scrivebymail.failures.log" INFO >>= \lh -> return $ setFormatter lh nullFormatter
    doceventLog <- fileHandler' "log/docevent.log" INFO >>= \lh -> return $ setFormatter lh fmt
    paymentsLog <- fileHandler' "log/payments.log" INFO >>= \lh -> return $ setFormatter lh fmt
    stdoutLog <- streamHandler stdout NOTICE

    let allLoggers = [ appLog
                     , accessLog
                     , cronLog
                     , stdoutLog
                     , mailLog
                     , debugLog
                     , errorLog
                     , amazonLog
                     , securityLog
                     , elegLog
                     , statsLog
                     , mailContentLog
                     , mailingServerLog
                     , messengerServerLog
                     , integrationLog
                     , scriveByMailLog
                     , scriveByMailFailuresLog
                     , doceventLog
                     , jsonMailAPILog
                     , mailAPILog
                     , paymentsLog
                     ]

    mapM_ (\lg -> hSetEncoding (privData lg) utf8) allLoggers

    -- Root Log
    updateGlobalLogger
        rootLoggerName
        (setLevel NOTICE . setHandlers [appLog])

    -- Access Log
    updateGlobalLogger
        "Happstack.Server.AccessLog.Combined"
        (setLevel INFO . setHandlers [accessLog])

    updateGlobalLogger
        "Kontrakcja.Cron"
        (setLevel NOTICE . setHandlers [cronLog, stdoutLog])

    -- Mail Log
    updateGlobalLogger
        "Kontrakcja.Mail"
        (setLevel NOTICE . setHandlers [mailLog])

    -- Mail Content Log
    updateGlobalLogger
        "Kontrakcja.MailContent"
        (setLevel NOTICE . setHandlers [mailContentLog])

    -- Mailing Server Log
    updateGlobalLogger
        "Kontrakcja.MailingServer"
        (setLevel NOTICE . setHandlers [mailingServerLog, stdoutLog])

    -- Mailing Server Log
    updateGlobalLogger
        "Kontrakcja.MessengerServer"
        (setLevel NOTICE . setHandlers [messengerServerLog, stdoutLog])

    -- Doc Converter Log
    updateGlobalLogger
        "Kontrakcja.DocConverter"
        (setLevel NOTICE . setHandlers [docConverterLog, stdoutLog])

    -- Amazon Log
    updateGlobalLogger
        "Kontrakcja.Amazon"
        (setLevel NOTICE . setHandlers [amazonLog])

    -- Security Log
    updateGlobalLogger
        "Kontrakcja.Security"
        (setLevel NOTICE . setHandlers [securityLog])

    -- Debug Log
    updateGlobalLogger
        "Kontrakcja.Debug"
        (setLevel NOTICE . setHandlers [debugLog, stdoutLog])

    -- Error Log
    updateGlobalLogger
        "Kontrakcja.Error"
        (setLevel NOTICE . setHandlers [errorLog, stdoutLog])

    -- Server Log
    updateGlobalLogger
        "Happstack.Server"
        (setLevel NOTICE . setHandlers [stdoutLog])

    -- ELeg Log
    updateGlobalLogger
        "Kontrakcja.Eleg"
        (setLevel NOTICE . setHandlers [elegLog, stdoutLog])

    -- Stats Log
    updateGlobalLogger
        "Kontrakcja.Stats"
        (setLevel NOTICE . setHandlers [statsLog])

    -- Integration API Log
    updateGlobalLogger
        "Kontrakcja.Integration"
        (setLevel NOTICE . setHandlers [integrationLog])

    -- ScriveByMail Log
    updateGlobalLogger
        "Kontrakcja.ScriveByMail"
        (setLevel NOTICE . setHandlers [scriveByMailLog])

    -- ScriveByMail Failed Message Log
    updateGlobalLogger
        "Kontrakcja.ScriveByMailFailures"
        (setLevel NOTICE . setHandlers [scriveByMailFailuresLog])

    -- DocEvent Log
    updateGlobalLogger
        "Kontrakcja.DocEvent"
        (setLevel NOTICE . setHandlers [doceventLog])

    -- JSONMailAPI Log
    updateGlobalLogger
        "Kontrakcja.JSONMailAPI"
        (setLevel NOTICE . setHandlers [jsonMailAPILog])

    -- MailAPI Log
    updateGlobalLogger
        "Kontrakcja.MailAPI"
        (setLevel NOTICE . setHandlers [mailAPILog])

    updateGlobalLogger
        "Kontrakcja.Payments"
        (setLevel NOTICE . setHandlers [paymentsLog])

    return $ LoggerHandle allLoggers

-- | Tear down the application logger; i.e. close all associated log handlers.
teardownLogger :: LoggerHandle -> IO ()
teardownLogger (LoggerHandle loggers) = do
    mapM_ close loggers

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

mixlogjs :: (ToJSValue js, MonadLog m) => String -> js -> m ()
mixlogjs title js = logM "Kontrakcja.Debug" NOTICE (title ++ "\n" ++ jsonShowYamlLn False 4 (toJSValue js))

mixlog_ :: (MonadLog m) => String -> m ()
mixlog_ title = logM "Kontrakcja.Debug" NOTICE title

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
