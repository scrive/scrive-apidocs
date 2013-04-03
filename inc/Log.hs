module Log (
    amazon
  , stats
  , eleg
  , debug
  , error
  , cron
  , forkIOLogWhenError
  , mail
  , mailContent
  , mailingServer
  , docConverter
  , security
  , server
  , integration
  , teardownLogger
  , withLogger
  , setupLogger
  , scrivebymail
  , scrivebymailfailure
  , docevent
  , jsonMailAPI
  , mailAPI
  , payments
  , MonadLog(..)
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
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Crypto.RNG
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity


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

debug :: (MonadLog m) => String -> m ()
debug msg = logM "Kontrakcja.Debug" NOTICE msg

error :: (MonadLog m) => String -> m ()
error msg = logM "Kontrakcja.Error" NOTICE msg

cron :: (MonadLog m) => String -> m ()
cron msg = logM "Kontrakcja.Cron" NOTICE msg

mail :: (MonadLog m) => String -> m ()
mail msg = logM "Kontrakcja.Mail" NOTICE msg

mailContent :: (MonadLog m) => String -> m ()
mailContent msg = logM "Kontrakcja.MailContent" NOTICE msg

mailingServer :: (MonadLog m) => String -> m ()
mailingServer msg = logM "Kontrakcja.MailingServer" NOTICE msg

docConverter :: (MonadLog m) => String -> m ()
docConverter msg = logM "Kontrakcja.DocConverter" NOTICE msg

amazon :: (MonadLog m) => String -> m ()
amazon msg = logM "Kontrakcja.Amazon" NOTICE msg

security :: (MonadLog m) => String -> m ()
security msg = logM "Kontrakcja.Security" NOTICE msg

server :: (MonadLog m) => String -> m ()
server msg = logM "Happstack.Server" NOTICE msg

eleg :: (MonadLog m) => String -> m ()
eleg msg = logM "Kontrakcja.Eleg" NOTICE msg

stats :: (MonadLog m) => String -> m ()
stats msg = logM "Kontrakcja.Stats" NOTICE msg

integration :: (MonadLog m) => String -> m ()
integration msg = logM "Kontrakcja.Integration" NOTICE msg

scrivebymail :: (MonadLog m) => String -> m ()
scrivebymail msg = logM "Kontrakcja.ScriveByMail" NOTICE msg

jsonMailAPI :: (MonadLog m) => String -> m ()
jsonMailAPI msg = logM "Kontrakcja.JSONMailAPI" NOTICE msg

mailAPI :: (MonadLog m) => String -> m ()
mailAPI msg = logM "Kontrakcja.MailAPI" NOTICE msg

scrivebymailfailure :: (MonadLog m) => String -> m ()
scrivebymailfailure msg = logM "Kontrakcja.ScriveByMailFailures" ERROR msg

docevent :: (MonadLog m) => String -> m ()
docevent msg = logM "Kontrakcja.DocEvent" NOTICE msg

payments :: (MonadLog m) => String -> m ()
payments msg = logM "Kontrakcja.Payments" NOTICE msg

-- | FIXME: use forkAction
forkIOLogWhenError :: (MonadIO m) => String -> IO () -> m ()
forkIOLogWhenError errmsg action =
  liftIO $ do
    _ <- C.forkIO (action `C.catch` \(e :: C.SomeException) -> error $ errmsg ++ " " ++ show e)
    return ()
