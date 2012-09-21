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
  , trustWeaver
  , withLogger
  , setupLogger
  , scrivebymail
  , scrivebymailfailure
  , docevent
  , jsonMailAPI
  , mailAPI
  , payments
  ) where

import Control.Exception.Extensible (bracket)
import Control.Monad.Trans
import Prelude hiding (error)
import System.Directory
import System.IO (stdout, Handle, hSetEncoding, utf8, IOMode(..), hClose)
import System.Log.Formatter
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (streamHandler, GenericHandler(..))
import System.Log.Logger (Priority(..), rootLoggerName, setLevel, setHandlers, updateGlobalLogger, noticeM, errorM)
import qualified Control.Concurrent as C
import qualified Control.Exception as C
import OpenFileShared

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
    trustWeaverLog <- fileHandler' "log/trustweaver.log" INFO >>= \lh -> return $ setFormatter lh fmt
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
                     , trustWeaverLog
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

    -- TrustWeaver Log
    updateGlobalLogger
        "Kontrakcja.TrustWeaver"
        (setLevel NOTICE . setHandlers [trustWeaverLog])

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

debug :: (MonadIO m) => String -> m ()
debug msg = liftIO $ noticeM "Kontrakcja.Debug" msg

error :: (MonadIO m) => String -> m ()
error msg = liftIO $ noticeM "Kontrakcja.Error" msg

cron :: (MonadIO m) => String -> m ()
cron msg = liftIO $ noticeM "Kontrakcja.Cron" msg

mail :: (MonadIO m) => String -> m ()
mail msg = liftIO $ noticeM "Kontrakcja.Mail" msg

mailContent :: (MonadIO m) => String -> m ()
mailContent msg = liftIO $ noticeM "Kontrakcja.MailContent" msg

mailingServer :: (MonadIO m) => String -> m ()
mailingServer msg = liftIO $ noticeM "Kontrakcja.MailingServer" msg

docConverter :: (MonadIO m) => String -> m ()
docConverter msg = liftIO $ noticeM "Kontrakcja.DocConverter" msg

amazon :: (MonadIO m) => String -> m ()
amazon msg = liftIO $ noticeM "Kontrakcja.Amazon" msg

trustWeaver :: (MonadIO m) => String -> m ()
trustWeaver msg = liftIO $ noticeM "Kontrakcja.TrustWeaver" msg

security :: (MonadIO m) => String -> m ()
security msg = liftIO $ noticeM "Kontrakcja.Security" msg

server :: (MonadIO m) => String -> m ()
server msg = liftIO $ noticeM "Happstack.Server" msg

eleg :: (MonadIO m) => String -> m ()
eleg msg = liftIO $ noticeM "Kontrakcja.Eleg" msg

stats :: (MonadIO m) => String -> m ()
stats msg = liftIO $ noticeM "Kontrakcja.Stats" msg

integration :: (MonadIO m) => String -> m ()
integration msg = liftIO $ noticeM "Kontrakcja.Integration" msg

scrivebymail :: (MonadIO m) => String -> m ()
scrivebymail msg = liftIO $ noticeM "Kontrakcja.ScriveByMail" msg

jsonMailAPI :: (MonadIO m) => String -> m ()
jsonMailAPI msg = liftIO $ noticeM "Kontrakcja.JSONMailAPI" msg

mailAPI :: (MonadIO m) => String -> m ()
mailAPI msg = liftIO $ noticeM "Kontrakcja.MailAPI" msg

scrivebymailfailure :: (MonadIO m) => String -> m ()
scrivebymailfailure msg = liftIO $ errorM "Kontrakcja.ScriveByMailFailures" msg

docevent :: (MonadIO m) => String -> m ()
docevent msg = liftIO $ noticeM "Kontrakcja.DocEvent" msg

payments :: MonadIO m => String -> m ()
payments msg = liftIO $ noticeM "Kontrakcja.Payments" msg

-- | FIXME: use forkAction
forkIOLogWhenError :: (MonadIO m) => String -> IO () -> m ()
forkIOLogWhenError errmsg action =
  liftIO $ do
    _ <- C.forkIO (action `C.catch` \(e :: C.SomeException) -> error $ errmsg ++ " " ++ show e)
    return ()

