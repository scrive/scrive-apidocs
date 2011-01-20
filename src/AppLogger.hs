module AppLogger ( setupLogger
                 , withLogger
                 , teardownLogger
                   
                 , debug
                 , mail
                 , amazon    
                 , trustWeaver  
                 , server
                 ) where

import System.Log.Logger
  ( Priority(..)
    , rootLoggerName
    , setLevel
    , setHandlers
    , updateGlobalLogger
      , noticeM
    )
import Control.Exception.Extensible (bracket)
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler, GenericHandler(..))
import System.Log.Formatter
import System.IO (stdout,Handle,hSetEncoding,utf8,hClose)
import System.Directory
import Control.Monad.Trans

-- | Opaque type covering all information needed to teardown the logger.
data LoggerHandle = LoggerHandle [GenericHandler Handle]


setupLogger = do
    createDirectoryIfMissing False "log"
    
    let fmt = tfLogFormatter "%F %T" "$time $msg"
        
    appLog <- fileHandler "log/app.log" INFO
    accessLog <- fileHandler "log/access.log" INFO
    mailLog <- fileHandler "log/mail.log" INFO >>= \lh -> return $ setFormatter lh fmt
    debugLog <- fileHandler "log/debug.log" INFO >>= \lh -> return $ setFormatter lh fmt
    amazonLog <- fileHandler "log/amazon.log" INFO >>= \lh -> return $ setFormatter lh fmt
    trustWeaverLog <- fileHandler "log/trustweaver.log" INFO >>= \lh -> return $ setFormatter lh fmt
    stdoutLog <- streamHandler stdout NOTICE

    hSetEncoding (privData appLog) utf8
    hSetEncoding (privData accessLog) utf8
    hSetEncoding (privData mailLog) utf8
    hSetEncoding (privData debugLog) utf8
    hSetEncoding (privData amazonLog) utf8
    hSetEncoding (privData trustWeaverLog) utf8

    -- Root Log
    updateGlobalLogger
        rootLoggerName
        (setLevel NOTICE . setHandlers [appLog])

    -- Access Log
    updateGlobalLogger
        "Happstack.Server.AccessLog.Combined"
        (setLevel NOTICE . setHandlers [accessLog])

    -- Mail Log
    updateGlobalLogger
        "Kontrakcja.Mail"
        (setLevel NOTICE . setHandlers [mailLog])

    -- Amazon Log
    updateGlobalLogger
        "Kontrakcja.Amazon"
        (setLevel NOTICE . setHandlers [amazonLog])
    
    -- TrustWeaver Log
    updateGlobalLogger
        "Kontrakcja.TrustWeaver"
        (setLevel NOTICE . setHandlers [trustWeaverLog])

    -- Debug Log
    updateGlobalLogger
        "Kontrakcja.Debug"
        (setLevel NOTICE . setHandlers [debugLog, stdoutLog])

    -- Server Log
    updateGlobalLogger
        "Happstack.Server"
        (setLevel NOTICE . setHandlers [stdoutLog])
        
    return $ LoggerHandle [ appLog
                          , accessLog
                          , stdoutLog
                          , mailLog
                          , debugLog
                          , trustWeaverLog
                          , amazonLog
                          ]

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

mail :: (MonadIO m) => String -> m ()
mail msg = liftIO $ noticeM "Kontrakcja.Mail" msg

amazon :: (MonadIO m) => String -> m ()
amazon msg = liftIO $ noticeM "Kontrakcja.Amazon" msg

trustWeaver :: (MonadIO m) => String -> m ()
trustWeaver msg = liftIO $ noticeM "Kontrakcja.TrustWeaver" msg

server :: (MonadIO m) => String -> m ()
server msg = liftIO $ noticeM "Happstack.Server" msg
