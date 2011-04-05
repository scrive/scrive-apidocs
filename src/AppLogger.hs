module AppLogger ( setupLogger
                 , withLogger
                 , teardownLogger
                   
                 , debug
                 , mail
                 , error
                 , amazon    
                 , trustWeaver
                 , security 
                 , server
                   , forkIOLogWhenError
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
import Prelude hiding (error)
import Control.Monad.Trans
import qualified Control.Exception as C
import qualified Control.Concurrent as C

-- | Opaque type covering all information needed to teardown the logger.
data LoggerHandle = LoggerHandle [GenericHandler Handle]


setupLogger = do
    -- under Windows createDirectoryIfMissing throws an exception if
    -- the directory already exists, pretty ugly bug, lets just catch
    -- and ignore the exception
    createDirectoryIfMissing False "log" `catch` (\_ -> return ())
    
    let fmt = tfLogFormatter "%F %T" "$time $msg"
        
    appLog <- fileHandler "log/app.log" INFO
    accessLog <- fileHandler "log/access.log" INFO
    mailLog <- fileHandler "log/mail.log" INFO >>= \lh -> return $ setFormatter lh fmt
    debugLog <- fileHandler "log/debug.log" INFO >>= \lh -> return $ setFormatter lh fmt
    errorLog <- fileHandler "log/error.log" INFO >>= \lh -> return $ setFormatter lh fmt
    amazonLog <- fileHandler "log/amazon.log" INFO >>= \lh -> return $ setFormatter lh fmt
    trustWeaverLog <- fileHandler "log/trustweaver.log" INFO >>= \lh -> return $ setFormatter lh fmt
    securityLog <- fileHandler "log/security.log" INFO >>= \lh -> return $ setFormatter lh fmt
    stdoutLog <- streamHandler stdout NOTICE

    let allLoggers = [ appLog
                     , accessLog
                     , stdoutLog
                     , mailLog
                     , debugLog
                     , errorLog
                     , trustWeaverLog
                     , amazonLog
                     , securityLog
                     ]

    mapM_ (\log -> hSetEncoding (privData log) utf8) allLoggers
    
    hSetEncoding (privData accessLog) utf8
    hSetEncoding (privData mailLog) utf8
    hSetEncoding (privData debugLog) utf8
    hSetEncoding (privData errorLog) utf8
    hSetEncoding (privData amazonLog) utf8
    hSetEncoding (privData trustWeaverLog) utf8

    -- Root Log
    updateGlobalLogger
        rootLoggerName
        (setLevel NOTICE . setHandlers [appLog])

    -- Access Log
    updateGlobalLogger
        "Happstack.Server.AccessLog.Combined"
        (setLevel INFO . setHandlers [accessLog])

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
        (setLevel NOTICE . setHandlers [errorLog])

    -- Server Log
    updateGlobalLogger
        "Happstack.Server"
        (setLevel NOTICE . setHandlers [stdoutLog])
        
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

mail :: (MonadIO m) => String -> m ()
mail msg = liftIO $ noticeM "Kontrakcja.Mail" msg

amazon :: (MonadIO m) => String -> m ()
amazon msg = liftIO $ noticeM "Kontrakcja.Amazon" msg

trustWeaver :: (MonadIO m) => String -> m ()
trustWeaver msg = liftIO $ noticeM "Kontrakcja.TrustWeaver" msg

security :: (MonadIO m) => String -> m ()
security msg = liftIO $ noticeM "Kontrakcja.Security" msg

server :: (MonadIO m) => String -> m ()
server msg = liftIO $ noticeM "Happstack.Server" msg


forkIOLogWhenError :: (MonadIO m) => String -> IO () -> m ()
forkIOLogWhenError errmsg action = 
  liftIO $ do
    C.forkIO (action `C.catch` \(e :: C.SomeException) -> error $ errmsg ++ " " ++ show e)
    return ()
  
  
