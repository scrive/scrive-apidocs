module Flow.Server where

import Control.Exception (SomeException, catch, throw)
import Control.Monad.Reader
import Crypto.RNG
import Data.Aeson.Types
import Data.Time.Clock
import Database.PostgreSQL.PQTypes hiding (JSON(..))
import Log.Data
import Log.Monad (getLoggerIO)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Log (mkApplicationLogger)
import Network.Wai.Log.Internal
import Network.Wai.Middleware.Servant.Errors (errorMw)
import Servant
import Servant.Server.Experimental.Auth

import EventStream.Kinesis
import Flow.OrphanInstances ()
import Flow.Routes
import Flow.Server.Api
import Flow.Server.AuthHandler
import Flow.Server.Pages
import Flow.Server.Types
import Log.Configuration (LogRunner(LogRunner, withLogger))

server :: ServerT Routes AppM
server = api :<|> pages

naturalFlow :: RunLogger -> FlowConfiguration -> AppM a -> Handler a
naturalFlow runLogger FlowConfiguration {..} flowApp =
  runLogger
    . runDBT dbConnectionPool defaultTransactionSettings
    . runCryptoRNGT cryptoRNG
    . runKinesisT (kinesisStream context)
    $ runReaderT flowApp context

genAuthServerContext
  :: RunLogger
  -> FlowConfiguration
  -> Context
       ( AuthHandler Request Account ': AuthHandler Request InstanceUser ': AuthHandler Request InstanceUserHTML ': '[]
       )
genAuthServerContext runLogger flowConfiguration =
  authHandlerAccount runLogger flowConfiguration
    :. authHandlerInstanceUser runLogger flowConfiguration
    :. authHandlerInstanceUserHTML runLogger flowConfiguration
    :. EmptyContext

logExceptionMiddleware :: LoggerIO -> Application -> Application
logExceptionMiddleware loggerIO app' req respond =
  app' req respond `catch` \(e :: SomeException) -> do
    logIO "Unhandled exception" $ object ["exception" .= showt e]
    throw e
  where
    logIO message value = do
      now <- getCurrentTime
      loggerIO now LogAttention message value

app :: RunLogger -> FlowConfiguration -> IO Application
app runLogger flowConfiguration = do
  loggingMiddleware <- runLogger mkApplicationLogger
  loggerIO          <- runLogger getLoggerIO
  return
    . errorMw @JSON @'["message", "code"]
    . logExceptionMiddleware loggerIO
    . loggingMiddleware
    . serveWithContext routesProxy (genAuthServerContext runLogger flowConfiguration)
    $ hoistServerWithContext
        routesProxy
        (Proxy :: Proxy
            '[AuthHandler Request Account, AuthHandler Request InstanceUser, AuthHandler
              Request
              InstanceUserHTML]
        )
        (naturalFlow runLogger flowConfiguration)
        server

runFlow :: LogRunner -> FlowConfiguration -> IO ()
runFlow LogRunner {..} conf@FlowConfiguration {..} = withLogger $ \runLogger -> do
  runSettings warpSettings =<< app runLogger conf
  where warpSettings = setPort flowPort defaultSettings
