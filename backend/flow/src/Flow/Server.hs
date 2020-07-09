{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
module Flow.Server where

import Control.Exception (SomeException, catch, throw)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Time
import Crypto.RNG
import Data.Aeson.Types
import Data.Time.Clock
import Database.PostgreSQL.PQTypes hiding (JSON(..))
import Log.Class
import Log.Data
import Log.Monad (LogT, getLoggerIO)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Log (mkApplicationLogger)
import Network.Wai.Log.Internal
import Network.Wai.Middleware.Servant.Errors (errorMw)
import Servant
import Servant.Server.Experimental.Auth

import AccessControl.Check
import AccessControl.Types
import Flow.Api as Api
import Flow.Error
import Flow.Guards
import Flow.HighTongue
import Flow.Id
import Flow.Model.Types
import Flow.OrphanInstances ()
import Flow.Process
import Flow.Server.AuthHandler
import Flow.Server.Handlers.Instance
import Flow.Server.Routes
import Flow.Server.Types
import Log.Configuration (LogRunner(LogRunner, withLogger))
import qualified Flow.Model as Model

server :: ServerT Routes AppM
server =
  (accountEndpoints :<|> instanceUserEndpoints :<|> noAuthEndpoints)
    :<|> (instanceUserPages :<|> noAuthPages)
  where
    accountEndpoints account =
      createTemplate account
        :<|> deleteTemplate account
        :<|> getTemplate account
        :<|> patchTemplate account
        :<|> listTemplates account
        :<|> commitTemplate account
        :<|> startInstance account
        :<|> getInstance account
        :<|> listInstances account
    instanceUserEndpoints = getInstanceView
    noAuthEndpoints       = validateTemplate
    instanceUserPages     = instanceOverview
    noAuthPages           = instanceOverviewMagicHash

createTemplate :: Account -> CreateTemplate -> AppM GetCreateTemplate
createTemplate account@Account {..} CreateTemplate {..} = do
  logInfo_ "Creating template"
  guardUserHasPermission account [canDo CreateA $ FlowTemplateR (folder ^. #id)]
  id <- Model.insertTemplate $ InsertTemplate name process (user ^. #id) (folder ^. #id)
  pure $ GetCreateTemplate { id }

-- TODO: Committed templates shouldn't be deleted.
deleteTemplate :: Account -> TemplateId -> AppM NoContent
deleteTemplate account id = do
  logInfo_ "Deleting template"
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate id
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo DeleteA $ FlowTemplateR fid]
  Model.deleteTemplate id
  pure NoContent

getTemplate :: Account -> TemplateId -> AppM GetTemplate
getTemplate account templateId = do
  logInfo_ "Getting template"
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate templateId
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo ReadA $ FlowTemplateR fid]
  pure $ toGetTemplate template

toGetTemplate :: Template -> GetTemplate
toGetTemplate t = GetTemplate { id        = t ^. #id
                              , name      = t ^. #name
                              , process   = t ^. #process
                              , committed = t ^. #committed
                              , folderId  = t ^. #folderId
                              }

patchTemplate :: Account -> TemplateId -> PatchTemplate -> AppM GetTemplate
patchTemplate account templateId PatchTemplate {..} = do
  logInfo_ "Patching template"
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate templateId
  when (isJust $ template ^. #committed) throwTemplateAlreadyCommittedError
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo UpdateA $ FlowTemplateR fid]
  let ut = UpdateTemplate templateId name process Nothing
  updated <- fromMaybeM throwTemplateNotFoundError $ Model.updateTemplate ut
  pure $ toGetTemplate updated

commitTemplate :: Account -> TemplateId -> AppM NoContent
commitTemplate account id = do
  logInfo_ "Committing template"
  now      <- liftIO currentTime
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate id
  when (isJust $ template ^. #committed) throwTemplateAlreadyCommittedError
  let templateDSL = template ^. #process
  -- We're currently not storing the machine, so we throw it away.
  either throwDSLValidationError' (const $ pure ()) $ decodeHighTongue templateDSL
  let fid = template ^. #folderId
  guardUserHasPermission account [canDo UpdateA $ FlowTemplateR fid]
  _ <- Model.updateTemplate $ UpdateTemplate id Nothing Nothing (Just now)
  pure NoContent
  where
    -- TODO: Currently, there's no way to get more than a singleton list of validation
    -- TODO: errors. This allows the Error module to be prettier, all this should be
    -- TODO: improved later.
    throwDSLValidationError' = \case
      []      -> throwDSLValidationError "Unknown validation error"
      err : _ -> throwDSLValidationError $ error_message err


-- TODO: Do better error messages.
-- TODO: Compilation step?
validateTemplate :: Process -> AppM [ValidationError]
validateTemplate template = do
  logInfo_ "Validating template"
  either pure (const (pure [])) $ decodeHighTongue template

listTemplates :: Account -> AppM [GetTemplate]
listTemplates account@Account {..} = do
  logInfo_ "Listing templates"
  templates <- Model.selectTemplatesByUserID $ user ^. #id
  let fids = view #folderId <$> templates
  guardUserHasPermission account [ canDo ReadA $ FlowTemplateR fid | fid <- fids ]
  pure $ fmap toGetTemplate templates

type RunLogger = forall m a . LogT m a -> m a

naturalFlow :: RunLogger -> FlowConfiguration -> AppM a -> Handler a
naturalFlow runLogger FlowConfiguration {..} flowApp =
  runLogger
    . runDBT dbConnectionPool defaultTransactionSettings
    . runCryptoRNGT cryptoRNG
    . runReaderT flowApp
    $ FlowContext handleWithKontra

genAuthServerContext
  :: FlowConfiguration
  -> Context (AuthHandler Request Account ': AuthHandler Request InstanceUser ': '[])
genAuthServerContext flowConfiguration =
  authHandlerAccount flowConfiguration
    :. authHandlerInstanceUser flowConfiguration
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
    . serveWithContext routesProxy (genAuthServerContext flowConfiguration)
    $ hoistServerWithContext
        routesProxy
        (Proxy :: Proxy '[AuthHandler Request Account, AuthHandler Request InstanceUser])
        (naturalFlow runLogger flowConfiguration)
        server

runFlow :: LogRunner -> FlowConfiguration -> IO ()
runFlow LogRunner {..} conf@FlowConfiguration {..} = withLogger $ \runLogger -> do
  runSettings warpSettings =<< app runLogger conf
  where warpSettings = setPort flowPort defaultSettings
