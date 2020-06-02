{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Flow.Server where

import Control.Arrow (left)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Time
import Data.Text
import Data.Text.Encoding
import Data.Yaml
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.SQL.Builder
import Log.Class
import Log.Monad (LogT)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Log (mkApplicationLogger)
import Servant
import Servant.Server.Experimental.Auth

import Auth.Model
import Auth.OAuth
import Auth.Parse
import Flow.Api as Api
import Flow.Error
import Flow.HighTongue
import Flow.Id
import Flow.Machinize as Machinize
import Flow.Model.Types
import Flow.OrphanInstances ()
import Flow.Server.Handlers.Instance
import Flow.Server.Types
import Log.Configuration (LogRunner(LogRunner, withLogger))
import User.UserID (unsafeUserID)
import UserGroup.Internal (unsafeUserGroupID)
import qualified Flow.Model as Model

server :: ServerT FlowAPI AppM
server = authenticated :<|> validateTemplate
  where
    authenticated account =
      createTemplate account
        :<|> deleteTemplate account
        :<|> getTemplate account
        :<|> patchTemplate account
        :<|> listTemplates account
        :<|> commitTemplate account
        :<|> startInstance account
        :<|> getInstance
        :<|> getInstanceView account

-- TODO: Handle decodeUtf8 exceptions
authHandler :: FlowConfiguration -> AuthHandler Request Account
authHandler flowConfiguration = mkAuthHandler handler
  where
    maybeToRight e = maybe (Left e) Right
    handler :: Request -> Handler Account
    handler req = do
      oauthTokens <- either (throwAuthError OAuthHeaderParseFailureError) pure
        $ getOAuthAuthorization req
      liftIO $ print oauthTokens
      maybeIds <- runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings
        $ authenticateToken oauthTokens
      (userId, userGroupId) <- maybe (throwAuthError InvalidTokenError "") pure maybeIds
      pure $ Account { userId      = unsafeUserID userId
                     , userGroupId = unsafeUserGroupID userGroupId
                     }

    getOAuthAuthorization :: Request -> Either Text OAuthAuthorization
    getOAuthAuthorization req =
      (maybeToRight err . lookup "authorization" $ requestHeaders req)
        >>= (parseParams . splitAuthorization . decodeUtf8)

    -- TODO handle the exception somehow
    -- ... but don't put it into the response, it leaks internal information!
    throwAuthError errorName e = do
      liftIO $ print e
      throwAuthenticationError errorName
    err = "AUTHORIZATION header not found."

-- TODO: Check user permissions to create templates.
createTemplate :: Account -> CreateTemplate -> AppM GetCreateTemplate
createTemplate Account {..} CreateTemplate {..} = do
  logInfo_ "creating template"
  id <- Model.insertTemplate $ InsertTemplate name process userId userGroupId
  pure $ GetCreateTemplate { id }

-- TODO: Authorization
-- TODO: Committed templates shouldn't be deleted.
-- TODO: Check user permissions to delete given template.
deleteTemplate :: Account -> TemplateId -> AppM NoContent
deleteTemplate _account id = do
  logInfo_ "deleting template"
    -- TODO: Can committed template be deleted?
  Model.deleteTemplate id
  pure NoContent

sqlMaybeSet :: (MonadState v m, SqlSet v, Show a, ToSQL a) => SQL -> Maybe a -> m ()
sqlMaybeSet sql = maybe (pure ()) (sqlSet sql)

getTemplate :: Account -> TemplateId -> AppM GetTemplate
getTemplate _account templateId = do
  logInfo_ "getting template"
  fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate templateId

-- TODO: Committed templates can't be updated.
-- TODO: Check user permissions to update given template.
patchTemplate :: Account -> TemplateId -> PatchTemplate -> AppM GetTemplate
patchTemplate Account {..} templateId patch = do
  logInfo_ "patching template"
  fromMaybeM throwTemplateNotFoundError $ Model.updateTemplate templateId patch

-- TODO: Check user permissions to commit given template.
-- TODO: Check if the template is already committed and return 204 in case of conflict.
commitTemplate :: Account -> TemplateId -> AppM NoContent
commitTemplate Account {..} id = do
  logInfo_ "committing template"
  now      <- liftIO currentTime
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate id
  when (isJust $ committed template) throwTemplateAlreadyCommittedError
  templateDSL <- fromMaybeM throwTemplateNotFoundError $ Model.getTemplateDsl id
  machine     <-
    either throwDSLValidationError' pure $ decodeHightTang templateDSL >>= machinize
  Model.commitTemplate now id
  Model.insertParsedStateMachine id machine
  pure NoContent
  where
    -- TODO: Currently, there's no way to get more than a singleton list of validation
    -- TODO: errors. This allows the Error module to be prettier, all this should be
    -- TODO: improved later.
    throwDSLValidationError' = \case
      []      -> throwDSLValidationError "Unknown validation error"
      err : _ -> throwDSLValidationError $ error_message err

machinize :: HighTongue -> Either [ValidationError] Machine
machinize highTongue = packError `left` linear highTongue
  where
    -- TODO: better error messages from transducer and machinize modules.
    packError err =
      [ValidationError { line_number = 0, column = 0, error_message = pack $ show err }]

decodeHightTang :: FlowDSL -> Either [ValidationError] HighTongue
decodeHightTang template = packError `left` decodeEither' (encodeUtf8 template)
  where
    packError err =
      [ ValidationError { line_number   = 0
                        , column        = 0
                        , error_message = pack $ prettyPrintParseException err
                        }
      ]

-- TODO: Do better error messages.
-- TODO: Compilation step?
validateTemplate :: FlowDSL -> AppM [ValidationError]
validateTemplate template = do
  logInfo_ "validating template"
  either pure (const (pure [])) $ decodeHightTang template >>= machinize

listTemplates :: Account -> AppM [GetTemplate]
listTemplates Account {..} = do
  logInfo_ "list templates"
  Model.selectTemplatesByUserID userId

naturalFlow
  :: (LogT (DBT Handler) a -> DBT Handler a) -> FlowConfiguration -> AppM a -> Handler a
naturalFlow runLogger flowConfiguration flowApp =
  runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings
    . runLogger
    $ runReaderT flowApp flowConfiguration

genAuthServerContext :: FlowConfiguration -> Context (AuthHandler Request Account ': '[])
genAuthServerContext flowConfiguration = authHandler flowConfiguration :. EmptyContext

app :: (forall m a . LogT m a -> m a) -> FlowConfiguration -> IO Application
app runLogger flowConfiguration = do
  loggingMiddleware <- runLogger mkApplicationLogger
  return
    . loggingMiddleware
    . serveWithContext apiProxy (genAuthServerContext flowConfiguration)
    $ hoistServerWithContext apiProxy
                             (Proxy :: Proxy '[AuthHandler Request Account])
                             (naturalFlow runLogger flowConfiguration)
                             server

runFlow :: LogRunner -> FlowConfiguration -> IO ()
runFlow LogRunner {..} conf@FlowConfiguration {..} = withLogger $ \runLogger -> do
  runSettings warpSettings =<< app runLogger conf
  where warpSettings = setPort flowPort defaultSettings
