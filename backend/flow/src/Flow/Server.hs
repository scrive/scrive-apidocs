{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Flow.Server where

import Control.Arrow (left)
import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Time
import Data.Int
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
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map

import Auth.Model
import Auth.OAuth
import Auth.Parse
import Doc.DocumentID (fromDocumentID, unsafeDocumentID)
import Flow.Api
import Flow.HighTongue
import Flow.Id
import Flow.Machinize
import Flow.Model.Types
import Flow.OrphanInstances ()
import Log.Configuration (LogRunner(LogRunner, withLogger))
import User.UserID (UserID, unUserID, unsafeUserID)
import UserGroup.Internal (UserGroupID, unsafeUserGroupID)
import qualified Flow.Model as Model

data FlowConfiguration = FlowConfiguration
    { dbConnectionPool :: forall m . (MonadBase IO m, MonadMask m)
        => ConnectionSourceM m
    , flowPort :: Int
    }

type AppM = ReaderT FlowConfiguration (LogT (DBT Handler))

server :: ServerT FlowAPI AppM
server = authenticated :<|> validateTemplate
  where
    authenticated account =
      createTemplate account
        :<|> deleteTemplate account
        :<|> getTemplate account
        :<|> patchTemplate account
        :<|> commitTemplate account
        :<|> startInstance account
        :<|> getInstance
--    :<|> getInstanceView

data Account = Account
    { userId :: UserID
    , userGroupId :: UserGroupID
    }
  deriving (Show)


type instance AuthServerData (AuthProtect "oauth") = Account

-- TODO: Handle decodeUtf8 exceptions
authHandler :: FlowConfiguration -> AuthHandler Request Account
authHandler flowConfiguration = mkAuthHandler handler
  where
    maybeToRight e = maybe (Left e) Right
    handler :: Request -> Handler Account
    handler req = do
      oauthTokens <- either (throwError401 "Cannot parse OAuth header") pure
        $ getOAuthAuthorization req
      liftIO $ print oauthTokens
      maybeIds <- runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings
        $ authenticateToken oauthTokens
      (userId, userGroupId) <- maybe (throwError401 "Not a valid token" "") pure maybeIds
      pure $ Account { userId      = unsafeUserID userId
                     , userGroupId = unsafeUserGroupID userGroupId
                     }

    getOAuthAuthorization :: Request -> Either Text OAuthAuthorization
    getOAuthAuthorization req =
      (maybeToRight err . lookup "authorization" $ requestHeaders req)
        >>= (parseParams . splitAuthorization . decodeUtf8)

    -- TODO handle the exception somehow
    -- ... but don't put it into the response, it leaks internal information!
    throwError401 errorName e = do
      liftIO $ print e
      throwError err401 { errBody = errorName }
    err = "AUTHORIZATION header not found."

-- TODO: Check user permissions to create templates.
createTemplate :: Account -> CreateTemplate -> AppM GetCreateTemplate
createTemplate Account {..} CreateTemplate {..} = do
  logInfo_ "creating template"
  id <- Model.insertTemplate $ InsertTemplate name process userId userGroupId
  pure $ GetCreateTemplate { id, name }

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
  fromMaybeM (throwError err404) $ Model.selectTemplate templateId

-- TODO: Committed templates can't be updated.
-- TODO: Check user permissions to update given template.
patchTemplate :: Account -> TemplateId -> PatchTemplate -> AppM GetTemplate
patchTemplate Account {..} templateId patch = do
  logInfo_ "patching template"
  fromMaybeM (throwError err404) $ Model.updateTemplate templateId patch

throwValidationErr409 :: [ValidationError] -> AppM a
throwValidationErr409 errors =
  throwError $ err409 { errBody = BL.fromStrict $ encode errors }

-- TODO: Check user permissions to commit given template.
-- TODO: Check if the template is already committed and return 204 in case of conflict.
commitTemplate :: Account -> TemplateId -> AppM NoContent
commitTemplate Account {..} id = do
  logInfo_ "committing template"
  now      <- liftIO currentTime
  template <- fromMaybeM (throwError err404) $ Model.getTemplateDsl id
  machine  <- either throwValidationErr409 pure $ decodeHightTang template >>= machinize
  Model.commitTemplate now id
  Model.insertParsedStateMachine id machine
  pure NoContent

machinize :: HighTongue -> Either [ValidationError] Machine
machinize highTongue = packError `left` linear highTongue
  where
    packError err =
      [ValidationError { line_number = 0, column = 0
            -- TODO: better error messages from transducer and machinize modules.
                                                    , error_message = pack $ show err }]

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

startInstance :: Account -> TemplateId -> InstanceToTemplateMapping -> AppM GetInstance
startInstance Account {..} templateId tp@InstanceToTemplateMapping {..} = do
  logInfo_ "starting instance"
  -- TODO: Check permissions create instance..
  -- TODO: Check permissions to the template.
  -- TODO: Validate mapping...
  -- TODO: Check template is committed.
  -- TODO: Check mapping value sizes???
  -- TODO: Replace value type with enum???
  -- TODO: Model instance state inside database somehow!
  id <- Model.insertFlowInstance templateId
  insertFlowInstanceKeyValues id "document" documents fromDocumentID
  insertFlowInstanceKeyValues id "user"     users     unUserID
  insertFlowInstanceKeyValues id "message"  messages  identity

  pure $ GetInstance
    { id
    , template           = templateId
    , templateParameters = tp
    , state = InstanceState
          -- TODO: What is purpose of this events field. Isn't it part of
          -- current stage?
                             { events = []
                            , history = []
                            , current = InstanceStage { stage = "test", events = [] }
                            }
    }
  where
    -- TODO: this probably needs to be moved to Model module.
    insertFlowInstanceKeyValues
      :: InstanceId -> Text -> Map.Map Text a -> (a -> Int64) -> AppM ()
    insertFlowInstanceKeyValues instanceId valueType keyValues f =
      mapM_ (Model.insertFlowInstanceKeyValue instanceId valueType . fmap f)
        $ Map.toList keyValues

getInstance :: InstanceId -> AppM GetInstance
getInstance instanceId = do
  logInfo_ "getting instance"
  -- TODO: Authorize user.
  -- TODO: Model instance state inside database somehow!
  templateId <- fromMaybeM (throwError err404) $ Model.selectInstance instanceId
  documents  <-
    Map.fromList . fmap (fmap unsafeDocumentID) <$> Model.selectInstanceKeyValues
      instanceId
      "document"
  users <- Map.fromList . fmap (fmap unsafeUserID) <$> Model.selectInstanceKeyValues
    instanceId
    "user"
  messages <- Map.fromList . fmap (fmap identity) <$> Model.selectInstanceKeyValues
    instanceId
    "message"
  pure $ GetInstance
    { id                 = instanceId
    , template           = templateId
    , templateParameters = InstanceToTemplateMapping { .. }
    , state = InstanceState
          -- TODO: What is purpose of this events field. Isn't it part of
          -- current stage?
                             { events = []
                            , history = []
                            , current = InstanceStage { stage = "test", events = [] }
                            }
    }

getInstanceView :: InstanceId -> AppM GetInstanceView
getInstanceView = undefined

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
