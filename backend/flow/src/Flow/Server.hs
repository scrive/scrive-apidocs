{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Flow.Server where

import Control.Monad.Base
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Time
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Int
import Data.Text.Encoding
import Data.Time.Clock
import Data.UUID
import qualified Data.Map as Map
import Network.Wai
import Optics
import Servant
import Servant.Server.Experimental.Auth
import Network.Wai.Handler.Warp

import Auth.Model
import Auth.OAuth
import Auth.Parse
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.SQL.Builder
import Flow.Api

data FlowConfiguration = FlowConfiguration
    { dbConnectionPool :: forall m . (MonadBase IO m, MonadMask m)
        => ConnectionSourceM m
    }

type AppM = ReaderT FlowConfiguration (DBT Handler)

data Template = Template
    { _templateName :: Text
    , _templateProcess :: Text
    , _templateUser :: UserId
    , _templateUserGroup :: UserGroupId
    , _templateCommitted :: Maybe UTCTime
    , _templateDeleted :: Maybe UTCTime
    }
  deriving (Show, Eq)

makeLenses ''Template

data Storage = Storage
    { _sTemplates :: Map.Map UUID Template
    }
  deriving (Show, Eq)

makeLenses ''Storage

server :: ServerT FlowAPI AppM
server = authServer
  where
    authServer account = createTemplate account
        :<|> deleteTemplate account
        :<|> getTemplate account
        :<|> patchTemplate account
--    :<|> commitTemplate
--    :<|> validateTemplate
--    :<|> startInstance
--    :<|> getInstance
--    :<|> getInstanceView
--    :<|> pushInstanceEvent

data Account = Account
    { userId :: UserId
    , userGroupId :: UserGroupId
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
        oauthTokens <- either (throwError401 "Cannot parse OAuth header") pure $ getOAuthAuthorization req
        liftIO $ print oauthTokens
        maybeIds <- runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings $
            authenticateToken oauthTokens
        (userId, userGroupId) <- maybe (throwError401 "Not a valid token" "") pure maybeIds
        liftIO $ print (userId, userGroupId)
        pure $ Account {..}

    getOAuthAuthorization :: Request -> Either Text OAuthAuthorization
    getOAuthAuthorization req = (maybeToRight err . lookup "authorization" $ requestHeaders req) >>=
        (parseParams . splitAuthorization . decodeUtf8)

    throwError401 errorName e = throwError err401 { errBody = errorName <> ": " <> (BL.fromStrict $ encodeUtf8 e) }
    err = "AUTHORIZATION header not found."

createTemplate :: Account -> CreateTemplate -> AppM GetCreateTemplate
createTemplate Account{..} CreateTemplate{..} = do
    runQuery_ . sqlInsert "flow_templates" $ do
        sqlSet "name" name -- TODO: validate size?
        sqlSet "process" process -- TODO: validate size?
        sqlSet "user_id" userId -- TODO: make use of authenticated user
        sqlSet "user_group_id" userGroupId -- TODO: make use of authenticated user
        sqlResult "id"
    id <- fetchOne runIdentity
    pure $ GetCreateTemplate
        { id
        , name
        , user = userId
        , userGroup = userGroupId
        }

-- TODO: Authorization
deleteTemplate :: Account -> TemplateId -> AppM NoContent
deleteTemplate _account id = do
    now <- liftIO currentTime
    runQuery_ . sqlUpdate "flow_templates" $ do
      sqlSet "deleted" now
      sqlWhereEq "id" id
    pure NoContent

fetchGetTemplate
    :: ( TemplateId
       , Text
       , Text
       , Int64
       , Int64
       , Maybe UTCTime
       , Maybe UTCTime
       )
    -> GetTemplate
fetchGetTemplate (id, name, process, userId, userGroupId, committed, deleted) =
    GetTemplate id name userId userGroupId process committed deleted


maybeSet :: (MonadState v m, SqlSet v, Show a, ToSQL a) => SQL -> Maybe a -> m ()
maybeSet sql maybeVal = maybe (pure ()) (sqlSet sql) maybeVal

getTemplate :: Account -> TemplateId -> AppM GetTemplate
getTemplate _account id = do
    runQuery_ . sqlSelect "flow_templates" $ do
      sqlResult "id"
      sqlResult "name"
      sqlResult "process"
      sqlResult "user_id"
      sqlResult "user_group_id"
      sqlResult "committed"
      sqlResult "deleted"
      sqlWhereEq "id" id
      sqlWhereIsNULL "deleted"
    fetchOne fetchGetTemplate

patchTemplate :: Account -> TemplateId -> PatchTemplate -> AppM GetTemplate
patchTemplate Account{..} id PatchTemplate{..} = do
    runQuery_ . sqlUpdate "flow_templates" $ do
      maybeSet "name" name -- TODO: validate size?
      maybeSet "process" process -- TODO: validate size?
      maybeSet "user_id" user -- TODO: make use of authenticated user
      maybeSet "user_group_id" userGroup -- TODO: make use of authenticated user
      sqlResult "id"
      sqlResult "name"
      sqlResult "process"
      sqlResult "user_id"
      sqlResult "user_group_id"
      sqlResult "committed"
      sqlResult "deleted"
      sqlWhereEq "id" id
      sqlWhereIsNULL "deleted"
    fetchOne fetchGetTemplate

commitTemplate :: (IORef Storage) -> TemplateId -> AppM NoContent
commitTemplate _storageIORef _id = undefined
--    id' <- maybe (throwError $ err400 { errBody = "Wrong id" }) pure
--        $ fromText $ idToText id
--    storage <- liftIO $ readIORef storageIORef
--    maybe (throwError err404) (void . pure) $ view (sTemplates % at id') storage
--    liftIO . atomicModifyIORef' storageIORef $ \a -> (a & sTemplates % at id' % _Just % templateCommited .~ True, ())
--    pure NoContent

validateTemplate :: FlowDSL -> AppM [ValidationError]
validateTemplate _template = undefined
--    case decodeEither' @HighTongue $ encodeUtf8 template of
--        Right _ -> pure []
--        Left err -> pure $ [ValidationError
--            { line_number = 0
--            , column = 0
--            , error_message = pack $ prettyPrintParseException err
--            }]


startInstance :: TemplateId -> InstanceToTemplateMapping -> AppM GetInstance
startInstance = undefined

getInstance :: InstanceId -> AppM GetInstance
getInstance = undefined

getInstanceView :: InstanceId -> AppM GetInstanceView
getInstanceView = undefined

pushInstanceEvent :: InstanceId -> AppM InstanceAction
pushInstanceEvent = undefined

naturalFlow
    :: FlowConfiguration
    -> AppM a
    -> Handler a
naturalFlow flowConfiguration flowApp =
    runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings $
    runReaderT flowApp flowConfiguration

genAuthServerContext :: FlowConfiguration -> Context (AuthHandler Request Account ': '[])
genAuthServerContext flowConfiguration = authHandler flowConfiguration :. EmptyContext

app :: FlowConfiguration -> Application
app flowConfiguration =
    serveWithContext apiProxy (genAuthServerContext flowConfiguration) $ hoistServerWithContext apiProxy (Proxy :: Proxy '[AuthHandler Request Account]) (naturalFlow flowConfiguration) server

runFlow :: FlowConfiguration -> IO ()
runFlow = run 9090 . app
