{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Flow.Server where

import Control.Arrow (left)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Time
import Data.ByteString (ByteString)
import Data.Text as T
import Data.Text.Encoding
import Data.Yaml
import Database.PostgreSQL.PQTypes hiding (JSON(..))
import Log.Class
import Log.Monad (LogT)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Log (mkApplicationLogger)
import Network.Wai.Middleware.Servant.Errors (errorMw)
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie (parseCookies)

import AccessControl.Check
import AccessControl.Model (GetRolesIncludingInherited(..))
import AccessControl.Types
import Auth.Model
import Auth.OAuth
import Auth.Session
import DB hiding (JSON(..))
import Flow.Api as Api
import Flow.Error
import Flow.Guards
import Flow.HighTongue
import Flow.Id
import Flow.Machinize as Machinize
import Flow.Model.Types
import Flow.OrphanInstances ()
import Flow.Server.Handlers.Instance
import Flow.Server.Types
import Folder.Model (FolderGet(..))
import Folder.Types (FolderID, unsafeFolderID)
import Log.Configuration (LogRunner(LogRunner, withLogger))
import User.Model
import UserGroup.Internal (unsafeUserGroupID)
import UserGroup.Model (UserGroupGet(..))
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
        :<|> getInstance account
        :<|> getInstanceView account

-- TODO: Handle decodeUtf8 exceptions
authHandler :: FlowConfiguration -> AuthHandler Request Account
authHandler flowConfiguration = mkAuthHandler handler
  where
    handler :: Request -> Handler Account
    handler req =
      runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings $ do
        (userId, userGroupId, folderId) <-
          case lookup "authorization" $ requestHeaders req of
            -- OAuth
            Just oauthHeader -> do
              oauthTokens <- either (throwAuthError OAuthHeaderParseFailureError) pure
                $ parseOAuthAuthorization oauthHeader
              liftIO . putStrLn $ ("Authenticating using OAuth: " <> show oauthTokens)
              maybeIds <- authenticateToken oauthTokens
              maybe (throwAuthError InvalidTokenError (show InvalidTokenError))
                    pure
                    maybeIds
            -- Session cookies
            Nothing -> do
              authCookies <- maybe
                (throwAuthError AuthCookiesParseError (show AuthCookiesParseError))
                pure
                (getAuthCookies req)
              liftIO . putStrLn $ ("Authenticating using cookies: " <> show authCookies)
              maybeIds <- authenticateSession authCookies
              maybe
                (throwAuthError InvalidAuthCookiesError (show InvalidAuthCookiesError))
                pure
                maybeIds

        -- TODO: fromJust is verboten - Handle the Nothing case with an error
        user   <- fmap fromJust . dbQuery . GetUserByID $ unsafeUserID userId
        ug     <- fmap fromJust . dbQuery . UserGroupGet $ unsafeUserGroupID userGroupId
        folder <- fmap fromJust . dbQuery . FolderGet $ unsafeFolderID folderId
        roles  <- dbQuery $ GetRolesIncludingInherited user ug

        pure $ Account { user = user, userGroup = ug, folder = folder, roles = roles }

    getAuthCookies :: Request -> Maybe (SessionCookieInfo, XToken)
    getAuthCookies req = do
      cookies       <- parseCookies <$> lookup "cookie" (requestHeaders req)
      sessionCookie <- readCookie cookieNameSessionID cookies
      xtoken        <- readCookie cookieNameXToken cookies
      pure (sessionCookie, xtoken)
      where
        readCookie name cookies = do -- Maybe
          cookie <-
            T.dropWhile (== '"')
            .   T.dropWhileEnd (== '"')
            .   decodeLatin1
            <$> lookup (encodeUtf8 name) cookies
          maybeRead cookie

    parseOAuthAuthorization :: ByteString -> Either Text OAuthAuthorization
    parseOAuthAuthorization = parseParams . splitAuthorization . decodeUtf8

    -- TODO handle the exception somehow
    -- ... but don't put it into the response, it leaks internal information!
    throwAuthError errorName e = do
      liftIO $ print e
      throwAuthenticationError errorName

-- TODO: Check user permissions to create templates.
createTemplate :: Account -> CreateTemplate -> AppM GetCreateTemplate
createTemplate account@Account {..} CreateTemplate {..} = do
  logInfo_ "creating template"
  guardUserHasPermission account [canDo CreateA $ FlowTemplateR (folder ^. #id)]
  id <- Model.insertTemplate $ InsertTemplate name process (user ^. #id) (folder ^. #id)
  pure $ GetCreateTemplate { id }

-- TODO: Committed templates shouldn't be deleted.
deleteTemplate :: Account -> TemplateId -> AppM NoContent
deleteTemplate account id = do
  logInfo_ "deleting template"
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate id
  let fid = folderId (template :: GetTemplate)
  guardUserHasPermission account [canDo DeleteA $ FlowTemplateR fid]
  Model.deleteTemplate id
  pure NoContent

getTemplate :: Account -> TemplateId -> AppM GetTemplate
getTemplate account templateId = do
  logInfo_ "getting template"
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate templateId
  let fid = folderId (template :: GetTemplate)
  guardUserHasPermission account [canDo ReadA $ FlowTemplateR fid]
  pure template

patchTemplate :: Account -> TemplateId -> PatchTemplate -> AppM GetTemplate
patchTemplate account templateId patch = do
  logInfo_ "patching template"
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate templateId
  when (isJust $ committed template) throwTemplateAlreadyCommittedError
  let fid = folderId (template :: GetTemplate)
  guardUserHasPermission account [canDo UpdateA $ FlowTemplateR fid]
  fromMaybeM throwTemplateNotFoundError $ Model.updateTemplate templateId patch

commitTemplate :: Account -> TemplateId -> AppM NoContent
commitTemplate account id = do
  logInfo_ "committing template"
  now      <- liftIO currentTime
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate id
  when (isJust $ committed template) throwTemplateAlreadyCommittedError
  templateDSL <- fromMaybeM throwTemplateNotFoundError $ Model.getTemplateDsl id
  machine     <-
    either throwDSLValidationError' pure $ decodeHightTang templateDSL >>= machinize
  let fid = folderId (template :: GetTemplate)
  guardUserHasPermission account [canDo UpdateA $ FlowTemplateR fid]
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
listTemplates account@Account {..} = do
  logInfo_ "list templates"
  templates <- Model.selectTemplatesByUserID $ user ^. #id
  let fids = (folderId :: GetTemplate -> FolderID) <$> templates
  guardUserHasPermission account [ canDo ReadA $ FlowTemplateR fid | fid <- fids ]
  pure templates

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
    . errorMw @JSON @'["message", "code"]
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
