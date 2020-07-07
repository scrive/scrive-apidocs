{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Flow.Server where

import Control.Monad.Extra (fromMaybeM)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Time
import Data.ByteString (ByteString)
import Data.Text as T
import Data.Text.Encoding
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
import Flow.Process
import Flow.Server.Handlers.Instance
import Flow.Server.Types
import Folder.Model (FolderGet(..))
import Folder.Types (unsafeFolderID)
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
        :<|> listInstances account

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
              -- TODO use MonadLog
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
              -- TODO use MonadLog
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
      -- TODO use MonadLog
      liftIO $ print e
      throwAuthenticationError errorName

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
  either throwDSLValidationError' (const $ pure ())
    $   decodeHighTongue templateDSL
    >>= machinize
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
  either pure (const (pure [])) $ decodeHighTongue template >>= machinize

listTemplates :: Account -> AppM [GetTemplate]
listTemplates account@Account {..} = do
  logInfo_ "Listing templates"
  templates <- Model.selectTemplatesByUserID $ user ^. #id
  let fids = view #folderId <$> templates
  guardUserHasPermission account [ canDo ReadA $ FlowTemplateR fid | fid <- fids ]
  pure $ fmap toGetTemplate templates

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
