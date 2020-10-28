module Flow.IntegrationTest.Common where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Binary.Builder
import Data.Proxy
import Database.PostgreSQL.PQTypes hiding (JSON)
import Database.PostgreSQL.PQTypes.SQL.Builder
import GHC.Conc
import Network.HTTP.Client hiding (Proxy, Request, responseBody)
import Network.HTTP.Types (http11, methodGet)
import Servant.API.ContentTypes
import Servant.Client
import Servant.Client.Core.Request
import Servant.Client.Internal.HttpClient
import Servant.HTML.Blaze
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Servant.Client as Servant

import Auth.Session
import Context.Internal
import Doc.DocumentID
import Flow.Core.Type.Url
import Flow.Names
import Flow.OrphanTestInstances ()
import Flow.Routes.Api
import Flow.TestUtil
import Session.Model
import Session.Types
import TestEnvSt.Internal (flowPort)
import TestingUtil (assertJust)
import TestKontra

mkEnvForUser :: TestEnv ClientEnv
mkEnvForUser = do
  TestEnvSt {..} <- ask
  mgr            <- liftIO $ newManager managerSettings
  url            <- parseBaseUrl "localhost"
  cookieJar      <- liftIO . newTVarIO $ createCookieJar []
  pure . setCookieJar cookieJar . mkClientEnv mgr $ url { baseUrlPort = flowPort }

setCookieJar :: TVar CookieJar -> ClientEnv -> ClientEnv
setCookieJar jar env = env { cookieJar = Just jar }

authenticateContext :: ClientEnv -> Context -> TestEnv Context
authenticateContext ClientEnv {..} context = do
  TestEnvSt {..} <- ask
  cookies        <-
    assertJust "cookie jar not present" cookieJar
    >>= fmap (fmap toTransformCookie . destroyCookieJar)
    .   liftIO
    .   readTVarIO
  (SessionCookieInfo {..}, _) <- assertJust "authentication cookies are not set"
    $ readAuthCookies cookies
  session <- getSession cookieSessionID cookieSessionToken flowTestCookieDomain
    >>= assertJust "can't create kontrakcja session"
  muser    <- getUserFromSession session
  mpaduser <- getPadUserFromSession session

  pure $ context { sessionID    = sesID session
                 , xToken       = sesCSRFToken session
                 , maybeUser    = muser
                 , maybePadUser = mpaduser
                 }

toTransformCookie :: Cookie -> (BSC.ByteString, BSC.ByteString)
toTransformCookie Cookie {..} = (cookie_name, cookie_value)

callFlowMagicHashLink
  :: MonadIO m => ClientEnv -> Text -> m (Either ClientError Servant.Response)
callFlowMagicHashLink env link =
  requestWithEnv env (mkMagicRequest link >>= performRequest)

mkMagicRequest :: MonadThrow m => Text -> m Request
mkMagicRequest link = do
  BaseUrl {..} <- parseBaseUrl $ Text.unpack link
  pure $ Request
    { requestPath        = fromByteString $ BSC.pack baseUrlPath
    , requestQueryString = mempty
    , requestBody        = Nothing
    , requestAccept = Seq.fromList [contentType $ Proxy @JSON, contentType $ Proxy @HTML]
    , requestHeaders     = mempty
    , requestHttpVersion = http11
    , requestMethod      = methodGet
    }

getDocumentsToBeSealed :: MonadDB m => [DocumentID] -> m [DocumentID]
getDocumentsToBeSealed docIds = do
  runQuery_ . sqlSelect "document_sealing_jobs d" $ do
    sqlResult "d.id"
    sqlWhereIn "d.id" docIds
  fetchMany runIdentity

createClientEnvFromFlowInstance :: UserName -> GetInstance -> TestEnv ClientEnv
createClientEnvFromFlowInstance username flowInstance = do
  userEnv  <- mkEnvForUser

  flowLink <- assertJust "user's access link should be present"
    $ Map.lookup username (flowInstance ^. #accessLinks)

  void
    . assertRight "authenticate with user flow link"
    . callFlowMagicHashLink userEnv
    $ fromUrl flowLink

  return userEnv
