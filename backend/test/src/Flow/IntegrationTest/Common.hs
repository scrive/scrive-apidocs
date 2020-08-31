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
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Servant.Client as Servant
import qualified Test.HUnit as T

import Auth.Session
import Context.Internal
import Doc.DocumentID
import Flow.OrphanTestInstances ()
import Flow.Server.Cookies
import Flow.TestUtil
import Session.Model
import Session.Types
import TestEnvSt.Internal (flowPort)
import TestKontra

mkEnvForUser :: TestEnv ClientEnv
mkEnvForUser = do
  TestEnvSt {..} <- ask
  mgr            <- liftIO $ newManager defaultManagerSettings
  url            <- parseBaseUrl "localhost"
  cookieJar      <- liftIO . newTVarIO $ createCookieJar []
  pure . setCookieJar cookieJar . mkClientEnv mgr $ url { baseUrlPort = flowPort }

setCookieJar :: TVar CookieJar -> ClientEnv -> ClientEnv
setCookieJar jar env = env { cookieJar = Just jar }

authenticateContext :: ClientEnv -> Context -> TestEnv Context
authenticateContext ClientEnv {..} context = do
  TestEnvSt {..} <- ask
  cookies        <-
    assertJust' "cookie jar not present" cookieJar
    >>= fmap (fmap toTransformCookie . destroyCookieJar)
    .   liftIO
    .   readTVarIO
  AuthCookies {..} <- assertJust' "authentication cookies are not set"
    $ readAuthCookies cookies
  session <-
    getSession (cookieSessionID authCookieSession)
               (cookieSessionToken authCookieSession)
               ("localhost:" <> showt flowPort)
      >>= assertJust' "can't create kontrakcja session"
  muser    <- getUserFromSession session
  mpaduser <- getPadUserFromSession session

  pure $ context { sessionID    = sesID session
                 , xToken       = sesCSRFToken session
                 , maybeUser    = muser
                 , maybePadUser = mpaduser
                 }

assertJust' :: MonadIO m => String -> Maybe a -> m a
assertJust' _   (Just v) = pure v
assertJust' msg Nothing  = liftIO $ T.assertFailure msg

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
