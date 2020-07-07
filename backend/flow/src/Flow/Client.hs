{-# LANGUAGE StrictData #-}
module Flow.Client where

import Data.Text as T
import Servant.API
import Servant.Client
import Servant.Client.Core.Auth
import Servant.Client.Core.Request as Client

import Auth.OAuth
import Auth.Session
import Flow.Api
import Flow.HighTongue
import Flow.Id
import Flow.Model.Types
import Flow.Process

-- TODO: Having Maybe in the AuthClientData instance makes it unclear as to what
-- the correct set of auth credentials is. However, we need to be able to generate
-- invalid authentication for testing. Maybe it would be better to define a separate
-- type instance just for the AuthenticationTest.hs
type OAuthOrCookies = Either OAuthAuthorization (Maybe SessionCookieInfo, Maybe XToken)
type instance AuthClientData (AuthProtect "account") = OAuthOrCookies

-- brittany-disable-next-binding
-- See: https://github.com/lspitzner/brittany/issues/89
type instance AuthClientData (AuthProtect "instance-user") =
  (Maybe SessionCookieInfo, Maybe XToken)

data ApiClient = ApiClient
  { createTemplate   :: CreateTemplate -> ClientM GetCreateTemplate
  , deleteTemplate   :: TemplateId -> ClientM NoContent
  , getTemplate      :: TemplateId -> ClientM GetTemplate
  , patchTemplate    :: TemplateId -> PatchTemplate -> ClientM GetTemplate
  , listTemplates    :: ClientM [GetTemplate]
  , commitTemplate   :: TemplateId -> ClientM NoContent
  , startTemplate    :: TemplateId -> InstanceKeyValues -> ClientM StartTemplate
  , getInstance      :: InstanceId -> ClientM GetInstance
  , getInstanceView  :: InstanceId -> ClientM GetInstanceView
  , listInstances    :: ClientM [GetInstance]
  , validateTemplate :: Process -> ClientM [ValidationError]
}

-- brittany-disable-next-binding
mkApiClient
  :: OAuthOrCookies
  -> (Maybe SessionCookieInfo, Maybe XToken)
  -> ApiClient
mkApiClient authDataAccount authDataInstanceUser = ApiClient { .. }
  where
    accountEndpoints :<|> instanceUserEndpoints :<|> noAuthEndpoints = client apiProxy
    createTemplate
        :<|> deleteTemplate
        :<|> getTemplate
        :<|> patchTemplate
        :<|> listTemplates
        :<|> commitTemplate
        :<|> startTemplate
        :<|> getInstance
        :<|> listInstances
      = accountEndpoints (mkAuthenticatedRequest authDataAccount addOAuthOrCookies)
    getInstanceView
      = instanceUserEndpoints (mkAuthenticatedRequest authDataInstanceUser addAuthCookies)
    validateTemplate = noAuthEndpoints

addOAuthOrCookies :: OAuthOrCookies -> Client.Request -> Client.Request
addOAuthOrCookies authData = case authData of
  Left  oauth       -> addOAuthAuthorization oauth
  Right authCookies -> addAuthCookies authCookies
  where
    addOAuthAuthorization :: OAuthAuthorization -> Client.Request -> Client.Request
    addOAuthAuthorization OAuthAuthorization {..} = Client.addHeader "authorization"
                                                                     authStr
      where
        authStr =
          "oauth_signature_method=\"PLAINTEXT\""
            <> ",oauth_consumer_key=\""
            <> show oaAPIToken
            <> "\""
            <> ",oauth_token=\""
            <> show oaAccessToken
            <> "\""
            <> ",oauth_signature=\""
            <> show oaAPISecret
            <> "&"
            <> show oaAccessSecret
            <> "\""

addAuthCookies
  :: (Maybe SessionCookieInfo, Maybe XToken) -> Client.Request -> Client.Request
addAuthCookies (mSessionCookieInfo, mXToken) = Client.addHeader "cookie" cookiesText
  where
    cookiesText = T.intercalate "; " $ catMaybes
      [ renderCookie cookieNameSessionID <$> mSessionCookieInfo
      , renderCookie cookieNameXToken <$> mXToken
      ]
    -- Kontrakcja quotes its auth cookie values and we want to be compatible
    renderCookie name val = name <> "=\"" <> showt val <> "\""

