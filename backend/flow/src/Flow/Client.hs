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
import Flow.Process

-- TODO: Having Maybe in the AuthClientData instance makes it unclear as to what
-- the correct set of auth credentials is. However, we need to be able to generate
-- invalid authentication for testing. Maybe it would be better to define a separate
-- type instance just for the AuthenticationTest.hs
type instance AuthClientData (AuthProtect "oauth-or-cookies") = OAuthOrCookies
type OAuthOrCookies = Either OAuthAuthorization (Maybe SessionCookieInfo, Maybe XToken)

data TemplateClient = TemplateClient
  { createTemplate   :: CreateTemplate -> ClientM GetCreateTemplate
  , deleteTemplate   :: TemplateId -> ClientM NoContent
  , getTemplate      :: TemplateId -> ClientM GetTemplate
  , patchTemplate    :: TemplateId -> PatchTemplate -> ClientM GetTemplate
  , listTemplates    :: ClientM [GetTemplate]
  , commitTemplate   :: TemplateId -> ClientM NoContent
  , startTemplate    :: TemplateId -> InstanceToTemplateMapping -> ClientM StartTemplate
  , getInstance      :: InstanceId -> ClientM GetInstance
  , getInstanceView  :: InstanceId -> ClientM GetInstanceView
  , listInstances    :: ClientM [GetInstance]
  , validateTemplate :: Process -> ClientM [ValidationError]
}

-- brittany-disable-next-binding
mkTemplateClient :: OAuthOrCookies -> TemplateClient
mkTemplateClient authData = TemplateClient { .. }
  where
    authReq = mkAuthenticatedRequest authData addAuthentication
    createTemplate
        :<|> deleteTemplate
        :<|> getTemplate
        :<|> patchTemplate
        :<|> listTemplates
        :<|> commitTemplate
        :<|> startTemplate
        :<|> getInstance
        :<|> getInstanceView
        :<|> listInstances
      = authenticatedEndpoints authReq
    authenticatedEndpoints :<|> validateTemplate = client apiProxy

addAuthentication :: OAuthOrCookies -> Client.Request -> Client.Request
addAuthentication authData = case authData of
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
