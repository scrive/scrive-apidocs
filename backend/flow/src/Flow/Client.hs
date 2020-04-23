module Flow.Client where

import Servant.API
import Servant.Client
import Servant.Client.Core.Auth
import Servant.Client.Core.Request as Client

import Auth.Model
import Flow.Api

type instance AuthClientData (AuthProtect "oauth") = OAuthAuthorization

type OauthReq = AuthenticatedRequest (AuthProtect "oauth")

data TemplateClient = TemplateClient {
    createTemplate :: CreateTemplate -> ClientM GetCreateTemplate
  , deleteTemplate :: Id 'TemplateId -> ClientM NoContent
  , getTemplate    :: Id 'TemplateId -> ClientM GetTemplate
  , patchTemplate  :: Id 'TemplateId -> PatchTemplate -> ClientM GetTemplate
}

mkTemplateClient :: OAuthAuthorization -> TemplateClient
mkTemplateClient oauth = TemplateClient{..}
  where
    oauthReq = mkAuthenticatedRequest oauth addAuthorization
    createTemplate
        :<|> deleteTemplate
        :<|> getTemplate
        :<|> patchTemplate
      = client apiProxy oauthReq

addAuthorization :: OAuthAuthorization -> Client.Request -> Client.Request
addAuthorization OAuthAuthorization{..} req = Client.addHeader "authorization" authStr req
  where
    authStr =
        "oauth_signature_method=\"PLAINTEXT\""
          ++ ",oauth_consumer_key=\""
          ++ show oaAPIToken
          ++ "\""
          ++ ",oauth_token=\""
          ++ show oaAccessToken
          ++ "\""
          ++ ",oauth_signature=\""
          ++ show oaAPISecret
          ++ "&"
          ++ show oaAccessSecret
          ++ "\""
