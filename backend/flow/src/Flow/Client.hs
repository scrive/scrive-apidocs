{-# LANGUAGE DataKinds #-}

module Flow.Client where

import Servant.API
import Servant.Client
import Servant.Client.Core.Auth
import Servant.Client.Core.Request as Client

import Auth.Model
import Flow.Api
import Flow.Id

type instance AuthClientData (AuthProtect "oauth") = OAuthAuthorization

type OauthReq = AuthenticatedRequest (AuthProtect "oauth")

data TemplateClient = TemplateClient {
    createTemplate   :: CreateTemplate -> ClientM GetCreateTemplate
  , deleteTemplate   :: TemplateId -> ClientM NoContent
  , getTemplate      :: TemplateId -> ClientM GetTemplate
  , patchTemplate    :: TemplateId -> PatchTemplate -> ClientM GetTemplate
  , commitTemplate   :: TemplateId -> ClientM NoContent
  , startTemplate    :: TemplateId -> InstanceToTemplateMapping -> ClientM StartTemplate
  , getInstance      :: InstanceId -> ClientM GetInstance
  , getInstanceView  :: InstanceId -> ClientM GetInstanceView
  , validateTemplate :: FlowDSL -> ClientM [ValidationError]
}

-- brittany-disable-next-binding
mkTemplateClient :: OAuthAuthorization -> TemplateClient
mkTemplateClient oauth = TemplateClient { .. }
  where
    oauthReq = mkAuthenticatedRequest oauth addAuthorization
    createTemplate
        :<|> deleteTemplate
        :<|> getTemplate
        :<|> patchTemplate
        :<|> commitTemplate
        :<|> startTemplate
        :<|> getInstance
        :<|> getInstanceView
      = authenticatedEndpoints oauthReq
    authenticatedEndpoints :<|> validateTemplate = client apiProxy

addAuthorization :: OAuthAuthorization -> Client.Request -> Client.Request
addAuthorization OAuthAuthorization {..} = Client.addHeader "authorization" authStr
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
