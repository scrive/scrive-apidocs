module Flow.TestUtil where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Aeson
import Network.HTTP.Client
  ( ManagerSettings, defaultManagerSettings, managerModifyRequest, method
  , newManager, redirectCount, requestHeaders
  )
import Network.HTTP.Types.Header (hHost, hSetCookie)
import Servant.Client
import Web.Cookie
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T

import Auth.Session
import DB
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import Flow.Client
import Flow.Model.Types
import Flow.Process
import Flow.Routes.Api hiding (method)
import Flow.Server.Cookies
import OAuth.Model
import TestEnvSt.Internal
import TestingUtil hiding (assertRight)
import TestKontra
import User.Types.User
import User.UserID

assertRight :: String -> TestEnv (Either ClientError b) -> TestEnv b
assertRight msg req = do
  res <- req
  case res of
    Right v   -> pure v
    Left  err -> fail $ msg <> ": " <> show err

assertLeft :: String -> TestEnv (Either ClientError b) -> TestEnv ClientError
assertLeft msg req = do
  res <- req
  case res of
    Right _   -> fail msg
    Left  err -> pure err

assertIsJsonError :: ClientError -> TestEnv ()
assertIsJsonError = assert . hasJsonBody
  where
    isJustObject :: Maybe Value -> Bool
    isJustObject = \case
      Just (Object _) -> True
      _               -> False
    hasJsonBody :: ClientError -> Bool
    hasJsonBody = \case
      FailureResponse _ resp -> isJustObject . decode $ responseBody resp
      _ -> False

getToken :: UserID -> TestEnv OAuthAuthorization
getToken uid = do
  void . dbUpdate $ CreatePersonalToken uid
  commit
  fmap fromJust . dbQuery $ GetPersonalToken uid

getEnv :: ManagerSettings -> TestEnv ClientEnv
getEnv mgrSettings = do
  TestEnvSt {..} <- ask
  mgr            <- liftIO $ newManager mgrSettings
  url            <- parseBaseUrl "localhost"
  pure . mkClientEnv mgr $ url { baseUrlPort = flowPort }

request :: ClientM a -> TestEnv (Either ClientError a)
request req = do
  env <- getEnv managerSettings
  liftIO $ runClientM req env

requestWithEnv :: MonadIO m => ClientEnv -> ClientM a -> m (Either ClientError a)
requestWithEnv env req = liftIO $ runClientM req env

managerSettings :: ManagerSettings
managerSettings = defaultManagerSettings
  { managerModifyRequest = setHostHeader >=> setXTokenHeader
  }
  where
    setHostHeader req =
      let headers = requestHeaders req
          host    = T.encodeUtf8 flowTestCookieDomain
      in  case lookup "host" headers of
            Nothing -> pure $ req { requestHeaders = (hHost, host) : headers }
            Just _  -> pure req
    setXTokenHeader req =
      let headers  = requestHeaders req
          mCookies = parseCookies <$> lookup "cookie" headers
          mXToken  = mCookies >>= readCookie cookieNameXToken
      in  case (lookup headerNameXToken headers, mXToken :: Maybe XToken) of
            (Nothing, Just xtoken) | method req /= "GET" ->
              let xTokenHeader = (headerNameXToken, (T.encodeUtf8 . showt) xtoken)
              in  pure $ req { requestHeaders = xTokenHeader : headers }
            _ -> pure req

managerSettingsNoRedirects :: ManagerSettings
managerSettingsNoRedirects = managerSettings
  { managerModifyRequest = managerModifyRequest managerSettings >=> setRedirectCount
  }
  where setRedirectCount req = pure $ req { redirectCount = 0 }

errorResponse :: ClientError -> Maybe Response
errorResponse (FailureResponse        _ resp) = Just resp
errorResponse (DecodeFailure          _ resp) = Just resp
errorResponse (UnsupportedContentType _ resp) = Just resp
errorResponse (InvalidContentTypeHeader resp) = Just resp
errorResponse (ConnectionError          _   ) = Nothing

flowTestCookieDomain :: Text
flowTestCookieDomain = "testdummy.scrive.com"

responseSetCookieHeaders :: Response -> [SetCookie]
responseSetCookieHeaders response =
  map (\(_, val) -> parseSetCookie val)
    . filter (\(key, _) -> key == hSetCookie)
    . Foldable.toList
    $ responseHeaders response

toCookies :: [SetCookie] -> Cookies
toCookies = map (\sc -> (setCookieName sc, setCookieValue sc))

-- Security note: We read the xtoken from cookies in client/test code only.
-- Do not use this function in server code that checks authentication.
readAuthCookies :: Cookies -> Maybe (SessionCookieInfo, XToken)
readAuthCookies cookies = do
  sessionCookie <- readCookie cookieNameSessionID cookies
  xtoken        <- readCookie cookieNameXToken cookies
  pure (sessionCookie, xtoken)

createInstance
  :: ApiClient
  -> Text
  -> Process
  -> TemplateParameters
  -> TestEnv (Either ClientError GetInstance)
createInstance ApiClient {..} name process templateParameters = do
  let createTemplateData = CreateTemplate name process
  template1 <- assertRight "create template" . request $ createTemplate createTemplateData
  let tid = id (template1 :: GetCreateTemplate)

  void . assertRight "validate response" . request $ validateTemplate process
  void . assertRight "commit template response" . request $ commitTemplate tid

  request . startTemplate tid $ CreateInstance Nothing templateParameters Nothing

toTemplateParameters :: InstanceKeyValues -> TemplateParameters
toTemplateParameters (InstanceKeyValues documents users messages) = TemplateParameters
  documents
  users'
  messages
  where users' = Map.map (\id -> UserConfiguration id Nothing Nothing) users

addRandomFlowDocumentWithSignatory :: User -> TestEnv Document
addRandomFlowDocumentWithSignatory author = addRandomDocument (rdaDefault author)
  { rdaTypes       = OneOf [Signable]
  , rdaStatuses    = OneOf [Preparation]
  , rdaTimeoutTime = False
  , rdaDaysToSign  = Just 90
  , rdaSignatories = let signatory = OneOf
                           [ [ RSC_IsSignatoryThatHasntSigned
                             , RSC_DeliveryMethodIs EmailDelivery
                             , RSC_AuthToViewIs StandardAuthenticationToView
                             , RSC_AuthToSignIs StandardAuthenticationToSign
                             , RSC_HasConsentModule False
                             ]
                           ]
                     in  OneOf [[signatory, signatory]]
  }
