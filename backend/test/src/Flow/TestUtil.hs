{-# LANGUAGE DuplicateRecordFields #-}

module Flow.TestUtil where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Network.HTTP.Client
  ( ManagerSettings, defaultManagerSettings, managerModifyRequest, newManager
  , redirectCount
  )
import Network.HTTP.Types.Header (hSetCookie)
import Servant.Client
import Web.Cookie
import qualified Data.Foldable as Foldable

import DB
import Flow.Client
import Flow.Model.Types
import Flow.Process
import Flow.Routes.Api
import OAuth.Model
import TestEnvSt.Internal
import TestKontra
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
  env <- getEnv defaultManagerSettings
  liftIO $ runClientM req env

requestWithEnv :: ClientEnv -> ClientM a -> TestEnv (Either ClientError a)
requestWithEnv env req = liftIO $ runClientM req env

managerSettingsNoRedirects :: ManagerSettings
managerSettingsNoRedirects = defaultManagerSettings
  { managerModifyRequest = \req -> pure $ req { redirectCount = 0 }
  }

errorResponse :: ClientError -> Maybe Response
errorResponse (FailureResponse        _ resp) = Just resp
errorResponse (DecodeFailure          _ resp) = Just resp
errorResponse (UnsupportedContentType _ resp) = Just resp
errorResponse (InvalidContentTypeHeader resp) = Just resp
errorResponse (ConnectionError          _   ) = Nothing

responseSetCookieHeaders :: Response -> [SetCookie]
responseSetCookieHeaders response =
  map (\(_, val) -> parseSetCookie val)
    . filter (\(key, _) -> key == hSetCookie)
    . Foldable.toList
    $ responseHeaders response

toCookies :: [SetCookie] -> Cookies
toCookies = map (\sc -> (setCookieName sc, setCookieValue sc))

createInstance
  :: ApiClient
  -> Text
  -> Process
  -> InstanceKeyValues
  -> TestEnv (Either ClientError GetInstance)
createInstance ApiClient {..} name process mapping = do
  let createTemplateData = CreateTemplate name process
  template1 <- assertRight "create template" . request $ createTemplate createTemplateData
  let tid = id (template1 :: GetCreateTemplate)

  void . assertRight "validate response" . request $ validateTemplate process
  void . assertRight "commit template response" . request $ commitTemplate tid

  request $ startTemplate tid mapping

getDefaultFlowBaseUrl :: TestEnv Text
getDefaultFlowBaseUrl = do
  testEnv <- ask
  pure $ "http://localhost:" <> showt (flowPort testEnv)
