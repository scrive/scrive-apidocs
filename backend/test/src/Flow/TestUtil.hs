{-# LANGUAGE DuplicateRecordFields #-}

module Flow.TestUtil where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Network.HTTP.Client
import Servant.Client

import DB
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

getEnv :: TestEnv ClientEnv
getEnv = do
  TestEnvSt {..} <- ask
  mgr            <- liftIO $ newManager defaultManagerSettings
  url            <- parseBaseUrl "localhost"
  pure . mkClientEnv mgr $ url { baseUrlPort = flowPort }

request :: ClientM a -> TestEnv (Either ClientError a)
request req = do
  env <- getEnv
  liftIO $ runClientM req env
