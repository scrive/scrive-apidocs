module Salesforce.AuthorizationWorkflow (
    initAuthorizationWorkflowUrl
  , getRefreshTokenFromCode
  , getAccessTokenFromRefreshToken
  , testSalesforce ) where


import Control.Monad.IO.Class
import Control.Monad.Reader
import System.Exit
import Text.JSON hiding (Ok)
import Text.JSON.FromJSValue
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)
import qualified Text.JSON as J

import DB
import KontraPrelude
import Salesforce.Conf
import Utils.IO
import qualified Log

{- Returns a link. When following this link, user will be asked in salesforce to give use propper permissions-}
initAuthorizationWorkflowUrl :: (MonadDB m, MonadIO m,MonadReader c m, HasSalesforceConf c) => (Maybe String) -> m String
initAuthorizationWorkflowUrl mstate = do
  sc <- getSalesforceConfM
  return $ salesforceAuthenticationUrl sc ++ "?client_id=" ++ salesforceConsumerKey sc ++
                                             "&response_type=code" ++
                                             "&redirect_uri=" ++ salesforceRedirectUrl sc ++
                                             (case mstate of
                                                  Just state -> "&state=" ++ state
                                                  _ -> "")

{- Returns a refresh token, that we will get back when user agrees in salesforce to give us persmission (after following link from initAuthorizationWorkflowUrl) -}
getRefreshTokenFromCode :: (MonadDB m, Log.MonadLog m, MonadIO m, MonadReader c m, HasSalesforceConf c) => String -> m (Either String String)
getRefreshTokenFromCode code = do
  sc <- getSalesforceConfM
  (exitcode, stdout , stderr) <- readCurl [
                "-X", "POST"
              , "-d", "grant_type=authorization_code&" ++
                      "code=" ++ code ++ "&" ++
                      "client_id=" ++ salesforceConsumerKey sc ++ "&" ++
                      "client_secret=" ++ salesforceConsumerSecret sc ++ "&" ++
                      "redirect_uri=" ++ salesforceRedirectUrl sc
              ,  salesforceTokenUrl sc
              ] BSL.empty
  case exitcode of
      ExitFailure err -> do
        Log.mixlog_ $ "Failed to recieve token from salesforce: " ++ show stderr
        return $ Left $ "Connection to Salesforce (refresh) closed with " ++ show err
      ExitSuccess ->  case decode $ BSL.toString stdout of
                         J.Ok js ->  do
                           mrt <- withJSValue js $ fromJSValueField "refresh_token"
                           case mrt of
                             Just rt -> return $ Right rt
                             Nothing -> do
                              Log.mixlog_ $ "Parsing Salesfoce refresh response - no token found"
                              return $ Left "Response from Salesforce was a valid JSON, but no refresh token was found"
                         _ -> do
                           Log.mixlog_ $ "Parsing error with:" ++ show stdout
                           return $ Left "Response from salesforce was not a valid JSON"

{- Every time we do a salesforce callback, we need to get new access token. We get it using refresh token -}
getAccessTokenFromRefreshToken :: (MonadDB m, MonadIO m, Log.MonadLog m, MonadReader c m, HasSalesforceConf c) => String -> m (Either String String)
getAccessTokenFromRefreshToken rtoken = do
  sc <- getSalesforceConfM
  (exitcode, stdout , stderr) <- readCurl [
                "-X", "POST"
              , "-d", "grant_type=refresh_token&" ++
                      "refresh_token=" ++ rtoken ++ "&" ++
                      "client_id=" ++ salesforceConsumerKey sc ++ "&" ++
                      "client_secret=" ++ salesforceConsumerSecret sc
              ,  salesforceTokenUrl sc
              ] BSL.empty
  case exitcode of
      ExitFailure err -> do
        Log.mixlog_ $ "Failed to recieve token from salesforce: " ++ show stderr
        return $ Left $ "Connection to Salesforce closed with " ++ show err
      ExitSuccess ->  case (decode $ BSL.toString stdout) of
                        J.Ok js -> do
                          mrt <- withJSValue js $ fromJSValueField "access_token"
                          case mrt of
                            Just rt -> return $ Right rt
                            Nothing -> do
                              Log.mixlog_ $ "Parsing Salesfoce access response - no token found"
                              return $ Left "Response from Salesforce was a valid JSON, but no access token was found"
                        _ -> do
                          Log.mixlog_ $ "Parsing error with:" ++ show stdout
                          return $ Left "Response from salesforce (access) was not a valid JSON"



{- Used by API call test salesforce. Let you check if salesfoce integration is set and working for a given url -}
testSalesforce :: (MonadDB m, MonadIO m, Log.MonadLog m, MonadReader c m, HasSalesforceConf c) => String -> String -> m (Maybe String)
testSalesforce rtoken url = do
  matoken <- getAccessTokenFromRefreshToken rtoken
  case matoken of
       Left msg -> return $ Just msg
       Right atoken -> do
        (exitcode, _ , _) <- readCurl [
                      "-X", "POST"
                    , "-f" -- make curl return exit code (22) if it got anything else but 2XX
                    , "-L" -- make curl follow redirects
                    , "--post302" -- make curl still post after redirect
                    , "-H", "Authorization: Bearer " ++ atoken
                    , url
                    ] BSL.empty
        case exitcode of
                    ExitSuccess -> return Nothing
                    ExitFailure c -> return $ Just $ "Authorization workflow started, but actuall test failed with code " ++ show c
