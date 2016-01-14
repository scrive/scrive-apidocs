module Salesforce.AuthorizationWorkflow (
    initAuthorizationWorkflowUrl
  , getRefreshTokenFromCode
  , getAccessTokenFromRefreshToken
  , testSalesforce ) where


import Control.Monad.IO.Class
import Control.Monad.Reader
import Log
import System.Exit
import Text.JSON hiding (Ok)
import Text.JSON.FromJSValue
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)
import qualified Text.JSON as J

import DB
import KontraPrelude
import Log.Utils
import Salesforce.Conf
import Utils.IO

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
getRefreshTokenFromCode :: (MonadDB m, MonadLog m, MonadIO m, MonadReader c m, HasSalesforceConf c) => String -> m (Either String String)
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
        logInfo "Failed to receive token from Salesforce" $ object [
            "stderr" `equalsExternalBSL` stderr
          ]
        return $ Left $ "Connection to Salesforce (refresh) closed with " ++ show err
      ExitSuccess ->  case decode $ BSL.toString stdout of
                         J.Ok js ->  do
                           mrt <- withJSValue js $ fromJSValueField "refresh_token"
                           case mrt of
                             Just rt -> return $ Right rt
                             Nothing -> do
                              logInfo_ "Parsing Salesfoce refresh response - no token found"
                              return $ Left "Response from Salesforce was a valid JSON, but no refresh token was found"
                         _ -> do
                           logInfo "Parsing standard output failed" $ object [
                               "stdout" `equalsExternalBSL` stdout
                             ]
                           return $ Left "Response from salesforce was not a valid JSON"

{- Every time we do a salesforce callback, we need to get new access token. We get it using refresh token -}
getAccessTokenFromRefreshToken :: (MonadDB m, MonadIO m, MonadLog m, MonadReader c m, HasSalesforceConf c)
                                => String -> m (Either (String, Int, String, String) String)
getAccessTokenFromRefreshToken rtoken = do
  sc <- getSalesforceConfM
  (exitcode, stdout_with_code, stderr) <- readCurl [
      "-X", "POST"
    , "--write-out","\n%{http_code}"
    , "-d", "grant_type=refresh_token&" ++
            "refresh_token=" ++ rtoken ++ "&" ++
            "client_id=" ++ salesforceConsumerKey sc ++ "&" ++
            "client_secret=" ++ salesforceConsumerSecret sc
    , salesforceTokenUrl sc
    ] BSL.empty
  let http_code = case reverse . lines . BSL.unpack $ stdout_with_code of
        [] -> ""
        c:_ -> c
      stdout = BSL.pack . unlines . init . lines . BSL.unpack $ stdout_with_code
  case exitcode of
      ExitFailure err -> do
        logInfo "curl failed to receive token from Salesforce" $ object [
            "curl_exit_code" .= err
          , "stderr" `equalsExternalBSL` stderr
          ]
        return $ Left ("curl connection to Salesforce closed", err, BSL.unpack stderr, http_code)
      ExitSuccess -> case (decode $ BSL.toString stdout) of
        J.Ok js -> do
          mrt <- withJSValue js $ fromJSValueField "access_token"
          case mrt of
            Just rt -> return $ Right rt
            Nothing -> do
              logInfo "No token found while parsing Salesforce access response" $ object [
                  "http_code" .= http_code
                , "stdout" `equalsExternalBSL` stdout
                ]
              return $ Left ("Salesforce access response is valid JSON, but no access token found", 0, BSL.unpack stderr, http_code)
        _ -> do
          logInfo "Parsing JSON from Salesforce access response stdout failed" $ object [
              "http_code" .= http_code
            , "stdout" `equalsExternalBSL` stdout
            ]
          return $ Left ("Salesforce access response is not valid JSON", 0, BSL.unpack stderr, http_code)



{- Used by API call test salesforce. Let you check if salesfoce integration is set and working for a given url -}
testSalesforce :: (MonadDB m, MonadIO m, MonadLog m, MonadReader c m, HasSalesforceConf c)
               => String -> String -> m (Either (String, Int, String, String) (String, String))
testSalesforce rtoken url = do
  matoken <- getAccessTokenFromRefreshToken rtoken
  case matoken of
    Left (msg, curl_err, stderr, http_code) ->
      return $ Left ("Getting access token failed: " ++ msg, curl_err, stderr, http_code)
    Right atoken -> do
      (exitcode, stdout, stderr) <- readCurl [
          "-X", "POST"
        , "--write-out","\n%{http_code}"
        , "-f" -- make curl return exit code (22) if it got anything else but 2XX
        , "-L" -- make curl follow redirects
        , "--post302" -- make curl still post after redirect
        , "-H", "Authorization: Bearer " ++ atoken
        , url
        ] BSL.empty
      let http_code = case reverse . lines . BSL.unpack $ stdout of
            [] -> ""
            c:_ -> c
      case exitcode of
        ExitSuccess ->
          return $ Right (http_code, BSL.unpack stdout)
        ExitFailure err ->
          return $ Left ("Salesforce access token worked, but callback failed", err, BSL.unpack stderr, http_code)
