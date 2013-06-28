module Salesforce.AuthorizationWorkflow (
    initAuthorizationWorkflowUrl
  , getRefreshTokenFromCode
  , getAccessTokenFromRefreshToken ) where


import DB
import Control.Monad.IO.Class
import Salesforce.Conf
import Control.Monad.Reader
import Utils.IO
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)
import qualified Log as Log
import Text.JSON.FromJSValue
import System.Exit
import Text.JSON hiding (Ok)
import qualified Text.JSON as J


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
getRefreshTokenFromCode :: (MonadDB m, MonadIO m,MonadReader c m, HasSalesforceConf c) => String -> m (Maybe String)
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
      ExitFailure _ -> (Log.debug $ "Failed to recieve token from salesforce: " ++ show stderr) >> return Nothing
      ExitSuccess ->  case decode $ BSL.toString stdout of
                         J.Ok js -> withJSValue js $ fromJSValueField "refresh_token"
                         _ -> (Log.debug $ "Parsing error with:" ++ show stdout) >> return Nothing

{- Every time we do a salesforce callback, we need to get new access token. We get it using refresh token -}
getAccessTokenFromRefreshToken :: (MonadDB m, MonadIO m,MonadReader c m, HasSalesforceConf c) => String -> m (Maybe String)
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
      ExitFailure _ -> (Log.debug $ "Failed to recieve token from salesforce: " ++ show stderr) >> return Nothing
      ExitSuccess ->  case decode $ BSL.toString stdout of
                         J.Ok js -> withJSValue js $ fromJSValueField "access_token"
                         _ -> (Log.debug $ "Parsing error with:" ++ show stdout) >> return Nothing
