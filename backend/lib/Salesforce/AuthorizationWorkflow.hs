module Salesforce.AuthorizationWorkflow (
    initAuthorizationWorkflowUrl
  , getRefreshTokenFromCode
  , getAccessTokenFromRefreshToken
  , testSalesforce ) where

import Control.Monad.Base
import Control.Monad.Reader
import Data.Int
import Log
import System.Exit
import Text.JSON hiding (Ok)
import Text.JSON.FromJSValue
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)
import qualified Data.Text as T
import qualified Text.JSON as J

import DB
import Log.Utils
import Salesforce.Conf
import Utils.IO

{- Returns a link. When following this link, user will be asked in salesforce to give use propper permissions-}
initAuthorizationWorkflowUrl
  :: (MonadBase IO m, MonadDB m, MonadIO m, MonadLog m, MonadReader SalesforceConf m)
  => Maybe Text
  -> m Text
initAuthorizationWorkflowUrl mstate = do
  sc <- ask
  return
    $  salesforceAuthenticationUrl sc
    <> "?client_id="
    <> salesforceConsumerKey sc
    <> "&response_type=code"
    <> "&redirect_uri="
    <> salesforceRedirectUrl sc
    <> maybe "" (\state -> "&state=" <> state) mstate

{- Returns a refresh token, that we will get back when user agrees in salesforce to give us persmission (after following link from initAuthorizationWorkflowUrl) -}
getRefreshTokenFromCode
  :: (MonadDB m, MonadLog m, MonadBase IO m, MonadReader SalesforceConf m)
  => Text
  -> m (Either Text Text)
getRefreshTokenFromCode code = do
  sc <- ask
  (exitcode, stdout, stderr) <- readCurl
    [ "--tlsv1.2"
    , "-X"
    , "POST"
    , "-d"
    , T.unpack
    $  "grant_type=authorization_code&"
    <> "code="
    <> code
    <> "&"
    <> "client_id="
    <> salesforceConsumerKey sc
    <> "&"
    <> "client_secret="
    <> salesforceConsumerSecret sc
    <> "&"
    <> "redirect_uri="
    <> salesforceRedirectUrl sc
    , T.unpack $ salesforceTokenUrl sc
    ]
    BSL.empty
  case exitcode of
    ExitFailure err -> do
      logInfo "Failed to receive token from Salesforce"
        $ object ["stderr" `equalsExternalBSL` stderr]
      return $ Left $ "Connection to Salesforce (refresh) closed with " <> (showt err)
    ExitSuccess -> case decode $ BSL.toString stdout of
      J.Ok js -> do
        mrt <- withJSValue js $ fromJSValueField "refresh_token"
        case mrt of
          Just rt -> return $ Right rt
          Nothing -> do
            logInfo "Parsing Salesfoce refresh response - no token found" $ object
              [ "exitcode" .= show exitcode
              , "stdout" `equalsExternalBSL` stdout
              , "stderr" `equalsExternalBSL` stderr
              ]
            return
              $ Left
                  "Response from Salesforce was a valid JSON, but no refresh token was found"
      _ -> do
        logInfo "Parsing standard output failed" $ object
          [ "exitcode" .= show exitcode
          , "stdout" `equalsExternalBSL` stdout
          , "stderr" `equalsExternalBSL` stderr
          ]
        return $ Left "Response from salesforce was not a valid JSON"

{- Every time we do a salesforce callback, we need to get new access token. We get it using refresh token -}
getAccessTokenFromRefreshToken
  :: (MonadDB m, MonadBase IO m, MonadLog m, MonadReader SalesforceConf m)
  => Text
  -> m (Either (Text, Int, Text, Text, Text) Text)
getAccessTokenFromRefreshToken rtoken = do
  sc <- ask
  (exitcode, stdoutWithCode, stderr) <- readCurl
    [ "--tlsv1.2"
    , "-X"
    , "POST"
    , "--write-out"
    , "\n%{http_code}"
    , "-d"
    , T.unpack
    $  "grant_type=refresh_token&"
    <> "refresh_token="
    <> rtoken
    <> "&"
    <> "client_id="
    <> salesforceConsumerKey sc
    <> "&"
    <> "client_secret="
    <> salesforceConsumerSecret sc
    , T.unpack $ salesforceTokenUrl sc
    ]
    BSL.empty
  let httpCode = httpCodeFromStdoutWithHTTPCode stdoutWithCode
      stdout   = stdoutFromStdoutWithHTTPCode stdoutWithCode
  case exitcode of
    ExitFailure err -> do
      logInfo "curl failed to receive token from Salesforce"
        $ object ["curl_exit_code" .= err, "stderr" `equalsExternalBSL` stderr]
      return $ Left
        ( "curl connection to Salesforce closed"
        , err
        , T.pack $ BSL.unpack stdout
        , T.pack $ BSL.unpack stderr
        , showt httpCode
        )
    ExitSuccess -> case decode $ BSL.toString stdout of
      J.Ok js -> do
        mrt <- withJSValue js $ fromJSValueField "access_token"
        case mrt of
          Just rt -> return $ Right rt
          Nothing -> do
            logInfo "No token found while parsing Salesforce access response" $ object
              ["http_code" .= show httpCode, "stdout" `equalsExternalBSL` stdout]
            return $ Left
              ( "Salesforce access response is valid JSON, but no access token found"
              , 0
              , T.pack $ BSL.unpack stdout
              , T.pack $ BSL.unpack stderr
              , showt httpCode
              )
      _ -> do
        logInfo "Parsing JSON from Salesforce access response stdout failed"
          $ object ["http_code" .= show httpCode, "stdout" `equalsExternalBSL` stdout]
        return $ Left
          ( "Salesforce access response is not valid JSON"
          , 0
          , T.pack $ BSL.unpack stdout
          , T.pack $ BSL.unpack stderr
          , showt httpCode
          )

{- Used by API call test salesforce. Let you check if salesfoce integration is set and working for a given url -}
testSalesforce
  :: (MonadDB m, MonadBase IO m, MonadLog m, MonadReader SalesforceConf m)
  => Text
  -> Text
  -> m (Either (Text, Int, Text, Text, Text) (Text, Text))
testSalesforce rtoken url = do
  matoken <- getAccessTokenFromRefreshToken rtoken
  case matoken of
    Left (msg, curlErr, stdout, stderr, httpCode) -> return
      $ Left ("Getting access token failed: " <> msg, curlErr, stdout, stderr, httpCode)
    Right atoken -> do
      (exitcode, stdoutWithCode, stderr) <- readCurl
        [ "--tlsv1.2"
        , "--location-trusted"
        , "-X"
        , "POST"
        , "--write-out"
        , "\n%{http_code}"
        , "-L" -- make curl follow redirects
        , "--post302" -- make curl still post after redirect
        , "-H"
        , T.unpack $ "Authorization: Bearer " <> atoken
        , "-H"
        , "Content-Length: 0"
        , T.unpack url
        ]
        BSL.empty
      let httpCode = httpCodeFromStdoutWithHTTPCode stdoutWithCode
          stdout   = stdoutFromStdoutWithHTTPCode stdoutWithCode
      case (exitcode, httpCode) of
        (ExitSuccess, n) | n < 300 ->
          return $ Right (showt httpCode, T.pack $ BSL.unpack stdout)
        (ExitSuccess, _) -> return $ Left
          ( "Salesforce access token worked, but callback failed"
          , 0
          , T.pack $ BSL.unpack stdout
          , T.pack $ BSL.unpack stderr
          , showt httpCode
          )
        (ExitFailure err, _) -> return $ Left
          ( "Salesforce access token worked, but callback failed"
          , err
          , T.pack $ BSL.unpack stdout
          , T.pack $ BSL.unpack stderr
          , showt httpCode
          )

stdoutFromStdoutWithHTTPCode :: BSL.ByteString -> BSL.ByteString
stdoutFromStdoutWithHTTPCode stdoutWithCode =
  BSL.take (lastEOLIndex stdoutWithCode) stdoutWithCode

httpCodeFromStdoutWithHTTPCode :: BSL.ByteString -> Int
httpCodeFromStdoutWithHTTPCode stdoutWithCode =
  case reads $ BSL.unpack $ BSL.drop (lastEOLIndex stdoutWithCode + 1) stdoutWithCode of
    [(n, "")] -> n
    _         -> unexpectedError "Couldn't parse http status from curl output"

lastEOLIndex :: BSL.ByteString -> Int64
lastEOLIndex bs = last (BSL.elemIndices '\n' bs)
