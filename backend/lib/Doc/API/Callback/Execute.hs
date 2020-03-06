module Doc.API.Callback.Execute (execute) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Int (Int32)
import Data.Unjson
import Log
import Network.HTTP as HTTP
import System.Exit
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.JSON as J

import API.APIVersion
import CronEnv
import DB
import Doc.API.Callback.Types
import Doc.API.V1.DocumentToJSON
import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.Document
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import Doc.Logging
import Doc.Model
import File.Storage
import KontraError
import Log.Identifier
import Log.Utils
import Mails.SendMail
import Salesforce.AuthorizationWorkflow
import Salesforce.Conf
import User.CallbackScheme.Model
import User.Model
import UserGroup.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.IO
import Utils.String
import qualified Utils.HTTP as Utils.HTTP

execute
  :: ( MonadFileStorage m
     , MonadDB m
     , MonadIO m
     , CryptoRNG m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , MonadReader CronEnv m
     )
  => DocumentAPICallback
  -> m Bool
execute dac@DocumentAPICallback {..} = logDocument dacDocumentID $ do
  exists <- dbQuery $ DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor dacDocumentID
  if not exists
    then do
      logInfo_
        "API callback dropped since document does not exists or is purged/reallydeleted"
      return True
    else do
      doc <- dbQuery $ GetDocumentByDocumentID dacDocumentID
      case maybesignatory =<< getAuthorSigLink doc of
        Nothing ->
          unexpectedError $ "Document" <+> (showt dacDocumentID) <+> "has no author"
        Just uid -> do
          mcallbackschema <- dbQuery $ GetUserCallbackSchemeByUserID uid
          case mcallbackschema of
            Just (SalesforceScheme rtoken) -> do
              sd <- ask
              case (ceSalesforceConf sd) of
                Nothing -> do
                  noConfigurationWarning "Salesforce" -- log a warning rather than raising an error not to disturb cron
                  return False
                Just sc -> flip runReaderT sc
                  $ executeSalesforceCallback doc rtoken dacURL dacAttempts uid
            Just (BasicAuthScheme lg pwd) ->
              executeStandardCallback (Just (lg, pwd)) doc dac
            Just (OAuth2Scheme lg pwd tokenUrl scope) ->
              executeOAuth2Callback (lg, pwd, tokenUrl, scope) doc dac
            Just (Hi3GScheme lg pwd tokenUrl scope) ->
              executeHi3GCallback (lg, pwd, tokenUrl, scope) doc dac
            _ -> executeStandardCallback Nothing doc dac

executeStandardCallback
  :: ( MonadFileStorage m
     , MonadDB m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     )
  => Maybe (Text, Text)
  -> Document
  -> DocumentAPICallback
  -> m Bool
executeStandardCallback mBasicAuth doc dac = logDocument (documentid doc) $ do
  callbackParams             <- callbackParamsWithDocumentJSON (dacApiVersion dac) doc
  (exitcode, stdout, stderr) <- readCurl curlParams callbackParams
  let httpCodeStr = case reverse . lines . BSL.unpack $ stdout of
        []    -> ""
        c : _ -> c
      httpCode = case reads httpCodeStr of
        [(n :: Int, "")] -> n
        _                -> unexpectedError "Couldn't parse http status from curl output"
  case (exitcode, httpCode) of
    (ExitSuccess, n) | n < 300 -> do
      logInfo "API callback executeStandardCallback succeeded" $ logObject_ dac
      now <- currentTime
      dbUpdate
        $  SetDocumentApiCallbackResult (documentid doc)
        $  Just
        $  "Success "
        <> (showt now)
      return True
    (ExitSuccess, _) -> do
      logInfo "API callback executeStandardCallback failed" $ object
        [ logPair_ dac
        , "stderr" `equalsExternalBSL` stderr
        , "stdout" `equalsExternalBSL` stdout
        ]
      now <- currentTime
      let msg = T.concat
            [ "Failure "
            , (showt now) <> "\n"
            , "stdout: "
            , T.pack $ BSL.unpack stdout <> "\n"
            , "stderr: "
            , T.pack $ BSL.unpack stderr
            ]
      dbUpdate $ SetDocumentApiCallbackResult (documentid doc) $ Just msg
      return False
    (ExitFailure ec, _) -> do
      logInfo "API callback executeStandardCallback failed" $ object
        [ logPair_ dac
        , "curl_exitcode" .= show ec
        , "stderr" `equalsExternalBSL` stderr
        , "stdout" `equalsExternalBSL` stdout
        ]
      now <- currentTime
      let msg = T.concat
            [ "Failure "
            , (showt now) <> "\n"
            , "stdout: "
            , T.pack $ BSL.unpack stdout <> "\n"
            , "stderr: "
            , T.pack $ BSL.unpack stderr
            ]
      dbUpdate $ SetDocumentApiCallbackResult (documentid doc) $ Just msg
      return False
  where
    curlParams =
      [ "-X"
        , "POST"
        , "-L" -- make curl follow redirects
        , "--write-out"
        , "\n%{http_code}"
        , "--data-binary"
        , "@-"          -- take binary data from stdin
        , "-H"
        , "Content-type: application/x-www-form-urlencoded; charset=UTF-8"
        ]
        <> (case mBasicAuth of
             Just (lg, pwd) ->
               [ "-H"
               , "Authorization: Basic "
                 <> (BSC8.unpack $ B64.encode $ TE.encodeUtf8 $ lg <> ":" <> pwd)
               ]
             _ -> []
           )
        <> [T.unpack $ dacURL dac]

executeOAuth2Callback
  :: ( MonadFileStorage m
     , MonadIO m
     , MonadDB m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     )
  => (Text, Text, Text, Text)
  -> Document
  -> DocumentAPICallback
  -> m Bool
executeOAuth2Callback (lg, pwd, tokenUrl, scope) doc dac =
  logDocument (documentid doc) $ do
    now <- currentTime
    (exitcode1, stdout1, stderr1) <- readCurl
      [ "-X"
      , "POST"
      , "-L" -- make curl follow redirects
      , "-d"
      , "grant_type=" <> (T.unpack scope) <> ""
      , "-H"
      , "Content-Type: application/x-www-form-urlencoded"
      , "-H"
      , "Authorization: Basic "
        <> (BSC8.unpack $ B64.encode $ TE.encodeUtf8 $ lg <> ":" <> pwd)
      , T.unpack tokenUrl
      ]
      BSL.empty
    case exitcode1 of
      ExitFailure ec1 -> do
        logInfo "API callback executeOAuth2Callback failed during authorization phase"
          $ object
              [ logPair_ dac
              , "curl_exitcode" .= show ec1
              , "stderr" `equalsExternalBSL` stderr1
              , "stdout" `equalsExternalBSL` stdout1
              ]
        let msg = T.concat
              [ "Failure "
              , (showt now) <> "\n"
              , "stdout: "
              , T.pack $ BSL.unpack stdout1 <> "\n"
              , "stderr: "
              , T.pack $ BSL.unpack stderr1
              ]
        dbUpdate $ SetDocumentApiCallbackResult (documentid doc) $ Just msg
        return False
      ExitSuccess -> case parseAccessToken stdout1 of
        Nothing -> do
          logInfo "API callback executeOAuth2Callback failed for token parsing"
            $ object ["stdout" .= show stdout1, "stderr" .= show stderr1]
          let msg = T.concat
                [ "Parsing failure "
                , (showt now) <> "\n"
                , "stdout: "
                , T.pack $ BSL.unpack stdout1 <> "\n"
                , "stderr: "
                , T.pack $ BSL.unpack stderr1
                ]
          dbUpdate $ SetDocumentApiCallbackResult (documentid doc) $ Just msg
          return False
        Just t -> do
          callbackParams <- callbackParamsWithDocumentJSON (dacApiVersion dac) doc
          (exitcode2, stdout2, stderr2) <- readCurl
            [ "-X"
            , "POST"
            , "--write-out"
            , "\n%{http_code}"
            , "-L" -- make curl follow redirects
            , "-H"
            , T.unpack $ "Authorization: Bearer " <> t
            , "--data-binary"
            , "@-"          -- take binary data from stdin
            , T.unpack $ dacURL dac
            ]
            callbackParams
          let httpCodeStr = case reverse . lines . BSL.unpack $ stdout2 of
                []    -> ""
                c : _ -> c
          httpCode <- case reads httpCodeStr of
            [(n :: Int, "")] -> return n
            _                -> do
              let msg = T.concat
                    [ "Parsing http failure "
                    , (showt now) <> "\n"
                    , "httpCodeStr: "
                    , T.pack $ httpCodeStr
                    ]
              dbUpdate $ SetDocumentApiCallbackResult (documentid doc) $ Just msg
              unexpectedError "Couldn't parse http status from curl output"
          case (exitcode2, httpCode) of
            (ExitSuccess, n)
              | n < 300 -> do
                logInfo "API callback executeOAuth2Callback succeeded" $ logObject_ dac
                dbUpdate
                  $  SetDocumentApiCallbackResult (documentid doc)
                  $  Just
                  $  "Success "
                  <> (showt now)
                return True
              | otherwise -> do
                logInfo "API callback executeOAuth2Callback failed" $ object
                  [ logPair_ dac
                  , "curl_exitcode" .= show exitcode2
                  , "http_code" .= show n
                  , "stderr" `equalsExternalBSL` stderr2
                  , "stdout" `equalsExternalBSL` stdout2
                  ]
                let msg = T.concat
                      [ "Failure "
                      , (showt now) <> "\n"
                      , "stdout: "
                      , T.pack $ BSL.unpack stdout2 <> "\n"
                      , "stderr: "
                      , T.pack $ BSL.unpack stderr2
                      ]
                dbUpdate $ SetDocumentApiCallbackResult (documentid doc) $ Just msg
                return False
            (ExitFailure ec2, _) -> do
              logInfo "API callback executeOAuth2Callback failed" $ object
                [ logPair_ dac
                , "curl_exitcode" .= show ec2
                , "stderr" `equalsExternalBSL` stderr2
                , "stdout" `equalsExternalBSL` stdout2
                ]
              let msg = T.concat
                    [ "Failure "
                    , (showt now) <> "\n"
                    , "stdout: "
                    , T.pack $ BSL.unpack stdout2 <> "\n"
                    , "stderr: "
                    , T.pack $ BSL.unpack stderr2
                    ]
              dbUpdate $ SetDocumentApiCallbackResult (documentid doc) $ Just msg
              return False
  where
    parseAccessToken :: BSLU.ByteString -> Maybe Text
    parseAccessToken str = do
      case decode $ BSLU.toString str of
        J.Ok js -> join $ withJSValue js $ fromJSValueField "access_token"
        _       -> Nothing

executeHi3GCallback
  :: ( MonadFileStorage m
     , MonadIO m
     , MonadDB m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     )
  => (Text, Text, Text, Text)
  -> Document
  -> DocumentAPICallback
  -> m Bool
executeHi3GCallback (lg, pwd, tokenUrl, scope) doc dac =
  logDocument (documentid doc) $ do
    (exitcode1, stdout1, stderr1) <- readCurl
      [ "-X"
      , "POST"
      , "-L" -- make curl follow redirects
      , "-d"
      , "{\"login\":{\"scope\":[\"" <> (T.unpack scope) <> "\"]}}\""
      , "-H"
      , "Content-Type: application/json; charset=UTF-8"
      , "-H"
      , "Authorization: Basic "
        <> (BSC8.unpack $ B64.encode $ TE.encodeUtf8 $ lg <> ":" <> pwd)
      , T.unpack tokenUrl
      ]
      BSL.empty
    case exitcode1 of
      ExitFailure ec1 -> do
        logInfo "API callback executeHi3GCallback failed during authorization phase"
          $ object
              [ logPair_ dac
              , "curl_exitcode" .= show ec1
              , "stderr" `equalsExternalBSL` stderr1
              , "stdout" `equalsExternalBSL` stdout1
              ]
        return False
      ExitSuccess -> case parseAccessToken stdout1 of
        Nothing -> do
          logInfo "API callback executeHi3GCallback failed for token parsing"
            $ object ["stdout" .= show stdout1, "stderr" .= show stderr1]
          return False
        Just t -> do
          callbackParams <- callbackParamsWithDocumentJSON (dacApiVersion dac) doc
          (exitcode2, stdout2, stderr2) <- readCurl
            [ "-X"
            , "POST"
            , "--write-out"
            , "\n%{http_code}"
            , "-L" -- make curl follow redirects
            , "-H"
            , "Authorization: Bearer " <> t
            , "--data-binary"
            , "@-"          -- take binary data from stdin
            , T.unpack $dacURL dac
            ]
            callbackParams
          let httpCodeStr = case reverse . lines . BSL.unpack $ stdout2 of
                []    -> ""
                c : _ -> c
              httpCode = case reads httpCodeStr of
                [(n :: Int, "")] -> n
                _ -> unexpectedError "Couldn't parse http status from curl output"
          case (exitcode2, httpCode) of
            (ExitSuccess, n)
              | n < 300 -> do
                logInfo "API callback executeHi3GCallback succeeded" $ logObject_ dac
                return True
              | otherwise -> do
                logInfo "API callback executeHi3GCallback failed" $ object
                  [ logPair_ dac
                  , "curl_exitcode" .= show exitcode2
                  , "http_code" .= show n
                  , "stderr" `equalsExternalBSL` stderr2
                  , "stdout" `equalsExternalBSL` stdout2
                  ]
                return False
            (ExitFailure ec2, _) -> do
              logInfo "API callback executeHi3GCallback failed" $ object
                [ logPair_ dac
                , "curl_exitcode" .= show ec2
                , "stderr" `equalsExternalBSL` stderr2
                , "stdout" `equalsExternalBSL` stdout2
                ]
              return False
  where
    parseAccessToken :: BSLU.ByteString -> Maybe String
    parseAccessToken str = do
      case decode $ BSLU.toString str of
        J.Ok js ->
          join $ withJSValue js $ fromJSValueFieldCustom "auth" $ fromJSValueField
            "access_token"
        _ -> Nothing


callbackParamsWithDocumentJSON
  :: ( MonadFileStorage m
     , MonadIO m
     , MonadDB m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     )
  => APIVersion
  -> Document
  -> m BSLU.ByteString
callbackParamsWithDocumentJSON apiVersion doc = case apiVersion of
  V1 -> do
    dJSON <- documentJSONV1 Nothing False True Nothing doc
    return $ BSLU.fromString $ urlEncodeVars
      [ ("documentid", show (documentid doc))
      , ( "signedAndSealed"
        , if isClosed doc && hasDigitalSignature doc then "true" else "false"
        )
      , ("json", encode dJSON)
      ]
  V2 -> do
    let json = unjsonToByteStringLazy'
          (Options { pretty = False, indent = 0, nulls = True })
          (unjsonDocument (documentAccessForAuthor doc))
          doc
    return $ Utils.HTTP.urlEncodeVars
      [ ("document_id", BSLU.fromString $ show (documentid doc))
      , ( "document_signed_and_sealed"
        , if isClosed doc && hasDigitalSignature doc then "true" else "false"
        )
      , ("document_json", json)
      ]

executeSalesforceCallback
  :: ( MonadDB m
     , CryptoRNG m
     , MonadLog m
     , MonadThrow m
     , MonadBase IO m
     , MonadReader SalesforceConf m
     )
  => Document
  -> Text
  -> Text
  -> Int32
  -> UserID
  -> m Bool
executeSalesforceCallback doc rtoken url attempts uid = logDocument (documentid doc) $ do
  mtoken <- getAccessTokenFromRefreshToken rtoken
  case mtoken of
    Left (msg, curl_err, stdout, stderr, http_code) -> do
      emailErrorIfNeeded
        ( "Getting access token failed: " <> msg
        , showt curl_err
        , stdout
        , stderr
        , http_code
        )
      return False
    Right token -> do
      (exitcode, stdout, stderr) <- readCurl
        [ "--tlsv1.2"
        , "--location-trusted"
        , "-X"
        , "POST"
        , "--write-out"
        , "\n%{http_code}"
        , "-L" -- make curl follow redirects
        , "--post302" -- make curl still post after redirect
        , "-d"
        , urlEncodeVars
          [ ("documentid", show (documentid doc))
          , ( "signedAndSealed"
            , (if (isClosed doc && (isJust $ documentsealedfile doc))
                then "true"
                else "false"
              )
            )
          ]
        , "-H"
        , T.unpack $ "Authorization: Bearer " <> token
        , T.unpack url
        ]
        BSL.empty
      let httpCodeStr = case reverse . lines . BSL.unpack $ stdout of
            []    -> ""
            c : _ -> c
          httpCode = case reads httpCodeStr of
            [(n :: Int, "")] -> n
            _ -> unexpectedError "Couldn't parse http status from curl output"
          sendAndFail mErr = do
            logInfo "Salesforce API callback failed" $ object
              [ "salesforce_callback_url" .= url
              , "exitcode" .= show exitcode
              , "stdout" `equalsExternalBSL` stdout
              , "stderr" `equalsExternalBSL` stderr
              ]
            emailErrorIfNeeded
              ( "Request to callback URL failed"
              , fromMaybe "<unknown>" mErr
              , T.pack $ BSL.unpack stdout
              , T.pack $ BSL.unpack stderr
              , showt httpCode
              )
            return False
      case (exitcode) of
        (ExitSuccess) -> if (httpCode < 300)
          then do
            logInfo_ "Salesforce API callback succeeded"
            return True
          else sendAndFail Nothing
        (ExitFailure err) -> sendAndFail $ Just $ showt err

  where
    emailErrorIfNeeded (msg, curl_err, stdout, stderr, http_code :: Text) = do
      sc <- ask
      when (attempts == 4 && isJust (salesforceErrorEmail sc)) $ do
        logInfo_ "Salesforce API callback failed for 5th time, sending email."
        muser <- dbQuery $ GetUserByID uid
        let userEmail = maybe "<unknown>" getEmail muser
            userName  = maybe "<unknown>" getFullName muser
        ug <- dbQuery $ UserGroupGetByUserID $ uid

        let mail = emptyMail
              { to      = [ MailAddress { fullname = "Salesforce Admin"
                                        , email    = fromJust (salesforceErrorEmail sc)
                                        }
                          ]
              , title   = "[Salesforce Callback Error] "
                          <> "(user: "
                          <> userEmail
                          <> ") "
                          <> "(company: "
                          <> (ug ^. #name)
                          <> ") "
                          <> "(documentid: "
                          <> (showt (documentid doc))
                          <> ") "
                          <> "(http_code: "
                          <> http_code
                          <> ")"
              , content = "<h2>Salesforce Callback Failed</h2>"
                          <> "<br />\r\n"
                          <> "<strong>Error:</strong> "
                          <> escapeHTML msg
                          <> "<br />\r\n"
                          <> "<br />"
                          <> "<strong>Salesforce access token URL:</strong> "
                          <> salesforceTokenUrl sc
                          <> "<br />\r\n"
                          <> "<strong>Callback URL:</strong> "
                          <> url
                          <> "<br />\r\n"
                          <> "<strong>Curl error code:</strong> "
                          <> curl_err
                          <> "<br />\r\n"
                          <> "<strong>Curl stdout:</strong> "
                          <> escapeHTML stdout
                          <> "<br />\r\n"
                          <> "<strong>Curl stderr:</strong> "
                          <> escapeHTML stderr
                          <> "<br />\r\n"
                          <> "<strong>HTTP response code:</strong> "
                          <> escapeHTML http_code
                          <> "<br />\r\n"
                          <> "<br />"
                          <> "<strong>User ID:</strong> "
                          <> (showt uid)
                          <> "<br />\r\n"
                          <> "<strong>Document ID:</strong> "
                          <> (showt (documentid doc))
                          <> "<br />\r\n"
                          <> "<strong>User Company ID:</strong> "
                          <> (showt (ug ^. #id))
                          <> "<br />\r\n"
                          <> "<strong>User Name:</strong> "
                          <> escapeHTML userName
                          <> "<br />\r\n"
                          <> "<strong>User Email:</strong> "
                          <> escapeHTML userEmail
                          <> "<br />\r\n"
              }
        scheduleEmailSendout mail
