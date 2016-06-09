module Doc.API.Callback.Execute (execute) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
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
import qualified Text.JSON as J

import Amazon
import API.APIVersion
import Company.Model
import Crypto.RNG
import DB
import Doc.API.Callback.Data
import Doc.API.V1.DocumentToJSON
import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.Document
import Doc.DocInfo
import Doc.DocStateData
import Doc.Logging
import Doc.Model
import KontraPrelude
import Log.Identifier
import Log.Utils
import MailContext.Class
import Mails.SendMail
import Salesforce.AuthorizationWorkflow
import Salesforce.Conf
import User.CallbackScheme.Model
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.IO
import Utils.String
import qualified Utils.HTTP as Utils.HTTP

execute :: (AmazonMonad m, MonadDB m, CryptoRNG m, MonadMask m, MonadLog m, MonadBaseControl IO m,  MonadReader c m, HasSalesforceConf c, MailContextMonad m) => DocumentAPICallback -> m Bool
execute DocumentAPICallback{..} = logDocument dacDocumentID $ do
  exists <- dbQuery $ DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor dacDocumentID
  if not exists then do
    logInfo_ "API callback dropped since document does not exists or is purged/reallydeleted"
    return True
  else do
    doc <- dbQuery $ GetDocumentByDocumentID dacDocumentID
    case (maybesignatory =<< getAuthorSigLink doc) of
      Nothing -> $unexpectedErrorM $ "Document" <+> show dacDocumentID <+> "has no author"
      Just uid -> do
        mcallbackschema <- dbQuery $ GetUserCallbackSchemeByUserID uid
        case mcallbackschema of
          Just (SalesforceScheme rtoken) -> executeSalesforceCallback doc rtoken dacURL dacAttempts uid
          Just (BasicAuthScheme lg pwd) -> executeStandardCallback (Just (lg,pwd)) doc dacURL dacApiVersion
          Just (OAuth2Scheme lg pwd tokenUrl scope) -> executeOAuth2Callback (lg,pwd,tokenUrl,scope) doc dacURL dacApiVersion
          _ -> executeStandardCallback Nothing doc dacURL dacApiVersion

executeStandardCallback :: (AmazonMonad m, MonadDB m, MonadMask m, MonadLog m, MonadBaseControl IO m) => Maybe (String,String) -> Document -> String -> APIVersion -> m Bool
executeStandardCallback mBasicAuth doc url apiVersion = logDocument (documentid doc) $ do
  callbackParams <- callbackParamsWithDocumentJSON apiVersion doc
  (exitcode, _ , stderr) <- readCurl curlParams callbackParams
  case exitcode of
    ExitSuccess -> do
      logInfo "API callback executeStandardCallback succeeded" $ object [
          identifier_ apiVersion
        , "url" .= url
        ]
      return True
    ExitFailure ec -> do
      logAttention "API callback executeStandardCallback failed" $ object [
          identifier_ apiVersion
        , "url" .= url
        , "curl_exitcode" .= show ec
        , "stderr" `equalsExternalBSL` stderr
        ]
      return False
  where
    curlParams =  [
          "-X", "POST"
        , "-f" -- make curl return exit code (22) if it got anything else but 2XX
        , "-L" -- make curl follow redirects
        , "--data-binary", "@-"          -- take binary data from stdin
        , "-H", "Content-type: application/x-www-form-urlencoded; charset=UTF-8"
        ] ++
        (case mBasicAuth of
            Just (lg,pwd) -> ["-H", "Authorization: Basic " ++ (BSC8.unpack $ B64.encode $ BSC8.pack $ lg ++ ":" ++  pwd)]
            _ -> []
        ) ++
        [ url]

executeOAuth2Callback :: (AmazonMonad m, MonadDB m, MonadMask m, MonadLog m, MonadBaseControl IO m) =>
                         (String,String,String,String) -> Document -> String -> APIVersion -> m Bool
executeOAuth2Callback (lg,pwd,tokenUrl,scope) doc callbackUrl apiVersion = logDocument (documentid doc) $ do
   (exitcode1, stdout1 , stderr1) <- readCurl [
          "-X", "POST"
        , "-f" -- make curl return exit code (22) if it got anything else but 2XX
        , "-L" -- make curl follow redirects
        , "-d", "{\"login\":{\"scope\":[\"" ++ scope ++ "\"]}}\""
        , "-H", "Content-Type: application/json; charset=UTF-8"
        , "-H", "Authorization: Basic " ++ (BSC8.unpack $ B64.encode $ BSC8.pack $ lg ++ ":" ++  pwd)
        , tokenUrl ] BSL.empty
   case exitcode1 of
    ExitFailure ec1 -> do
      logAttention "API callback executeOAuth2Callback failed during authorization phase" $ object [
          identifier_ apiVersion
        , "curl_exitcode" .= show ec1
        , "stderr" `equalsExternalBSL` stderr1
        ]
      return False
    ExitSuccess -> case parseAccessToken stdout1 of
      Nothing -> do
       logAttention "API callback executeOAuth2Callback failed for token parsing" $ object [
         "stdout" .= show stdout1
         ]
       return False
      Just t -> do
        callbackParams <- callbackParamsWithDocumentJSON apiVersion doc
        (exitcode2, _ , stderr2) <- readCurl [
            "-X", "POST"
          , "-f" -- make curl return exit code (22) if it got anything else but 2XX
          , "-L" -- make curl follow redirects
          , "-H", "Authorization: Bearer " ++ t
          , "--data-binary", "@-"          -- take binary data from stdin
          , callbackUrl ] callbackParams
        case exitcode2 of
          ExitSuccess -> do
            logInfo "API callback executeOAuth2Callback succeeded" $ object [
                identifier_ apiVersion
              , "url" .= callbackUrl
              ]
            return True
          ExitFailure ec2 -> do
            logAttention "API callback executeOAuth2Callback failed" $ object [
                identifier_ apiVersion
              , "url" .= callbackUrl
              , "curl_exitcode" .= show ec2
              , "stderr" `equalsExternalBSL` stderr2
              ]
            return False


callbackParamsWithDocumentJSON :: (AmazonMonad m, MonadDB m, MonadMask m, MonadLog m, MonadBaseControl IO m) =>
                                  APIVersion -> Document -> m BSLU.ByteString
callbackParamsWithDocumentJSON apiVersion doc = case apiVersion of
  V1 -> do
    dJSON <- documentJSONV1 Nothing False True Nothing doc
    return $ BSLU.fromString $ urlEncodeVars [
        ("documentid", show (documentid doc))
      , ("signedAndSealed", if isClosed doc && isJust (documentsealedfile doc)
          then "true"
          else "false")
      , ("json", encode dJSON)
      ]
  V2 -> do
    let json = unjsonToByteStringLazy' (Options { pretty = False, indent = 0, nulls = True }) (unjsonDocument (documentAccessForAuthor doc)) doc
    return $ Utils.HTTP.urlEncodeVars [
        ("document_id", BSLU.fromString $ show (documentid doc))
      , ("document_signed_and_sealed", if isClosed doc && isJust (documentsealedfile doc)
          then "true"
          else "false")
      , ("document_json", json)
      ]

parseAccessToken :: BSLU.ByteString -> Maybe String
parseAccessToken str = do
  case decode $ BSLU.toString str of
    J.Ok js ->  join $ withJSValue js $ fromJSValueFieldCustom "auth" $ fromJSValueField "access_token"
    _ -> Nothing

executeSalesforceCallback :: (MonadDB m, CryptoRNG m, MonadLog m, MonadThrow m, MonadBase IO m, MonadReader c m, HasSalesforceConf c) => Document -> String ->  String -> Int32 -> UserID -> m Bool
executeSalesforceCallback doc rtoken url attempts uid = logDocument (documentid doc) $ do
  mtoken <- getAccessTokenFromRefreshToken rtoken
  case mtoken of
       Left (msg, curl_err, stdout, stderr, http_code) -> do
         emailErrorIfNeeded ("Getting access token failed: " ++ msg, curl_err, stdout, stderr, http_code)
         return False
       Right token -> do
        (exitcode, stdout, stderr) <- readCurl [
            "-X", "POST"
          , "--write-out","\n%{http_code}"
          , "-f" -- make curl return exit code (22) if it got anything else but 2XX
          , "-L" -- make curl follow redirects
          , "--post302" -- make curl still post after redirect
          , "-d",  urlEncodeVars [
                    ("documentid", show (documentid doc))
                  , ("signedAndSealed", (if (isClosed doc && (isJust $ documentsealedfile doc)) then "true" else "false") )
                ]
          , "-H", "Authorization: Bearer " ++ token
          , url
          ] BSL.empty
        let http_code = case reverse . lines . BSL.unpack $ stdout of
              [] -> ""
              c:_ -> c
        case exitcode of
                    ExitSuccess -> return True
                    ExitFailure err -> do
                      logInfo "Salesforce API callback failed" $ object [
                          "stderr" `equalsExternalBSL` stderr
                        ]
                      emailErrorIfNeeded ("Request to callback URL failed", err, BSL.unpack stdout , BSL.unpack stderr, http_code)
                      return False

  where emailErrorIfNeeded (msg, curl_err, stdout, stderr, http_code) = do
          sc <- getSalesforceConfM
          when (attempts == 4 && isJust (salesforceErrorEmail sc)) $ do
            logInfo_ "Salesforce API callback failed for 5th time, sending email."
            user <- fmap $fromJust (dbQuery $ GetUserByID uid)
            company <- dbQuery $ GetCompanyByUserID $ uid

            let mail = emptyMail {
                    to = [ MailAddress { fullname = "Salesforce Admin", email = $fromJust (salesforceErrorEmail sc) } ]
                  , title = "[Salesforce Callback Error] " ++
                            "(user: " ++ getEmail user ++ ") " ++
                            "(company: " ++ companyname (companyinfo company) ++ ") " ++
                            "(documentid: " ++ show (documentid doc) ++ ") " ++
                            "(http_code: " ++ http_code ++ ")"
                  , content = "<h2>Salesforce Callback Failed</h2>" ++ "<br />\r\n"
                          ++ "<strong>Error:</strong> " ++ escapeHTML msg ++ "<br />\r\n"
                          ++ "<br />"
                          ++ "<strong>Salesforce access token URL:</strong> " ++ salesforceTokenUrl sc ++ "<br />\r\n"
                          ++ "<strong>Callback URL:</strong> " ++ url ++ "<br />\r\n"
                          ++ "<strong>Curl error code:</strong> " ++ show curl_err ++ "<br />\r\n"
                          ++ "<strong>Curl stdout:</strong> " ++ escapeHTML stdout ++ "<br />\r\n"
                          ++ "<strong>Curl stderr:</strong> " ++ escapeHTML stderr ++ "<br />\r\n"
                          ++ "<strong>HTTP response code:</strong> " ++ escapeHTML http_code ++ "<br />\r\n"
                          ++ "<br />"
                          ++ "<strong>User ID:</strong> " ++ show (userid user) ++ "<br />\r\n"
                          ++ "<strong>Document ID:</strong> " ++ show (documentid doc) ++ "<br />\r\n"
                          ++ "<strong>User Company ID:</strong> " ++ show (companyid company) ++ "<br />\r\n"
                          ++ "<strong>User Name:</strong> " ++ escapeHTML (getFullName user) ++ "<br />\r\n"
                          ++ "<strong>User Email:</strong> " ++ escapeHTML (getEmail user) ++ "<br />\r\n"
                  }
            scheduleEmailSendout mail
