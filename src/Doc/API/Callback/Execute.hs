module Doc.API.Callback.Execute (execute) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Int (Int32)
import Log
import Network.HTTP as HTTP
import System.Exit
import Text.JSON
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU

import Amazon
import API.APIVersion
import Company.Model
import Crypto.RNG
import DB
import Doc.API.Callback.Data
import Doc.API.V1.DocumentToJSON
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

execute :: (AmazonMonad m, MonadDB m, CryptoRNG m, MonadThrow m, MonadLog m, MonadIO m, MonadBase IO m,  MonadReader c m, HasSalesforceConf c, MailContextMonad m) => DocumentAPICallback -> m Bool
execute DocumentAPICallback{..} = logDocument dacDocumentID $ do
  exists <- dbQuery $ DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor dacDocumentID
  if not exists then do
    logInfo_ "API callback dropped since document does not exists or is purged/reallydeleted"
    return True
  else if dacApiVersion == V1 then do -- TODO APIv2: Get rid of the API version check here, executeStandardCallback will handle it
    doc <- dbQuery $ GetDocumentByDocumentID dacDocumentID
    case (maybesignatory =<< getAuthorSigLink doc) of
      Nothing -> $unexpectedErrorM $ "Document" <+> show dacDocumentID <+> "has no author"
      Just uid -> do
        mcallbackschema <- dbQuery $ GetUserCallbackSchemeByUserID uid
        case mcallbackschema of
          Just (SalesforceScheme rtoken) -> executeSalesforceCallback doc rtoken dacURL dacAttempts uid
          Just (BasicAuthScheme lg pwd) -> executeStandardCallback (Just (lg,pwd)) doc dacURL dacApiVersion
          _ -> executeStandardCallback Nothing doc dacURL dacApiVersion
  else do -- TODO APIv2: Get rid of this block too, see TODO above
    logAttention "API callback was dropped as it is not a V1 callback: this is not yet implemented and shouldn't happen!" $ object [
          identifier_ dacApiVersion
        , "url" .= dacURL
        ]
    return True

executeStandardCallback :: (AmazonMonad m, MonadDB m, MonadThrow m, MonadLog m, MonadBase IO m, MonadIO m) => Maybe (String,String) -> Document -> String -> APIVersion -> m Bool
executeStandardCallback mBasicAuth doc url apiVersion = logDocument (documentid doc) $ do
  dJSON <- case apiVersion of
    -- TODO APIv2: Use correct JSON for the version given
    _ -> documentJSONV1 Nothing False True Nothing doc
  (exitcode, _ , stderr) <- readCurl
    curlParams
    (BSLU.fromString $ urlEncodeVars [
        ("documentid", show (documentid doc))
      , ("signedAndSealed", if isClosed doc && isJust (documentsealedfile doc)
        then "true"
        else "false")
      , ("json", encode dJSON)
      ])
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

executeSalesforceCallback :: (MonadDB m, CryptoRNG m, MonadLog m, MonadThrow m, MonadIO m, MonadBase IO m, MonadReader c m, HasSalesforceConf c, MailContextMonad m) => Document -> String ->  String -> Int32 -> UserID -> m Bool
executeSalesforceCallback doc rtoken url attempts uid = logDocument (documentid doc) $ do
  mtoken <- getAccessTokenFromRefreshToken rtoken
  case mtoken of
       Left (msg, curl_err, stderr, http_code) -> do
         emailErrorIfNeeded ("Getting access token failed: " ++ msg, curl_err, stderr, http_code)
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
                      emailErrorIfNeeded ("Request to callback URL failed", err, BSL.unpack stderr, http_code)
                      return False

  where emailErrorIfNeeded (msg, curl_err, stderr, http_code) = do
          sc <- getSalesforceConfM
          when (attempts == 4 && isJust (salesforceErrorEmail sc)) $ do
            logInfo_ "Salesforce API callback failed for 5th time, sending email."
            mctx <- getMailContext
            user <- fmap $fromJust (dbQuery $ GetUserByID uid)
            company <- dbQuery $ GetCompanyByUserID $ uid

            let mail = emptyMail {
                    to = [ MailAddress { fullname = "Salesforce Admin", email = $fromJust (salesforceErrorEmail sc) } ]
                  , title = "[Salesforce Callback Error] " ++
                            "(user: " ++ getEmail user ++ ") " ++
                            "(company: " ++ companyname (companyinfo company) ++ ") " ++
                            "(documentid: " ++ show (documentid doc) ++ ")"
                  , content = "<h2>Salesforce Callback Failed</h2>" ++ "<br />\r\n"
                          ++ "<strong>Error:</strong> " ++ msg ++ "<br />\r\n"
                          ++ "<br />"
                          ++ "<strong>Salesforce access token URL:</strong> " ++ salesforceTokenUrl sc ++ "<br />\r\n"
                          ++ "<strong>Callback URL:</strong> " ++ url ++ "<br />\r\n"
                          ++ "<strong>Curl error code:</strong> " ++ show curl_err ++ "<br />\r\n"
                          ++ "<strong>Curl stderr:</strong> " ++ stderr ++ "<br />\r\n"
                          ++ "<strong>HTTP response code:</strong> " ++ http_code ++ "<br />\r\n"
                          ++ "<br />"
                          ++ "<strong>User ID:</strong> " ++ show (userid user) ++ "<br />\r\n"
                          ++ "<strong>Document ID:</strong> " ++ show (documentid doc) ++ "<br />\r\n"
                          ++ "<strong>User Company ID:</strong> " ++ show (companyid company) ++ "<br />\r\n"
                          ++ "<strong>User Name:</strong> " ++ getFullName user ++ "<br />\r\n"
                          ++ "<strong>User Email:</strong> " ++ getEmail user ++ "<br />\r\n"
                  }
            scheduleEmailSendout (mctxmailsconfig mctx) mail
