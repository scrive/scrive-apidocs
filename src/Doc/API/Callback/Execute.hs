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
       Left e -> do
         emailErrorIfNeeded e
         return False
       Right token -> do
        (exitcode, _ , stderr) <- readCurl [
                      "-X", "POST"
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
        case exitcode of
                    ExitSuccess -> return True
                    ExitFailure e -> do
                      logInfo "Salesforce API callback failed" $ object [
                          "stderr" `equalsExternalBSL` stderr
                        ]
                      emailErrorIfNeeded $ "ExitFailure with error code: " ++ show e
                      return False

  where emailErrorIfNeeded e = when (attempts == 4) $ do
          logInfo "Salesforce API callback failed for 5th time, sending email" $ object []
          sc <- getSalesforceConfM
          mctx <- getMailContext
          muser <- dbQuery $ GetUserByID uid
          let user = $fromJust muser
              cid = show $ usercompany user
          let mail = emptyMail {
                  to = [ MailAddress { fullname = "Salesforce Admin", email = salesforceErrorEmail sc } ]
                , title = "[Salesforce Callback Error] [companyid:" ++ cid ++ "]"
                , content = "# Salesforce Callback Failed" ++ "\r\n\r\n"
                         ++ "Error message: " ++ e ++ "\r\n"
                         ++ "Document ID: " ++ (show $ documentid doc) ++ "\r\n"
                         ++ "User ID: " ++ (show $ uid) ++ "\r\n"
                         ++ "User Name: " ++ getFullName user ++ "\r\n"
                         ++ "User Email: " ++ getEmail user ++ "\r\n"
                         ++ "User Company ID: " ++ cid ++ "\r\n"
                         ++ "\r\n" ++ "# END\r\n"
                }
          scheduleEmailSendout (mctxmailsconfig mctx) mail
