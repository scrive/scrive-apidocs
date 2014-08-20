module Doc.API.Callback.Execute ( execute ) where

import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString, fromString)

import DB
import Control.Monad.IO.Class

import Utils.IO
import qualified Log
import Doc.API.Callback.DocumentAPICallback
import Doc.Model
import Doc.DocInfo
import Doc.DocStateData
import Data.Maybe
import Salesforce.AuthorizationWorkflow
import Salesforce.Conf
import Control.Monad.Reader
import Control.Applicative
import User.CallbackScheme.Model
import Util.SignatoryLinkUtils
import Text.JSON
import Doc.API.V1.DocumentJSON
import Amazon
import Network.HTTP as HTTP
import Text.JSON.Gen

execute :: (AmazonMonad m, MonadDB m, Log.MonadLog m, MonadIO m, MonadReader c m, HasSalesforceConf c) => DocumentAPICallback -> m Bool
execute DocumentAPICallback{..} = do
  doc <- dbQuery $ GetDocumentByDocumentID dacDocumentID
  do
        case (join $ maybesignatory <$> getAuthorSigLink doc) of
          Nothing -> return False
          Just userid -> do
             mcallbackschema <- dbQuery $ GetUserCallbackSchemeByUserID userid
             case mcallbackschema of
                  Just (SalesforceScheme rtoken) -> executeSalesforceCallback doc rtoken dacURL
                  _ -> executeStandardCallback doc dacURL

executeStandardCallback :: (AmazonMonad m, MonadDB m, Log.MonadLog m, MonadIO m, MonadReader c m, HasSalesforceConf c) => Document -> String -> m Bool
executeStandardCallback doc url = do
  dJSON <- documentJSONV1 Nothing False False True Nothing doc
  (exitcode, _ , stderr) <- readCurl
     [ "-X", "POST"
     , "-f" -- make curl return exit code (22) if it got anything else but 2XX
     , "-L" -- make curl follow redirects
     , "--data-binary", "@-"          -- take binary data from stdin
     , "-H", "Content-type: application/x-www-form-urlencoded"
     , url
     ] (BSL.fromString (urlEncodeVars [ ("documentid", show (documentid doc))
                                      , ("signedAndSealed", (if (isClosed doc && (isJust $ documentsealedfile doc))
                                                             then "true" else "false"))
                                      , ("json", encode dJSON)
                                      ]))
  case exitcode of
    ExitSuccess -> do
      Log.mixlog "API callback executeStandardCallback succeeded" $ do
        value "document_id" (show (documentid doc))
        value "url" url
      return True
    ExitFailure ec -> do
      Log.attention "API callback executeStandardCallback failed" $ do
        value "document_id" (show (documentid doc))
        value "url" url
        value "curl_exitcode" ec
        value "stderr" (BSL.toString stderr)
      return False

executeSalesforceCallback :: (MonadDB m, Log.MonadLog m, MonadIO m, MonadReader c m, HasSalesforceConf c) => Document -> String ->  String -> m Bool
executeSalesforceCallback doc rtoken url = do
  mtoken <- getAccessTokenFromRefreshToken rtoken
  case mtoken of
       Left _ -> return False
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
                    ExitFailure _ -> (Log.mixlog_ $ "Salesforce API callback for #" ++ show (documentid doc)  ++ " failed: " ++ BSL.toString stderr) >> return False
