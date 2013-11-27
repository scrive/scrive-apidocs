module Doc.API.Callback.Execute ( execute ) where

import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)

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
import Doc.DocView (documentJSON)
import Amazon
import Network.HTTP as HTTP

execute :: (AmazonMonad m, MonadDB m, MonadIO m, MonadReader c m, HasSalesforceConf c) => DocumentAPICallback -> m Bool
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

executeStandardCallback :: (AmazonMonad m, MonadDB m, MonadIO m, MonadReader c m, HasSalesforceConf c) => Document -> String -> m Bool
executeStandardCallback doc url = do
  dJSON <- documentJSON Nothing False False True Nothing Nothing doc
  (exitcode, _ , stderr) <- readCurl [
                "-X", "POST"
              , "-d", urlEncodeVars [
                          ("documentid", show (documentid doc))
                        , ("signedAndSealed", (if (isClosed doc && (isJust $ documentsealedfile doc)) then "true" else "false") )
                        , ("json", encode dJSON)
                      ]
              , url
              ] BSL.empty
  case exitcode of
              ExitSuccess -> return True
              ExitFailure _ -> (Log.debug $ "API callback for #" ++ show (documentid doc)  ++ " failed: " ++ BSL.toString stderr) >> return False

executeSalesforceCallback :: (MonadDB m, MonadIO m, MonadReader c m, HasSalesforceConf c) => Document -> String ->  String -> m Bool
executeSalesforceCallback doc rtoken url = do
  mtoken <- getAccessTokenFromRefreshToken rtoken
  case mtoken of
       Nothing -> return False
       Just token -> do
        (exitcode, _ , stderr) <- readCurl [
                      "-X", "POST"
                    , "-d",  urlEncodeVars [
                              ("documentid", show (documentid doc))
                            , ("signedAndSealed", (if (isClosed doc && (isJust $ documentsealedfile doc)) then "true" else "false") )
                          ]
                    , "-H", "Authorization: Bearer " ++ token
                    , url
                    ] BSL.empty
        case exitcode of
                    ExitSuccess -> return True
                    ExitFailure _ -> (Log.debug $ "Salesforce API callback for #" ++ show (documentid doc)  ++ " failed: " ++ BSL.toString stderr) >> return False
