module Salesforce.Control (
     handleSalesforceIntegration
   , getSalesforceKeys
  ) where

import Control.Monad.Reader
import Data.Functor
import Text.JSON
import Text.JSON.Gen

import DB
import Happstack.Fields
import Kontra
import KontraLink
import KontraPrelude
import Salesforce.AuthorizationWorkflow
import Salesforce.Conf
import User.CallbackScheme.Model
import User.Model
import User.Utils

{- This handlers sets SalesforceScheme for callbacks for give user -}
handleSalesforceIntegration :: Kontrakcja m => m (Redir KontraLink)
handleSalesforceIntegration  = withUser $ \user -> do
  ctx <- getContext
  case ctxsalesforceconf ctx of
    Nothing -> noConfigurationError "Salesforce"
    Just sc -> do
      mcode <- getField "code"
      mstate <- getField "state" -- Internall salesforce param. We use it for holding url, where we will redirect user after authorization flow is done.
      case mcode of
        Nothing   -> LinkExternal <$> (flip runReaderT sc (initAuthorizationWorkflowUrl mstate))
        Just code -> do
          mtoken <- flip runReaderT sc (getRefreshTokenFromCode code)
          case mtoken of
            Left _      -> internalError
            Right token -> do
              dbUpdate $ UpdateUserCallbackScheme (userid user) (SalesforceScheme token)
              return $ fromMaybe LinkDesignView (LinkExternal <$> mstate)

{- Returns access keys for salesforce user. User by They salesfroce plugin to start propper oauth wokflow. Keys are hardcodded in config file. -}
getSalesforceKeys :: Kontrakcja m => m JSValue
getSalesforceKeys = do
  ctx <- getContext
  case ctxsalesforceconf ctx of
    Nothing -> noConfigurationError "Salesforce"
    Just sc ->
      runJSONGenT $ do
        value "token"  $ salesforceIntegrationAPIToken sc
        value "secret" $ salesforceIntegrationAPISecret sc
