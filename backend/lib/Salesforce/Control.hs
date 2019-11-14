module Salesforce.Control (
     handleSalesforceIntegration
   , getSalesforceKeys
  ) where

import Control.Monad.Reader
import Text.JSON
import Text.JSON.Gen

import DB
import Happstack.Fields
import InternalResponse
import Kontra
import KontraLink
import Salesforce.AuthorizationWorkflow
import Salesforce.Conf
import User.CallbackScheme.Model
import User.Utils

{- This handlers sets SalesforceScheme for callbacks for a user -}
handleSalesforceIntegration :: Kontrakcja m => m InternalKontraResponse
handleSalesforceIntegration = withUser $ \user -> do
  ctx <- getContext
  case ctx ^. #salesforceConf of
    Nothing -> noConfigurationError "Salesforce"
    Just sc -> do
      mcode  <- getField "code"
      mstate <- getField "state" -- Internal salesforce param. We use
                                 -- it to hold url to redirect user
                                 -- after authorization flow is done.
      case mcode of
        Nothing ->
          (internalResponse . LinkExternal)
            <$> (flip runReaderT sc (initAuthorizationWorkflowUrl mstate))
        Just code -> do
          mtoken <- flip runReaderT sc (getRefreshTokenFromCode code)
          case mtoken of
            Left  _     -> internalError
            Right token -> do
              dbUpdate $ UpdateUserCallbackScheme (user ^. #id) (SalesforceScheme token)
              return $ internalResponse $ fromMaybe LinkDesignView
                                                    (LinkExternal <$> mstate)

{- Returns access keys for salesforce user. User by the salesforce plugin to start oauth wokflow. Keys are hardcoded in config file. -}
getSalesforceKeys :: Kontrakcja m => m JSValue
getSalesforceKeys = do
  ctx <- getContext
  case ctx ^. #salesforceConf of
    Nothing -> noConfigurationError "Salesforce"
    Just sc -> runJSONGenT $ do
      value "token" $ salesforceIntegrationAPIToken sc
      value "secret" $ salesforceIntegrationAPISecret sc
