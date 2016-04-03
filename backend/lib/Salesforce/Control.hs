module Salesforce.Control (
     handleSalesforceIntegration
   , getSalesforceKeys
  ) where

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

handleSalesforceIntegration :: Kontrakcja m => m (Either KontraLink KontraLink)
handleSalesforceIntegration  = withUserGet $ do
  ctx <- getContext
  mcode <- getField "code"
  mstate <- getField "state" -- Internall salesforce param. We use it for holding url, where we will redirect user after authorization flow is done.
  case (mcode) of
       (Just code) -> do
         mtoken <- withSalesforceConf ctx (getRefreshTokenFromCode code)
         case mtoken of
              Left _ -> internalError
              Right token -> do
                dbUpdate $ UpdateUserCallbackScheme (userid $ $fromJust $ ctxmaybeuser ctx) (SalesforceScheme token)
                return $ fromMaybe LinkDesignView (LinkExternal <$> mstate)
       _ ->  LinkExternal <$> (withSalesforceConf ctx (initAuthorizationWorkflowUrl mstate))

{- Returns access keys for salesforce user. User by They salesfroce plugin to start propper oauth wokflow. Keys are hardcodded in config file. -}
getSalesforceKeys :: Kontrakcja m => m JSValue
getSalesforceKeys = do
  sc <- getSalesforceConf <$> getContext
  runJSONGenT $ do
    value "token"  $ salesforceIntegrationAPIToken sc
    value "secret" $ salesforceIntegrationAPISecret sc
