module Salesforce.Control (
    handleSalesforceIntegration
  ) where

import Data.Functor
import Kontra
import KontraLink
import Happstack.Fields
import Salesforce.AuthorizationWorkflow
import User.CallbackScheme.Model
import Salesforce.Conf
import User.Utils
import User.Model
import Data.Maybe
import DB


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
              Nothing -> internalError
              Just token -> do
                dbUpdate $ UpdateUserCallbackScheme (userid $ fromJust $ ctxmaybeuser ctx) (SalesforceScheme token)
                return $ fromMaybe LinkDesignView (LinkExternal <$> mstate)
       _ ->  LinkExternal <$> (withSalesforceConf ctx (initAuthorizationWorkflowUrl mstate))
