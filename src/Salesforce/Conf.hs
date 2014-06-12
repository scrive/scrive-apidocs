module Salesforce.Conf (
      SalesforceConf(..)
    , HasSalesforceConf(..)
    , withSalesforceConf
  ) where

import Control.Monad.Reader
import Control.Applicative
import Data.Unjson

data SalesforceConf = SalesforceConf {
    salesforceAuthenticationUrl :: String
  , salesforceTokenUrl :: String
  , salesforceConsumerKey :: String
  , salesforceConsumerSecret :: String
  , salesforceRedirectUrl :: String
    -- oauth_access_token for user that is coresponding to SalesForce
    -- service in our system. It should be hardcoded in salesforce
    -- plugin, but due to their policy it can't.
  , salesforceIntegrationAPIToken  :: String
  , salesforceIntegrationAPISecret :: String
} deriving (Show, Read, Eq, Ord)

unjsonSalesforceConf :: UnjsonDef SalesforceConf
unjsonSalesforceConf = objectOf $ pure SalesforceConf
  <*> field' "authentication_url"
      salesforceAuthenticationUrl
      "SalesForce OAuth authentication url"
  <*> field' "token_url"
      salesforceTokenUrl
      "SalesForce OAuth token url"
  <*> field' "consumer_key"
      salesforceConsumerKey
      "SalesForce OAuth consumer key"
  <*> field' "consumer_secret"
      salesforceConsumerSecret
      "SalesForce OAuth consumer secret"
  <*> field' "redirect_url"
      salesforceRedirectUrl
      "SalesForce OAuth redirect url"
  <*> field' "api_token"
      salesforceIntegrationAPIToken
      "SalesForce OAuth API token"
  <*> field' "api_secret"
      salesforceIntegrationAPISecret
      "SalesForce OAuth API secret"

instance Unjson SalesforceConf where
  unjsonDef = unjsonSalesforceConf

class HasSalesforceConf c where
  getSalesforceConf :: c -> SalesforceConf
  getSalesforceConfM :: (MonadReader c m, HasSalesforceConf c) => m SalesforceConf
  getSalesforceConfM =  ask >>= return . getSalesforceConf


instance HasSalesforceConf SalesforceConf where
  getSalesforceConf = id

withSalesforceConf :: (HasSalesforceConf c, Monad m) => c -> ReaderT SalesforceConf m a -> m a
withSalesforceConf c m = runReaderT m (getSalesforceConf c)
