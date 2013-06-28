module Salesforce.Conf (
      SalesforceConf(..)
    , HasSalesforceConf(..)
    , withSalesforceConf
  ) where

import Control.Monad.Reader

data SalesforceConf = SalesforceConf {
    salesforceAuthenticationUrl :: String
  , salesforceTokenUrl :: String
  , salesforceConsumerKey :: String
  , salesforceConsumerSecret :: String
  , salesforceRedirectUrl :: String
  } deriving (Show, Read, Eq, Ord)

class HasSalesforceConf c where
  getSalesforceConf :: c -> SalesforceConf
  getSalesforceConfM :: (MonadReader c m, HasSalesforceConf c) => m SalesforceConf
  getSalesforceConfM =  ask >>= return . getSalesforceConf


instance HasSalesforceConf SalesforceConf where
  getSalesforceConf = id

withSalesforceConf :: (HasSalesforceConf c, Monad m) => c -> ReaderT SalesforceConf m a -> m a
withSalesforceConf c m = runReaderT m (getSalesforceConf c)