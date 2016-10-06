module Salesforce.Conf (
      SalesforceConf(..)
  ) where

import Data.Unjson

import KontraPrelude

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
  , salesforceErrorEmail :: Maybe String
} deriving (Show, Eq, Ord)

unjsonSalesforceConf :: UnjsonDef SalesforceConf
unjsonSalesforceConf = objectOf $ pure SalesforceConf
  <*> field "authentication_url"
      salesforceAuthenticationUrl
      "SalesForce OAuth authentication url"
  <*> field "token_url"
      salesforceTokenUrl
      "SalesForce OAuth token url"
  <*> field "consumer_key"
      salesforceConsumerKey
      "SalesForce OAuth consumer key"
  <*> field "consumer_secret"
      salesforceConsumerSecret
      "SalesForce OAuth consumer secret"
  <*> field "redirect_url"
      salesforceRedirectUrl
      "SalesForce OAuth redirect url"
  <*> field "api_token"
      salesforceIntegrationAPIToken
      "SalesForce OAuth API token"
  <*> field "api_secret"
      salesforceIntegrationAPISecret
      "SalesForce OAuth API secret"
  <*> fieldOpt "error_email"
      salesforceErrorEmail
      "SalesForce Error email"

instance Unjson SalesforceConf where
  unjsonDef = unjsonSalesforceConf
