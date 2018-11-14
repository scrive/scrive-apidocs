module UserGroup.Types.Subscription (
    Subscription(..)
  , getSubscription
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Time
import Data.Unjson

import Chargeable.Model (GetNumberOfDocumentsStartedThisMonth(..))
import DB
import FeatureFlags.Model
import User.Model.Query (UserGroupGetUsers(..))
import UserGroup.Types
import UserGroup.Types.PaymentPlan
import UserGroup.Model
import Util.MonadUtils

data Subscription = Subscription {
    ugSubInvoicingType        :: InvoicingType
  , ugSubPaymentPlan          :: Maybe PaymentPlan
  , ugSubInheritedPaymentPlan :: Maybe PaymentPlan
  , ugSubCountUsers           :: Maybe Int
  , ugSubCountDocsMTD         :: Maybe Int
  , ugSubFeatures             :: Features
} deriving (Eq, Ord, Show)

instance Unjson Subscription where
  unjsonDef = objectOf $ Subscription
    <$> field "invoicing_type" ugSubInvoicingType "Invoicing type"
    <*> fieldOpt "payment_plan" ugSubPaymentPlan "Payment plan"
    <*> fieldOpt "inherited_plan" ugSubInheritedPaymentPlan "Inherited payment plan"
    <*> fieldOpt "number_of_users" ugSubCountUsers "Number of active users"
    <*> fieldOpt "started_last_month" ugSubCountDocsMTD "Number of documents started month to date"
    <*> field "features" ugSubFeatures "Features enabled"

getSubscription :: (MonadDB m, MonadBase IO m, MonadThrow m, MonadTime m) => UserGroup -> m Subscription
getSubscription ug = do
  let ugid = get ugID ug
  users <- dbQuery $ UserGroupGetUsers ugid
  ugwps <- guardJustM . dbQuery $ UserGroupGetWithParents ugid
  docsMTD  <- fromIntegral <$> dbQuery (GetNumberOfDocumentsStartedThisMonth ugid)
  fs <- getFeaturesFor ugid
  return Subscription { ugSubInvoicingType = ugInvoicingType ug
                      , ugSubPaymentPlan = ugPaymentPlan ug
                      , ugSubInheritedPaymentPlan = ugwpInheritedPaymentPlan ugwps
                      , ugSubCountUsers = Just $ length users
                      , ugSubCountDocsMTD = Just docsMTD
                      , ugSubFeatures = fs
                      }
