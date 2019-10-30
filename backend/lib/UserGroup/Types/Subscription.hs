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

data Subscription = Subscription
  { ugSubInvoicingType        :: InvoicingType
  , ugSubPaymentPlan          :: Maybe PaymentPlan
  , ugSubInheritedPaymentPlan :: Maybe PaymentPlan
  , ugSubCountUsers           :: Maybe Int
  , ugSubCountDocsMTD         :: Maybe Int
  , ugSubFeatures             :: Maybe Features
  , ugSubInheritedFeatures    :: Maybe Features
  , ugSubFeaturesIsInherited  :: Bool
  } deriving (Eq, Ord, Show)

instance Unjson Subscription where
  unjsonDef =
    objectOf
      $   Subscription
      <$> field "invoicing_type" ugSubInvoicingType "Invoicing type"
      <*> fieldOpt "payment_plan"    ugSubPaymentPlan          "Payment plan"
      <*> fieldOpt "inherited_plan"  ugSubInheritedPaymentPlan "Inherited payment plan"
      <*> fieldOpt "number_of_users" ugSubCountUsers           "Number of active users"
      <*> fieldOpt "started_last_month"
                   ugSubCountDocsMTD
                   "Number of documents started month to date"
      <*> fieldOpt "features" ugSubFeatures "Features enabled"
      <*> fieldOpt "inherited_features"
                   ugSubInheritedFeatures
                   "Inherited Features enabled"
      <*> field "features_is_inherited"
                ugSubFeaturesIsInherited
                "When enabled, features are inherited"

getSubscription
  :: (MonadDB m, MonadBase IO m, MonadThrow m, MonadTime m)
  => UserGroupWithParents
  -> m Subscription
getSubscription ugwp = do
  let ug   = ugwpUG ugwp
      ugid = ugID ug
  users   <- dbQuery $ UserGroupGetUsers ugid
  docsMTD <- fromIntegral <$> dbQuery (GetNumberOfDocumentsStartedThisMonth ugid)
  let mInheritedFeatures = ugwpFeatures <$> ugwpOnlyParents ugwp
  return Subscription
    { ugSubInvoicingType        = ugInvoicingType ug
    , ugSubPaymentPlan          = ugPaymentPlan ug
    , ugSubInheritedPaymentPlan = ugwpPaymentPlan <$> ugwpOnlyParents ugwp
    , ugSubCountUsers           = Just $ length users
    , ugSubCountDocsMTD         = Just docsMTD
    , ugSubFeatures             = ugFeatures ug
    , ugSubInheritedFeatures    = mInheritedFeatures
    , ugSubFeaturesIsInherited  = isNothing $ ugFeatures ug
    }
