module UserGroup.Types.Subscription (
    Subscription(..)
  , getSubscription
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Time
import Data.Int
import Data.Time.Clock
import Data.Unjson

import DB
import FeatureFlags.Model
import MinutesTime
import User.Model.Query (UserGroupGetUsers(..))
import UserGroup.FreeDocumentTokens.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan

data Subscription = Subscription
  { ugSubInvoicingType        :: InvoicingType
  , ugSubPaymentPlan          :: Maybe PaymentPlan
  , ugSubInheritedPaymentPlan :: Maybe PaymentPlan
  , ugSubCountUsers           :: Maybe Int
  , ugSubValidDocTokensCount  :: Int32
  , ugSubDocTokensValidTill   :: UTCTime
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
      <*> fieldDef "free_document_tokens"
                   0
                   ugSubValidDocTokensCount
                   "Number of free document tokens"
      <*> fieldDef "free_document_tokens_valid_till"
                   unixEpoch
                   ugSubDocTokensValidTill
                   "Free document tokens validity date"
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
      ugid = ug ^. #id
  users              <- dbQuery $ UserGroupGetUsers ugid
  freeTokens         <- dbQuery (UserGroupFreeDocumentTokensGet ugid)
  freeTokensCount    <- numberOfValidTokens freeTokens
  freeTokensValidity <- validityOfTokens freeTokens
  let mInheritedFeatures = ugwpFeatures <$> ugwpOnlyParents ugwp
  return Subscription
    { ugSubInvoicingType        = ugInvoicingType ug
    , ugSubPaymentPlan          = ugPaymentPlan ug
    , ugSubInheritedPaymentPlan = ugwpPaymentPlan <$> ugwpOnlyParents ugwp
    , ugSubCountUsers           = Just $ length users
    , ugSubValidDocTokensCount  = freeTokensCount
    , ugSubDocTokensValidTill   = freeTokensValidity
    , ugSubFeatures             = ug ^. #features
    , ugSubInheritedFeatures    = mInheritedFeatures
    , ugSubFeaturesIsInherited  = isNothing $ ug ^. #features
    }
