module Payments.Model where

import Data.Int (Int64)
--import Control.Monad.Trans.Error
import Control.Monad.Base
import Data.Typeable
import Control.Applicative
import Data.Maybe

import User.Model
import Company.Model
import DB
import DB.SQL2
import KontraError

-- new data types

data PricePlan = FreePricePlan     -- when a user has downgraded
               | BasicPricePlan    
               | BrandingPricePlan
               | AdvancedPricePlan
               | InvoicePricePlan  -- when they are invoiced
               deriving (Eq)
                 
newtype AccountCode = AccountCode Int64
                    deriving (Eq, Ord, Typeable)

data PaymentPlan = UserPaymentPlan    { ppAccountCode :: AccountCode
                                      , ppUserID      :: UserID
                                      , ppPricePlan   :: PricePlan 
                                      , ppStatus      :: PaymentPlanStatus
                                      }
                 | CompanyPaymentPlan { ppAccountCode :: AccountCode
                                      , ppCompanyID   :: CompanyID
                                      , ppPricePlan   :: PricePlan 
                                      , ppStatus      :: PaymentPlanStatus
                                      }
                   
data PaymentPlanStatus = ActiveStatus   -- everything is great (unblocked)
                       | InactiveStatus -- account overdue, something went wrong
                       | PendingStatus  -- pending approval (and push notification)
                       deriving (Eq)
                                
-- db operations
data GetAccountCode = GetAccountCode
instance (MonadBase IO m, MonadDB m) => DBUpdate m GetAccountCode AccountCode where
  update GetAccountCode = do
    kPrepare $ "SELECT nextval('payment_plans_account_code_seq')"
    _ <- kExecute []
    results <- foldDB (flip (:)) []
    case results of
      [x] -> return x
      _ -> internalError -- should never happen
      
data DeletePaymentPlan = DeletePaymentPlan (Either UserID CompanyID)
instance (MonadDB m) => DBUpdate m DeletePaymentPlan () where
  update (DeletePaymentPlan eid) = do
    kRun_ $ sqlDelete "payment_plans" $ do
      case eid of
        Left  uid -> sqlWhereEq "payment_plans.user_id" uid
        Right cid -> sqlWhereEq "payment_plans.company_id" cid

data GetPaymentPlan = GetPaymentPlan (Either UserID CompanyID)
instance (MonadDB m) => DBQuery m GetPaymentPlan (Maybe PaymentPlan) where
  query (GetPaymentPlan eid) = do
    kRun_ $ sqlSelect "payment_plans" $ do
      sqlResult "account_code"
      sqlResult "account_type"
      sqlResult "user_id"
      sqlResult "company_id"
      sqlResult "plan"
      sqlResult "status"
      case eid of
        Left  uid -> sqlWhereEq "user_id"    uid
        Right cid -> sqlWhereEq "company_id" cid
    listToMaybe <$> foldDB f []
      where f :: [PaymentPlan] -> AccountCode -> Int -> Maybe UserID -> Maybe CompanyID -> PricePlan -> PaymentPlanStatus -> [PaymentPlan]
            f acc ac 1 (Just uid) _ p s = UserPaymentPlan    ac uid p s : acc
            f acc ac 2 _ (Just cid) p s = CompanyPaymentPlan ac cid p s : acc
            f acc _  _ _ _   _ _ = acc -- ignore other codes to get rid of warning


-- update payment_plans set 

data SavePaymentPlan = SavePaymentPlan PaymentPlan
instance MonadDB m => DBUpdate m SavePaymentPlan Bool where
  update (SavePaymentPlan pp) = do
    kRun_ $ sqlInsert "payment_plans" $ do
      sqlSet "account_code" $ ppAccountCode pp
      sqlSet "plan" $ ppPricePlan pp
      sqlSet "status" $ ppStatus pp
      case pp of
        UserPaymentPlan {} -> do
          sqlSet "account_type" (1 :: Int)
          sqlSet "user_id" $ ppUserID pp
        CompanyPaymentPlan {} -> do
          sqlSet "account_type" (2 :: Int)
          sqlSet "company_id" $ ppCompanyID pp
      sqlResult "account_code"
    (results :: [AccountCode]) <- foldDB (flip (:)) []
    return (1 == length results)
    
-- how do things look as a string?
    
instance Show PricePlan where
  showsPrec _ FreePricePlan     = (++) "free"
  showsPrec _ BasicPricePlan    = (++) "basic"
  showsPrec _ BrandingPricePlan = (++) "branding"
  showsPrec _ AdvancedPricePlan = (++) "advanced"
  showsPrec _ InvoicePricePlan  = (++) "invoiced"

instance Read PricePlan where
  readsPrec _ "free"     = [(FreePricePlan,     "")]
  readsPrec _ "basic"    = [(BasicPricePlan,    "")]
  readsPrec _ "branding" = [(BrandingPricePlan, "")]
  readsPrec _ "advanced" = [(AdvancedPricePlan, "")]
  readsPrec _ "invoiced" = [(InvoicePricePlan,  "")]
  readsPrec _ _          = []

instance Show PaymentPlanStatus where
  showsPrec _ ActiveStatus   = (++) "active"
  showsPrec _ InactiveStatus = (++) "inactive"
  showsPrec _ PendingStatus  = (++) "pending"

-- conversions for cramming values into the database
instance Convertible PricePlan Int where
  safeConvert FreePricePlan     = return 0
  safeConvert BasicPricePlan    = return 10
  safeConvert BrandingPricePlan = return 11
  safeConvert AdvancedPricePlan = return 12
  safeConvert InvoicePricePlan  = return 100

instance Convertible Int PricePlan where
  safeConvert 0   = return FreePricePlan
  safeConvert 10  = return BasicPricePlan
  safeConvert 11  = return BrandingPricePlan
  safeConvert 12  = return AdvancedPricePlan
  safeConvert 100 = return InvoicePricePlan
  safeConvert s = Left ConvertError { convSourceValue  = show s
                                    , convSourceType   = "Int"
                                    , convDestType     = "PricePlan"
                                    , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                    }

instance Convertible PricePlan SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue PricePlan where
  safeConvert s = safeConvert (fromSql s :: Int)

$(newtypeDeriveConvertible ''AccountCode)
$(newtypeDeriveUnderlyingReadShow ''AccountCode)

instance Convertible PaymentPlanStatus Int where
  safeConvert ActiveStatus   = return 0
  safeConvert InactiveStatus = return 10
  safeConvert PendingStatus  = return 100

instance Convertible Int PaymentPlanStatus where
  safeConvert 0   = return ActiveStatus
  safeConvert 10  = return InactiveStatus
  safeConvert 100 = return PendingStatus
  safeConvert s   = Left ConvertError { convSourceValue  = show s
                                      , convSourceType   = "Int"
                                      , convDestType     = "PaymentPlanStatus"
                                      , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                      }

instance Convertible PaymentPlanStatus SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue PaymentPlanStatus where
  safeConvert s = safeConvert (fromSql s :: Int)
  
