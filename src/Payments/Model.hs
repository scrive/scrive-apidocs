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
--import Misc

-- new data types

data PricePlan = FreePricePlan
               | BasicPricePlan    
               | BrandingPricePlan
               | AdvancedPricePlan
               | EnterprisePricePlan  -- when they are invoiced
               deriving (Eq, Ord)
                 
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

{- | Get a new, unique account code. -}
data GetAccountCode = GetAccountCode
instance (MonadBase IO m, MonadDB m) => DBUpdate m GetAccountCode AccountCode where
  update GetAccountCode = do
    kPrepare $ "SELECT nextval('payment_plans_account_code_seq')"
    _ <- kExecute []
    results <- foldDB (flip (:)) []
    case results of
      [x] -> return x
      _ -> internalError -- should never happen
      
{- | Get the quantity of users that should be charged in a company. -}
data GetCompanyQuantity = GetCompanyQuantity CompanyID
instance (MonadBase IO m, MonadDB m) => DBQuery m GetCompanyQuantity Int where
  query (GetCompanyQuantity cid) = do
    kRun_ $ sqlSelect "users" $ do
      sqlWhereEq "company_id" cid
      sqlWhereEq "is_free"    False
      sqlResult "count(id)"
    res <- foldDB (flip (:)) []
    case res of
      [x] -> return x
      _   -> internalError

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
    kPrepare $ "UPDATE payment_plans " ++
               "SET plan = ?, status = ? " ++ 
               "WHERE account_code = ? "
    r <- kExecute [toSql $ ppPricePlan pp
                  ,toSql $ ppStatus pp
                  ,toSql $ ppAccountCode pp]
    case r of
      1 -> return True
      _ -> do
        kPrepare $ "INSERT INTO payment_plans (account_code, plan, status, account_type, user_id, company_id) " ++
          "SELECT ?, ?, ?, ?, ?, ? " ++
          "WHERE ? NOT IN (SELECT account_code FROM payment_plans) "
        r' <- kExecute $ [toSql $ ppAccountCode pp
                         ,toSql $ ppPricePlan pp
                         ,toSql $ ppStatus pp] ++
              case pp of
                UserPaymentPlan {} -> [toSql (1::Int)
                                      ,toSql $ ppUserID pp
                                      ,toSql (Nothing :: Maybe CompanyID)]
                CompanyPaymentPlan {} -> [toSql (2::Int)
                                         ,toSql (Nothing :: Maybe UserID)
                                         ,toSql $ ppCompanyID pp]
              ++ [toSql $ ppAccountCode pp]
        return $ r' == 1

-- how do things look as a string?
    
instance Show PricePlan where
  showsPrec _ FreePricePlan       = (++) "free"
  showsPrec _ BasicPricePlan      = (++) "basic"
  showsPrec _ BrandingPricePlan   = (++) "branding"
  showsPrec _ AdvancedPricePlan   = (++) "advanced"
  showsPrec _ EnterprisePricePlan = (++) "enterprise"

instance Read PricePlan where
  readsPrec _ "free"       = [(FreePricePlan,     "")]
  readsPrec _ "basic"      = [(BasicPricePlan,    "")]
  readsPrec _ "branding"   = [(BrandingPricePlan, "")]
  readsPrec _ "advanced"   = [(AdvancedPricePlan, "")]
  readsPrec _ "enterprise" = [(EnterprisePricePlan,  "")]
  readsPrec _ _            = []

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
  safeConvert EnterprisePricePlan  = return 100

instance Convertible Int PricePlan where
  safeConvert 0   = return FreePricePlan
  safeConvert 10  = return BasicPricePlan
  safeConvert 11  = return BrandingPricePlan
  safeConvert 12  = return AdvancedPricePlan
  safeConvert 100 = return EnterprisePricePlan
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
  
