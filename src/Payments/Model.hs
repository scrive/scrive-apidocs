module Payments.Model where

import Data.Int (Int64)
--import Control.Monad.Trans.Error
import Control.Monad.Base
import Data.Typeable
import Control.Applicative
import Data.Maybe

import MinutesTime
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

data PaymentPlan = UserPaymentPlan    { ppAccountCode      :: AccountCode
                                      , ppUserID           :: UserID
                                      , ppPricePlan        :: PricePlan 
                                      , ppPendingPricePlan :: PricePlan    -- plan next month
                                      , ppStatus           :: PaymentPlanStatus
                                      , ppPendingStatus    :: PaymentPlanStatus -- status next month
                                      , ppQuantity         :: Int
                                      , ppPendingQuantity  :: Int
                                      }
                 | CompanyPaymentPlan { ppAccountCode      :: AccountCode
                                      , ppCompanyID        :: CompanyID
                                      , ppPricePlan        :: PricePlan 
                                      , ppPendingPricePlan :: PricePlan    -- plan next month
                                      , ppStatus           :: PaymentPlanStatus
                                      , ppPendingStatus    :: PaymentPlanStatus -- status next month
                                      , ppQuantity         :: Int
                                      , ppPendingQuantity  :: Int
                                      }
accountType :: PaymentPlan -> Int
accountType (UserPaymentPlan   {}) = 1
accountType (CompanyPaymentPlan{}) = 2                   
                   
data PaymentPlanStatus = ActiveStatus      -- everything is great (unblocked)
                       | OverdueStatus     -- didn't pay!
                       | CanceledStatus    -- they canceled
                       | DeactivatedStatus -- account overdue, something went wrong
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
      sqlResult "quantity"
      sqlResult "plan_pending"
      sqlResult "status_pending"
      sqlResult "quantity_pending"
      case eid of
        Left  uid -> sqlWhereEq "user_id"    uid
        Right cid -> sqlWhereEq "company_id" cid
    listToMaybe <$> foldDB f []
      where f :: [PaymentPlan] -> AccountCode -> Int -> Maybe UserID -> Maybe CompanyID -> PricePlan -> PaymentPlanStatus -> Int -> PricePlan -> PaymentPlanStatus -> Int -> [PaymentPlan]
            f acc ac 1 (Just uid) _ p s q pp sp qp = 
              UserPaymentPlan    { ppAccountCode      = ac
                                 , ppUserID           = uid
                                 , ppPricePlan        = p
                                 , ppPendingPricePlan = pp
                                 , ppStatus           = s
                                 , ppPendingStatus    = sp
                                 , ppQuantity         = q
                                 , ppPendingQuantity  = qp } : acc
            f acc ac 2 _ (Just cid) p s q pp sp qp = 
              CompanyPaymentPlan { ppAccountCode      = ac
                                 , ppCompanyID        = cid
                                 , ppPricePlan        = p
                                 , ppPendingPricePlan = pp
                                 , ppStatus           = s
                                 , ppPendingStatus    = sp
                                 , ppQuantity         = q
                                 , ppPendingQuantity  = qp } : acc
            f acc _  _ _ _   _ _ _ _ _ _ = acc -- ignore other codes to get rid of warning


-- update payment_plans set 

data SavePaymentPlan = SavePaymentPlan PaymentPlan MinutesTime
instance MonadDB m => DBUpdate m SavePaymentPlan Bool where
  update (SavePaymentPlan pp tm) = do
    kPrepare $ "UPDATE payment_plans " ++
               "SET account_type = ?, user_id = ?, company_id = ? " ++
               ",   plan = ?, status = ? " ++ 
               ",   plan_pending = ?, status_pending = ? " ++
               ",   quantity = ?, quantity_pending = ? " ++
               ",   sync_date = ? " ++
               "WHERE account_code = ? "
    r <- kExecute [toSql $ accountType pp
                  ,toSql $ if 1 == accountType pp then Just $ ppUserID    pp else Nothing
                  ,toSql $ if 2 == accountType pp then Just $ ppCompanyID pp else Nothing
                  ,toSql $ ppPricePlan pp
                  ,toSql $ ppStatus pp
                  ,toSql $ ppPendingPricePlan pp
                  ,toSql $ ppPendingStatus pp
                  ,toSql $ ppQuantity pp
                  ,toSql $ ppPendingQuantity pp
                  ,toSql $ tm
                  ,toSql $ ppAccountCode pp]
    case r of
      1 -> return True
      _ -> do
        kPrepare $ "INSERT INTO payment_plans (account_code, plan, status, plan_pending, status_pending, quantity, quantity_pending, sync_date, account_type, user_id, company_id) " ++
          "SELECT ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? " ++
          "WHERE ? NOT IN (SELECT account_code FROM payment_plans) "
        r' <- kExecute $ [toSql $ ppAccountCode pp
                         ,toSql $ ppPricePlan pp
                         ,toSql $ ppStatus pp
                         ,toSql $ ppPendingPricePlan pp 
                         ,toSql $ ppPendingStatus pp
                         ,toSql $ ppQuantity pp
                         ,toSql $ ppPendingQuantity pp
                         ,toSql $ tm] ++
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
  showsPrec _ ActiveStatus      = (++) "active"
  showsPrec _ OverdueStatus     = (++) "overdue"
  showsPrec _ CanceledStatus    = (++) "canceled"
  showsPrec _ DeactivatedStatus = (++) "deactivated"  

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
  safeConvert ActiveStatus      = return 1
  safeConvert OverdueStatus     = return 2
  safeConvert CanceledStatus    = return 3
  safeConvert DeactivatedStatus = return 4

instance Convertible Int PaymentPlanStatus where
  safeConvert 1 = return ActiveStatus
  safeConvert 2 = return OverdueStatus
  safeConvert 3 = return CanceledStatus
  safeConvert 4 = return DeactivatedStatus
  safeConvert s = Left ConvertError { convSourceValue  = show s
                                    , convSourceType   = "Int"
                                    , convDestType     = "PaymentPlanStatus"
                                    , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                    }

instance Convertible PaymentPlanStatus SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue PaymentPlanStatus where
  safeConvert s = safeConvert (fromSql s :: Int)
  
