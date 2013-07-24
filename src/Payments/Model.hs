{-# LANGUAGE StandaloneDeriving #-}

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
               | TeamPricePlan
               | FormPricePlan
               | EnterprisePricePlan  -- nothing gets blocked
               | TrialTeamPricePlan -- team plan for 3 months
               deriving (Eq, Ord)

newtype AccountCode = AccountCode Int64
                    deriving (Eq, Ord, Typeable)

-- | Data structure for storing in DB
data PaymentPlan = PaymentPlan    { ppAccountCode         :: AccountCode
                                  , ppCompanyID           :: CompanyID
                                  , ppPricePlan           :: PricePlan
                                  , ppPendingPricePlan    :: PricePlan    -- plan next month
                                  , ppStatus              :: PaymentPlanStatus
                                  , ppPendingStatus       :: PaymentPlanStatus -- status next month
                                  , ppQuantity            :: Int
                                  , ppPendingQuantity     :: Int
                                  , ppPaymentPlanProvider :: PaymentPlanProvider
                                  , ppDunningStep         :: Maybe Int
                                  , ppDunningDate         :: Maybe MinutesTime
                                  , ppBillingEndDate      :: MinutesTime
                                  }
                 deriving (Eq)

data PaymentPlanStatus = ActiveStatus      -- everything is great (unblocked)
                       | OverdueStatus     -- didn't pay!
                       | CanceledStatus    -- they canceled
                       | DeactivatedStatus -- account overdue, something went wrong
                       deriving (Eq)

data PaymentPlanProvider = NoProvider
                         | RecurlyProvider
                         deriving (Eq, Show, Read)

-- db operations

{- | Get a new, unique account code. -}
data GetAccountCode = GetAccountCode -- tested
instance (MonadBase IO m, MonadDB m) => DBUpdate m GetAccountCode AccountCode where
  update GetAccountCode = do
    kRun_ $ SQL "SELECT nextval('payment_plans_account_code_seq')" []
    results <- kFold (flip (:)) []
    case results of
      [x] -> return x
      _ -> internalError -- should never happen

{- | Get the quantity of users that should be charged in a company. -}
data GetCompanyQuantity = GetCompanyQuantity CompanyID --tested
instance MonadDB m => DBQuery m GetCompanyQuantity Int where
  query (GetCompanyQuantity cid) = do
    _ <- kRun $ "SELECT count(email) " <+>
                "FROM ((SELECT users.email " <+>
                "       FROM users " <+>
                "       WHERE users.company_id = " <?> cid <+>
                "         AND users.deleted = FALSE)" <+>
                "      UNION " <+>
                "      (SELECT companyinvites.email " <+>
                "       FROM companyinvites " <+>
                "       WHERE companyinvites.company_id = " <?> cid <+>
                "         AND NOT EXISTS (SELECT 1 FROM users " <+>
                "                         WHERE email = companyinvites.email " <+>
                "                           AND company_id = " <?> cid <+> "))) AS emails"
    res <- kFold (flip (:)) []
    case res of
      (x:_) -> return x
      _     -> return 0

data DeletePaymentPlan = DeletePaymentPlan CompanyID
instance (MonadDB m) => DBUpdate m DeletePaymentPlan () where
  update (DeletePaymentPlan cid) = do
    kRun_ $ sqlDelete "payment_plans" $ do
       sqlWhereEq "payment_plans.company_id" cid

-- tested
data GetPaymentPlan = GetPaymentPlan CompanyID
instance (MonadDB m) => DBQuery m GetPaymentPlan (Maybe PaymentPlan) where
  query (GetPaymentPlan cid) = do
    kRun_ $ sqlSelect "payment_plans" $ do
      sqlResult "account_code"
      sqlResult "company_id"
      sqlResult "plan"
      sqlResult "status"
      sqlResult "quantity"
      sqlResult "plan_pending"
      sqlResult "status_pending"
      sqlResult "quantity_pending"
      sqlResult "provider"
      sqlResult "dunning_step"
      sqlResult "dunning_date"
      sqlResult "billing_ends"
      sqlWhereEq "company_id" cid
    listToMaybe <$> kFold fetchPaymentPlans []

-- tested
data GetPaymentPlanInactiveUser = GetPaymentPlanInactiveUser UserID
instance MonadDB m => DBQuery m GetPaymentPlanInactiveUser (Maybe PaymentPlan) where
  query (GetPaymentPlanInactiveUser uid) = do
    kRun_ $ sqlSelect "payment_plans" $ do
      sqlResult "account_code"
      sqlResult "payment_plans.company_id"
      sqlResult "plan"
      sqlResult "status"
      sqlResult "quantity"
      sqlResult "plan_pending"
      sqlResult "status_pending"
      sqlResult "quantity_pending"
      sqlResult "provider"
      sqlResult "dunning_step"
      sqlResult "dunning_date"
      sqlResult "billing_ends"
      sqlJoinOn "users" "payment_plans.user_id = users.id"
      sqlWhereEq "user_id" uid
      sqlWhereIsNULL "users.has_accepted_terms_of_service"
    listToMaybe <$> kFold fetchPaymentPlans []

-- tested
data GetPaymentPlanByAccountCode = GetPaymentPlanByAccountCode AccountCode
instance (MonadDB m) => DBQuery m GetPaymentPlanByAccountCode (Maybe PaymentPlan) where
  query (GetPaymentPlanByAccountCode ac) = do
    kRun_ $ sqlSelect "payment_plans" $ do
      sqlResult "account_code"
      sqlResult "company_id"
      sqlResult "plan"
      sqlResult "status"
      sqlResult "quantity"
      sqlResult "plan_pending"
      sqlResult "status_pending"
      sqlResult "quantity_pending"
      sqlResult "provider"
      sqlResult "dunning_step"
      sqlResult "dunning_date"
      sqlResult "billing_ends"
      sqlWhereEq "account_code" ac
    listToMaybe <$> kFold fetchPaymentPlans []

fetchPaymentPlans :: [PaymentPlan]       ->
                     AccountCode         ->
                     CompanyID           ->
                     PricePlan           ->
                     PaymentPlanStatus   ->
                     Int                 ->
                     PricePlan           ->
                     PaymentPlanStatus   ->
                     Int                 ->
                     PaymentPlanProvider ->
                     Maybe Int           ->
                     Maybe MinutesTime   ->
                     MinutesTime         ->
                     [PaymentPlan]
fetchPaymentPlans acc ac cid p s q pp sp qp pr mds mdd be =
    PaymentPlan { ppAccountCode                     = ac
                            , ppCompanyID           = cid
                            , ppPricePlan           = p
                            , ppPendingPricePlan    = pp
                            , ppStatus              = s
                            , ppPendingStatus       = sp
                            , ppQuantity            = q
                            , ppPendingQuantity     = qp
                            , ppPaymentPlanProvider = pr
                            , ppDunningStep         = mds
                            , ppDunningDate         = mdd
                            , ppBillingEndDate     = be} : acc

{- | How often, in days, do we sync with recurly? -}
daysBeforeSync :: Int
daysBeforeSync = 7

--tested
data PaymentPlansRequiringSync = PaymentPlansRequiringSync PaymentPlanProvider MinutesTime
instance MonadDB m => DBQuery m PaymentPlansRequiringSync [PaymentPlan] where
  query (PaymentPlansRequiringSync prov time) = do
    let past = daysBefore daysBeforeSync time
    kRun_ $ SQL ("SELECT account_code,payment_plans.company_id, plan, status, quantity, plan_pending, status_pending, quantity_pending, provider, dunning_step, dunning_date, billing_ends " <>
             "  FROM payment_plans " <>
             "  LEFT OUTER JOIN (SELECT c.company_id, count(email) AS q " <>
             "                   FROM ((SELECT company_id, users.email " <>
             "                          FROM users " <>
             "                          WHERE users.deleted = FALSE) " <>
             "                         UNION " <>
             "                         (SELECT companyinvites.company_id, companyinvites.email " <>
             "                          FROM companyinvites " <>
             "                          WHERE NOT EXISTS (SELECT 1 FROM users " <>
             "                                            WHERE users.email = companyinvites.email " <>
             "                                              AND users.company_id = companyinvites.company_id))) as c" <>
             "                   GROUP BY c.company_id) AS ccount" <>
             "              ON (ccount.company_id = payment_plans.company_id)" <>
             "  WHERE provider = ? " <>
             "    AND (sync_date < ? " <> -- stuff older than 7 days needs sync
             "     OR  (q > quantity " <> -- current # of users > cache
             "     OR   q <> quantity_pending)) ") -- current # of users <> pending cache
            [toSql prov, toSql past]
    kFold fetchPaymentPlans []

--tested
data PaymentPlansExpiredDunning = PaymentPlansExpiredDunning MinutesTime
instance MonadDB m => DBQuery m PaymentPlansExpiredDunning [PaymentPlan] where
  query (PaymentPlansExpiredDunning time) = do
    kRun_ $ SQL ("SELECT account_code, company_id, plan, status, quantity, " <>
             "    plan_pending, status_pending, quantity_pending, provider, dunning_step, dunning_date, billing_ends " <>
             "  FROM payment_plans " <>
             "  WHERE dunning_date < ? " <>
             "    AND provider = ? ")
            [toSql time, toSql RecurlyProvider]
    kFold fetchPaymentPlans []

-- tested
data SavePaymentPlan = SavePaymentPlan PaymentPlan MinutesTime
instance MonadDB m => DBUpdate m SavePaymentPlan Bool where
  update (SavePaymentPlan pp tm) = do
    r <- kRun $ SQL ("UPDATE payment_plans " <>
               "SET company_id = ? " <>
               ",   plan = ?, status = ? " <>
               ",   plan_pending = ?, status_pending = ? " <>
               ",   quantity = ?, quantity_pending = ? " <>
               ",   provider = ? " <>
               ",   sync_date = ? " <>
               ",   dunning_step = ? " <>
               ",   dunning_date = ? " <>
               ",   billing_ends = ? " <>
               "WHERE account_code = ? ")
                  [
                   toSql $ ppCompanyID pp
                  ,toSql $ ppPricePlan pp
                  ,toSql $ ppStatus pp
                  ,toSql $ ppPendingPricePlan pp
                  ,toSql $ ppPendingStatus pp
                  ,toSql $ ppQuantity pp
                  ,toSql $ ppPendingQuantity pp
                  ,toSql $ ppPaymentPlanProvider pp
                  ,toSql $ tm
                  ,toSql $ ppDunningStep pp
                  ,toSql $ ppDunningDate pp
                  ,toSql $ ppBillingEndDate pp
                  ,toSql $ ppAccountCode pp]
    case r of
      1 -> return True
      _ -> do
        r' <- kRun $ SQL ("INSERT INTO payment_plans (account_code, plan, status, plan_pending, status_pending, quantity, " <>
          "quantity_pending, provider, sync_date, dunning_step, dunning_date, billing_ends, company_id) " <>
          "SELECT ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? " <>
          "WHERE ? NOT IN (SELECT account_code FROM payment_plans) ")
                         [toSql $ ppAccountCode pp
                         ,toSql $ ppPricePlan pp
                         ,toSql $ ppStatus pp
                         ,toSql $ ppPendingPricePlan pp
                         ,toSql $ ppPendingStatus pp
                         ,toSql $ ppQuantity pp
                         ,toSql $ ppPendingQuantity pp
                         ,toSql $ ppPaymentPlanProvider pp
                         ,toSql $ tm
                         ,toSql $ ppDunningStep pp
                         ,toSql $ ppDunningDate pp
                         ,toSql $ ppBillingEndDate pp
                         ,toSql $ ppCompanyID pp
                         ,toSql $ ppAccountCode pp]
        return $ r' == 1

-- how do things look as a string?

instance Show PricePlan where
  showsPrec _ FreePricePlan         = (++) "free"
  showsPrec _ TeamPricePlan         = (++) "team"
  showsPrec _ FormPricePlan         = (++) "form"
  showsPrec _ EnterprisePricePlan   = (++) "enterprise"
  showsPrec _ TrialTeamPricePlan    = (++) "trial"

instance Read PricePlan where
  readsPrec _ "free"         = [(FreePricePlan,         "")]
  readsPrec _ "team"         = [(TeamPricePlan,         "")]
  readsPrec _ "form"         = [(FormPricePlan,         "")]
  readsPrec _ "enterprise"   = [(EnterprisePricePlan,   "")]
  readsPrec _ "trial"        = [(TrialTeamPricePlan,    "")]
  readsPrec _ _              = []

instance Show PaymentPlanStatus where
  showsPrec _ ActiveStatus      = (++) "active"
  showsPrec _ OverdueStatus     = (++) "overdue"
  showsPrec _ CanceledStatus    = (++) "canceled"
  showsPrec _ DeactivatedStatus = (++) "deactivated"

instance Read PaymentPlanStatus where
  readsPrec _ "active"       = [(ActiveStatus,      "")]
  readsPrec _ "overdue"      = [(OverdueStatus,     "")]
  readsPrec _ "canceled"     = [(CanceledStatus,    "")]
  readsPrec _ "deactivated"  = [(DeactivatedStatus, "")]
  readsPrec _ _              = []

-- conversions for cramming values into the database
instance Convertible PricePlan Int where
  safeConvert FreePricePlan         = return 0
  safeConvert TeamPricePlan         = return 1
  safeConvert FormPricePlan         = return 2
  safeConvert EnterprisePricePlan   = return 3
  safeConvert TrialTeamPricePlan    = return 4

instance Convertible Int PricePlan where
  safeConvert 0  = return FreePricePlan
  safeConvert 1  = return TeamPricePlan
  safeConvert 2  = return FormPricePlan
  safeConvert 3  = return EnterprisePricePlan
  safeConvert 4  = return TrialTeamPricePlan
  safeConvert s  = Left ConvertError { convSourceValue  = show s
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

instance Convertible PaymentPlanProvider Int where
  safeConvert NoProvider      = return 0
  safeConvert RecurlyProvider = return 1

instance Convertible Int PaymentPlanProvider where
  safeConvert 0 = return NoProvider
  safeConvert 1 = return RecurlyProvider
  safeConvert s = Left ConvertError { convSourceValue  = show s
                                    , convSourceType   = "Int"
                                    , convDestType     = "PaymentPlanProvider"
                                    , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                    }

instance Convertible PaymentPlanProvider SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue PaymentPlanProvider where
  safeConvert s = safeConvert (fromSql s :: Int)

deriving instance Show PaymentPlan
