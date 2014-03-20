module Payments.Model where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Base
import Data.Int
import Data.Monoid
import Data.Monoid.Space
import Data.Typeable
import qualified Control.Exception as E

import Company.Model
import DB
import MinutesTime

-- new data types

data PricePlan = FreePricePlan
               | TeamPricePlan
               | FormPricePlan
               | EnterprisePricePlan  -- nothing gets blocked
               | TrialPricePlan        -- a trial plan 
               deriving (Eq, Ord)

instance Show PricePlan where
  showsPrec _ FreePricePlan         = (++) "free"
  showsPrec _ TeamPricePlan         = (++) "team"
  showsPrec _ FormPricePlan         = (++) "form"
  showsPrec _ EnterprisePricePlan   = (++) "enterprise"
  showsPrec _ TrialPricePlan        = (++) "trial"

instance Read PricePlan where
  readsPrec _ "free"         = [(FreePricePlan,         "")]
  readsPrec _ "team"         = [(TeamPricePlan,         "")]
  readsPrec _ "form"         = [(FormPricePlan,         "")]
  readsPrec _ "enterprise"   = [(EnterprisePricePlan,   "")]
  readsPrec _ "trial"        = [(TrialPricePlan,        "")]
  readsPrec _ _              = []

instance PQFormat PricePlan where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL PricePlan where
  type PQBase PricePlan = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      0 -> return FreePricePlan
      1 -> return TeamPricePlan
      2 -> return FormPricePlan
      3 -> return EnterprisePricePlan
      4 -> return TrialPricePlan
      _ -> E.throwIO $ RangeError {
        reRange = [(0, 4)]
      , reValue = n
      }

instance ToSQL PricePlan where
  type PQDest PricePlan = PQDest Int16
  toSQL FreePricePlan       = toSQL (0::Int16)
  toSQL TeamPricePlan       = toSQL (1::Int16)
  toSQL FormPricePlan       = toSQL (2::Int16)
  toSQL EnterprisePricePlan = toSQL (3::Int16)
  toSQL TrialPricePlan      = toSQL (4::Int16)

newtype AccountCode = AccountCode Int64
  deriving (Eq, Ord, Typeable, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''AccountCode)

instance FromSQL AccountCode where
  type PQBase AccountCode = PQBase Int64
  fromSQL mbase = AccountCode <$> fromSQL mbase
instance ToSQL AccountCode where
  type PQDest AccountCode = PQDest Int64
  toSQL (AccountCode n) = toSQL n

-- | Data structure for storing in DB
data PaymentPlan = PaymentPlan {
  ppAccountCode         :: AccountCode
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
} deriving (Eq, Show)

data PaymentPlanStatus = ActiveStatus      -- everything is great (unblocked)
                       | OverdueStatus     -- didn't pay!
                       | CanceledStatus    -- they canceled
                       | DeactivatedStatus -- account overdue, something went wrong
                       deriving (Eq)

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

instance PQFormat PaymentPlanStatus where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL PaymentPlanStatus where
  type PQBase PaymentPlanStatus = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return ActiveStatus
      2 -> return OverdueStatus
      3 -> return CanceledStatus
      4 -> return DeactivatedStatus
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 4)]
      , reValue = n
      }

instance ToSQL PaymentPlanStatus where
  type PQDest PaymentPlanStatus = PQDest Int16
  toSQL ActiveStatus      = toSQL (1::Int16)
  toSQL OverdueStatus     = toSQL (2::Int16)
  toSQL CanceledStatus    = toSQL (3::Int16)
  toSQL DeactivatedStatus = toSQL (4::Int16)

data PaymentPlanProvider = NoProvider
                         | RecurlyProvider
                         deriving (Eq, Show, Read)

instance PQFormat PaymentPlanProvider where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL PaymentPlanProvider where
  type PQBase PaymentPlanProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      0 -> return NoProvider
      1 -> return RecurlyProvider
      _ -> E.throwIO $ RangeError {
        reRange = [(0, 1)]
      , reValue = n
      }

instance ToSQL PaymentPlanProvider where
  type PQDest PaymentPlanProvider = PQDest Int16
  toSQL NoProvider      = toSQL (0::Int16)
  toSQL RecurlyProvider = toSQL (1::Int16)

-- db operations

{- | Get a new, unique account code. -}
data GetAccountCode = GetAccountCode -- tested
instance (MonadBase IO m, MonadDB m) => DBUpdate m GetAccountCode AccountCode where
  update GetAccountCode = do
    runSQL_ "SELECT nextval('payment_plans_account_code_seq')"
    fetchOne unSingle

{- | Get the quantity of users that should be charged in a company. -}
data GetCompanyQuantity = GetCompanyQuantity CompanyID --tested
instance MonadDB m => DBQuery m GetCompanyQuantity Int where
  query (GetCompanyQuantity cid) = do
    runQuery_ $ "SELECT count(email) " <+>
                "FROM ((SELECT users.email " <+>
                "       FROM users " <+>
                "       WHERE users.company_id = " <?> cid <+>
                "         AND users.deleted IS NULL)" <+>
                ") AS emails"
    (fromIntegral :: Int64 -> Int) <$> fetchOne unSingle

data DeletePaymentPlan = DeletePaymentPlan CompanyID
instance (MonadDB m) => DBUpdate m DeletePaymentPlan () where
  update (DeletePaymentPlan cid) = do
    runQuery_ $ sqlDelete "payment_plans" $ do
       sqlWhereEq "payment_plans.company_id" cid

-- tested
data GetPaymentPlan = GetPaymentPlan CompanyID
instance (MonadDB m) => DBQuery m GetPaymentPlan (Maybe PaymentPlan) where
  query (GetPaymentPlan cid) = do
    runQuery_ $ sqlSelect "payment_plans" $ do
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
    fetchMaybe fetchPaymentPlan

-- tested
data GetPaymentPlanByAccountCode = GetPaymentPlanByAccountCode AccountCode
instance (MonadDB m) => DBQuery m GetPaymentPlanByAccountCode (Maybe PaymentPlan) where
  query (GetPaymentPlanByAccountCode ac) = do
    runQuery_ $ sqlSelect "payment_plans" $ do
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
    fetchMaybe fetchPaymentPlan

fetchPaymentPlan :: (AccountCode, CompanyID, PricePlan, PaymentPlanStatus, Int32, PricePlan, PaymentPlanStatus, Int32, PaymentPlanProvider, Maybe Int16, Maybe MinutesTime, MinutesTime) -> PaymentPlan
fetchPaymentPlan (ac, cid, p, s, q, pp, sp, qp, pr, mds, mdd, be) = PaymentPlan {
  ppAccountCode         = ac
, ppCompanyID           = cid
, ppPricePlan           = p
, ppPendingPricePlan    = pp
, ppStatus              = s
, ppPendingStatus       = sp
, ppQuantity            = fromIntegral q
, ppPendingQuantity     = fromIntegral qp
, ppPaymentPlanProvider = pr
, ppDunningStep         = fromIntegral <$> mds
, ppDunningDate         = mdd
, ppBillingEndDate      = be
}

{- | How often, in days, do we sync with recurly? -}
daysBeforeSync :: Int
daysBeforeSync = 7

--tested
data PaymentPlansRequiringSync = PaymentPlansRequiringSync PaymentPlanProvider MinutesTime
instance MonadDB m => DBQuery m PaymentPlansRequiringSync [PaymentPlan] where
  query (PaymentPlansRequiringSync prov time) = do
    let past = daysBefore daysBeforeSync time
    runQuery_ $ rawSQL ("SELECT account_code,payment_plans.company_id, plan, status, quantity, plan_pending, status_pending, quantity_pending, provider, dunning_step, dunning_date, billing_ends " <>
             "  FROM payment_plans " <>
             "  LEFT OUTER JOIN (SELECT c.company_id, count(email) AS q " <>
             "                   FROM ((SELECT company_id, users.email " <>
             "                          FROM users " <>
             "                          WHERE users.deleted IS NULL) " <>
             "                         ) as c" <>
             "                   GROUP BY c.company_id) AS ccount" <>
             "              ON (ccount.company_id = payment_plans.company_id)" <>
             "  WHERE provider = $1 " <>
             "    AND (sync_date < $2 " <> -- stuff older than 7 days needs sync
             "     OR (CASE WHEN q IS NULL THEN 0 ELSE q END) <> quantity " <> -- current # of users > cache
             "     OR (CASE WHEN q IS NULL THEN 0 ELSE q END) <> quantity_pending) ") -- current # of users <> pending cache
            (prov, past)
    fetchMany fetchPaymentPlan

--tested
data PaymentPlansExpiredDunning = PaymentPlansExpiredDunning MinutesTime
instance MonadDB m => DBQuery m PaymentPlansExpiredDunning [PaymentPlan] where
  query (PaymentPlansExpiredDunning time) = do
    runQuery_ $ rawSQL ("SELECT account_code, company_id, plan, status, quantity, " <>
             "    plan_pending, status_pending, quantity_pending, provider, dunning_step, dunning_date, billing_ends " <>
             "  FROM payment_plans " <>
             "  WHERE dunning_date < $1 " <>
             "    AND provider = $2 ")
            (time, RecurlyProvider)
    fetchMany fetchPaymentPlan

-- tested
data SavePaymentPlan = SavePaymentPlan PaymentPlan MinutesTime
instance MonadDB m => DBUpdate m SavePaymentPlan Bool where
  update (SavePaymentPlan PaymentPlan{..} tm) = do
    let
    updated <- runQuery01 . sqlUpdate "payment_plans" $ do
      setFields
      sqlWhereEq "account_code" ppAccountCode
    if updated
      then return True
      else do
        -- FIXME: possible race condition here.
        runQuery01 . sqlInsertSelect "payment_plans" "" $ do
          sqlSet "account_code" ppAccountCode
          setFields
          sqlWhere $ sqlParam ppAccountCode <+> "NOT IN (SELECT account_code FROM payment_plans)"
    where
      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "company_id" ppCompanyID
        sqlSet "plan" ppPricePlan
        sqlSet "status" ppStatus
        sqlSet "plan_pending" ppPendingPricePlan
        sqlSet "status_pending" ppPendingStatus
        sqlSet "quantity" (fromIntegral ppQuantity :: Int32)
        sqlSet "quantity_pending" (fromIntegral ppPendingQuantity :: Int32)
        sqlSet "provider" ppPaymentPlanProvider
        sqlSet "sync_date" tm
        sqlSet "dunning_step" (fromIntegral <$> ppDunningStep :: Maybe Int16)
        sqlSet "dunning_date" ppDunningDate
        sqlSet "billing_ends" ppBillingEndDate
