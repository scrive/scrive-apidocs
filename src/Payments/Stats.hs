module Payments.Stats (
    record
  , PaymentsAction(..)
  , handlePaymentsStatsCSV
  ) where

import Control.Monad.Base
import Data.Int
import Data.Maybe
import Data.Monoid
import qualified Control.Exception.Lifted as E

import MinutesTime
import DB

import Kontra
import Company.Model
import Payments.Model
import Util.CSVUtil

record :: MonadDB m => UTCTime -> PaymentsAction -> PaymentPlanProvider -> Int -> PricePlan -> CompanyID -> AccountCode -> m Bool
record time action provider quantity plan cid ac =
  dbUpdate $ AddPaymentsStat time provider action quantity plan cid ac

handlePaymentsStatsCSV :: Kontrakcja m => m CSV
handlePaymentsStatsCSV = onlySalesOrAdmin $ do
  let header = ["sync time", "account code","companyid", "quantity", "plan", "provider", "billing end time", "status", "name"]
  stats <- dbQuery $ GetPaymentsStats
  return $ CSV { csvFilename = "paymentsstats.csv"
               , csvHeader   = header
               , csvContent  = stats
               }

data AddPaymentsStat = AddPaymentsStat { psTime        :: UTCTime
                                       , psProvider    :: PaymentPlanProvider
                                       , psAction      :: PaymentsAction
                                       , psQuantity    :: Int
                                       , psPlan        :: PricePlan
                                       , psCompanyID   :: CompanyID
                                       , psAccountCode :: AccountCode
                                       }
instance MonadDB m => DBUpdate m AddPaymentsStat Bool where
  update AddPaymentsStat{..} = do
    runQuery01 $ rawSQL ("INSERT INTO payment_stats (time, provider, action, quantity, plan, company_id, account_code) "
      <> "      SELECT $1, $2, $3, $4, $5, $6, $7"
      <> "      WHERE EXISTS (SELECT 1 FROM companies     WHERE id = $8)"
      <> "        AND EXISTS  (SELECT 1 FROM payment_plans WHERE account_code = $9)")
               (psTime
               ,psProvider
               ,psAction
               ,fromIntegral psQuantity :: Int32
               ,psPlan
               ,psCompanyID
               ,psAccountCode
               -- WHERE clause
               ,psCompanyID
               ,psAccountCode
               )

data GetPaymentsStats = GetPaymentsStats
instance (MonadBase IO m, MonadDB m) => DBQuery m GetPaymentsStats [[String]] where
  query GetPaymentsStats = do
    runSQL_ $ "SELECT payment_plans.sync_date, payment_plans.account_code, payment_plans.company_id, " <>
                     "       payment_plans.quantity, payment_plans.plan, payment_plans.provider, payment_plans.billing_ends, payment_plans.status, " <>
                     "       trim(companies.name) " <>
                     "FROM payment_plans " <>
                     "LEFT OUTER JOIN companies ON payment_plans.company_id = companies.id " <>
                     "ORDER BY payment_plans.account_code DESC"
    foldlM f []
      where f :: [[String]] -> (UTCTime, AccountCode, CompanyID, Int32, PricePlan, PaymentPlanProvider, UTCTime, PaymentPlanStatus, Maybe String) -> m [[String]]
            f acc (t, ac, cid, q, pp, pr, be, st, cn) =
              let smartname = fromMaybe "" cn
              in return $ [formatTimeUTC t, show ac, show cid, show q, show pp, show pr, formatTimeUTC be, show st, smartname] : acc



data PaymentsAction = SignupAction
                    | ChangeAction
                    | CancelAction
                    | ReactivateAction
                    | CompanySwitchAction -- switch from user to company
                    | UserSwitchAction    -- switch from company to user
                    | SyncAction
                    | PushAction          -- from recurly push notifications
                    | OwnerSwitchAction
                    deriving (Show, Eq)

instance PQFormat PaymentsAction where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL PaymentsAction where
  type PQBase PaymentsAction = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return SignupAction
      2 -> return ChangeAction
      3 -> return CancelAction
      4 -> return ReactivateAction
      5 -> return CompanySwitchAction
      6 -> return UserSwitchAction
      7 -> return SyncAction
      8 -> return PushAction
      9 -> return OwnerSwitchAction
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 9)]
      , reValue = n
      }

instance ToSQL PaymentsAction where
  type PQDest PaymentsAction = PQDest Int16
  toSQL SignupAction        = toSQL (1::Int16)
  toSQL ChangeAction        = toSQL (2::Int16)
  toSQL CancelAction        = toSQL (3::Int16)
  toSQL ReactivateAction    = toSQL (4::Int16)
  toSQL CompanySwitchAction = toSQL (5::Int16)
  toSQL UserSwitchAction    = toSQL (6::Int16)
  toSQL SyncAction          = toSQL (7::Int16)
  toSQL PushAction          = toSQL (8::Int16)
  toSQL OwnerSwitchAction   = toSQL (9::Int16)
