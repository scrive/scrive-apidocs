module Payments.Stats (record 
                      ,PaymentsAction(..)
                      ,handlePaymentsStatsCSV)
       
       where

import Control.Monad.Base
import Data.Maybe

import MinutesTime
import DB

import Kontra
import User.Model
import Company.Model
import Payments.Model
import Util.CSVUtil

record :: MonadDB m => MinutesTime -> PaymentsAction -> PaymentPlanProvider -> Int -> PricePlan -> Either UserID CompanyID -> AccountCode -> m Bool
record time action provider quantity plan eid ac =
  dbUpdate $ AddPaymentsStat time provider action quantity plan eid ac
  
handlePaymentsStatsCSV :: Kontrakcja m => m CSV
handlePaymentsStatsCSV = onlySalesOrAdmin $ do
  let header = ["sync time", "account code", "userid", "companyid", "quantity", "plan", "provider", "billing end time", "status", "name"]
  stats <- dbQuery $ GetPaymentsStats
  return $ CSV { csvFilename = "paymentsstats.csv"
               , csvHeader   = header
               , csvContent  = stats
               }

data AddPaymentsStat = AddPaymentsStat { psTime        :: MinutesTime
                                       , psProvider    :: PaymentPlanProvider
                                       , psAction      :: PaymentsAction
                                       , psQuantity    :: Int
                                       , psPlan        :: PricePlan
                                       , psID          :: Either UserID CompanyID
                                       , psAccountCode :: AccountCode
                                       }
instance MonadDB m => DBUpdate m AddPaymentsStat Bool where
  update AddPaymentsStat{..} = do
    kPrepare $ "INSERT INTO payment_stats (time, provider, action, quantity, plan, account_type, user_id, company_id, account_code) "
      <> "      SELECT ?, ?, ?, ?, ?, ?, ?, ?, ? "
      <> "      WHERE (EXISTS (SELECT 1 FROM users         WHERE id = ?)"
      <> "         OR  EXISTS (SELECT 1 FROM companies     WHERE id = ?))"
      <> "        AND EXISTS  (SELECT 1 FROM payment_plans WHERE account_code = ?)"
    kExecute01 [toSql psTime
               ,toSql psProvider
               ,toSql psAction
               ,toSql psQuantity
               ,toSql psPlan
               ,toSql $ either (const (1 :: Int)) (const 2) psID
               ,toSql $ either Just (const Nothing) psID
               ,toSql $ either (const Nothing) Just psID
               ,toSql psAccountCode
               -- WHERE clause 
               ,toSql $ either Just (const Nothing) psID
               ,toSql $ either (const Nothing) Just psID
               ,toSql psAccountCode
               ]

data GetPaymentsStats = GetPaymentsStats
instance (MonadBase IO m, MonadDB m) => DBQuery m GetPaymentsStats [[String]] where
  query GetPaymentsStats = do
    _ <- kRun $ SQL ("SELECT payment_plans.sync_date, payment_plans.account_code, payment_plans.user_id, payment_plans.company_id, " <>
                     "       payment_plans.quantity, payment_plans.plan, payment_plans.provider, payment_plans.billing_ends, payment_plans.status, " <>
                     "       trim(trim(users.first_name) || ' ' || trim(users.last_name)), trim(users.email), " <>
                     "       trim(users.company_name), trim(companies.name) " <>
                     "FROM payment_plans " <>
                     "LEFT OUTER JOIN users     ON payment_plans.user_id    = users.id " <>
                     "LEFT OUTER JOIN companies ON payment_plans.company_id = companies.id " <>
                     "ORDER BY payment_plans.account_code DESC") []
    foldDB f []
      where f :: [[String]] -> MinutesTime -> AccountCode -> Maybe UserID -> Maybe CompanyID -> Int -> PricePlan -> PaymentPlanProvider -> MinutesTime -> PaymentPlanStatus -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> [[String]]
            f acc t ac muid mcid q pp pr be st un em ucn cn =
              let smartname = case cn of
                    Just name -> name
                    _ -> case ucn of
                      Just name | not $ null name -> name
                      _ -> case un of
                        Just name | not $ null name -> name
                        _ -> fromMaybe "" em                        
              in [formatMinutesTimeISO t, show ac, maybe "" show muid, maybe "" show mcid, show q, show pp, show pr, formatMinutesTimeISO be, show st, smartname] : acc



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

instance Convertible PaymentsAction Int where
  safeConvert SignupAction        = return 1
  safeConvert ChangeAction        = return 2
  safeConvert CancelAction        = return 3
  safeConvert ReactivateAction    = return 4
  safeConvert CompanySwitchAction = return 5
  safeConvert UserSwitchAction    = return 6
  safeConvert SyncAction          = return 7
  safeConvert PushAction          = return 8
  safeConvert OwnerSwitchAction   = return 9

instance Convertible Int PaymentsAction where
  safeConvert 1 = return SignupAction
  safeConvert 2 = return ChangeAction
  safeConvert 3 = return CancelAction
  safeConvert 4 = return ReactivateAction
  safeConvert 5 = return CompanySwitchAction
  safeConvert 6 = return UserSwitchAction
  safeConvert 7 = return SyncAction
  safeConvert 8 = return PushAction
  safeConvert 9 = return OwnerSwitchAction
  safeConvert s = Left ConvertError { convSourceValue  = show s
                                    , convSourceType   = "Int"
                                    , convDestType     = "PaymentsAction"
                                    , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                    }
                  

instance Convertible PaymentsAction SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue PaymentsAction where
  safeConvert s = safeConvert (fromSql s :: Int)
