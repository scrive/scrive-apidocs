module Payments.Stats (record 
                      ,PaymentsAction(..)
                      ,handlePaymentsStatsCSV)
       
       where

import Control.Applicative
import Control.Monad.Base
import Data.Maybe

import MinutesTime
import DB

import Kontra
import User.Model
import Company.Model
import Payments.Model
import Util.CSVUtil

record :: Kontrakcja m => PaymentsAction -> PaymentPlanProvider -> Int -> PricePlan -> Either UserID CompanyID -> AccountCode -> m Bool
record action provider quantity plan eid ac = do
  time <- ctxtime <$> getContext
  dbUpdate $ AddPaymentsStat time provider action quantity plan eid ac
  
handlePaymentsStatsCSV :: Kontrakcja m => m CSV
handlePaymentsStatsCSV = onlySalesOrAdmin $ do
  let header = ["time", "account_code", "userid", "companyid", "quantity", "plan", "action", "provider", "name"]
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
instance (MonadBase IO m, MonadDB m) => DBUpdate m AddPaymentsStat Bool where
  update AddPaymentsStat{..} = do
    kPrepare $ "INSERT INTO payment_stats (time, provider, action, quantity, plan, account_type, user_id, company_id, account_code) "
      ++ "      SELECT ?, ?, ?, ?, ?, ?, ?, ?, ? "
      ++ "      WHERE (EXISTS (SELECT 1 FROM users         WHERE id = ?)"
      ++ "         OR  EXISTS (SELECT 1 FROM companies     WHERE id = ?))"
      ++ "        AND EXISTS  (SELECT 1 FROM payment_plans WHERE account_code = ?)"
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
    _ <- kRun $ SQL ("SELECT payment_stats.time, payment_stats.account_code, payment_stats.user_id, payment_stats.company_id, " ++
                     "       payment_stats.quantity, payment_stats.plan, payment_stats.action, payment_stats.provider, " ++
                     "       trim(trim(users.first_name) || ' ' || trim(users.last_name)), trim(users.email), " ++
                     "       trim(users.company_name), trim(companies.name) " ++
                     "FROM payment_stats " ++
                     "LEFT OUTER JOIN users     ON payment_stats.user_id    = users.id " ++
                     "LEFT OUTER JOIN companies ON payment_stats.company_id = companies.id " ++
                     "ORDER BY payment_stats.time DESC") []
    foldDB f []
      where f :: [[String]] -> MinutesTime -> AccountCode -> Maybe UserID -> Maybe CompanyID -> Int -> PricePlan -> PaymentsAction -> PaymentPlanProvider -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> [[String]]
            f acc t ac muid mcid q pp pa pr un em ucn cn =
              let smartname = case cn of
                    Just name -> name
                    _ -> case ucn of
                      Just name | not $ null name -> name
                      _ -> case un of
                        Just name | not $ null name -> name
                        _ -> fromMaybe "" em                        
              in [formatMinutesTimeISO t, show ac, maybe "" show muid, maybe "" show mcid, show q, show pp, show pa, show pr, smartname] : acc



data PaymentsAction = SignupAction
                    | ChangeAction
                    | CancelAction
                    | ReactivateAction
                    | CompanySwitchAction -- switch from user to company
                    | UserSwitchAction    -- switch from company to user
                    | SyncAction
                    deriving (Show, Eq)

instance Convertible PaymentsAction Int where
  safeConvert SignupAction        = return 1
  safeConvert ChangeAction        = return 2
  safeConvert CancelAction        = return 3
  safeConvert ReactivateAction    = return 4
  safeConvert CompanySwitchAction = return 5
  safeConvert UserSwitchAction    = return 6
  safeConvert SyncAction          = return 7

instance Convertible Int PaymentsAction where
  safeConvert 1 = return SignupAction
  safeConvert 2 = return ChangeAction
  safeConvert 3 = return CancelAction
  safeConvert 4 = return ReactivateAction
  safeConvert 5 = return CompanySwitchAction
  safeConvert 6 = return UserSwitchAction
  safeConvert 7 = return SyncAction
  safeConvert s = Left ConvertError { convSourceValue  = show s
                                    , convSourceType   = "Int"
                                    , convDestType     = "PaymentsAction"
                                    , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                    }
                  

instance Convertible PaymentsAction SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue PaymentsAction where
  safeConvert s = safeConvert (fromSql s :: Int)
