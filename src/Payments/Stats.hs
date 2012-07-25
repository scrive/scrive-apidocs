module Payments.Stats (record 
                      ,PaymentsAction(..))
       
       where

import Control.Applicative
import Control.Monad.Base

import MinutesTime
import DB

import Kontra
import User.Model
import Company.Model
import Payments.Model

record :: Kontrakcja m => PaymentsAction -> PaymentPlanProvider -> Int -> PricePlan -> Either UserID CompanyID -> AccountCode -> m Bool
record action provider quantity plan eid ac = do
  time <- ctxtime <$> getContext
  dbUpdate $ AddPaymentsStat time provider action quantity plan eid ac

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

data PaymentsAction = SignupAction
                    | ChangeAction
                    | CancelAction
                    | ReactivateAction
                    | CompanySwitchAction -- switch from user to company
                    | UserSwitchAction    -- switch from company to user

instance Convertible PaymentsAction Int where
  safeConvert SignupAction        = return 1
  safeConvert ChangeAction        = return 2
  safeConvert CancelAction        = return 3
  safeConvert ReactivateAction    = return 4
  safeConvert CompanySwitchAction = return 5
  safeConvert UserSwitchAction    = return 6

instance Convertible Int PaymentsAction where
  safeConvert 1 = return SignupAction
  safeConvert 2 = return ChangeAction
  safeConvert 3 = return CancelAction
  safeConvert 4 = return ReactivateAction
  safeConvert 5 = return CompanySwitchAction
  safeConvert 6 = return UserSwitchAction
  safeConvert s = Left ConvertError { convSourceValue  = show s
                                    , convSourceType   = "Int"
                                    , convDestType     = "PaymentsAction"
                                    , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                    }
                  

instance Convertible PaymentsAction SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue PaymentsAction where
  safeConvert s = safeConvert (fromSql s :: Int)
