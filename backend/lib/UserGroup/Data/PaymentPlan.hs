module UserGroup.Data.PaymentPlan (
    PaymentPlan(..)
  ) where

import Control.Monad.Catch
import Data.Int (Int16)

import DB

data PaymentPlan =
    FreePlan
  | OnePlan
  | TeamPlan
  | EnterprisePlan
  | TrialPlan
  deriving (Eq, Ord, Show)

instance PQFormat PaymentPlan where
  pqFormat = const $ pqFormat (undefined::Int16)

instance FromSQL PaymentPlan where
  type PQBase PaymentPlan = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      -- Note:
      -- If changing this, please also update `pure_sql/invoice_stat.sql`
      0 -> return FreePlan
      1 -> return OnePlan
      2 -> return TeamPlan
      3 -> return EnterprisePlan
      4 -> return TrialPlan
      _ -> throwM RangeError {
        reRange = [(0, 4)]
      , reValue = n
      }

instance ToSQL PaymentPlan where
  type PQDest PaymentPlan = PQDest Int16
  toSQL FreePlan        = toSQL (0::Int16)
  toSQL OnePlan         = toSQL (1::Int16)
  toSQL TeamPlan        = toSQL (2::Int16)
  toSQL EnterprisePlan  = toSQL (3::Int16)
  toSQL TrialPlan       = toSQL (4::Int16)
