module UserGroup.Types.PaymentPlan (
    PaymentPlan(..)
  ) where

import Control.Monad.Catch
import Data.Int (Int16)
import Data.Unjson
import qualified Data.Aeson as A

import DB
import Doc.API.V2.JSON.Utils

data PaymentPlan =
    FreePlan
  | TrialPlan
  | PaidPlan
  deriving (Eq, Ord, Show, Enum)

instance PQFormat PaymentPlan where
  pqFormat = pqFormat @Int16

instance FromSQL PaymentPlan where
  type PQBase PaymentPlan = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      -- Note:
      -- If changing this, please also update `pure_sql/invoice_stat.sql`
      0 -> return FreePlan
      1 -> return TrialPlan
      2 -> return PaidPlan
      _ -> throwM RangeError { reRange = [(0, 2)], reValue = n }

instance ToSQL PaymentPlan where
  type PQDest PaymentPlan = PQDest Int16
  toSQL FreePlan  = toSQL (0 :: Int16)
  toSQL TrialPlan = toSQL (1 :: Int16)
  toSQL PaidPlan  = toSQL (2 :: Int16)

instance Unjson PaymentPlan where
  unjsonDef = unjsonEnumBy
    "PaymentPlan"
    [(FreePlan, "free"), (TrialPlan, "trial"), (PaidPlan, "paid")]

instance Unjson (Maybe PaymentPlan) where
  unjsonDef = SimpleUnjsonDef "Maybe payment plan"
                              (parse unjsonDef)
                              (maybe A.Null $ unjsonToJSON unjsonDef)
