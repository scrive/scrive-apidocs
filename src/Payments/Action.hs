module Payments.Action where

import Data.Functor

import Company.Model
import DB hiding (update, query)
import Kontra
import User.Model
import Utils.Either

import Payments.Model
import qualified Payments.Stats as Stats

switchPlanToCompany :: Kontrakcja m => UserID -> CompanyID -> m Bool
switchPlanToCompany uid cid = do
  time <- ctxtime <$> getContext
  mplan <- dbQuery $ GetPaymentPlan (Left uid)
  case mplan of
    Just pp | isLeft $ ppID pp -> do
      let pp' = pp { ppID = Right cid }
      b <- dbUpdate $ SavePaymentPlan pp' time
      _ <- Stats.record time Stats.CompanySwitchAction RecurlyProvider (ppQuantity pp') (ppPricePlan pp') (Right cid) (ppAccountCode pp)
      return b
    _ -> return False
