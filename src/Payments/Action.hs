module Payments.Action where

import Data.Functor

import Company.Model
import DB hiding (update, query)
import Kontra

import Payments.Model
import qualified Payments.Stats as Stats

changePaymentPlanOwner :: Kontrakcja m => CompanyID -> CompanyID -> m Bool
changePaymentPlanOwner sourceCID destinationCID = do
  time <- ctxtime <$> getContext
  mplan <- dbQuery $ GetPaymentPlan sourceCID
  exists <- do
      mcmpy <- dbQuery $ GetCompany destinationCID
      return $ maybe False (const True) mcmpy
  case (exists, mplan) of
    (True, Just pp) -> do
      let pp' = pp { ppCompanyID = destinationCID }
      b <- dbUpdate $ SavePaymentPlan pp' time
      _ <- Stats.record time Stats.OwnerSwitchAction RecurlyProvider (ppQuantity pp') (ppPricePlan pp') destinationCID (ppAccountCode pp)
      return b
    _ -> return False

getNonTrialPlanName :: PricePlan -> String
getNonTrialPlanName TrialPricePlan = show TeamPricePlan
getNonTrialPlanName x = show x

