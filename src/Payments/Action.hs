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

changePaymentPlanOwner :: Kontrakcja m => Either UserID CompanyID -> Either UserID CompanyID -> m Bool
changePaymentPlanOwner source destination = do
  time <- ctxtime <$> getContext
  mplan <- dbQuery $ GetPaymentPlan source
  exists <- case destination of
    Left uid -> do
      muser <- dbQuery $ GetUserByID uid
      return $ maybe False (const True) muser
    Right cid -> do
      mcmpy <- dbQuery $ GetCompany cid
      return $ maybe False (const True) mcmpy
  case (exists, mplan) of
    (True, Just pp) -> do
      let pp' = pp { ppID = destination }
      b <- dbUpdate $ SavePaymentPlan pp' time
      _ <- Stats.record time Stats.OwnerSwitchAction RecurlyProvider (ppQuantity pp') (ppPricePlan pp') destination (ppAccountCode pp)
      return b
    _ -> return False

getNonTrialPlanName :: PricePlan -> String
getNonTrialPlanName TrialTeamPricePlan = show TeamPricePlan
getNonTrialPlanName x = show x

