module Payments.Rules where

import Payments.Model
import Recurly

import Data.Maybe

import Misc

data RecurlyAction = RNoAction
                   | RUpdateNow
                   | RUpdateRenewal
                   | RReactivateNow
                   | RReactivateRenewal
                   | RCancel
                   deriving (Show, Eq, Read)

subInfo :: Subscription -> Either String (Int, Int, PricePlan, PricePlan)
subInfo Subscription {..} = do
  sub_p <- case subState of
    "expired" -> return FreePricePlan
    _ -> toEither ("Unknown price plan " ++ subPricePlan) $ maybeRead subPricePlan
  let sub_q  = subQuantity
      pend_q = maybe subQuantity penQuantity subPending
  let pend_p =  case subState of
        "canceled" -> FreePricePlan
        _ -> fromMaybe sub_p (maybeRead . penPricePlan =<< subPending)
  return (sub_q, pend_q, sub_p, pend_p)

syncAction :: (Int, PricePlan) -> (Int, Int, PricePlan, PricePlan) -> RecurlyAction
syncAction (quantity, plan) (sub_q, pend_q, sub_p, pend_p) | quantity == pend_q &&
                                                             quantity <= sub_q  &&
                                                             plan     == pend_p &&
                                                             plan     <= sub_p     = RNoAction
syncAction (_, plan) (_, _, sub_p, FreePricePlan) | plan > sub_p = RReactivateNow
syncAction (quantity, plan) (sub_q, _, sub_p, _) | quantity > sub_q ||
                                                   plan     > sub_p    = RUpdateNow
syncAction (_, FreePricePlan) _ = RCancel
syncAction (_, _) (_, _, _, FreePricePlan) = RReactivateRenewal
syncAction _ _ = RUpdateRenewal