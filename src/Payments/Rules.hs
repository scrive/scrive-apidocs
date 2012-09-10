{- |

  Module: Payments.Rules
  Author: Eric Normand
  Date: Jul 25, 2012

  Rules regarding payments go here.

 -}

module Payments.Rules where

import Payments.Model
import Recurly

import Utils.Either
import Utils.Read

import Data.Maybe

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

{- |
  What sync action should we take given the requested quantity and plan (first param)
  and the actual values stored in recurly (second param)?

  There is a best practice referred to in the Recurly docs:

    when a user signs up, they are committing for the entire month.
    if they want to upgrade, we perform the upgrade immediately, charge their account for a prorated amount
    if they downgrade, we leave them at the current level until their current month expires, then downgrade them
       for the next month.

    This "downgrade at the end of the month" means the current month's plan is different from the next month's
       plan. The next month's plan is called "pending".

    There is also some logic of the FreePricePlan should cancel the account (which happens at the end of the month)
       Cancelling is good because a free price plan would invoice for $0 monthly. Cancelling sends no invoice.

    But if they have cancelled, we need to renew.

  This function takes care of the logic and decides what kind of action to take.

 -}
syncAction :: (Int, PricePlan) -> (Int, Int, PricePlan, PricePlan) -> RecurlyAction
-- we take no action when it is not an upgrade and we will switch next month
syncAction (quantity, plan) (sub_q, pend_q, sub_p, pend_p) | quantity == pend_q &&
                                                             quantity <= sub_q  &&
                                                             plan     == pend_p &&
                                                             plan     <= sub_p     = RNoAction
-- an upgrade from a canceled plan, we reactivate now
syncAction (_, plan) (_, _, sub_p, FreePricePlan) | plan > sub_p = RReactivateNow
-- an upgrade happens now 
syncAction (quantity, plan) (sub_q, _, sub_p, _) | quantity > sub_q ||
                                                   plan     > sub_p    = RUpdateNow
-- we cancel if we switch to free plan
syncAction (_, FreePricePlan) _ = RCancel
-- if we are switching from a canceled plan, we reactivate at renewal
syncAction (_, _) (_, _, _, FreePricePlan) = RReactivateRenewal
-- otherwise (downgrade) we update at the end of the month (renewal)
syncAction _ _ = RUpdateRenewal