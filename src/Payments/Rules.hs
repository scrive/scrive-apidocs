module Payments.Rules where

import Payments.Model

data RecurlyAction = RNoAction
                   | RUpdateNow
                   | RUpdateRenewal
                   | RCancel
                   deriving (Show, Eq, Read)

syncAction :: (Int, PricePlan) -> (Int, Int, PricePlan, PricePlan) -> RecurlyAction
syncAction (quantity, plan) (sub_q, pend_q, sub_p, pend_p) | quantity == pend_q &&
                                                             quantity <= sub_q  &&
                                                             plan     == pend_p &&
                                                             plan     <= sub_p     = RNoAction
syncAction (quantity, plan) (sub_q, _, sub_p, _) | quantity > sub_q ||
                                                   plan     > sub_p    = RUpdateNow
syncAction (_, FreePricePlan) _ = RCancel
syncAction _ _ = RUpdateRenewal