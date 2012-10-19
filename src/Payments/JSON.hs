{-# OPTIONS_GHC -fno-warn-orphans #-}
module Payments.JSON where

import Recurly

import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J

instance ToJSValue Subscription where
  toJSValue (Subscription{..}) = runJSONGen $ do
    case subState of
      "expired" -> do
        J.value "planName" "Free"
        J.value "planCode" "free"
        J.value "unitAmountInCents" (0 :: Int)
      _ -> do
        J.value "planName" subName
        J.value "planCode" subPricePlan
        J.value "unitAmountInCents" subUnitAmountInCents
    J.value "quantity"  subQuantity
    J.value "currency"  subCurrency
    J.value "activated" subActivateDate
    J.value "cancelled" subCancelledDate
    J.value "billingStarted" subCurrentBillingStarted
    J.value "billingEnds" subCurrentBillingEnds
    case subState of
      "canceled" -> J.value "pending" $ 
                    PendingSubscription { penName              = "Free"
                                        , penPricePlan         = "free"
                                        , penUnitAmountInCents = 0
                                        , penQuantity          = subQuantity }
      _ -> J.value "pending" $ subPending
    
instance ToJSValue PendingSubscription where
  toJSValue (PendingSubscription{..}) = runJSONGen $ do
    J.value "planName" penName
    J.value "planCode" penPricePlan
    J.value "unitAmountInCents" penUnitAmountInCents
    J.value "quantity" penQuantity

instance ToJSValue Invoice where
  toJSValue (Invoice{..}) = runJSONGen $ do
    J.value "invoiceNumber" inNumber
    J.value "totalInCents"  inTotalInCents
    J.value "currency"      inCurrency
    J.value "state"         inState
    J.value "accountCode"   inAccount
    J.value "date"          inDate
    
