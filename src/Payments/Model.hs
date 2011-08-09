module Payments.Model where

import DB.Derive

data PaymentAccountType = FreeTrial | Private | Minimal | Medium | Maximal | Corp
    deriving (Eq, Ord, Read, Show, Enum, Bounded)
$(enumDeriveConvertible ''PaymentAccountType)
