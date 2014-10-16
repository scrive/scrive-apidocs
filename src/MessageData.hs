{- Extra information that we embed in mails and SMS message.
   This way we can get informed when some signatory read them, etc.
-}

module MessageData (MessageData(..)) where

import Doc.DocumentID
import Doc.SignatoryLinkID

data MessageData =
    Invitation DocumentID SignatoryLinkID
  | SMSPinSendout SignatoryLinkID
  | None
    deriving (Eq, Ord, Show, Read)
