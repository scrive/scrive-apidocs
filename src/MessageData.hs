{- Extra information that we embed in mails and SMS message.
   This way we can get informed when some signatory read them, etc.
-}

module MessageData (MessageData(..)) where

import Doc.SignatoryLinkID
import Doc.DocumentID


data MessageData =
    Invitation DocumentID SignatoryLinkID
  | None
    deriving (Eq, Ord, Show, Read)
