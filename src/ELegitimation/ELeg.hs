module ELegitimation.ELeg where

import Happstack.Data

import Doc.DocState
import MinutesTime
import Misc

data ELegTransaction = ELegTransaction { transactiontransactionid   :: String
                                       , transactionnonce           :: String
                                       , transactiontbs             :: String
                                       , transactionencodedtbs      :: String
                                       , transactionservertime      :: MinutesTime
                                       , transactionsignatorylinkid :: Maybe SignatoryLinkID
                                       , transactiondocumentid      :: DocumentID
                                       , transactionmagichash       :: Maybe MagicHash
                                       }
                     deriving (Eq, Ord, Show, Typeable)

$(deriveSerialize ''ELegTransaction)
instance Version (ELegTransaction)
