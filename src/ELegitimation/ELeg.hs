
module ELegitimation.ELeg where

import Happstack.Data
import Happstack.State
import Doc.DocState
import MinutesTime
import Misc
import Data.Data

data ELegTransaction = ELegTransaction { transactiontransactionid :: String
                                             , transactionnonce :: String
                                             , transactiontbs :: String
                                             , transactionencodedtbs :: String
                                             , transactionservertime :: MinutesTime
                                             , transactionsignatorylinkid :: Maybe SignatoryLinkID
                                             , transactiondocumentid :: DocumentID
                                             , transactionmagichash :: Maybe MagicHash
                                             }
                     deriving (Eq, Ord, Show, Typeable, Data)

$(deriveSerialize ''ELegTransaction)
instance Version (ELegTransaction)
