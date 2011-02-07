
module ELegitimation.ELeg where

import Happstack.Data
import Happstack.State
import Doc.DocState
import MinutesTime
import Misc

$(deriveAll [''Eq, ''Ord, ''Default]
  [d|
      data ELegTransaction = ELegTransaction { transactiontransactionid :: String
                                             , transactionnonce :: String
                                             , transactiontbs :: String
                                             , transactionencodedtbs :: String
                                             , transactionservertime :: MinutesTime
                                             , transactionsignatorylinkid :: Maybe SignatoryLinkID
                                             , transactiondocumentid :: DocumentID
                                             , transactionmagichash :: Maybe MagicHash
                                             }
  |])

$(deriveSerialize ''ELegTransaction)
instance Version (ELegTransaction)

deriving instance Show ELegTransaction