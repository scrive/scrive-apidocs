module ELegitimation.ELeg where

import Happstack.Data

import DB.Types
import Doc.DocState
import MinutesTime

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
