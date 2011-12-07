module ELegitimation.ELegTransaction where

import Happstack.Data

import DB.Types
import Doc.DocStateData
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
