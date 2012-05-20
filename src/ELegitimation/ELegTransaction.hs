{-# OPTIONS_GHC -fno-warn-name-shadowing #-} -- ghc-7.4.1 workaround
module ELegitimation.ELegTransaction where

import Happstack.Data

import Doc.DocStateData
import MagicHash (MagicHash)
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
