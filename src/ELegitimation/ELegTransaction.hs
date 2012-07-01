module ELegitimation.ELegTransaction where

import Data.SafeCopy
import Data.Typeable

import Doc.DocStateData
import MagicHash
import ELegitimation.BankIDRequests

data ELegTransaction = ELegTransaction {
    transactiontransactionid   :: String
  , transactionnonce           :: Maybe String
  , transactiontbs             :: String
  , transactionencodedtbs      :: Maybe String
  , transactionsignatorylinkid :: Maybe SignatoryLinkID
  , transactiondocumentid      :: DocumentID
  , transactionmagichash       :: Maybe MagicHash
  , transactionstatus          :: Either String CollectResponse
  , transactionoref            :: Maybe String
  } deriving (Eq, Ord, Show, Typeable)

$(deriveSafeCopy 0 'base ''ELegTransaction)
