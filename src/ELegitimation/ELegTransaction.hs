{-# OPTIONS_GHC -fno-warn-name-shadowing #-} -- ghc-7.4.1 workaround
module ELegitimation.ELegTransaction where

import Happstack.Data

import Doc.DocStateData
import MagicHash (MagicHash)
import MinutesTime
import ELegitimation.BankIDRequests (CollectResponse(..))

data ELegTransaction1 = ELegTransaction1 { transactiontransactionid1   :: String
                                         , transactionnonce1           :: String
                                         , transactiontbs1             :: String
                                         , transactionencodedtbs1      :: String
                                         , transactionservertime1      :: MinutesTime
                                         , transactionsignatorylinkid1 :: Maybe SignatoryLinkID
                                         , transactiondocumentid1      :: DocumentID
                                         , transactionmagichash1       :: Maybe MagicHash
                                         }
                      deriving (Eq, Ord, Show, Typeable)

data ELegTransaction = ELegTransaction { transactiontransactionid   :: String
                                       , transactionnonce           :: Maybe String
                                       , transactiontbs             :: String
                                       , transactionencodedtbs      :: Maybe String
                                       , transactionsignatorylinkid :: Maybe SignatoryLinkID
                                       , transactiondocumentid      :: DocumentID
                                       , transactionmagichash       :: Maybe MagicHash
                                       , transactionstatus          :: Either String CollectResponse
                                       , transactionoref            :: Maybe String
                                       }
                     deriving (Eq, Ord, Show, Typeable)
                              
$(deriveSerialize ''ELegTransaction1)
instance Version (ELegTransaction1)

$(deriveSerialize ''ELegTransaction)
instance Version (ELegTransaction) where
  mode = extension 2 (Proxy :: Proxy ELegTransaction1)
  
instance Migrate () ELegTransaction1 where
  migrate _ = error "Nothing to migrate."
  
instance Migrate ELegTransaction1 ELegTransaction where
  migrate (ELegTransaction1{..}) = ELegTransaction { 
      transactiontransactionid   = transactiontransactionid1
    , transactionnonce           = Just transactionnonce1
    , transactiontbs             = transactiontbs1
    , transactionencodedtbs      = Just transactionencodedtbs1
    , transactionsignatorylinkid = transactionsignatorylinkid1
    , transactiondocumentid      = transactiondocumentid1
    , transactionmagichash       = transactionmagichash1
    , transactionoref            = Nothing
    , transactionstatus          = Left "Not for mobile bankid"
    }

