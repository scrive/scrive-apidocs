{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, TypeSynonymInstances,
    GeneralizedNewtypeDeriving, StandaloneDeriving, NamedFieldPuns #-}

module ELegitimation.ELeg where

import Happstack.Data
import Happstack.State
import DocState
import MinutesTime

$(deriveAll [''Eq, ''Ord, ''Default]
  [d|
      data ELegTransaction = ELegTransaction { transactiontransactionid :: String
                                             , transactionnonce :: String
                                             , transactiontbs :: String
                                             , transactionencodedtbs :: String
                                             , transactionservertime :: MinutesTime
                                             , transactionsignatorylinkid :: SignatoryLinkID
                                             }
  |])

$(deriveSerialize ''ELegTransaction)
instance Version (ELegTransaction)

deriving instance Show ELegTransaction