module Flow.EID.EIDService.Provider.Common
  (getTransactionForUser) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Log

import DB
import Doc.Types.SignatoryLink
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Types hiding
  ( EIDServiceTransactionFromDB(..), UnifiedRedirectUrl(..)
  )
import Flow.Aggregator
import Flow.EID.EIDService.Model
import Flow.EID.EIDService.Types
import Flow.Id
import Flow.Model.Types
import Flow.Names
import KontraMonad
import Session.Model
import qualified Flow.Model as Model

getTransactionForUser
  :: (FromJSON a, Show a, Kontrakcja m)
  => EIDServiceConf
  -> EIDServiceTransactionProvider
  -> InstanceId
  -> UserName
  -> m
       ( Maybe
           ( AuthenticationKind
           , EIDServiceTransactionFromDB
           , EIDServiceTransactionResponse a
           )
       )
getTransactionForUser conf eidProvider instanceId userName = runMaybeT $ do
  sessionID         <- getNonTempSessionID
  Just fullInstance <- Model.selectFullInstance instanceId
  let authKind = if isComplete $ instanceToAggregator fullInstance
        then AuthenticationToViewArchived
        else AuthenticationToView
  Just estDB <-
    dbQuery
    . GetEIDServiceTransactionGuardSessionID sessionID instanceId userName
    $ EIDServiceAuthToView authKind
  Just trans <- getTransactionFromEIDService conf eidProvider (estID estDB)
  logInfo_ $ "EID transaction: " <> showt trans
  pure (authKind, estDB, trans)

