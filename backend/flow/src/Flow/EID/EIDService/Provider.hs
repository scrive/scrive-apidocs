module Flow.EID.EIDService.Provider (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  ) where

import Data.Aeson

import Doc.Types.SignatoryLink
import EID.EIDService.Conf
import EID.EIDService.Types
import Flow.Id
import Flow.Names
import Kontra
import qualified Flow.EID.EIDService.Provider.Onfido as Onfido

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionProvider
  -> AuthenticationKind
  -> InstanceId
  -> UserName
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf provider authKind instanceId userName = do
  case provider of
    EIDServiceTransactionProviderVerimi ->
      unexpectedError "Verimi auth not supported for Flow via EID service"
    EIDServiceTransactionProviderNLIDIN ->
      unexpectedError "NLIDIN auth not supported for Flow via EID service"
    EIDServiceTransactionProviderDKNemID ->
      unexpectedError "DKNemID auth not supported for Flow via EID service"
    EIDServiceTransactionProviderNOBankID ->
      unexpectedError "NOBankID auth not supported for Flow via EID service"
    EIDServiceTransactionProviderFITupas ->
      unexpectedError "FITupas auth not supported for Flow via EID service"
    EIDServiceTransactionProviderOnfido ->
      Onfido.beginEIDServiceTransaction conf authKind instanceId userName
    EIDServiceTransactionProviderSEBankID ->
      unexpectedError "SEBankID auth not supported for Flow via EID service"

completeEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionProvider
  -> InstanceId
  -> UserName
  -> m (Maybe EIDServiceTransactionStatus)
completeEIDServiceAuthTransaction conf provider instanceId userName = case provider of
  EIDServiceTransactionProviderDKNemID ->
    unexpectedError "DKNemID sign not supported via EID service"
  EIDServiceTransactionProviderNLIDIN ->
    unexpectedError "NLIDIN sign not supported via EID service"
  EIDServiceTransactionProviderNOBankID ->
    unexpectedError "NOBankID sign not supported via EID service"
  EIDServiceTransactionProviderVerimi ->
    unexpectedError "Verimi sign not supported via EID service"
  EIDServiceTransactionProviderFITupas ->
    unexpectedError "FITupas sign not supported via EID service"
  EIDServiceTransactionProviderOnfido ->
    Onfido.completeEIDServiceAuthTransaction conf instanceId userName
  EIDServiceTransactionProviderSEBankID ->
    unexpectedError "SEBankID sign not supported via EID service"
