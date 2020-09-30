module Flow.EID.EIDService.Provider (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  ) where

import Data.Aeson

import Doc.Types.SignatoryLink
import EID.EIDService.Conf
import EID.EIDService.Types
import Flow.Core.Type.AuthenticationConfiguration
import Flow.Id
import Flow.Names
import Kontra
import qualified Flow.EID.EIDService.Provider.Onfido as Onfido

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionProvider
  -> AuthenticationKind
  -> AuthenticationConfiguration
  -> InstanceId
  -> UserName
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf provider authKind authenticationConfig instanceId userName
  = case (provider, authenticationConfig ^. #provider) of
    (EIDServiceTransactionProviderVerimi, _) ->
      unexpectedError "Verimi auth not supported for Flow via EID service"
    (EIDServiceTransactionProviderNLIDIN, _) ->
      unexpectedError "NLIDIN auth not supported for Flow via EID service"
    (EIDServiceTransactionProviderDKNemID, _) ->
      unexpectedError "DKNemID auth not supported for Flow via EID service"
    (EIDServiceTransactionProviderNOBankID, _) ->
      unexpectedError "NOBankID auth not supported for Flow via EID service"
    (EIDServiceTransactionProviderFITupas, _) ->
      unexpectedError "FITupas auth not supported for Flow via EID service"
    (EIDServiceTransactionProviderOnfido, Onfido onfidoAuthenticationData) ->
      Onfido.beginEIDServiceTransaction conf
                                        authKind
                                        onfidoAuthenticationData
                                        instanceId
                                        userName
    (EIDServiceTransactionProviderSEBankID, _) ->
      unexpectedError "SEBankID auth not supported for Flow via EID service"
    _ -> unexpectedError
      "Authentication provider have to match configured provider on flow participant."

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
