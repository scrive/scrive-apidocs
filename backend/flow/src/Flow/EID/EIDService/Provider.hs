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
import qualified Flow.EID.EIDService.Provider.SmsOtp as SmsOtp

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
      unexpectedError "Verimi authentication not supported for Flow via EID service"
    (EIDServiceTransactionProviderNLIDIN, _) ->
      unexpectedError "NLIDIN authentication not supported for Flow via EID service"
    (EIDServiceTransactionProviderDKNemID, _) ->
      unexpectedError "DKNemID authentication not supported for Flow via EID service"
    (EIDServiceTransactionProviderNOBankID, _) ->
      unexpectedError "NOBankID authentication not supported for Flow via EID service"
    (EIDServiceTransactionProviderFITupas, _) ->
      unexpectedError "FITupas authentication not supported for Flow via EID service"
    (EIDServiceTransactionProviderOnfido, Onfido onfidoAuthenticationData) ->
      Onfido.beginEIDServiceTransaction conf
                                        authKind
                                        onfidoAuthenticationData
                                        instanceId
                                        userName
    (EIDServiceTransactionProviderSEBankID, _) ->
      unexpectedError "SEBankID authentication not supported for Flow via EID service"
    (EIDServiceTransactionProviderSmsOtp, SmsOtp) ->
      SmsOtp.beginEIDServiceTransaction conf authKind instanceId userName
    _ -> unexpectedError
      "Authentication provider has to match Flow participant provider configuration"

completeEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionProvider
  -> InstanceId
  -> UserName
  -> m (Maybe EIDServiceTransactionStatus)
completeEIDServiceAuthTransaction conf provider instanceId userName = case provider of
  EIDServiceTransactionProviderDKNemID ->
    unexpectedError "DKNemID authentication not supported via EID service"
  EIDServiceTransactionProviderNLIDIN ->
    unexpectedError "NLIDIN authentication not supported via EID service"
  EIDServiceTransactionProviderNOBankID ->
    unexpectedError "NOBankID authentication not supported via EID service"
  EIDServiceTransactionProviderVerimi ->
    unexpectedError "Verimi authentication not supported via EID service"
  EIDServiceTransactionProviderFITupas ->
    unexpectedError "FITupas authentication not supported via EID service"
  EIDServiceTransactionProviderOnfido ->
    Onfido.completeEIDServiceAuthTransaction conf instanceId userName
  EIDServiceTransactionProviderSEBankID ->
    unexpectedError "SEBankID authentication not supported via EID service"
  EIDServiceTransactionProviderSmsOtp ->
    SmsOtp.completeEIDServiceAuthTransaction conf instanceId userName
