module EID.EIDService.Provider (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  , completeEIDServiceSignTransaction
  ) where

import Data.Aeson

import Doc.DocStateData
import EID.EIDService.Conf
import EID.EIDService.Types
import Kontra
import qualified EID.EIDService.Provider.DKNemID as DKNemID
import qualified EID.EIDService.Provider.FITupas as FITupas
import qualified EID.EIDService.Provider.NLIDIN as NLIDIN
import qualified EID.EIDService.Provider.NOBankID as NOBankID
import qualified EID.EIDService.Provider.Onfido as Onfido
import qualified EID.EIDService.Provider.SEBankID as SEBankID
import qualified EID.EIDService.Provider.Verimi as Verimi

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionProvider
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf provider authKind doc sl = do
  case provider of
    EIDServiceTransactionProviderVerimi ->
      Verimi.beginEIDServiceTransaction conf authKind doc sl
    EIDServiceTransactionProviderNLIDIN ->
      NLIDIN.beginEIDServiceTransaction conf authKind doc sl
    EIDServiceTransactionProviderDKNemID ->
      DKNemID.beginEIDServiceTransaction conf authKind doc sl
    EIDServiceTransactionProviderNOBankID ->
      NOBankID.beginEIDServiceTransaction conf authKind doc sl
    EIDServiceTransactionProviderFITupas ->
      FITupas.beginEIDServiceTransaction conf authKind doc sl
    EIDServiceTransactionProviderOnfido ->
      Onfido.beginEIDServiceTransaction conf authKind doc sl
    EIDServiceTransactionProviderSEBankID ->
      SEBankID.beginEIDServiceTransaction conf authKind doc sl
    EIDServiceTransactionProviderSmsOtp ->
      unexpectedError "SmsOtp authentication not supported via EID service"

completeEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionProvider
  -> Document
  -> SignatoryLink
  -> m (Maybe EIDServiceTransactionStatus)
completeEIDServiceAuthTransaction conf provider doc sl = case provider of
  EIDServiceTransactionProviderDKNemID ->
    DKNemID.completeEIDServiceAuthTransaction conf doc sl
  EIDServiceTransactionProviderNLIDIN ->
    NLIDIN.completeEIDServiceAuthTransaction conf doc sl
  EIDServiceTransactionProviderNOBankID ->
    NOBankID.completeEIDServiceAuthTransaction conf doc sl
  EIDServiceTransactionProviderVerimi ->
    Verimi.completeEIDServiceAuthTransaction conf doc sl
  EIDServiceTransactionProviderFITupas ->
    FITupas.completeEIDServiceAuthTransaction conf doc sl
  EIDServiceTransactionProviderOnfido ->
    Onfido.completeEIDServiceAuthTransaction conf doc sl
  EIDServiceTransactionProviderSEBankID ->
    SEBankID.completeEIDServiceAuthTransaction conf doc sl
  EIDServiceTransactionProviderSmsOtp ->
    unexpectedError "SmsOtp authentication not supported via EID service"

completeEIDServiceSignTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceTransactionProvider
  -> SignatoryLink
  -> m Bool
completeEIDServiceSignTransaction conf provider sl = case provider of
  EIDServiceTransactionProviderDKNemID ->
    unexpectedError "DKNemID sign not supported via EID service"
  EIDServiceTransactionProviderNLIDIN -> NLIDIN.completeEIDServiceSignTransaction conf sl
  EIDServiceTransactionProviderNOBankID ->
    NOBankID.completeEIDServiceSignTransaction conf sl
  EIDServiceTransactionProviderVerimi -> Verimi.completeEIDServiceSignTransaction conf sl
  EIDServiceTransactionProviderFITupas ->
    FITupas.completeEIDServiceSignTransaction conf sl
  EIDServiceTransactionProviderOnfido -> Onfido.completeEIDServiceSignTransaction conf sl
  EIDServiceTransactionProviderSEBankID ->
    SEBankID.completeEIDServiceSignTransaction conf sl
  EIDServiceTransactionProviderSmsOtp ->
    unexpectedError "SmsOtp sign not supported via EID service"
