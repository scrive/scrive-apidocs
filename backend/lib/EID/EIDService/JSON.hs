module EID.EIDService.JSON (
    encodeNewTransactionRequest
  ) where

import Data.Aeson
import Data.Aeson.Encoding

import EID.EIDService.Types

encodeNewTransactionRequest :: EIDServiceProviderParams -> Encoding
encodeNewTransactionRequest espp =
  pairs
    $  ("method" .= ("auth" :: String))
    <> ("provider" .= provider)
    <> ("redirectUrl" .= esppRedirectURL espp)
    <> providerParameters
  where
    -- TODO: This function has repetition in many places
    provider = case espp of
      EIDServiceProviderParamsVerimi{}   -> "verimi"
      EIDServiceProviderParamsIDIN{}     -> "nlIDIN"
      EIDServiceProviderParamsNemID{}    -> "dkNemID"
      EIDServiceProviderParamsNOBankID{} -> "noBankID"
    makeProviderParams vals =
      pair "providerParameters" . pairs . pair provider $ pairs vals
    providerParameters = case espp of
      EIDServiceProviderParamsVerimi{} -> mempty
      EIDServiceProviderParamsIDIN{} -> makeProviderParams $ ("requestBirthdate" .= True)
      EIDServiceProviderParamsNemID {..} ->
        makeProviderParams $ ("limitedClientMode" .= True) <> ("uiLocale" .= esppUILocale)
      EIDServiceProviderParamsNOBankID {..} ->
        let phonePair = maybe mempty ("phoneNumber" .=) esppPhoneNumber
        in  makeProviderParams $ ("personalNumber" .= esppPersonalNumber) <> phonePair
