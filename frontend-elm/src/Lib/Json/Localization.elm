module Lib.Json.Localization exposing (..)

import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)

import Lib.Types.Localization exposing (..)

localisationDecoder : Decoder Localization
localisationDecoder =
  succeed Localization
  |> required "cancel" string
  |> required "docsignview" (
      succeed Docsignview
      |> required "eleg" (
          succeed Eleg
          |> required "bankid" (
              succeed Bankid
              |> required "accessDenied" string
              |> required "invalidParametersCanChange" string
              |> required "invalidParametersCantChange" string
              |> required "rfa1" string
              |> required "rfa3" string
              |> required "rfa5" string
              |> required "rfa6" string
              |> required "rfa8" string
              |> required "rfa9" string
              |> required "rfa12" string
              |> required "rfa13" string
              |> required "rfa14" string
              |> required "rfa16" string
              |> required "rfa17" string
              |> required "rfa18" string
              )
          )
      |> required "networkIssueErrorMessage" string
      )
  |> required "eID" (
      succeed EID
      |> required "infoText" (
          succeed EidInfoText
          |> required "cpr" string
          )
      )
  |> required "esigningpoweredbyscrive" string
  |> required "header" (
      succeed Header
      |> required "contact" string
      )
  |> required "identify" (
      succeed Identify
      |> required "identifyWithIdin" string
      |> required "identifyWithTupas" string
      |> required "identifyWithVerimi" string
      )
  |> required "identifyBankId" string
  |> required "identifyDocument" string
  |> required "identifyFITupasError" (
        succeed IdentifyFITupasError
        |> required "auth" string
        |> required "blocked" string
        |> required "canceled" string
        |> required "expired" string
        |> required "failed" string
        |> required "revoked" string
        |> required "useragent" string
      )
  |> required "identifyWithPin" (
      succeed IdentifyWithPin
      |> required "enterPin" string
      |> required "invalidPin" string
      |> required "pinInfotext" string
      )
  |> required "idNumber" string
  |> required "ok" string
  |> required "openBankId" string
  |> required "ssnInfoText" string
  |> required "verifyIdentityWithName" string
  |> required "verifyIdentityWithoutName" string
  |> required "yourEmail" string
  |> required "yourIdNumber" string
  |> required "yourMobileNumber" string
