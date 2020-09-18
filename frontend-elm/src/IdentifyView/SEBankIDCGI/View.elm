module IdentifyView.SEBankIDCGI.View exposing (..)

import Html.Attributes exposing (class, classList, tabindex, style, src)
import Html.Events exposing (onClick)
import Html exposing (Html, text, div, span, b, a, h3, img)

import Lib.Types.Localization exposing (Localization)
import Lib.Types.SignatoryLink exposing (SignatoryLink(..), DeliveryMethod(..))
import Lib.Misc.SignatoryLink exposing (getPersonalNumber)

import IdentifyView.Common exposing (maskedPersonalNumber)
import IdentifyView.SEBankIDCGI.BankID exposing (..)
import IdentifyView.SEBankIDCGI.Model exposing (..)

translatedStatusMessage : {useBankIdLink : Bool} -> Localization -> BankIdState -> String
translatedStatusMessage {useBankIdLink} localization bankIdState = case bankIdState of
  BankIdComplete -> "complete"  -- never shown
  BankIdUserSign -> localization.docsignview.eleg.bankid.rfa9
  BankIdOutstandingTransaction ->
    if useBankIdLink
      then localization.docsignview.eleg.bankid.rfa13
      else localization.docsignview.eleg.bankid.rfa1
  BankIdNoClient -> localization.docsignview.eleg.bankid.rfa1
  BankIdStarted -> localization.docsignview.eleg.bankid.rfa14

translatedErrorMessage : Localization -> BankIdError -> String
translatedErrorMessage localization bankIdError = case bankIdError of
  BankIdInvalidParameters ->
    if False
    -- ^ We should show `invalidParametersCanChange` if the signatory can change
    -- the personal number, but that's never the case!
      then localization.docsignview.eleg.bankid.invalidParametersCanChange
      else localization.docsignview.eleg.bankid.invalidParametersCantChange
  BankIdAlreadyInProgress -> localization.docsignview.eleg.bankid.rfa3
  BankIdAccessDeniedRP -> localization.docsignview.eleg.bankid.accessDenied
  BankIdRetry -> localization.docsignview.eleg.bankid.rfa5
  BankIdInternalError -> localization.docsignview.eleg.bankid.rfa5
  BankIdExpiredTransaction -> localization.docsignview.eleg.bankid.rfa8
  BankIdUserCancel -> localization.docsignview.eleg.bankid.rfa6
  BankIdClientErr -> localization.docsignview.eleg.bankid.rfa12
  BankIdCertificateErr -> localization.docsignview.eleg.bankid.rfa16
  BankIdCancelled -> localization.docsignview.eleg.bankid.rfa3
  BankIdStartFailed -> localization.docsignview.eleg.bankid.rfa17

view : Params msg -> State -> Html msg
view params state = case state.innerState of
  -- | Masked personal number; identify button; 'open on this device' checkbox.
  Idle ->
    div [ class "identify-box-content" ] [
      span [] [
        text params.localization.idNumber,
        text " ",
        b [] [ text <| maskedPersonalNumber 4 <| getPersonalNumber params.signatory ] ],

      div [ class "identify-box-button" ] [
        a [ class "button", class "button-large", class "action"
          , style "margin-top" "30px", style "margin-bottom" "38px"
          , onClick <| params.embed IdentifyButtonClickedMsg ] [
          div [ class "label" ] [
            text params.localization.identifyBankId ] ] ],

      let method = case params.signatory of SignatoryLink {deliveryMethod} -> deliveryMethod
      in case method of
        PadDelivery -> div [] []
        -- ^ When signing in-store we disable 'open BankID on this device'.
        _ ->
          div [ class "checkbox-box", class "identify-box-checkbox"
              , onClick <| params.embed UseBankIdCheckboxMsg ] [
            div [ class "checkbox-box-wrapper" ] [
              div [ classList [("checkbox", True), ("checked", state.useBankIdLink)]
                  , tabindex 0 ] [
                div [ class "checkmark" ] [] ] ],
            div [ class "checkbox-label" ] [ text params.localization.openBankId ] ]
    ]

  -- | Status text (rfa1/13); loading spinner; cancel button.
  WaitForCgiGrpAuthEndpoint _ ->
    div [ class "identify-box-content" ] [
      h3 [ class "identify-box-heading" ] [
        text <| translatedStatusMessage {useBankIdLink = state.useBankIdLink} params.localization BankIdOutstandingTransaction ],

      div [ class "identify-box-spinner" ] [],

      div [ class "identify-box-cancel", onClick <| params.embed CancelButtonClickedMsg ] [
        text params.localization.cancel ]
    ]

  -- | Status text; loading spinner; if someTimeHasPassed then bankid://<..>
  -- button; cancel button.
  StartClientViaBankIdUrl {bankIdState, someTimeHasPassed} ->
    div [ class "identify-box-content" ] <| [
      h3 [ class "identify-box-heading" ] [
        text <| translatedStatusMessage {useBankIdLink = state.useBankIdLink} params.localization bankIdState ],

      div [ class "identify-box-spinner" ] [] ]

      ++ (let startBankIdAppButton =
                  div [ class "identify-box-button" ] [
                    a [ class "button", class "button-large", class "action"
                      , style "margin-bottom" "38px"
                      , onClick <| params.embed StartBankIdAppButtonClickedMsg ] [
                      div [ class "label" ] [
                        text params.localization.docsignview.eleg.bankid.rfa18 ] ] ]
          in if someTimeHasPassed then [startBankIdAppButton] else [])

      ++ [ div [ class "identify-box-cancel", onClick <| params.embed CancelButtonClickedMsg ] [
            text params.localization.cancel ] ]

  -- | Status text; loading spinner; cancel button.
  WaitForUserToStartClient {bankIdState} ->
    div [ class "identify-box-content" ] [
      h3 [ class "identify-box-heading" ] [
        text <| translatedStatusMessage {useBankIdLink = state.useBankIdLink} params.localization bankIdState ],

      div [ class "identify-box-spinner" ] [],

      div [ class "identify-box-cancel", onClick <| params.embed CancelButtonClickedMsg ] [
        text params.localization.cancel ]
    ]

  -- | Status text; loading spinner; cancel button.
  WaitForUserToSign _ ->
    div [ class "identify-box-content" ] [
      h3 [ class "identify-box-heading" ] [
        text <| translatedStatusMessage {useBankIdLink = state.useBankIdLink} params.localization BankIdUserSign ],

      div [ class "identify-box-spinner" ] [],

      div [ class "identify-box-cancel", onClick <| params.embed CancelButtonClickedMsg ] [
        text params.localization.cancel ]
    ]

  -- | Status text; ok button.
  Problem {bankIdError} ->
    div [ class "identify-box-content" ] [
      text <| translatedErrorMessage params.localization bankIdError,

      div [ class "identify-box-button" ] [
        a [ class "button", class "button-large", class "action"
          , style "margin-top" "34px"
          , onClick <| params.embed ProblemAcknowledgedButtonClickedMsg ] [
          div [ class "label" ] [
            text params.localization.ok ] ] ]
    ]

viewFooter : Params msg -> Html msg
viewFooter {localization, signatory} =
  div [] [
    text localization.yourIdNumber,
    text " ",
    b [] [
      text <| maskedPersonalNumber 4 <| getPersonalNumber signatory ]
  ]

viewLogo : Params msg -> Html msg
viewLogo params =
  div [] [
    img [ src <| params.cdnBaseUrl ++ "/img/mobilebankid.png"
        , class "identify-box-footer-first-logo"] [],
    img [ src <| params.cdnBaseUrl ++ "/img/bankid2.png" ] []
  ]
