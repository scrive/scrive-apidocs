module IdentifyView.SMSPin.SMSPin exposing (..)

import Http exposing (Error, post, expectWhatever)
import Html.Attributes exposing (class, type_, autocomplete, value, placeholder)
import Html.Events exposing (onClick, onInput, keyCode, on)
import Html exposing (Html, div, input, text, a, b)
import Platform.Cmd as Cmd
import Lib.Types.Localization exposing (Localization)
import Browser.Navigation exposing (reload)
import Json.Decode as JD
import Json.Encode as JE

import Lib.Misc.Cmd exposing (perform)
import Lib.Misc.Http exposing (formBody, encodeError)
import Lib.Types.ID exposing (ID, showId)
import Lib.Types.Document exposing (Document)
import Lib.Types.SignatoryLink exposing (SignatoryLink(..))
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Lib.Misc.SignatoryLink exposing (getMobileNumber)

type alias Params msg =
  { embed : Msg -> msg
  , addFlashMessageMsg : FlashMessage -> msg
  , errorTraceMsg : List (String, JE.Value) -> msg
  , xtoken : String
  , localization : Localization
  , participantMaskedMobile : String
  , sendUrl : String
  , verifyUrl : String
  }

-- modifiable part of the model
type State = Idle | EnterPin {pin : String}

type Msg = IdentifyButtonClickedMsg
         | SendSMSPinToViewCallbackMsg (Result Error ())
         | UpdatePinFieldMsg String
         | ConfirmPinButtonClickedMsg
         | SMSPinIdentifyToViewCallbackMsg (Result Error ())

update : Params msg -> State -> Msg -> (State, Cmd msg)
update params state msg = case msg of
  IdentifyButtonClickedMsg ->
    let postReq = post
          { url = params.sendUrl
          , body = formBody params []
          , expect = expectWhatever <| params.embed << SendSMSPinToViewCallbackMsg
          }
    in (EnterPin {pin = ""}, postReq)

  SendSMSPinToViewCallbackMsg res -> case res of
    Ok () -> (state, Cmd.none)
    Err err ->
      let flashMessage = "Failed to request SMS pin"
          errorFields =
            [ ("where", JE.string "IdentifyView.SMSPin.update SendSMSPinToViewCallbackMsg")
            , ("what", JE.string flashMessage)
            , ("http_error", encodeError err)
            ]
      in ( Idle
         , Cmd.batch [ perform <| params.addFlashMessageMsg <| FlashError flashMessage
                     , perform <| params.errorTraceMsg errorFields ] )

  UpdatePinFieldMsg pin -> case state of
    EnterPin _ -> (EnterPin {pin = pin}, Cmd.none)
    _ -> (state, Cmd.none)

  ConfirmPinButtonClickedMsg -> case state of
    EnterPin {pin} ->
      let postReq = post
            { url = params.verifyUrl
            , body = formBody params [("sms_pin", pin)]
            , expect = expectWhatever <| params.embed << SMSPinIdentifyToViewCallbackMsg
            }
      in (state, postReq)
    _ -> (state, Cmd.none)

  SMSPinIdentifyToViewCallbackMsg res -> case res of
    Ok () -> (state, reload)  -- reloading the page will show the document
    Err err -> case err of
      Http.BadStatus 400 ->  -- wrong PIN results in a 400 response
        let flashMessage = params.localization.identifyWithPin.invalidPin
        in ( state, perform <| params.addFlashMessageMsg <| FlashError flashMessage )

      _ ->  -- An unexpected error has occurred. Show 'invalid pin' message to make the user try again.
        let flashMessage = params.localization.identifyWithPin.invalidPin
            errorFields =
              [ ("where", JE.string "IdentifyView.SMSPin.update SMSPinIdentifyToViewCallbackMsg")
              , ("what", JE.string "smspinidentifytoview request failed")
              , ("http_error", encodeError err)
              ]
        in ( EnterPin {pin = ""}
           , Cmd.batch [ perform <| params.addFlashMessageMsg <| FlashError flashMessage
                       , perform <| params.errorTraceMsg errorFields ] )

viewContent : Params msg -> State -> Html msg
viewContent params state = case state of
  -- Identify button.
  Idle ->
    div [ class "identify-box-content" ] [
      div [ class "identify-box-button" ] [
        a [ class "button", class "button-large", class "action"
          , onClick <| params.embed IdentifyButtonClickedMsg ] [
          div [ class "label" ] [
            text params.localization.identifyBankId ] ]
      ]
    ]

  -- "Enter pin"; pin input box; confirm button.
  EnterPin {pin} ->
    div [ class "identify-box-content" ] [ div [ class "identify-with-pin-box" ] [
      text params.localization.identifyWithPin.enterPin,

      -- Does not reimplement the IE9 fake placeholder hack!
      div [ class "info-text-input" ] [
        input [ class "info-text-input"
              , type_ "number"
              , placeholder params.localization.identifyWithPin.pinInfotext
              , autocomplete False
              , value pin
              , onInput <| params.embed << UpdatePinFieldMsg
              , let enterKeyCode = 13
                    parseEnterMsg = keyCode
                                    |> JD.andThen
                                        (\code -> if code == enterKeyCode
                                                  then JD.succeed (params.embed ConfirmPinButtonClickedMsg)
                                                  else JD.fail "non-enter key ignored")

                in on "keydown" parseEnterMsg ] [] ],

        div [ class "identify-box-button" ] [
          a [ class "button", class "button-large", class "action"
            , onClick <| params.embed ConfirmPinButtonClickedMsg ] [
            div [ class "label" ] [
              text params.localization.identifyBankId ] ] ] ]
    ]

viewFooter : Params msg -> Html msg
viewFooter { localization, participantMaskedMobile } =
  div [] [
    text localization.yourMobileNumber,
    text " ",
    b [] [
      text <| participantMaskedMobile ]
  ]
