module IdentifyView.FITupasNets.View exposing (..)

import Url.Builder as Url
import Base64
import Html exposing (Html, div, a, input, iframe, text, span, b, img)
import Html.Attributes exposing (class, type_, placeholder, autocomplete, value, src, style, classList)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode as JD

import Lib.Misc.SignatoryLink exposing (getPersonalNumber)
import Lib.Types.Document exposing (Document(..), Lang(..))
import Lib.Types.SignatoryLink exposing (SignatoryLink(..))
import Lib.Types.ID exposing (showId)

import IdentifyView.Common exposing (maskedPersonalNumber)
import IdentifyView.FITupasNets.Model exposing (..)
import IdentifyView.FITupasNets.Tupas exposing (..)

view : Params msg -> State -> Html msg
view params state = case state of
  Idle ->
    div [ class "identify-box-content" ] [
      span [] [
        text params.localization.idNumber,
        text " ",
        b [] [
          text <| maskedPersonalNumber 4 <| getPersonalNumber params.signatory ] ],

      div [ class "identify-box-button" ] [
        a [ class "button", class "button-large", class "action"
          , onClick <| params.embed IdentifyButtonClickedMsg
          , style "margin-top" "27px" ] [
          div [ class "label" ] [
            text params.localization.identifyBankId ] ] ]
    ]

  Inquiry {personalNumber} ->
    div [ class "identify-box-content" ] [
      div [ class "info-text-input", classList [("redborder", not <| isPersonalNumberValid personalNumber)] ] [
        input [ class "info-text-input"
              , type_ "text"
              , placeholder params.localization.ssnInfoText
              , autocomplete False
              , value personalNumber
              , onInput <| params.embed << SetPersonalNumberInquiryMsg
              , let enterKeyCode = 13
                    parseEnterMsg = keyCode
                                    |> JD.andThen
                                        (\code -> if code == enterKeyCode
                                                  then JD.succeed (params.embed IdentifyButtonClickedMsg)
                                                  else JD.fail "non-enter key ignored")
                in on "keydown" parseEnterMsg ] [] ],
              -- ^ does not reimplement the IE9 fake placeholder hack
      div [ class "identify-box-button" ] [
        a [ class "button", class "button-large", class "action"
          , classList [("disabled", not <| isPersonalNumberValid personalNumber)]
          , style "margin-top" "27px"
          , onClick <| params.embed IdentifyButtonClickedMsg ] [
          div [ class "label" ] [
            text params.localization.identifyBankId ] ] ]
    ]

  Processing {personalNumber} ->
    iframe [ src <| identifyLink params personalNumber, style "min-height" "350px"
           , style "width" "100%", style "margin" "auto" ] []

viewFooter : Params msg -> Html msg
viewFooter {localization, signatory} =
  div [] [
    text localization.yourIdNumber,
    text " ",
    b [] [
      text <| maskedPersonalNumber 4 <| getPersonalNumber signatory ]
  ]

netsErrorMessageTranslated : Params msg -> NetsErrorMessage -> String
netsErrorMessageTranslated params err = case err of
  None -> params.localization.identifyFITupasError.failed
  AuthFailed -> params.localization.identifyFITupasError.auth
  Cancel -> params.localization.identifyFITupasError.canceled
  NoBrowser -> params.localization.identifyFITupasError.useragent
  NoCookies -> params.localization.identifyFITupasError.useragent
  NoJava -> params.localization.identifyFITupasError.useragent
  NoJavaScript -> params.localization.identifyFITupasError.useragent
  NoOS -> params.localization.identifyFITupasError.useragent
  OldOS -> params.localization.identifyFITupasError.useragent
  OldJava -> params.localization.identifyFITupasError.useragent
  OldJS -> params.localization.identifyFITupasError.useragent
  UnsupportedVersion -> params.localization.identifyFITupasError.useragent
  UnsupportedCharset -> params.localization.identifyFITupasError.useragent
  Blocked -> params.localization.identifyFITupasError.blocked
  Revoked -> params.localization.identifyFITupasError.revoked
  Expired -> params.localization.identifyFITupasError.expired

-- See https://www.nets.eu/developer/e-ident/overview/Pages/SAML.aspx.
identifyLink : Params msg -> String -> String
identifyLink params personalNumber =
  let target =
        "(" ++
          String.join ","
            [ "\"" ++ params.origin ++ "\""
            , case params.document of Document doc -> showId doc.id
            , case params.signatory of SignatoryLink sig -> showId sig.id
            , "\"" ++ params.location ++ "\""
            , personalNumber
            ]
        ++ ")"
      locale = case params.document of
        Document doc -> case doc.lang of
          Lang "sv" -> "sv_SE"
          Lang "no" -> "nb_NO"
          Lang "da" -> "da_DK"
          Lang "fi" -> "fi_FI"
          _ -> "en_GB"
  in params.netsConf.netsIdentifyUrl
    ++ Url.toQuery
      [ Url.string "mid" params.netsConf.netsMerchantIdentifier
      , Url.string "wi" "r"  -- Web interface hint: Embedded GUI
      , Url.string "forcepkivendor" "fi_tupas"
      , Url.string "style" <| params.netsConf.netsTrustedDomain ++ "/css/assets/nets_fi.css"
      , Url.string "start" <| params.netsConf.netsTrustedDomain ++ "/nets/start?status="
      , Url.string "status" <| params.netsConf.netsTrustedDomain ++ "/nets/status?status="
      , Url.string "locale" locale
      , Url.string "TARGET" <| Base64.encode target
      ]

viewLogo : Params msg -> Html msg
viewLogo params = img [ src <| params.cdnBaseUrl ++ "/img/tupas-fi.png" ] []
