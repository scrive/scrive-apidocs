module IdentifyView.GenericEidService.GenericEidService exposing (..)

import Http exposing (post, Error, expectJson)
import Html exposing (Html, text, span, iframe, div, a, img, b)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Encode as JE
import Url
import Base64
import Browser.Navigation

import Lib.Types.Localization exposing (Localization)
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Lib.Types.ID exposing (ID, showId)
import Lib.Types.Document exposing (Document)
import Lib.Types.SignatoryLink exposing (SignatoryLink(..))
import IdentifyView.Common exposing (maskedPersonalNumber)
import Lib.Misc.SignatoryLink exposing (getPersonalNumber, getEmail)
import Lib.Misc.Http exposing (formBody, encodeError)
import Lib.Misc.Cmd exposing (perform)

{- Provider specific stuff -}

type Provider = DKNemID | FITupas | IDIN | NOBankID | SEBankID | Verimi | Onfido

type ProcessingAction = Embed | Redirect

-- What's the reasoning behind these?
processingAction : Provider -> ProcessingAction
processingAction provider = case provider of
  DKNemID -> Embed
  NOBankID -> Embed
  SEBankID -> Embed
  FITupas -> Redirect
  IDIN -> Redirect
  Verimi -> Redirect
  Onfido -> Redirect

identifyButtonText : Params msg -> String
identifyButtonText params = case params.provider of
  DKNemID -> params.localization.identifyBankId
  FITupas -> params.localization.identify.identifyWithTupas
  IDIN -> params.localization.identify.identifyWithIdin
  NOBankID -> params.localization.identifyBankId
  SEBankID -> params.localization.identifyBankId
  Verimi -> params.localization.identify.identifyWithVerimi
  Onfido -> "Identify with Onfido" -- TODO: Localisation

logoSrc : Provider -> String
logoSrc provider = case provider of
  DKNemID -> "/img/nemid-dk.png"
  NOBankID -> "/img/bankid-no.png"
  SEBankID -> "/img/bankid2.png"
  FITupas -> "/img/tupas-fi.png"
  IDIN -> "/img/iDIN.png"
  Verimi -> "/img/verimi.svg"
  Onfido -> "/img/onfido.png"

{- Params / State -}

type alias Params msg =
  { embed : Msg -> msg
  , addFlashMessageMsg : FlashMessage -> msg
  , errorTraceMsg : List (String, JE.Value) -> msg
  , provider : Provider
  , xtoken : String
  , location : String  -- window.location.href
  , localization : Localization
  , cdnBaseUrl : String
  , participantEmail : String
  , startUrl : String
  }

type State = Idle | Loading | Processing { accessUrl : String }

{- Msg / update -}

type Msg = IdentifyButtonClickedMsg
         | EidServiceStartCallbackMsg (Result Error String)

update : Params msg -> State -> Msg -> (State, Cmd msg)
update params _ msg = case msg of
  IdentifyButtonClickedMsg ->
    let postReq = post
          { url = params.startUrl
          , body = formBody params [("redirect", Base64.encode <| Url.percentEncode params.location)]
          , expect = expectJson (params.embed << EidServiceStartCallbackMsg) (JD.field "accessUrl" JD.string)
          }
    in (Loading, postReq)

  EidServiceStartCallbackMsg res -> case res of
    Ok url ->
      let cmd = if processingAction params.provider == Redirect
                then Browser.Navigation.load url
                else Cmd.none
      in (Processing {accessUrl = url}, cmd)
    Err err ->
      let flashMessage = "Internal error: failed to communicate with backend."
          errorFields =
            [ ("where", JE.string "IdentifyView.GenericEidService.update EidServiceStartCallbackMsg")
            , ("what", JE.string flashMessage)
            , ("http_error", encodeError err) ]
      in (Idle
          , Cmd.batch [ perform <| params.addFlashMessageMsg <| FlashError flashMessage
                      , perform <| params.errorTraceMsg errorFields])

{- view -}

viewContent : Params msg -> State -> Html msg
viewContent params state = case state of
  Idle ->
    div [ class "identify-box-content" ] [
      div [ class "identify-box-button" ] [
        a [ class "button", class "button-large", class "action"
          , onClick <| params.embed IdentifyButtonClickedMsg ] [
          div [ class "label" ] [
            text <| identifyButtonText params ] ] ]
    ]

  Loading -> div [ class "loadingSpinner" ] []

  Processing { accessUrl } -> case processingAction params.provider of
    Redirect -> div [ class "loadingSpinner" ] []
    Embed ->
      span [] [
        iframe [ src accessUrl, style "min-height" "350px", style "width" "100%"
              , style "margin" "auto" ] []
      ]

viewFooter : Params msg -> Html msg
viewFooter {provider, localization, participantEmail} =
  div [] [
    text localization.yourEmail,
    text " ",
    b [] [ text participantEmail ]
  ]

viewLogo : Params msg -> Html msg
viewLogo params = case params.provider of
  -- SEBankID has two logos
  SEBankID ->
    div [] [
      img [ src <| params.cdnBaseUrl ++ "/img/mobilebankid.png"
          , class "identify-box-footer-first-logo"] [],
      img [ src <| params.cdnBaseUrl ++ "/img/bankid2.png" ] []
    ]

  _ -> img [ src <| params.cdnBaseUrl ++ logoSrc params.provider ] []

