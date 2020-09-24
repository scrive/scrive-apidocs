module IdentifyView.View exposing (..)

import Html exposing (Html, div, text, img, h4, b, a)
import Html.Attributes exposing (class, src, style, href)

import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.Document exposing (Document(..))
import Lib.Types.ID exposing (showId)
import Lib.Misc.SignatoryLink exposing (getSmartName)
import Lib.Misc.HtmlTemplates exposing (evaluateHtmlTemplate)
import Lib.Misc.Document exposing (getAuthorName)
import Utils exposing (stringNonEmpty)

import IdentifyView.SMSPin.SMSPin as SMSPin
import IdentifyView.GenericEidService.GenericEidService as GenericEidService
import IdentifyView.Model exposing (..)

view : Model -> Html Msg
view {flashMessages, state} = case state of
  Error {errorView} ->
    div [] [
      errorView,
      FlashMessage.view {embed = FlashMessageMsg} flashMessages
    ]

  Loading _ -> div [ class "loadingSpinner" ] []

  IdentifyView {flags, innerModel} ->
    div [] [
      underConstructionNotice flags.location,

      FlashMessage.view {embed = FlashMessageMsg} flashMessages,

      div [ class "identify-content"] [
        div [ class "identify-logo-box" ] [
          img [ class "identify-logo-img", src flags.logoUrl ] [],
          div [ class "divider-line" ] [],
          h4 [ class "identify-logo-text" ] [
            text flags.localization.esigningpoweredbyscrive ] ],

        div [ class "identify-box" ] [
          div [ class "identify-box-header" ] [ text flags.welcomeText ],

          case innerModel of
            IdentifySMSPin innerState ->
              SMSPin.viewContent (toSMSPinParams flags) innerState
            IdentifyGenericEidService provider innerState ->
              GenericEidService.viewContent (toGenericEidServiceParams flags provider) innerState,

          div [ class "identify-box-footer" ] [
            div [ class "identify-box-footer-text" ] [
              div [] [
                text flags.localization.header.contact,
                text " ",
                b [] [
                  text <| Maybe.withDefault "Empty" <| stringNonEmpty flags.authorName ] ],

              div [] [
                text flags.entityTypeLabel,
                text " ",
                b [] [ text <| Maybe.withDefault "Untitled" <| stringNonEmpty flags.entityTitle ] ],

              case innerModel of
                IdentifySMSPin _ -> SMSPin.viewFooter (toSMSPinParams flags)
                IdentifyGenericEidService provider _ -> GenericEidService.viewFooter (toGenericEidServiceParams flags provider)
            ],

            div [ class "identify-box-footer-logo" ] [
              case innerModel of
                IdentifySMSPin _ -> div [] []
                IdentifyGenericEidService provider _ -> GenericEidService.viewLogo (toGenericEidServiceParams flags provider)
            ] ] ] ]
    ]

-- TODO: Temporary: Remove this when the app is usable with Flow.
underConstructionNotice : String -> Html Msg
underConstructionNotice currentUrl =
    div [ style "background" "#EDA621"
        , style "color" "#fff"
        , style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "height" "40px"
        , style "line-height" "40px"
        , style "text-align" "center"
        , style "width" "100%"
        ]
        [ text "This authentication page is under construction. You can "
        , a [ href <| currentUrl ++ "?bypass_identify"
            , style "color" "#fff"
            , style "text-decoration" "underline"
            ]
            [ text "go directly to the Flow overview page" ]
        ]
