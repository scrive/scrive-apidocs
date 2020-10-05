module IdentifyView.View exposing (..)

import Html exposing (Html, div, text, img, h4, b, a, p, textarea)
import Html.Attributes exposing (class, src, style, href)
import Html.Events exposing (onClick)

import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.Document exposing (Document(..))
import Lib.Types.ID exposing (showId)
import Lib.Misc.SignatoryLink exposing (getSmartName)
import Lib.Misc.HtmlTemplates exposing (evaluateHtmlTemplate)
import Lib.Misc.Document exposing (getAuthorName)
import Utils exposing (stringNonEmpty)

import IdentifyView.SMSPin.SMSPin as SMSPin
import IdentifyView.GenericEidService.GenericEidService as GenericEidService
import IdentifyView.Rejection.Rejection as Rejection
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
      FlashMessage.view {embed = FlashMessageMsg} flashMessages,

      div [ class "identify-content"] [
        div [ class "identify-logo-box" ] [
          img [ class "identify-logo-img", src flags.logoUrl ] [],
          div [ class "divider-line" ] [],
          h4 [ class "identify-logo-text" ] [
            text flags.localization.esigningpoweredbyscrive ] ],

        div [ class "identify-box" ] [
          div [ class "identify-box-header" ] [ text flags.welcomeText ],

          case (innerModel, flags.maxFailuresExceeded) of
            (IdentifyRejection params innerState, _) ->
                Rejection.viewContent params innerState
            (_, True) ->
                maxFailuresExceededContent
            (IdentifySMSPin params innerState, False) ->
                SMSPin.viewContent params innerState rejectionLink
            (IdentifyGenericEidService params innerState, False) ->
                GenericEidService.viewContent params innerState rejectionLink,

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
                IdentifySMSPin params _ -> SMSPin.viewFooter params
                IdentifyGenericEidService params _ -> GenericEidService.viewFooter params
                IdentifyRejection params _ -> Rejection.viewFooter params
            ],

            div [ class "identify-box-footer-logo" ] [
              case innerModel of
                IdentifySMSPin _ _ -> div [] []
                IdentifyGenericEidService params _ -> GenericEidService.viewLogo params
                IdentifyRejection _ _ -> div [] []
            ] ] ] ]
    ]

maxFailuresExceededContent : Html Msg
maxFailuresExceededContent =
    div [ class "identify-box-content" ]
        [ p [ style "margin-bottom" "1em" ]
            [ text """We are sorry but you have exceeded the maximum number of
                      authentication attempts."""
            ]
        , p []
            [ a [ class "button"
                , onClick EnterRejectionClickedMsg
                ]
                [ text "Reject the documents and contact us" ]
            ]
        ]

rejectionLink : Html Msg
rejectionLink =
    div [ style "margin-top" "1em"]
        [ text "You can also "
        , a [ class "text-positivecolor"
            , onClick <| EnterRejectionClickedMsg
            ]
            [ text "reject the documents and contact us." ]
        ]

