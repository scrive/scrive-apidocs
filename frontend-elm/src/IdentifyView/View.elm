module IdentifyView.View exposing (..)

import Html exposing (Html, div, text, img, h4, b, a)
import Html.Attributes exposing (class, src, style, href)

import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.Document exposing (Document(..))
import Lib.Types.ID exposing (showId)
import Lib.Misc.SignatoryLink exposing (getSmartName)
import Lib.Misc.HtmlTemplates exposing (evaluateHtmlTemplate)
import Lib.Misc.Document exposing (getAuthorName)

import IdentifyView.SEBankIDCGI.View as SEBankIDCGI
import IdentifyView.SMSPin.SMSPin as SMSPin
import IdentifyView.FITupasNets.View as FITupasNets
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

  IdentifyView {params, innerModel} ->
    let logoSrc = String.join "/"
          [ params.cdnBaseUrl
          , "signview_logo"
          , params.brandingDomainId
          , case params.document of Document doc -> showId doc.id
          , params.brandingHash ]
    in
    div [] [
      underConstructionNotice params.location,

      FlashMessage.view {embed = FlashMessageMsg} flashMessages,

      div [ class "identify-content"] [
        div [ class "identify-logo-box" ] [
          img [ class "identify-logo-img", src logoSrc ] [],
          div [ class "divider-line" ] [],
          h4 [ class "identify-logo-text" ] [
            text params.localization.esigningpoweredbyscrive ] ],

        div [ class "identify-box" ] [
          div [ class "identify-box-header" ] [
            case getSmartName params.signatory of
              Just name -> evaluateHtmlTemplate [{key = "name-of-signatory", value = name}] params.localization.verifyIdentityWithName
              Nothing -> text params.localization.verifyIdentityWithoutName ],

          case innerModel of
            IdentifySMSPin innerState ->
              SMSPin.viewContent (toSMSPinParams params) innerState
            IdentifyGenericEidService provider innerState ->
              GenericEidService.viewContent (toGenericEidServiceParams params provider) innerState
            IdentifySEBankIDCGI inner ->
              SEBankIDCGI.view (toSEBankIDCGIParams params) inner
            IdentifyFITupasNets inner -> FITupasNets.view (toFITupasNetsParams params) inner,

          div [ class "identify-box-footer" ] [
            div [ class "identify-box-footer-text" ] [
              div [] [
                text params.localization.header.contact,
                text " ",
                b [] [
                  text <| Maybe.withDefault "Empty" <| getAuthorName params.document ] ],

              div [] [
                text params.localization.identifyDocument,
                text " ",
                b [] [
                  text <| case params.document of Document doc -> doc.title ] ],

              case innerModel of
                IdentifySMSPin _ -> SMSPin.viewFooter (toSMSPinParams params)
                IdentifyGenericEidService provider _ -> GenericEidService.viewFooter (toGenericEidServiceParams params provider)
                IdentifySEBankIDCGI _ -> SEBankIDCGI.viewFooter (toSEBankIDCGIParams params)
                IdentifyFITupasNets _ -> FITupasNets.viewFooter (toFITupasNetsParams params) ],

            div [ class "identify-box-footer-logo" ] [
              case innerModel of
                IdentifySMSPin _ -> div [] []
                IdentifyGenericEidService provider _ -> GenericEidService.viewLogo (toGenericEidServiceParams params provider)
                IdentifySEBankIDCGI _ -> SEBankIDCGI.viewLogo (toSEBankIDCGIParams params)
                IdentifyFITupasNets _ -> FITupasNets.viewLogo (toFITupasNetsParams params) ] ] ] ]
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
