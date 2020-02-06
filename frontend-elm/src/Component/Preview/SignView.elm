module Component.Preview.SignView exposing (view)

import Component.Theme.Data exposing (Theme)
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (class, src, style)
import Vendor.ColorPickerExtra exposing (color2Hex)


view : Theme -> Html msg
view theme =
    let
        themeColors =
            theme.themeColors

        actionTextColor =
            themeColors.actionColors.textColor

        actionBackgroundColor =
            themeColors.actionColors.backgroundColor

        actionSecondaryColor =
            themeColors.secondaryActionColors.backgroundColor

        brandTextColor =
            themeColors.brandColors.textColor

        brandBackgroundColor =
            themeColors.brandColors.backgroundColor
    in
    div
        [ class "sample-sign-view" ]
        [ div
            [ class "sample-sign-view-header"
            , style "background-color" <| color2Hex brandBackgroundColor
            ]
            [ div [ class "logo" ] [ img [ src <| theme.logo ] [] ]
            , div
                [ style "color" <|
                    color2Hex brandTextColor
                , class "header-text"
                ]
                [ -- localization.sampleSignView.signViewHeader
                  text "Name"
                ]
            , div [ style "clear" "both" ] []
            ]
        , div [ style "font-family" theme.font, class "content" ]
            [ div [ class "inner-content" ]
                [ div [ class "sample-controls" ]
                    [ div [ class "download-container" ]
                        [ -- var DownloadPdfIcon = require("../../icons/download_pdf_icon.svg");
                          div [] []
                        ]
                    , div [ class "document-title" ]
                        [ text "Signing document: Demo contract"
                        ]
                    ]
                , div [ class "section contentheader" ]
                    [ div [ class "instructions" ]
                        [ span [] [ text "Follow the " ]
                        , span
                            [ class "arrowtext"
                            , style "color" <| color2Hex actionBackgroundColor
                            ]
                            [ text "ARROW" ]
                        ]
                    ]
                , div [ class "document" ]
                    [ -- var ArrowDown = require("../../icons/arrow-down.svg");
                      div [] []
                    , div [ class "field mandatoryfield" ]
                        [ div
                            [ class "placedfield mandatoryplacedfield"
                            , style "border-color" <| color2Hex actionBackgroundColor
                            ]
                            [ div [ class "placedfieldvalue" ]
                                [ -- localization.sampleSignView.email
                                  text "Email"
                                ]
                            ]
                        , -- var ArrowRight = require("../../icons/arrow-right.svg");
                          div [] []
                        ]
                    , div [ class "field optionalfield" ]
                        [ div
                            [ class "placedfield optionalplacedfield"
                            , style "border-color" <| color2Hex actionSecondaryColor
                            ]
                            [ div [ class "placedfieldvalue" ]
                                [ -- localization.sampleSignView.phone
                                  text "Mobile"
                                ]
                            ]
                        ]
                    , img [ class "exampledocument", src "/img/document_example.png" ] []
                    ]
                , div [ class "section signsection" ]
                    [ a
                        [ class "button action"
                        , style "background-color" <| color2Hex actionBackgroundColor
                        , style "color" <| color2Hex actionTextColor
                        ]
                        [ -- localization.sampleSignView.signButton
                          text "Next"
                        ]
                    , a [ class "button reject" ]
                        [ div [ class "label" ]
                            [ -- localization.sampleSignView.rejectButton
                              text "Reject and reply"
                            ]
                        ]
                    ]
                , div [ class "section footer" ]
                    [ img [ class "logo", src "/img/poweredby.svg" ] []
                    ]
                ]
            ]
        ]
