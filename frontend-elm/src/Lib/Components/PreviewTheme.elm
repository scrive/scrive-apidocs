module Lib.Components.PreviewTheme exposing (viewEmailThemePreview, viewSignViewThemePreview, viewServiceThemePreview, viewLoginThemePreview)

import Lib.Types.Theme exposing (..)
import Html exposing (Html, a, div, img, p, strong, text, span, input)
import Html.Attributes exposing (class, placeholder, readonly, src, style, type_, value)
import Vendor.ColorPickerExtra exposing (color2Hex)
import EnumExtra as Enum
import Color exposing (Color, rgb255)
import Url

getThemeColor : Theme -> ColorIdentifier -> Color
getThemeColor theme ident = Maybe.withDefault (rgb255 85 107 47) <| Enum.get ident theme.colors

viewEmailThemePreview : Theme -> Html msg
viewEmailThemePreview theme =
  let color label = style label << color2Hex << getThemeColor theme
      themeFont = style "font-family" (Enum.toString enumFont theme.font)
  in div
        [ class "email-preview"
        , color "backgroundColor" { kind = BrandColors, component = BackgroundColor }
        ]
        [ div [ class "logo-wrapper" ] [ img [ src theme.logo ] [] ]
        , div [ class "content-container", themeFont ]
            [ div [ class "content" ]
                [ div [ class "document-preview" ] [ img [ src "/img/document.png" ] [] ]
                , div [ class "invitation" ]
                    [ p []
                        [ -- localization.companyBranding.brandingPreview.emailContent
                          strong [] <| [ text "John Smith" ]
                        , text " has invited you to e-sign the document "
                        , strong [] <| [ text "Demo Document" ]
                        , text "."
                        ]
                    , p []
                        [ strong [] <|
                            [ -- localization.companyBranding.brandingPreview.emailInstructions
                              text "Click the button for further instructions."
                            ]
                        ]
                    , a
                        [ class "button action"
                        , color "backgroundColor" { kind = ActionColors, component = BackgroundColor }
                        , color "color" { kind = ActionColors, component = TextColor }
                        , themeFont
                        ]
                        [ div [ class "label" ]
                            [ -- localization.companyBranding.brandingPreview.emailButtonLabel
                              text "Go to document"
                            ]
                        ]
                    ]
                ]
            , div [ class "footer" ]
                [ p []
                    [ -- localization.companyBranding.brandingPreview.emailFooter
                      text "This email contains confidential information and should not be forwarded. Disclosure, copying, distribution or other processing of the information contained in this message is prohibited and may be unlawful. If you have received this in error please notify the sender and delete immediately."
                    ]
                ]
            ]
        ]

viewSignViewThemePreview : Theme -> Html msg
viewSignViewThemePreview theme =
  let color label = style label << color2Hex << getThemeColor theme
  in div
        [ class "sample-sign-view" ]
        [ div
            [ class "sample-sign-view-header"
            , color "background-color" { kind = BrandColors, component = BackgroundColor }
            ]
            [ div [ class "logo" ] [ img [ src <| theme.logo ] [] ]
            , div
                [ color "color" { kind = BrandColors, component = TextColor }
                , class "header-text"
                ]
                [ -- localization.sampleSignView.signViewHeader
                  text "Name"
                ]
            , div [ style "clear" "both" ] []
            ]
        , div [ class "content" ]
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
                            , color "color" { kind = ActionColors, component = BackgroundColor }
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
                            , color "border-color" { kind = ActionColors, component = BackgroundColor }
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
                            , color "border-color" { kind = SecondaryActionColors, component = BackgroundColor }
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
                        , color "background-color" { kind = ActionColors, component = BackgroundColor }
                        , color "color" { kind = ActionColors, component = TextColor }
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

viewServiceThemePreview : Theme -> Html msg
viewServiceThemePreview theme =
  let color label = style label << color2Hex << getThemeColor theme
  in div [ class "service-preview" ]
        [ div
            [ class "service-preview-header"
            , color "background-color" { kind = BrandColors, component = BackgroundColor }
            ]
            [ div
                [ class "service-preview-logo" ]
                [ a [ class "hoverable" ] [ img [ src theme.logo ] [] ] ]
            , div [ class "service-preview-innerheader" ]
                [ a
                    [ class "button service-preview-header-button main button-small"
                    , color "background-color" { kind = BrandColors, component = BackgroundColor }
                    , color "border-color" { kind = BrandColors, component = TextColor }
                    ]
                    [ div
                        [ class "label"
                        , color "color" { kind = BrandColors, component = TextColor }
                        ]
                        [ span []
                            [ text "Start new process" ]
                        ]
                    ]
                , a
                    [ class "button service-preview-header-button main button-small"
                    , color "background-color" { kind = BrandColors, component = BackgroundColor }
                    , color "border-color" { kind = BrandColors, component = TextColor }
                    ]
                    [ div
                        [ class "label"
                        , color "color" { kind = BrandColors, component = TextColor }
                        ]
                        [ span []
                            [ text "Start from template" ]
                        ]
                    ]
                , div
                    [ class "service-preview-header-text"
                    , color "color" { kind = BrandColors, component = TextColor }
                    ]
                    [ a [ class "hoverable" ]
                        [ text "E-archive" ]
                    , span []
                        []
                    ]
                , div
                    [ class "service-preview-header-text"
                    , color "color" { kind = BrandColors, component = TextColor }
                    ]
                    [ a [ class "hoverable" ]
                        [ text "Account" ]
                    , span []
                        []
                    ]
                , div
                    [ class "service-preview-header-text"
                    , color "color" { kind = BrandColors, component = TextColor }
                    ]
                    [ a [ class "hoverable" ]
                        [ text "Log out" ]
                    ]
                ]
            ]
        , div [ class "service-preview-content" ]
            [ img [ src "/img/user_details_preview.png" ]
                []
            , a
                [ class "button action button-small"
                , color "background-color" { kind = ActionColors, component = BackgroundColor }
                , color "color" { kind = ActionColors, component = TextColor }
                ]
                [ div [ class "label" ]
                    [ span []
                        [ text "Save" ]
                    ]
                ]
            ]
        , div [ class "service-preview-footer" ]
            [ div []
                [ text "Powered by Scrive" ]
            ]
        ]

viewLoginThemePreview : Theme -> Html msg
viewLoginThemePreview theme =
  let color label = style label << color2Hex << getThemeColor theme
      brandTextColor = getThemeColor theme { kind = BrandColors, component = TextColor }
  in div
        [ class "login-preview"
        , color "background-color" { kind = BrandColors, component = BackgroundColor }
        ]
        [ div [ class "logo-wrapper" ]
            [ img [ src <| theme.logo ] []
            , div
                [ class "divider-line"
                , style
                    "background-image"
                    ("url(/colored_image?file=divider-line.png&color="
                        ++ Url.percentEncode
                            (color2Hex brandTextColor)
                        ++ ")"
                    )
                ]
                []
            , p
                [ class "small"
                , color "color" { kind = BrandColors, component = TextColor }
                ]
                [ text "E-signing powered by Scrive" ]
            ]
        , div [ class "content-container" ]
            [ div [ class "content" ]
                [ div [ style "width" "200px", class "info-text-input readonly" ]
                    [ input
                        [ type_ "text"
                        , readonly True
                        , placeholder "Email address"
                        , value "" ]
                        []
                    , div [ class "after" ] []
                    ]
                , div [ style "width" "200px", class "info-text-input readonly" ]
                    [ input
                        [ type_ "text"
                        , readonly True
                        , placeholder "Password"
                        , value "" ]
                        []
                    , div [ class "internal-button-wrapper", style "width" "auto" ]
                        [ div [ class "internal-button" ]
                            [ text "Forgot?" ]
                        ]
                    , div [ class "after" ] []
                    ]
                , a
                    [ class "button main"
                    , color "background-color" { kind = BrandColors, component = BackgroundColor }
                    , color "border-color" { kind = BrandColors, component = TextColor }
                    , color "color" { kind = BrandColors, component = TextColor }
                    ]
                    [ div [ class "label" ] [ span [] [ text "Log in" ] ] ]
                , p [ color "color" { kind = BrandColors, component = TextColor } ]
                    [ span [] [ text "Don't have an account? " ]
                    , a [ color "color" { kind = BrandColors, component = TextColor } ] [ text "Sign up for free" ]
                    ]
                , div [ class "select-container" ]
                    [ div
                        [ class "select "
                        , style "width" "150px"
                        , color "background-color" { kind = BrandColors, component = BackgroundColor }
                        , style "border" ("1px solid " ++ color2Hex brandTextColor)
                        , color "color" { kind = BrandColors, component = TextColor }
                        ]
                        [ div [ class "select-button" ]
                            [ div [ class "select-button-left" ] []
                            , div
                                [ class "select-button-label"
                                , style "width" "150px"
                                , color "background-color" { kind = BrandColors, component = BackgroundColor }
                                , color "color" { kind = BrandColors, component = TextColor }
                                ]
                                [ text "English" ]
                            , div
                                [ class "select-button-right"
                                , style "background-image"
                                    ("url(/colored_image?file=select-arrow-for-branding.png&color="
                                        ++ Url.percentEncode (color2Hex brandTextColor)
                                        ++ ")"
                                    )
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        ]
