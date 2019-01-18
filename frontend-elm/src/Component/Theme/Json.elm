module Component.Theme.Json exposing (colorDecoder, encodeTheme, themeDecoder, themesDecoder)

import Color exposing (Color)
import Component.Theme.Data exposing (Theme)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Vendor.ColorPickerExtra exposing (color2Hex, hex2Color)


colorDecoder : Decoder Color
colorDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                hex2Color str
                    |> Maybe.map JD.succeed
                    |> Maybe.withDefault (JD.fail <| "Cannot parse color: " ++ str)
            )


themesDecoder : Decoder (List Theme)
themesDecoder =
    JD.field "themes" <|
        JD.list themeDecoder


themeDecoder : Decoder Theme
themeDecoder =
    let
        buildTheme id name logo font actionColor actionTextColor actionSecondaryColor actionSecondaryTextColor brandColor brandTextColor negativeColor negativeTextColor positiveColor positiveTextColor =
            { id = id
            , name = name
            , logo = logo
            , font = font
            , themeColors =
                { brandColors =
                    { backgroundColor = brandColor
                    , textColor = brandTextColor
                    }
                , actionColors =
                    { backgroundColor = actionColor
                    , textColor = actionTextColor
                    }
                , secondaryActionColors =
                    { backgroundColor = actionSecondaryColor
                    , textColor = actionSecondaryTextColor
                    }
                , positiveColors =
                    { backgroundColor = positiveColor
                    , textColor = positiveTextColor
                    }
                , negativeColors =
                    { backgroundColor = negativeColor
                    , textColor = negativeTextColor
                    }
                }
            }
    in
    JD.succeed buildTheme
        |> JDP.required "id" JD.string
        |> JDP.required "name" JD.string
        |> JDP.required "logo" JD.string
        |> JDP.required "font" JD.string
        |> JDP.required "actionColor" colorDecoder
        |> JDP.required "actionTextColor" colorDecoder
        |> JDP.required "actionSecondaryColor" colorDecoder
        |> JDP.required "actionSecondaryTextColor" colorDecoder
        |> JDP.required "brandColor" colorDecoder
        |> JDP.required "brandTextColor" colorDecoder
        |> JDP.required "negativeColor" colorDecoder
        |> JDP.required "negativeTextColor" colorDecoder
        |> JDP.required "positiveColor" colorDecoder
        |> JDP.required "positiveTextColor" colorDecoder


encodeTheme : Theme -> JE.Value
encodeTheme theme =
    JE.object <|
        [ ( "ready", JE.bool True )
        , ( "dirty", JE.bool True )
        , ( "id", JE.string theme.id )
        , ( "name", JE.string theme.name )
        , ( "logo", JE.string theme.logo )
        , ( "font", JE.string theme.font )
        , ( "actionColor"
          , JE.string <|
                color2Hex theme.themeColors.actionColors.backgroundColor
          )
        , ( "actionTextColor"
          , JE.string <|
                color2Hex theme.themeColors.actionColors.textColor
          )
        , ( "actionSecondaryColor"
          , JE.string <|
                color2Hex theme.themeColors.secondaryActionColors.backgroundColor
          )
        , ( "actionSecondaryTextColor"
          , JE.string <|
                color2Hex theme.themeColors.secondaryActionColors.textColor
          )
        , ( "brandColor"
          , JE.string <|
                color2Hex theme.themeColors.brandColors.backgroundColor
          )
        , ( "brandTextColor"
          , JE.string <|
                color2Hex theme.themeColors.brandColors.textColor
          )
        , ( "negativeColor"
          , JE.string <|
                color2Hex theme.themeColors.negativeColors.backgroundColor
          )
        , ( "negativeTextColor"
          , JE.string <|
                color2Hex theme.themeColors.negativeColors.textColor
          )
        , ( "positiveColor"
          , JE.string <|
                color2Hex theme.themeColors.positiveColors.backgroundColor
          )
        , ( "positiveTextColor"
          , JE.string <|
                color2Hex theme.themeColors.positiveColors.textColor
          )
        ]
