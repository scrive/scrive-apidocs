module Lib.Json.Theme exposing (..)

import EnumExtra as Enum
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Lib.Types.Theme exposing (..)
import Utils exposing (..)
import Vendor.ColorPickerExtra exposing (color2Hex, hex2Color)


themeDecoder : Decoder Theme
themeDecoder =
    let
        decodeColor ident =
            JD.field (Enum.toString enumColorIdentifier ident)
                (decodeJust <| JD.map hex2Color JD.string)
    in
    JD.succeed Theme
        |> JDP.required "id" (decodeJust <| JD.map String.toInt JD.string)
        |> JDP.required "name" JD.string
        |> JDP.required "logo" JD.string
        |> JDP.required "font" (decodeJust <| JD.map (Enum.fromString enumFont) JD.string)
        |> JDP.custom (decodeFullDict enumColorIdentifier decodeColor)


encodeTheme : Theme -> JE.Value
encodeTheme theme =
    let
        colorField ( i, c ) =
            ( Enum.toString enumColorIdentifier i, JE.string <| color2Hex c )

        colorFields =
            List.map colorField <| Enum.toList theme.colors
    in
    JE.object <|
        [ ( "id", JE.string <| String.fromInt theme.id )
        , ( "name", JE.string theme.name )
        , ( "logo", JE.string theme.logo )
        , ( "font", JE.string <| Enum.toString enumFont theme.font )
        ]
            ++ colorFields
