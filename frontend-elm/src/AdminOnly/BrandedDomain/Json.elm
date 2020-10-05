module AdminOnly.BrandedDomain.Json exposing (..)

import AdminOnly.BrandedDomain.Types exposing (..)
import EnumExtra as Enum
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Utils exposing (..)
import Vendor.ColorPickerExtra exposing (color2Hex, hex2Color)


brandedDomainDecoder : Decoder BrandedDomain
brandedDomainDecoder =
    let
        decodeColor ident =
            JD.field (Enum.toString enumColorIdentifier ident)
                (decodeJust <| JD.map hex2Color JD.string)

        decodeTheme kind =
            JD.field (Enum.toString enumThemeKind kind)
                (decodeJust <| JD.map String.toInt JD.string)
    in
    JD.succeed BrandedDomain
        |> JDP.required "id" (decodeJust <| JD.map String.toInt JD.string)
        |> JDP.required "mainDomain" JD.bool
        |> JDP.required "url" JD.string
        |> JDP.required "smsOriginator" JD.string
        |> JDP.required "emailOriginator" JD.string
        |> JDP.custom (decodeFullDict enumThemeKind decodeTheme)
        |> JDP.required "browserTitle" JD.string
        |> JDP.required "favicon" JD.string
        |> JDP.custom (decodeFullDict enumColorIdentifier decodeColor)


encodeBrandedDomain : BrandedDomain -> JE.Value
encodeBrandedDomain brandedDomain =
    let
        colors =
            List.map (\( i, c ) -> ( Enum.toString enumColorIdentifier i, JE.string <| color2Hex c )) <|
                Enum.toList brandedDomain.colors

        themes =
            List.map (\( k, id ) -> ( Enum.toString enumThemeKind k, JE.string <| String.fromInt id )) <|
                Enum.toList brandedDomain.themes
    in
    JE.object <|
        [ ( "id", JE.string <| String.fromInt brandedDomain.id )
        , ( "mainDomain", JE.bool brandedDomain.mainDomain )
        , ( "url", JE.string brandedDomain.url )
        , ( "smsOriginator", JE.string brandedDomain.smsOriginator )
        , ( "emailOriginator", JE.string brandedDomain.emailOriginator )
        , ( "browserTitle", JE.string brandedDomain.browserTitle )
        , ( "favicon", JE.string brandedDomain.favicon )
        ]
            ++ themes
            ++ colors
