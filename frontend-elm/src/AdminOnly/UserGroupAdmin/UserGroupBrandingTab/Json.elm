module AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Json exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Types exposing (..)
import EnumExtra as Enum
import Maybe.Extra as Maybe
import Utils exposing (..)

import Lib.Types.Theme exposing (..)

brandingDecoder : Decoder UserGroupBranding
brandingDecoder =
    let decodeTheme kind =
          JD.field (Enum.toString enumThemeKind kind)
            <| decodeJust <| JD.map String.toInt JD.string
    in
    JD.succeed UserGroupBranding
        |> JDP.custom (decodeDict enumThemeKind decodeTheme)
        |> JDP.required "browserTitle" (JD.nullable JD.string)
        |> JDP.required "smsOriginator" (JD.nullable JD.string)
        |> JDP.required "favicon" (JD.nullable JD.string)

encodeUserGroupBranding : String -> UserGroupBranding -> JE.Value
encodeUserGroupBranding ugid branding =
  let themeKindField k =
        (Enum.toString enumThemeKind k,
          Maybe.withDefault JE.null
          <| Maybe.map (JE.string << String.fromInt)
          <| Enum.get k branding.themes)
      maybeString mStr = case mStr of
        Nothing -> JE.null
        Just str -> JE.string str
  in JE.object
      [ ( "companyid", JE.string ugid )
      , ( "browserTitle", maybeString branding.browserTitle )
      , ( "smsOriginator", maybeString branding.smsOriginator )
      , ( "favicon", maybeString branding.favicon )
      , themeKindField EmailTheme
      , themeKindField SignViewTheme
      , themeKindField ServiceTheme
      ]
