module AdminOnly.Types.UserGroup.PadAppMode exposing
    ( PadAppMode
    , decoder
    , enum
    )

import EnumExtra exposing (Enum, findEnumValue, makeEnum)
import Json.Decode as JD exposing (Decoder)
import List


type PadAppMode
    = ListView
    | PinCode
    | QRCode


decoder : String -> Decoder PadAppMode
decoder padAppModeString =
    case findEnumValue enum padAppModeString of
        Err _ ->
            JD.fail <| "Cannot parse pad app mode: " ++ padAppModeString

        Ok padAppMode ->
            JD.succeed padAppMode


enum : Enum PadAppMode
enum =
    let
        values =
            [ ListView, PinCode, QRCode ]

        toHumanStr padAppMode =
            case padAppMode of
                ListView ->
                    "ListView"

                PinCode ->
                    "PinCode"

                QRCode ->
                    "QRCode"

        toStr padAppMode =
            case padAppMode of
                ListView ->
                    "list_view"

                PinCode ->
                    "pin_code"

                QRCode ->
                    "qr_code"
    in
    makeEnum values toStr toHumanStr
