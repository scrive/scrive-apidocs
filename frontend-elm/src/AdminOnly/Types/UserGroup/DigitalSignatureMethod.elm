module AdminOnly.Types.UserGroup.DigitalSignatureMethod exposing
    ( DigitalSignatureMethod(..)
    , decoder
    , enum
    )

import EnumExtra exposing (Enum, findEnumValue, makeEnum)
import Json.Decode as JD exposing (Decoder)


type DigitalSignatureMethod
    = GuardTime
    | Pades


enum : Enum DigitalSignatureMethod
enum =
    let
        toString sm =
            case sm of
                GuardTime ->
                    "guardtime"

                Pades ->
                    "pades"

        toHumanString sm =
            case sm of
                GuardTime ->
                    "GuardTime"

                Pades ->
                    "PAdES"

        allValues =
            [ GuardTime, Pades ]
    in
    makeEnum allValues toString toHumanString


decoder : String -> Decoder DigitalSignatureMethod
decoder sealingMethodString =
    case findEnumValue enum sealingMethodString of
        Err _ ->
            JD.fail <| "Cannot parse sealing method: " ++ sealingMethodString

        Ok digitalSignatureMethod ->
            JD.succeed digitalSignatureMethod
