module AdminOnly.Types.UserGroup.SEBankIDSigningProviderOverride exposing
    ( SEBankIDSigningProviderOverride(..)
    , decoder
    , enum
    )

import EnumExtra exposing (Enum, findEnumValue, makeEnum)
import Json.Decode as JD exposing (Decoder)


type SEBankIDSigningProviderOverride
    = ForceCGIForSEBankIDSigning
    | ForceEIDHubForSEBankIDSigning
    | DefaultSEBankIDSigning


enum : Enum SEBankIDSigningProviderOverride
enum =
    let
        values =
            [ ForceCGIForSEBankIDSigning, ForceEIDHubForSEBankIDSigning, DefaultSEBankIDSigning ]

        toStr override =
            case override of
                ForceCGIForSEBankIDSigning ->
                    "force_cgi"

                ForceEIDHubForSEBankIDSigning ->
                    "force_eidhub"

                DefaultSEBankIDSigning ->
                    "no_override"

        toHumanStr override =
            case override of
                ForceCGIForSEBankIDSigning ->
                    "Force CGI"

                ForceEIDHubForSEBankIDSigning ->
                    "Force EID Hub"

                DefaultSEBankIDSigning ->
                    "Use global default"
    in
    makeEnum values toStr toHumanStr


decoder : Decoder SEBankIDSigningProviderOverride
decoder =
    JD.string
        |> JD.andThen
            (\str ->
                case findEnumValue enum str of
                    Err _ ->
                        JD.fail <| "Cannot parse SEBankIDSigningProviderOverride: " ++ str

                    Ok override ->
                        JD.succeed override
            )
