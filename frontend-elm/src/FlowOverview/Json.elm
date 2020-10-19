module FlowOverview.Json exposing (..)

import Cookie
import FlowOverview.Model exposing (..)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP


decodeFlags : Decoder Flags
decodeFlags =
    JD.succeed Flags
        |> JDP.required "kontraApiUrl" JD.string
        |> JDP.required "flowApiUrl" JD.string
        |> JDP.required "flowInstanceId" JD.string
        |> JDP.required "xtoken" JD.string


instanceViewDecoder : Decoder GetInstanceView
instanceViewDecoder =
    JD.succeed GetInstanceView
        |> JDP.required "id" JD.string
        |> JDP.required "state" instanceUserStateDecoder
        |> JDP.required "actions" (JD.list instanceUserActionDecoder)
        |> JDP.required "status" (JD.string |> JD.andThen statusDecoder)


statusDecoder : String -> Decoder Status
statusDecoder str =
    case str of
        "in_progress" ->
            JD.succeed InProgress

        "completed" ->
            JD.succeed Completed

        "failed" ->
            JD.succeed Failed

        _ ->
            JD.fail "Unable to decode Flow instance status"


instanceUserActionDecoder : Decoder InstanceUserAction
instanceUserActionDecoder =
    JD.succeed InstanceUserAction
        |> JDP.required "type" JD.string
        |> JDP.required "document" JD.string
        |> JDP.required "signatory_id" JD.string
        |> JDP.required "link" JD.string


instanceUserStateDecoder : Decoder InstanceUserState
instanceUserStateDecoder =
    JD.succeed InstanceUserState
        |> JDP.required "documents" (JD.list instanceUserDocumentDecoder)


instanceUserDocumentDecoder : Decoder InstanceUserDocument
instanceUserDocumentDecoder =
    JD.succeed InstanceUserDocument
        |> JDP.required "document_id" JD.string
        |> JDP.required "document_state" JD.string
        |> JDP.required "signatory_id" JD.string


signatoryLinkDecoder : Decoder SignatoryLink
signatoryLinkDecoder =
    JD.succeed SignatoryLink
        |> JDP.required "id" JD.string
        |> JDP.required "is_author" JD.bool
        |> JDP.required "fields" (JD.list signatoryFieldDecoder)


signatoryFieldDecoder : Decoder SignatoryField
signatoryFieldDecoder =
    JD.oneOf
        [ JD.succeed (\_ n v -> SignatoryNameField { nameOrder = n, value = v })
            |> JDP.required "type" (constantStringDecoder "name")
            |> JDP.required "order" JD.int
            |> JDP.required "value" JD.string
        , JD.succeed (\_ v -> SignatoryPersonalNumberField { value = v })
            |> JDP.required "type" (constantStringDecoder "personal_number")
            |> JDP.required "value" JD.string
        , JD.succeed (\_ v -> SignatoryEmailField { value = v })
            |> JDP.required "type" (constantStringDecoder "email")
            |> JDP.required "value" JD.string
        , JD.succeed (\_ v -> SignatoryMobileField { value = v })
            |> JDP.required "type" (constantStringDecoder "mobile")
            |> JDP.required "value" JD.string
        , JD.succeed (\sfType -> SignatoryOtherField { sfType = sfType })
            |> JDP.required "type" JD.string
        ]


documentDecoder : Decoder Document
documentDecoder =
    JD.succeed Document
        |> JDP.required "id" JD.string
        |> JDP.required "title" JD.string
        |> JDP.required "parties" (JD.list signatoryLinkDecoder)


constantStringDecoder : String -> Decoder String
constantStringDecoder str =
    JD.string
        |> JD.andThen
            (\str_ ->
                if str == str_ then
                    JD.succeed str

                else
                    JD.fail <|
                        "Failed to decode constant, "
                            ++ "expected \""
                            ++ str
                            ++ "\" "
                            ++ "but got \""
                            ++ str_
                            ++ "\"."
            )
