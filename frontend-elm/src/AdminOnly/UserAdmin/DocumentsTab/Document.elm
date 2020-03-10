module AdminOnly.UserAdmin.DocumentsTab.Document exposing
    ( Document
    , FieldType(..)
    , SignatoryRole(..)
    , documentAuthor
    , documentDecoder
    , documentsDecoder
    , enumDocumentStatus
    , enumDocumentType
    , signatoryFieldText
    , signatorySmartName
    )

import EnumExtra exposing (Enum, makeEnum)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP
import List as L
import Maybe as M
import Time exposing (Posix)
import Utils exposing (datetimeDecoder, firstJust, ite)


type alias Document =
    { id : String
    , title : String
    , cTime : Posix
    , mTime : Posix
    , signatories : List Signatory
    , status : DocumentStatus
    , isTemplate : Bool
    , isShared : Bool
    , shareableLink : M.Maybe String
    }


type alias Signatory =
    { isAuthor : Bool
    , role : SignatoryRole
    , fields : List SignatoryField
    }


type alias SignatoryField =
    { sfType : FieldType
    , value : String
    }


type SignatoryRole
    = SignatoryRoleSigningParty
    | SignatoryRoleViewer
    | SignatoryRoleApprover
    | SignatoryRoleForwardedParty


type FieldType
    = FTName
    | FTCompany
    | FTPersonalNumber
    | FTCompanyNumber
    | FTEmail
    | FTText
    | FTSignature
    | FTCheckbox
    | FTMobile
    | FTRadioGroup


type DocumentStatus
    = Preparation
    | Pending
    | Closed
    | Canceled
    | Timedout
    | Rejected
    | DocumentError


documentDecoder : Decoder Document
documentDecoder =
    D.succeed Document
        |> DP.required "id" D.string
        |> DP.required "title" D.string
        |> DP.required "ctime" datetimeDecoder
        |> DP.required "mtime" datetimeDecoder
        |> DP.required "parties" (D.list signatoryDecoder)
        |> DP.required "status" documentStatusDecoder
        |> DP.required "is_template" D.bool
        |> DP.required "is_shared" D.bool
        |> DP.required "shareable_link" (D.nullable D.string)


documentsDecoder : Decoder (List Document)
documentsDecoder =
    D.field "documents" <| D.list documentDecoder


documentStatusDecoder : Decoder DocumentStatus
documentStatusDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "preparation" ->
                        D.succeed Preparation

                    "pending" ->
                        D.succeed Pending

                    "closed" ->
                        D.succeed Closed

                    "canceled" ->
                        D.succeed Canceled

                    "timedout" ->
                        D.succeed Timedout

                    "rejected" ->
                        D.succeed Rejected

                    "document_error" ->
                        D.succeed DocumentError

                    _ ->
                        D.fail <| "Cannot parse DocumentStatus: " ++ s
            )


signatoryDecoder : Decoder Signatory
signatoryDecoder =
    D.succeed Signatory
        |> DP.required "is_author" D.bool
        |> DP.required "signatory_role" signatoryRoleDecoder
        |> DP.required "fields" (D.list fieldDecoder)


fieldDecoder : Decoder SignatoryField
fieldDecoder =
    D.succeed SignatoryField
        |> DP.required "type" fieldTypeDecoder
        |> DP.optional "value" D.string ""


fieldTypeDecoder : Decoder FieldType
fieldTypeDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "name" ->
                        D.succeed FTName

                    "company" ->
                        D.succeed FTCompany

                    "personal_number" ->
                        D.succeed FTPersonalNumber

                    "company_number" ->
                        D.succeed FTCompanyNumber

                    "email" ->
                        D.succeed FTEmail

                    "mobile" ->
                        D.succeed FTMobile

                    "text" ->
                        D.succeed FTText

                    "signature" ->
                        D.succeed FTSignature

                    "checkbox" ->
                        D.succeed FTCheckbox

                    "radiogroup" ->
                        D.succeed FTRadioGroup

                    _ ->
                        D.fail <| "Cannot parse FieldType: " ++ s
            )


signatoryRoleDecoder : Decoder SignatoryRole
signatoryRoleDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "viewer" ->
                        D.succeed SignatoryRoleViewer

                    "approver" ->
                        D.succeed SignatoryRoleApprover

                    "signing_party" ->
                        D.succeed SignatoryRoleSigningParty

                    "forwarded_party" ->
                        D.succeed SignatoryRoleForwardedParty

                    _ ->
                        D.fail <| "Cannot parse SignatoryRole: " ++ s
            )


documentAuthor : Document -> Maybe Signatory
documentAuthor document =
    document.signatories |> L.filter (\s -> s.isAuthor) |> L.head


signatorySmartName : Signatory -> String
signatorySmartName signatory =
    M.withDefault "Not named party" <|
        firstJust <|
            L.map (\f -> f signatory)
                [ signatoryFieldText FTName
                , signatoryFieldText FTEmail
                , signatoryFieldText FTMobile
                ]


signatoryFieldText : FieldType -> Signatory -> Maybe String
signatoryFieldText fieldType signatory =
    signatory.fields
        |> L.filter (\f -> f.sfType == fieldType && f.value /= "")
        |> L.head
        |> M.map (\f -> f.value)



-- DOCUMENT TYPE


allDocumentTypes : List Bool
allDocumentTypes =
    [ True, False ]


encodeDocumentType : Bool -> String
encodeDocumentType isTemplate =
    ite isTemplate "Template" "Signable"


enumDocumentType : Enum Bool
enumDocumentType =
    makeEnum allDocumentTypes encodeDocumentType encodeDocumentType



-- DOCUMENT STATUS


allDocumentStatuses : List DocumentStatus
allDocumentStatuses =
    [ Preparation
    , Pending
    , Closed
    , Canceled
    , Timedout
    , Rejected
    , DocumentError
    ]


encodeDocumentStatus : DocumentStatus -> String
encodeDocumentStatus documentStatus =
    case documentStatus of
        Preparation ->
            "Preparation"

        Pending ->
            "Pending"

        Closed ->
            "Closed"

        Canceled ->
            "Canceled"

        Timedout ->
            "Timedout"

        Rejected ->
            "Rejected"

        DocumentError ->
            "DocumentError"


enumDocumentStatus : Enum DocumentStatus
enumDocumentStatus =
    makeEnum allDocumentStatuses encodeDocumentStatus encodeDocumentStatus
