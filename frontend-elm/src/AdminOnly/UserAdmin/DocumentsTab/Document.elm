module AdminOnly.UserAdmin.DocumentsTab.Document exposing
    ( Document
    , FieldType(..)
    , SignatoryRole(..)
    , documentAuthor
    , documentDecoder
    , documentsDecoder
    , enumDocumentStatus
    , enumDocumentType
    , extendedDocumentStatus
    , signatoryFieldText
    , signatorySmartName
    )

import EnumExtra as Enum exposing (Enum, makeEnum)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP
import List as L
import Maybe as M
import Maybe.Extra as M
import Time exposing (Posix)
import Tuple
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
    , deliveryMethod : DeliveryMethod
    , mailInvitationDeliveryStatus : DeliveryStatus
    , smsInvitationDeliveryStatus : DeliveryStatus
    , seenTime : Maybe Posix
    , signTime : Maybe Posix
    , readInvitationTime : Maybe Posix
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


type DeliveryStatus
    = Delivered
    | Undelivered
    | Unknown
    | Deferred


type DeliveryMethod
    = EmailDelivery
    | PadDelivery
    | APIDelivery
    | MobileDelivery
    | EmailAndMobileDelivery
    | PortalDelivery


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


documentsDecoder : Decoder ( Int, List Document )
documentsDecoder =
    D.map2 Tuple.pair
        (D.field "total_matching" D.int)
        (D.field "documents" <| D.list documentDecoder)


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
        |> DP.required "delivery_method" deliveryMethodDecoder
        |> DP.required "email_delivery_status" deliveryStatusDecoder
        |> DP.required "mobile_delivery_status" deliveryStatusDecoder
        |> DP.required "seen_time" (D.nullable datetimeDecoder)
        |> DP.required "sign_time" (D.nullable datetimeDecoder)
        |> DP.required "read_invitation_time" (D.nullable datetimeDecoder)


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


deliveryStatusDecoder : Decoder DeliveryStatus
deliveryStatusDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "delivered" ->
                        D.succeed Delivered

                    "not_delivered" ->
                        D.succeed Undelivered

                    "unknown" ->
                        D.succeed Unknown

                    "deferred" ->
                        D.succeed Deferred

                    _ ->
                        D.fail <| "Cannot parse DeliveryStatus: " ++ s
            )


deliveryMethodDecoder : Decoder DeliveryMethod
deliveryMethodDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "email" ->
                        D.succeed EmailDelivery

                    "pad" ->
                        D.succeed PadDelivery

                    "api" ->
                        D.succeed APIDelivery

                    "mobile" ->
                        D.succeed MobileDelivery

                    "email_mobile" ->
                        D.succeed EmailAndMobileDelivery

                    "portal" ->
                        D.succeed PortalDelivery

                    _ ->
                        D.fail <| "Cannot parse DeliveryMethod: " ++ s
            )


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


-- A port of documentStatus from the old adminonly
-- (frontend/app/scripts/archive/utils.jsx:175)
extendedDocumentStatus : Document -> String
extendedDocumentStatus document =
    let
        someSignatoryHasDeliveryProblem =
            L.any
                (\s ->
                    (s.mailInvitationDeliveryStatus == Undelivered)
                        || (s.smsInvitationDeliveryStatus == Undelivered)
                )
                document.signatories

        everySignatoryOpened =
            L.all (\s -> M.isJust s.seenTime || M.isJust s.signTime) document.signatories

        everySignatoryReadInvitation =
            L.all
                (\s ->
                    M.isJust s.readInvitationTime
                        || M.isJust s.seenTime
                        || M.isJust s.signTime
                )
                document.signatories

        everySignatoryDelivered =
            -- We handle the author separately as we often end up in the situation
            -- where the author is invited with email, sign_order=1 and then we
            -- never get a delivery report, so we end up with email_delivery_status = unknown.
            -- This method doesn't care about sign orders, so this is OK.
            L.all
                (\s ->
                    (s.deliveryMethod == APIDelivery)
                        || (s.deliveryMethod == PadDelivery)
                        || s.isAuthor
                        || (s.mailInvitationDeliveryStatus == Delivered)
                        || (s.smsInvitationDeliveryStatus == Delivered)
                )
                document.signatories
    in
    if document.isTemplate then
        "Template"

    else if document.status /= Pending then
        Enum.toHumanString enumDocumentStatus document.status

    else if everySignatoryOpened then
        "Opened"

    else if everySignatoryReadInvitation then
        "Read"

    else if everySignatoryDelivered then
        "Delivered"

    else if someSignatoryHasDeliveryProblem then
        "Delivery problem"

    else
        "Sent"
