-- need to evaluate parser error messages
module Lib.Json.SignatoryLink exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP

import Lib.Types.SignatoryLink exposing (..)
import Lib.Json.ID exposing (idDecoder)
import Utils exposing (datetimeDecoder)
import Lib.Json.Extras exposing (constantStringDecoder, quotedIntDecoder)

{-
fieldTypeDecoder : Decoder SignatoryFieldType
fieldTypeDecoder =
  let ftFromString s = case s of
        "name" -> FTName
        "company" -> FTCompany
        "personal_number" -> FTPersonalNumber
        "email" -> FTEmail
        "mobile" -> FTMobile
        _ -> FTOther s
  in JD.string |> JD.map ftFromString
-}

signatoryFieldDecoder : Decoder SignatoryField
signatoryFieldDecoder =
  JD.oneOf
    [ JD.succeed (\_ n v -> SignatoryNameField {nameOrder = n, value = v})
      |> JDP.required "type" (constantStringDecoder "name")
      |> JDP.required "order" JD.int
      |> JDP.required "value" JD.string
    , JD.succeed (\_ v -> SignatoryPersonalNumberField {value = v})
      |> JDP.required "type" (constantStringDecoder "personal_number")
      |> JDP.required "value" JD.string
    , JD.succeed (\_ v -> SignatoryEmailField {value = v})
      |> JDP.required "type" (constantStringDecoder "email")
      |> JDP.required "value" JD.string
    , JD.succeed (\_ v -> SignatoryMobileField {value = v})
      |> JDP.required "type" (constantStringDecoder "mobile")
      |> JDP.required "value" JD.string
    , JD.succeed (\sfType -> SignatoryOtherField { sfType = sfType })
      |> JDP.required "type" JD.string
    ]


signatoryRoleDecoder : Decoder SignatoryRole
signatoryRoleDecoder =
  JD.string
  |> JD.andThen
      (\s -> case s of
        "viewer" -> JD.succeed Viewer
        "approver" -> JD.succeed Approver
        "signing_party" -> JD.succeed SigningParty
        "forwarded_party" -> JD.succeed ForwardedParty
        _ -> JD.fail <| "Cannot parse SignatoryRole: " ++ s
      )

deliveryStatusDecoder : Decoder DeliveryStatus
deliveryStatusDecoder =
  JD.string
  |> JD.andThen
      (\s -> case s of
        "delivered" -> JD.succeed Delivered
        "not_delivered" -> JD.succeed Undelivered
        "unknown" -> JD.succeed Unknown
        "deferred" -> JD.succeed Deferred
        _ -> JD.fail <| "Cannot parse DeliveryStatus: " ++ s
      )

deliveryMethodDecoder : Decoder DeliveryMethod
deliveryMethodDecoder =
  JD.string
  |> JD.andThen
      (\s -> case s of
        "email" -> JD.succeed EmailDelivery
        "pad" -> JD.succeed PadDelivery
        "api" -> JD.succeed APIDelivery
        "mobile" -> JD.succeed MobileDelivery
        "email_mobile" -> JD.succeed EmailAndMobileDelivery
        "portal" -> JD.succeed PortalDelivery
        _ -> JD.fail <| "Cannot parse DeliveryMethod: " ++ s
      )

authenticationToViewMethodDecoder : Decoder AuthenticationToViewMethod
authenticationToViewMethodDecoder =
  JD.string
  |> JD.andThen
      (\s -> case s of
        "standard" -> JD.succeed StandardAuthenticationToView
        "se_bankid" -> JD.succeed SEBankIDAuthenticationToView
        "no_bankid" -> JD.succeed NOBankIDAuthenticationToView
        "dk_nemid" -> JD.succeed LegacyDKNemIDAuthenticationToView
        "dk_nemid_cpr" -> JD.succeed DKNemIDCPRAuthenticationToView
        "dk_nemid_pid" -> JD.succeed DKNemIDPIDAuthenticationToView
        "dk_nemid_cvr" -> JD.succeed DKNemIDCVRAuthenticationToView
        "fi_tupas" -> JD.succeed FITupasAuthenticationToView
        "sms_pin" -> JD.succeed SMSPinAuthenticationToView
        "verimi" -> JD.succeed VerimiAuthenticationToView
        "nl_idin" -> JD.succeed IDINAuthenticationToView
        "onfido_document_check" -> JD.succeed OnfidoDocumentCheckAuthenticationToView
        "onfido_document_and_photo_check" -> JD.succeed OnfidoDocumentAndPhotoCheckAuthenticationToView
        _ -> JD.fail <| "Cannot parse DeliveryMethod: " ++ s
      )

signatoryLinkDecoder : Decoder SignatoryLink
signatoryLinkDecoder =
  JD.succeed SignatoryLinkRecord
  |> JDP.required "id" idDecoder
  |> JDP.required "is_author" JD.bool
  |> JDP.required "signatory_role" signatoryRoleDecoder
  |> JDP.required "fields" (JD.list signatoryFieldDecoder)
  |> JDP.required "sign_time" (JD.nullable datetimeDecoder)
  |> JDP.required "seen_time" (JD.nullable datetimeDecoder)
  |> JDP.required "read_invitation_time" (JD.nullable datetimeDecoder)
  |> JDP.required "email_delivery_status" deliveryStatusDecoder
  |> JDP.required "mobile_delivery_status" deliveryStatusDecoder
  |> JDP.required "delivery_method" deliveryMethodDecoder
  |> JDP.required "authentication_method_to_view" authenticationToViewMethodDecoder
  |> JDP.required "authentication_method_to_view" authenticationToViewMethodDecoder
  |> JD.map SignatoryLink
