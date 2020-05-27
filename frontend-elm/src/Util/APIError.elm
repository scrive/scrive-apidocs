module Util.APIError exposing (APIError, APIErrorType, apiErrorDecoder, apiErrorMessage)

import EnumExtra as Enum exposing (Enum)
import Http.Detailed
import Json.Decode as D exposing (Decoder)


-- Public API error
-- https://apidocs.scrive.com/#apierror
type alias APIError =
    { errorType : APIErrorType
    , errorMessage : String
    , httpCode : Int
    }


apiErrorDecoder : Decoder APIError
apiErrorDecoder =
    D.map3 APIError
        (D.field "error_type" D.string |> D.andThen errorTypeDecoder)
        (D.field "error_message" D.string)
        (D.field "http_code" D.int)


type APIErrorType
    = ServerError
    | RequestFailed
    | EndpointNotFound
    | InvalidAuthorization
    | InsufficientPrivileges
    | ResourceNotFound
    | DocumentActionForbidden
    | RequestParametersMissing
    | RequestParametersParseError
    | RequestParametersInvalid
    | DocumentObjectVersionMismatch
    | DocumentStateError
    | SignatoryStateError
    | ActionNotPermitted
    | ConflictError


enumApiErrorType : Enum APIErrorType
enumApiErrorType =
    let
        allValues =
            [ ServerError
            , RequestFailed
            , EndpointNotFound
            , InvalidAuthorization
            , InsufficientPrivileges
            , ResourceNotFound
            , DocumentActionForbidden
            , RequestParametersMissing
            , RequestParametersParseError
            , RequestParametersInvalid
            , DocumentObjectVersionMismatch
            , DocumentStateError
            , SignatoryStateError
            , ActionNotPermitted
            , ConflictError
            ]

        toString e =
            case e of
                ServerError -> "server_error"
                RequestFailed -> "request_failed"
                EndpointNotFound -> "endpoint_not_found"
                InvalidAuthorization -> "invalid_authorisation"
                InsufficientPrivileges -> "insufficient_privileges"
                ResourceNotFound -> "resource_not_found"
                DocumentActionForbidden -> "document_action_forbidden"
                RequestParametersMissing -> "request_parameters_missing"
                RequestParametersParseError -> "request_parameters_parse_error"
                RequestParametersInvalid -> "request_parameters_invalid"
                DocumentObjectVersionMismatch -> "document_object_version_mismatch"
                DocumentStateError -> "document_state_error"
                SignatoryStateError -> "signatory_state_error"
                ActionNotPermitted -> "action_not_permitted"
                ConflictError -> "conflict_error"

        toHumanString =
            toString

    in
    Enum.makeEnum allValues toString toHumanString


errorTypeDecoder : String -> Decoder APIErrorType
errorTypeDecoder str =
    case Enum.fromString enumApiErrorType str of
        Just errorType ->
            D.succeed errorType

        Nothing ->
            D.fail "Cannot decode APIErrorType"


apiErrorMessage : Http.Detailed.Error String -> String -> String
apiErrorMessage error defaultMessage =
    case error of
        Http.Detailed.BadStatus _ body ->
            case D.decodeString apiErrorDecoder body of
                Ok apiError ->
                    apiError.errorMessage

                Err decodeError ->
                    D.errorToString decodeError

        _ ->
            defaultMessage

