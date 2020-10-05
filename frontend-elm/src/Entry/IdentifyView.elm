module Entry.IdentifyView exposing (..)

import Browser
import Html
import Http
import IdentifyView.GenericEidService.GenericEidService as GenericEidService
import IdentifyView.Model exposing (..)
import IdentifyView.Rejection.Rejection as Rejection
import IdentifyView.SMSPin.SMSPin as SMSPin
import IdentifyView.Update exposing (..)
import IdentifyView.View exposing (..)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Lib.Components.FlashMessage as FlashMessage
import Lib.Json.Document exposing (documentDecoder)
import Lib.Json.FlashMessage exposing (flashMessageDecoder)
import Lib.Json.ID exposing (idDecoder)
import Lib.Json.Localization exposing (localisationDecoder)
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Lib.Types.ID exposing (showId)
import Utils exposing (perform)


decodeFlags : JD.Decoder Flags
decodeFlags =
    JD.succeed Flags
        |> JDP.required "xtoken" JD.string
        |> JDP.required "localization" localisationDecoder
        |> JDP.required "cdnBaseUrl" JD.string
        |> JDP.required "location" JD.string
        |> JDP.required "currentYear" JD.int
        |> JDP.required "origin" JD.string
        |> JDP.required "logoUrl" JD.string
        |> JDP.required "welcomeText" JD.string
        |> JDP.required "entityTypeLabel" JD.string
        |> JDP.required "entityTitle" JD.string
        |> JDP.required "authenticationMethod" authenticationToViewFlowMethodDecoder
        |> JDP.required "authorName" JD.string
        |> JDP.required "participantEmail" JD.string
        |> JDP.required "participantMaskedMobile" JD.string
        |> JDP.required "genericEidServiceStartUrl" (JD.nullable JD.string)
        |> JDP.required "smsPinSendUrl" JD.string
        |> JDP.required "smsPinVerifyUrl" JD.string
        |> JDP.required "rejectionRejectUrl" JD.string
        |> JDP.required "rejectionAlreadyRejected" JD.bool
        |> JDP.required "errorMessage" (JD.nullable JD.string)
        |> JDP.required "maxFailuresExceeded" JD.bool


init : JD.Value -> ( Model, Cmd Msg )
init value =
    case JD.decodeValue decodeFlags value of
        Ok flags ->
            let
                flashMessages =
                    case flags.errorMessage of
                        Just err ->
                            Tuple.first <|
                                FlashMessage.addFlashMessage { embed = FlashMessageMsg } (FlashError err) FlashMessage.initialState

                        Nothing ->
                            FlashMessage.initialState

                model =
                    { flashMessages = flashMessages
                    , state = Loading flags
                    }

                rInnerModel =
                    if flags.rejectionAlreadyRejected then
                        Ok <| IdentifyRejection (toRejectionParams flags) Rejection.AlreadyRejected

                    else
                        case flags.authenticationMethod of
                            OnfidoDocumentCheckAuthenticationToView ->
                                eidServiceModel GenericEidService.Onfido

                            OnfidoDocumentAndPhotoCheckAuthenticationToView ->
                                eidServiceModel GenericEidService.Onfido

                            SmsOtpAuthenticationToView ->
                                eidServiceModel GenericEidService.SmsOtp

                unsupportedMethodError =
                    "Authentication method not supported by IdentifyView"

                eidServiceModel provider =
                    Result.map (\params -> IdentifyGenericEidService params GenericEidService.Idle)
                        (toGenericEidServiceParams flags provider)

                smsPinModel =
                    Result.map (\params -> IdentifySMSPin params SMSPin.Idle) (toSMSPinParams flags)

                criticalError str fields =
                    let
                        ( newFlashMessages, cmd ) =
                            FlashMessage.addFlashMessage { embed = FlashMessageMsg } (FlashError str) model.flashMessages

                        newModel =
                            { flashMessages = newFlashMessages, state = Error { errorView = Html.text str } }

                        ( newModel2, cmd2 ) =
                            update (ErrorTraceMsg fields) newModel
                    in
                    ( newModel2, Cmd.batch [ cmd, cmd2 ] )
            in
            case rInnerModel of
                Ok innerModel ->
                    ( { model | state = IdentifyView { flags = flags, innerModel = innerModel } }, Cmd.none )

                Err errMsg ->
                    let
                        flashMessage =
                            "Internal error: " ++ errMsg

                        errorFields =
                            [ ( "where", JE.string "IdentifyView.init" )
                            , ( "what", JE.string errMsg )
                            ]
                    in
                    criticalError flashMessage errorFields

        Err err ->
            let
                model =
                    { flashMessages = FlashMessage.initialState
                    , state = Error { errorView = Html.text <| JD.errorToString err }
                    }

                cmd =
                    perform <| AddFlashMessageMsg (FlashError "Failed to decode flags")
            in
            ( model, cmd )


main : Program JD.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
