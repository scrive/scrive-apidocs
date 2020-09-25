module Entry.IdentifyView exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Browser
import Http
import Html

import Utils exposing (perform)
import Lib.Types.ID exposing (showId)
import Lib.Json.ID exposing (idDecoder)
import Lib.Json.Document exposing (documentDecoder)
import Lib.Json.FlashMessage exposing (flashMessageDecoder)
import Lib.Json.Localization exposing (localisationDecoder)
import Lib.Json.SignatoryLink exposing (authenticationToViewMethodDecoder)
import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Lib.Types.SignatoryLink exposing (AuthenticationToViewMethod(..))

import IdentifyView.GenericEidService.GenericEidService as GenericEidService
import IdentifyView.Model exposing (..)
import IdentifyView.SMSPin.SMSPin as SMSPin
import IdentifyView.Update exposing (..)
import IdentifyView.View exposing (..)


decodeFlags : JD.Decoder Flags
decodeFlags =
  JD.succeed Flags
  |> JDP.required "flashMessageFromCookie" (JD.maybe flashMessageDecoder)
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
  |> JDP.required "authenticationMethod" authenticationToViewMethodDecoder
  |> JDP.required "authorName" JD.string
  |> JDP.required "participantEmail" JD.string
  |> JDP.required "participantMaskedMobile" JD.string
  |> JDP.required "genericEidServiceStartUrl" (JD.nullable JD.string)
  |> JDP.required "smsPinSendUrl" JD.string
  |> JDP.required "smsPinVerifyUrl" JD.string

init : JD.Value -> (Model, Cmd Msg)
init value = case JD.decodeValue decodeFlags value of
  Ok flags ->
    let model = { flashMessages = FlashMessage.initialState
                , state = Loading flags
                }

        rInnerModel = case flags.authenticationMethod of
            StandardAuthenticationToView -> Err unsupportedMethodError
            SEBankIDAuthenticationToView -> eidServiceModel GenericEidService.SEBankID
            NOBankIDAuthenticationToView -> eidServiceModel GenericEidService.NOBankID
            LegacyDKNemIDAuthenticationToView -> eidServiceModel GenericEidService.DKNemID
            DKNemIDCPRAuthenticationToView -> Err unsupportedMethodError
            DKNemIDPIDAuthenticationToView -> Err unsupportedMethodError
            DKNemIDCVRAuthenticationToView -> Err unsupportedMethodError
            SMSPinAuthenticationToView -> smsPinModel
            FITupasAuthenticationToView -> eidServiceModel GenericEidService.FITupas
            VerimiAuthenticationToView -> eidServiceModel GenericEidService.Verimi
            IDINAuthenticationToView -> eidServiceModel GenericEidService.IDIN
            OnfidoDocumentCheckAuthenticationToView -> eidServiceModel GenericEidService.Onfido
            OnfidoDocumentAndPhotoCheckAuthenticationToView -> eidServiceModel GenericEidService.Onfido

        unsupportedMethodError = "Authentication method not supported by IdentifyView"

        eidServiceModel provider =
            Result.map (\params -> IdentifyGenericEidService params GenericEidService.Idle)
                       (toGenericEidServiceParams flags provider)

        smsPinModel =
            Result.map (\params -> IdentifySMSPin params SMSPin.Idle) (toSMSPinParams flags)

        criticalError str fields =
            let (newFlashMessages, cmd) =
                  FlashMessage.addFlashMessage {embed = FlashMessageMsg} (FlashError str) model.flashMessages
                newModel = { flashMessages = newFlashMessages, state = Error { errorView = Html.text str } }
                (newModel2, cmd2) = update (ErrorTraceMsg fields) newModel
            in (newModel2, Cmd.batch [cmd, cmd2])

        in case rInnerModel of
            Ok innerModel -> ({model | state = IdentifyView { flags = flags, innerModel = innerModel } }, Cmd.none)
            Err errMsg ->
                let flashMessage = "Internal error: " ++ errMsg
                    errorFields =
                        [ ("where", JE.string "IdentifyView.init")
                        , ("what", JE.string errMsg)
                        ]
                in criticalError flashMessage errorFields

  Err err ->
    let model = { flashMessages = FlashMessage.initialState
                , state = Error {errorView = Html.text <| JD.errorToString err}
                }
        cmd = perform <| AddFlashMessageMsg (FlashError "Failed to decode flags")
    in (model, cmd)

main : Program JD.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
