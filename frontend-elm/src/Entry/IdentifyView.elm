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
  |> JDP.required "genericEidServiceStartUrl" JD.string
  |> JDP.required "smsPinSendUrl" JD.string
  |> JDP.required "smsPinVerifyUrl" JD.string

init : JD.Value -> (Model, Cmd Msg)
init value = case JD.decodeValue decodeFlags value of
  Ok flags ->
    let model = { flashMessages = FlashMessage.initialState
                , state = Loading flags
                }

        mInnerModel = case flags.authenticationMethod of
            StandardAuthenticationToView -> Nothing
            SEBankIDAuthenticationToView -> Just <| IdentifyGenericEidService GenericEidService.SEBankID GenericEidService.Idle
            NOBankIDAuthenticationToView -> Just <| IdentifyGenericEidService GenericEidService.NOBankID GenericEidService.Idle
            LegacyDKNemIDAuthenticationToView -> Just <| IdentifyGenericEidService GenericEidService.DKNemID GenericEidService.Idle
            DKNemIDCPRAuthenticationToView -> Nothing
            DKNemIDPIDAuthenticationToView -> Nothing
            DKNemIDCVRAuthenticationToView -> Nothing
            SMSPinAuthenticationToView -> Just <| IdentifySMSPin SMSPin.Idle
            FITupasAuthenticationToView -> Just <| IdentifyGenericEidService GenericEidService.FITupas GenericEidService.Idle
            VerimiAuthenticationToView -> Just <| IdentifyGenericEidService GenericEidService.Verimi GenericEidService.Idle
            IDINAuthenticationToView -> Just <| IdentifyGenericEidService GenericEidService.IDIN GenericEidService.Idle
            OnfidoDocumentCheckAuthenticationToView -> Nothing -- TODO: Add Onfido to GenericEidService
            OnfidoDocumentAndPhotoCheckAuthenticationToView -> Nothing -- TODO: Add Onfido to GenericEidService

        criticalError str fields =
            let (newFlashMessages, cmd) =
                  FlashMessage.addFlashMessage {embed = FlashMessageMsg} (FlashError str) model.flashMessages
                newModel = { flashMessages = newFlashMessages, state = Error { errorView = Html.text str } }
                (newModel2, cmd2) = update (ErrorTraceMsg fields) newModel
            in (newModel2, Cmd.batch [cmd, cmd2])

        in case mInnerModel of
            Just innerModel -> ({model | state = IdentifyView { flags = flags, innerModel = innerModel } }, Cmd.none)
            Nothing ->
                let flashMessage = "Internal error: Authentication method not supported by IdentifyView"
                    errorFields =
                        [ ("where", JE.string "IdentifyView.init")
                        , ("what", JE.string "Authentication method not supported by IdentifyView")
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
