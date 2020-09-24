port module IdentifyView.Update exposing (..)

import Html
import Json.Encode as JE

import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.Document exposing (Document(..), DocumentStatus(..))
import Lib.Types.SignatoryLink exposing (SignatoryLink(..), AuthenticationToViewMethod(..))
import Lib.Types.ID exposing (showId)
import Lib.Misc.Http as Http

import IdentifyView.Model exposing (..)
import IdentifyView.SMSPin.SMSPin as SMSPin
import IdentifyView.GenericEidService.GenericEidService as GenericEidService

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

port errorTraceMsg : JE.Value -> Cmd msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let errorFlashMessage str =
        update (AddFlashMessageMsg <| FlashError str) model

  in case msg of

  ErrorTraceMsg fields ->
    let object = case model.state of
          Loading flags ->
            JE.object <|
              -- TODO: Add a logging identifier
              -- [ ("document_id", JE.string <| showId flags.documentId)
              -- , ("signatory_link_id", JE.string <| showId flags.signatoryLinkId) ]
              -- ++ fields
              fields

          IdentifyView { flags } ->
            JE.object <|
              -- TODO: Add a logging identifier
              -- [ ("document_id", JE.string <| showId <| case params.document of Document doc -> doc.id)
              -- , ("signatory_link_id", JE.string <| showId <| case params.signatory of SignatoryLink sig -> sig.id) ]
              -- ++ fields
              fields

          Error _ -> JE.object fields

    in (model, errorTraceMsg object)

  AddFlashMessageMsg flashMessage ->
    let (newFlashMessages, cmd) = FlashMessage.addFlashMessage {embed = FlashMessageMsg} flashMessage model.flashMessages
    in ({ model | flashMessages = newFlashMessages}, cmd)

  SMSPinMsg msg_ -> case model.state of
    IdentifyView { flags, innerModel } -> case innerModel of
      IdentifySMSPin state ->
        let (newInnerState, cmd) = SMSPin.update (toSMSPinParams flags) state msg_
            newState = IdentifyView {flags = flags, innerModel = IdentifySMSPin newInnerState}
        in ( {model | state = newState}, cmd )

      _ -> errorFlashMessage "Internal error: got SMSPin message while in an incompatible state."
    _ -> errorFlashMessage "Internal error: got SMSPin message while in an incompatible state."

  GenericEidServiceMsg msg_ -> case model.state of
    IdentifyView {flags, innerModel} -> case innerModel of
      IdentifyGenericEidService provider state ->
        let (newInnerState, cmd) = GenericEidService.update (toGenericEidServiceParams flags provider) state msg_
            newState = IdentifyView
                        { flags = flags
                        , innerModel = IdentifyGenericEidService provider newInnerState
                        }
        in ( {model | state = newState}, cmd )

      _ -> errorFlashMessage "Internal error: got GenericEidService message while in an incompatible state."
    _ -> errorFlashMessage "Internal error: got GenericEidService message while in an incompatible state."

  FlashMessageMsg msg_ ->
    let (newFlashMessages, cmd) = FlashMessage.update {embed = FlashMessageMsg} msg_ model.flashMessages
    in ( {model | flashMessages = newFlashMessages}, cmd )
