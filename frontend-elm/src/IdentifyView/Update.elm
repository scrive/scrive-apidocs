port module IdentifyView.Update exposing (..)

import Html
import IdentifyView.GenericEidService.GenericEidService as GenericEidService
import IdentifyView.Model exposing (..)
import IdentifyView.Rejection.Rejection as Rejection
import IdentifyView.SMSPin.SMSPin as SMSPin
import Json.Encode as JE
import Lib.Components.FlashMessage as FlashMessage
import Lib.Misc.Http as Http
import Lib.Types.Document exposing (Document(..), DocumentStatus(..))
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Lib.Types.ID exposing (showId)
import Lib.Types.SignatoryLink exposing (AuthenticationToViewMethod(..), SignatoryLink(..))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


port errorTraceMsg : JE.Value -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        errorFlashMessage str =
            update (AddFlashMessageMsg <| FlashError str) model
    in
    case msg of
        ErrorTraceMsg fields ->
            let
                object =
                    case model.state of
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

                        Error _ ->
                            JE.object fields
            in
            ( model, errorTraceMsg object )

        AddFlashMessageMsg flashMessage ->
            let
                ( newFlashMessages, cmd ) =
                    FlashMessage.addFlashMessage { embed = FlashMessageMsg } flashMessage model.flashMessages
            in
            ( { model | flashMessages = newFlashMessages }, cmd )

        SMSPinMsg msg_ ->
            case model.state of
                IdentifyView { flags, innerModel } ->
                    case innerModel of
                        IdentifySMSPin params state ->
                            let
                                ( newInnerState, cmd ) =
                                    SMSPin.update params state msg_

                                newState =
                                    IdentifyView { flags = flags, innerModel = IdentifySMSPin params newInnerState }
                            in
                            ( { model | state = newState }, cmd )

                        _ ->
                            errorFlashMessage "Internal error: got SMSPin message while in an incompatible state."

                _ ->
                    errorFlashMessage "Internal error: got SMSPin message while in an incompatible state."

        GenericEidServiceMsg msg_ ->
            case model.state of
                IdentifyView { flags, innerModel } ->
                    case innerModel of
                        IdentifyGenericEidService params state ->
                            let
                                ( newInnerState, cmd ) =
                                    GenericEidService.update params state msg_

                                newState =
                                    IdentifyView
                                        { flags = flags
                                        , innerModel = IdentifyGenericEidService params newInnerState
                                        }
                            in
                            ( { model | state = newState }, cmd )

                        _ ->
                            errorFlashMessage "Internal error: got GenericEidService message while in an incompatible state."

                _ ->
                    errorFlashMessage "Internal error: got GenericEidService message while in an incompatible state."

        RejectionMsg msg_ ->
            case model.state of
                IdentifyView { flags, innerModel } ->
                    case innerModel of
                        IdentifyRejection params state ->
                            let
                                ( newInnerState, cmd ) =
                                    Rejection.update params state msg_ ExitRejectionMsg

                                newState =
                                    IdentifyView { flags = flags, innerModel = IdentifyRejection params newInnerState }
                            in
                            ( { model | state = newState }, cmd )

                        _ ->
                            errorFlashMessage "Internal error: got Rejection message while in an incompatible state."

                _ ->
                    errorFlashMessage "Internal error: got Rejection message while in an incompatible state."

        EnterRejectionClickedMsg ->
            case model.state of
                IdentifyView { flags, innerModel } ->
                    let
                        newInnerModel =
                            IdentifyRejection
                                (toRejectionParams flags (Just innerModel))
                                (Rejection.EnterMessage { message = "" })

                        newState =
                            IdentifyView { flags = flags, innerModel = newInnerModel }
                    in
                    ( { model | state = newState }, Cmd.none )

                _ ->
                    errorFlashMessage "Internal error: got EnterRejectionClicked message while in an incompatible state."

        ExitRejectionMsg prevProviderModel ->
            case model.state of
                IdentifyView { flags } ->
                    let
                        newState =
                            IdentifyView { flags = flags, innerModel = prevProviderModel }
                    in
                    ( { model | state = newState }, Cmd.none )

                _ ->
                    errorFlashMessage "Internal error: got ExitRejection message while in an incompatible state."

        FlashMessageMsg msg_ ->
            let
                ( newFlashMessages, cmd ) =
                    FlashMessage.update { embed = FlashMessageMsg } msg_ model.flashMessages
            in
            ( { model | flashMessages = newFlashMessages }, cmd )
