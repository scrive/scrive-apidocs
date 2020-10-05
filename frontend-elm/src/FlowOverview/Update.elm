port module FlowOverview.Update exposing (update)

import Dict
import FlowOverview.Json exposing (..)
import FlowOverview.Model as Model exposing (Model, Msg(..), State(..))
import Http
import Json.Encode as JE
import Lib.Components.FlashMessage as FlashMessage exposing (addFlashMessage)
import Lib.Misc.Cmd exposing (perform)
import Lib.Misc.Http exposing (encodeError)
import Lib.Types.FlashMessage exposing (FlashMessage(..))


port errorTraceMsg : JE.Value -> Cmd msg


type alias ErrorInput =
    { message : String
    , source : String
    , error : Http.Error
    }


errorCmd : ErrorInput -> Cmd Msg
errorCmd { message, source, error } =
    let
        flashMessage =
            message

        errorFields =
            [ ( "where", JE.string source )
            , ( "what", JE.string flashMessage )
            , ( "http_error", encodeError error )
            ]
    in
    Cmd.batch
        [ perform <| AddFlashMessage <| FlashError flashMessage
        , perform <| ErrorTrace errorFields
        ]


getDocuments : Model.Flags -> Model -> Model.GetInstanceView -> ( Model, Cmd Msg )
getDocuments flags model instance =
    let
        documentsToGet =
            List.map (\a -> ( a.actionDocument, a.actionSignatoryId ))
                instance.actions
                ++ List.map (\d -> ( d.documentId, d.signatoryId ))
                    instance.state.documents

        getDocumentCmds =
            List.map
                (\( documentId, signatoryId ) ->
                    Http.get
                        { url =
                            flags.kontraApiUrl
                                ++ "/documents/"
                                ++ documentId
                                ++ "/get?signatory_id="
                                ++ signatoryId
                        , expect = Http.expectJson GetDocumentReceived documentDecoder
                        }
                )
                documentsToGet

        mDocuments =
            if List.isEmpty documentsToGet then
                Just Dict.empty

            else
                Nothing

        newModel =
            updateModel model
                (\inner -> { inner | mInstance = Just instance, mDocuments = mDocuments })
    in
    ( newModel, Cmd.batch getDocumentCmds )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInstanceViewReceived result ->
            case model.state of
                AppOk { flags } ->
                    case result of
                        Ok instance ->
                            getDocuments flags model instance

                        -- {instance | status = Model.Failed}
                        Err error ->
                            let
                                cmd =
                                    errorCmd
                                        { message = "Internal error: failed to communicate with backend."
                                        , source = "FlowOverview.Update.update GetInstanceViewReceived"
                                        , error = error
                                        }
                            in
                            ( model, cmd )

                Failure _ ->
                    ( model, Cmd.none )

        GetDocumentReceived result ->
            case result of
                Ok document ->
                    case model.state of
                        AppOk { innerModel } ->
                            let
                                newDocuments =
                                    case innerModel.mDocuments of
                                        Nothing ->
                                            Dict.singleton document.id document

                                        Just docs ->
                                            Dict.insert document.id document docs

                                newModel =
                                    updateModel model (\inner -> { inner | mDocuments = Just newDocuments })
                            in
                            ( newModel, Cmd.none )

                        Failure _ ->
                            ( model, Cmd.none )

                Err error ->
                    let
                        cmd =
                            errorCmd
                                { message = "Internal error: failed to communicate with backend."
                                , source = "FlowOverview.Update.update GetDocumentReceived"
                                , error = error
                                }
                    in
                    ( model, cmd )

        EnterRejectionClicked ->
            let
                newModel =
                    updateModel model (\inner -> { inner | mRejection = Just <| Model.Rejection { message = "" } })
            in
            ( newModel, Cmd.none )

        RejectButtonClicked ->
            case model.state of
                AppOk { flags, innerModel } ->
                    case innerModel.mRejection of
                        Just (Model.Rejection { message }) ->
                            let
                                postReq =
                                    Http.post
                                        { url = flags.flowApiUrl ++ "/instances/" ++ flags.flowInstanceId ++ "/reject"
                                        , body = Http.jsonBody <| JE.object [ ( "message", JE.string message ) ]
                                        , expect = Http.expectWhatever RejectCallback
                                        }
                            in
                            ( model, postReq )

                        _ ->
                            ( model, Cmd.none )

                Failure _ ->
                    ( model, Cmd.none )

        CancelButtonClicked ->
            let
                newModel =
                    updateModel model (\inner -> { inner | mRejection = Nothing })
            in
            ( newModel, Cmd.none )

        RejectCallback res ->
            case res of
                Ok () ->
                    let
                        newModel =
                            updateModel model
                                (\inner ->
                                    { inner
                                        | mInstance =
                                            case inner.mInstance of
                                                Just instance ->
                                                    Just { instance | status = Model.Failed }

                                                Nothing ->
                                                    Nothing
                                    }
                                )

                        message =
                            "Your rejection message has been sent. Thank you!"

                        cmd =
                            perform <| AddFlashMessage <| FlashSuccess message
                    in
                    ( newModel, cmd )

                Err error ->
                    let
                        cmd =
                            errorCmd
                                { message = "Failed to reject the Flow"
                                , source = "FlowOverview.Update.update RejectCallback"
                                , error = error
                                }
                    in
                    ( model, cmd )

        UpdateTextarea message ->
            let
                newModel =
                    updateModel model (\inner -> { inner | mRejection = Just <| Model.Rejection { message = message } })
            in
            ( newModel, Cmd.none )

        AddFlashMessage flashMessage ->
            let
                ( newFlashMessages, cmd ) =
                    FlashMessage.addFlashMessage { embed = FlashMessageMsg } flashMessage model.flashMessages
            in
            ( { model | flashMessages = newFlashMessages }, cmd )

        ErrorTrace fields ->
            let
                object =
                    JE.object fields
            in
            ( model, errorTraceMsg object )

        FlashMessageMsg msg_ ->
            let
                ( newFlashMessages, cmd ) =
                    FlashMessage.update { embed = FlashMessageMsg } msg_ model.flashMessages
            in
            ( { model | flashMessages = newFlashMessages }, cmd )


updateModel : Model -> (Model.InnerModel -> Model.InnerModel) -> Model
updateModel model f =
    case model.state of
        AppOk { flags, innerModel } ->
            { model | state = AppOk { flags = flags, innerModel = f innerModel } }

        Failure _ ->
            model
