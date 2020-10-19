port module FlowOverview.Update exposing (update)

import Dict
import FlowOverview.Json exposing (..)
import FlowOverview.Model as Model exposing (Model, Msg(..), State(..))
import Http
import Json.Encode as JE
import Lib.Components.FlashMessage as FlashMessage exposing (addFlashMessage)
import Lib.Flow exposing (flowPost)
import Lib.Misc.Cmd exposing (perform)
import Lib.Misc.Http exposing (encodeError)
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Return exposing (..)


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


getDocuments : Model.Flags -> Model -> Model.GetInstanceView -> Return Msg Model
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
    return newModel <| Cmd.batch getDocumentCmds


update : Msg -> Model -> Return Msg Model
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
                            return model <|
                                errorCmd
                                    { message = "Internal error: failed to communicate with backend."
                                    , source = "FlowOverview.Update.update GetInstanceViewReceived"
                                    , error = error
                                    }

                Failure _ ->
                    singleton model

        GetDocumentReceived result ->
            case result of
                Ok document ->
                    singleton <|
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
                                newModel

                            Failure _ ->
                                model

                Err error ->
                    return model <|
                        errorCmd
                            { message = "Internal error: failed to communicate with backend."
                            , source = "FlowOverview.Update.update GetDocumentReceived"
                            , error = error
                            }

        EnterRejectionClicked ->
            singleton <|
                updateModel model (\inner -> { inner | mRejection = Just <| Model.Rejection { message = "" } })

        RejectButtonClicked ->
            case model.state of
                AppOk { flags, innerModel } ->
                    case innerModel.mRejection of
                        Just (Model.Rejection { message }) ->
                            return model <|
                                flowPost
                                    { url = flags.flowApiUrl ++ "/instances/" ++ flags.flowInstanceId ++ "/reject"
                                    , body = Http.jsonBody <| JE.object [ ( "message", JE.string message ) ]
                                    , expect = Http.expectWhatever RejectCallback
                                    , xtoken = flags.xtoken
                                    }

                        _ ->
                            singleton model

                Failure _ ->
                    singleton model

        CancelButtonClicked ->
            let
                newModel =
                    updateModel model (\inner -> { inner | mRejection = Nothing })
            in
            singleton newModel

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
                    in
                    return newModel <| perform <| AddFlashMessage <| FlashSuccess "Your rejection message has been sent. Thank you!"

                Err error ->
                    return model <|
                        errorCmd
                            { message = "Failed to reject the Flow"
                            , source = "FlowOverview.Update.update RejectCallback"
                            , error = error
                            }

        UpdateTextarea message ->
            let
                newModel =
                    updateModel model (\inner -> { inner | mRejection = Just <| Model.Rejection { message = message } })
            in
            singleton newModel

        AddFlashMessage flashMessage ->
            let
                ( newFlashMessages, cmd ) =
                    FlashMessage.addFlashMessage { embed = FlashMessageMsg } flashMessage model.flashMessages
            in
            return { model | flashMessages = newFlashMessages } cmd

        ErrorTrace fields ->
            let
                object =
                    JE.object fields
            in
            return model <| errorTraceMsg object

        FlashMessageMsg msg_ ->
            let
                ( newFlashMessages, cmd ) =
                    FlashMessage.update { embed = FlashMessageMsg } msg_ model.flashMessages
            in
            return { model | flashMessages = newFlashMessages } cmd


updateModel : Model -> (Model.InnerModel -> Model.InnerModel) -> Model
updateModel model f =
    case model.state of
        AppOk { flags, innerModel } ->
            { model | state = AppOk { flags = flags, innerModel = f innerModel } }

        Failure _ ->
            model
