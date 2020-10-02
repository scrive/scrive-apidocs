module Entry.FlowOverview exposing (main)

import Browser
import Dict
import FlowOverview.Json exposing (..)
import FlowOverview.Model exposing (..)
import FlowOverview.View as View
import FlowOverview.Update as Update
import Json.Decode as JD exposing (Value)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Http
import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Maybe as M


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , update = Update.update
        , subscriptions = subscriptions
        , view = view
        }


init : Value -> ( Model, Cmd Msg )
init flagsValue =
    let (state, cmd) = case JD.decodeValue decodeFlags flagsValue of
            Ok flags ->
                case Dict.get "xtoken" flags.cookies of
                    Just _ ->
                        let
                            innerModel =
                                { mInstance = Nothing, mDocuments = Nothing, mRejection = Nothing }

                            getInstanceViewCmd =
                                Http.get
                                    { url = flags.flowApiUrl ++ "/instances/" ++ flags.flowInstanceId ++ "/view"
                                    , expect = Http.expectJson GetInstanceViewReceived instanceViewDecoder
                                    }
                        in
                        ( AppOk { flags = flags, innerModel = innerModel }, getInstanceViewCmd )

                    Nothing ->
                        ( Failure "Missing xtoken cookie", Cmd.none )

            Err err ->
                ( Failure (JD.errorToString err), Cmd.none )
    in ({state = state, flashMessages = FlashMessage.initialState}, cmd)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model.state of
        Failure err ->
            div []
                [
                  FlashMessage.view {embed = FlashMessageMsg} model.flashMessages,
                  text <| "Error: " ++ err
                ]

        AppOk { innerModel } ->
            case ( innerModel.mInstance, innerModel.mDocuments ) of
                ( Just instance, Just documents ) ->
                    let
                        mDocWithLink mDoc mLink =
                            M.map (\doc -> ( doc, mLink )) mDoc

                        actionDocsWithLinks =
                            let
                                actionDocs =
                                    List.map (\a -> Dict.get a.actionDocument documents) instance.actions

                                actionLinks =
                                    List.map (\a -> Just a.actionLink) instance.actions
                            in
                            List.map2 mDocWithLink actionDocs actionLinks
                                |> List.filterMap identity

                        stateDocsNoLinks =
                            List.map (\d -> Dict.get d.documentId documents) instance.state.documents
                                |> List.map (\mDoc -> mDocWithLink mDoc Nothing)
                                |> List.filterMap identity

                        ( statusHeader, statusMessage ) =
                            case instance.status of
                                InProgress ->
                                    ( "Document workflow is in progress"
                                    , "Please wait for other participants to complete their actions."
                                    )

                                Completed ->
                                    ( "Document workflow is complete"
                                    , "There are no further actions to take."
                                    )

                                Failed ->
                                    ( "Document workflow failed"
                                    , """This document workflow did not complete successfully.
                                       There are no further actions to take."""
                                    )

                        statusSection =
                            View.section statusHeader (p [] [ text statusMessage ])

                        actionDocsSection =
                            View.section "Select a document to sign or approve"
                                (View.docTable actionDocsWithLinks "No documents to sign or approve")
                    in
                    div []
                        [
                          FlashMessage.view {embed = FlashMessageMsg} model.flashMessages,

                          if List.isEmpty actionDocsWithLinks then
                            statusSection

                          else
                            actionDocsSection
                        , View.section "Signed or approved documents"
                            (View.docTable stateDocsNoLinks "No signed or approved documents")
                        , if instance.status == InProgress then
                            let content = case innerModel.mRejection of
                                    Just rejectionModel -> View.rejectionComponent rejectionModel
                                    Nothing -> View.rejectionButton
                            in View.section "Flow actions" content
                          else
                            div [] []
                        ]

                ( Just _, Nothing ) ->
                    div [ class "loading" ]
                        [ text "Loading documents..." ]

                _ ->
                    div [ class "loading" ]
                        [ text "Loading instance..." ]
