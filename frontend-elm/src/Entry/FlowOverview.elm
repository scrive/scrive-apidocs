module Entry.FlowOverview exposing (main)

import Browser
import Dict exposing (Dict)
import FlowOverview.Json exposing (..)
import FlowOverview.Model exposing (..)
import FlowOverview.Update as Update
import FlowOverview.View as View
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as JD exposing (Value)
import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Maybe as M
import Return exposing (..)


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , update = Update.update
        , subscriptions = subscriptions
        , view = view
        }


init : Value -> Return Msg Model
init flagsValue =
    let
        ( state, cmd ) =
            case JD.decodeValue decodeFlags flagsValue of
                Ok flags ->
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

                Err err ->
                    singleton <| Failure <| JD.errorToString err
    in
    return { state = state, flashMessages = FlashMessage.initialState } cmd


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


maybeAdd : M.Maybe a -> b -> M.Maybe ( a, b )
maybeAdd ma b =
    M.map (\a -> ( a, b )) ma


getDocs :
    GetInstanceView
    -> Dict String Document
    ->
        ( List ( Document, M.Maybe Url )
        , List ( Document, M.Maybe a )
        )
getDocs instance documents =
    ( let
        actionDocs =
            List.map (\a -> Dict.get a.actionDocument documents) instance.actions

        actionLinks =
            List.map (\a -> Just a.actionLink) instance.actions
      in
      List.map2 maybeAdd actionDocs actionLinks
        |> List.filterMap identity
    , List.map (\d -> Dict.get d.documentId documents) instance.state.documents
        |> List.map (\mDoc -> maybeAdd mDoc Nothing)
        |> List.filterMap identity
    )


view : Model -> Html Msg
view model =
    case model.state of
        Failure err ->
            div []
                [ FlashMessage.view { embed = FlashMessageMsg } model.flashMessages
                , text <| "Error: " ++ err
                ]

        AppOk { innerModel } ->
            case ( innerModel.mInstance, innerModel.mDocuments ) of
                ( Just instance, Just documents ) ->
                    let
                        ( actionDocsWithLinks, stateDocsNoLinks ) =
                            getDocs instance documents

                        active =
                            not <| List.isEmpty actionDocsWithLinks

                        statusHeader =
                            case instance.status of
                                InProgress ->
                                    "Document workflow is in progress"

                                Completed ->
                                    "Document workflow is complete"

                                Failed ->
                                    "Document workflow failed"

                        statusMessage =
                            case instance.status of
                                InProgress ->
                                    if active then
                                        "Please perform the actions below."

                                    else
                                        "Please wait for other participants to complete their actions."

                                Completed ->
                                    "There are no further actions to take."

                                Failed ->
                                    """This document workflow did not complete successfully.
                                       There are no further actions to take."""
                    in
                    div []
                        [ FlashMessage.view { embed = FlashMessageMsg } model.flashMessages
                        , View.section statusHeader (p [] [ text statusMessage ])
                        , if instance.status == InProgress && active then
                            View.section "Select a document to sign or approve"
                                (View.docTable actionDocsWithLinks "No documents to sign or approve")

                          else
                            div [] []
                        , View.section "Signed or approved documents"
                            (View.docTable stateDocsNoLinks "No signed or approved documents")
                        , if instance.status == InProgress then
                            let
                                content =
                                    case innerModel.mRejection of
                                        Just rejectionModel ->
                                            View.rejectionComponent rejectionModel

                                        Nothing ->
                                            View.rejectionButton
                            in
                            div [ class "main" ]
                                [ div [ class "section" ]
                                    [ content
                                    ]
                                ]

                          else
                            div [] []
                        ]

                ( Just _, Nothing ) ->
                    div [ class "loading" ]
                        [ text "Loading documents..." ]

                _ ->
                    div [ class "loading" ]
                        [ text "Loading instance..." ]
