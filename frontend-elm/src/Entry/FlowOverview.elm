module Entry.FlowOverview exposing (main)

import Browser
import Dict
import FlowOverview.Json exposing (..)
import FlowOverview.Model exposing (..)
import FlowOverview.View as View
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as JD exposing (Value)
import Maybe as M


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Value -> ( Model, Cmd Msg )
init flagsValue =
    case JD.decodeValue decodeFlags flagsValue of
        Ok flags ->
            case Dict.get "xtoken" flags.cookies of
                Just _ ->
                    let
                        innerModel =
                            { mInstance = Nothing, mDocuments = Nothing }

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        AppOk { flags, innerModel } ->
            case msg of
                GetInstanceViewReceived result ->
                    case result of
                        Ok instance ->
                            let
                                newModel =
                                    updateModel model (\inner -> { inner | mInstance = Just instance })

                                docs =
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
                                        docs
                            in
                            ( newModel, Cmd.batch getDocumentCmds )

                        Err _ ->
                            -- TODO: Show more specific error info for failed json decoding
                            ( Failure "Request or json decoding failed for 'view instance' API call.", Cmd.none )

                GetDocumentReceived result ->
                    case result of
                        Ok document ->
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

                        Err _ ->
                            -- TODO: Show more specific error info for failed json decoding
                            ( Failure "Request or json decoding failed for 'get document' API call.", Cmd.none )

        Failure _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Failure err ->
            text <| "Error: " ++ err

        AppOk { flags, innerModel } ->
            case ( innerModel.mInstance, innerModel.mDocuments ) of
                ( Just instance, Just documents ) ->
                    let
                        mDocWithLink mDoc mLink =
                            M.map (\doc -> ( doc, mLink )) mDoc

                        actionDocs =
                            List.map (\a -> Dict.get a.actionDocument documents) instance.actions

                        actionLinks =
                            List.map (\a -> Just a.actionLink) instance.actions

                        actionDocsWithLinks =
                            List.map2 mDocWithLink actionDocs actionLinks
                                |> List.filterMap identity

                        stateDocsNoLinks =
                            List.map (\d -> Dict.get d.documentId documents) instance.state.documents
                                |> List.map (\mDoc -> mDocWithLink mDoc Nothing)
                                |> List.filterMap identity
                    in
                    div []
                        [ View.section "Select a document to sign or approve"
                            (View.docTable actionDocsWithLinks "No documents to sign or approve")
                        , View.section "Signed or approved documents"
                            (View.docTable stateDocsNoLinks "No signed or approved documents")
                        ]

                ( Just _, Nothing ) ->
                    div [ class "loading" ]
                        [ text "Loading documents..." ]

                _ ->
                    div [ class "loading" ]
                        [ text "Loading instance..." ]

