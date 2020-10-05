module FlowOverview.View exposing (docTable, rejectionButton, rejectionComponent, section)

import Bootstrap.Table as Table
import FlowOverview.Model as Model exposing (..)
import Html exposing (Html, a, div, h4, p, text, textarea)
import Html.Attributes exposing (autocomplete, class, colspan, href, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Maybe as M


section : String -> Html Msg -> Html Msg
section title content =
    div [ class "main" ]
        [ div [ class "section" ]
            [ h4 [] [ text title ]
            , content
            ]
        ]


docTable : List ( Document, Maybe Url ) -> String -> Html Msg
docTable documents noDocumentsText =
    let
        fullName signatory =
            List.filterMap
                (\field ->
                    case field of
                        SignatoryNameField rec ->
                            Just rec.value

                        SignatoryPersonalNumberField _ ->
                            Nothing

                        SignatoryEmailField _ ->
                            Nothing

                        SignatoryMobileField _ ->
                            Nothing

                        SignatoryOtherField _ ->
                            Nothing
                )
                signatory.fields
                |> String.join " "

        displayName name =
            case String.trim name of
                "" ->
                    "n/a"

                nonEmptyName ->
                    nonEmptyName

        namesOfSignatories doc =
            List.map (\signatory -> displayName <| fullName signatory) doc.parties
                |> String.join ", "

        authorName doc =
            List.filter .isAuthor doc.parties
                |> List.map (\signatory -> displayName <| fullName signatory)
                |> String.concat

        cellClass =
            Table.cellAttr << class

        maybeLink mUrl label =
            M.map (\url -> a [ href url ] [ text label ]) mUrl
                |> M.withDefault (text label)
    in
    div [ class "table" ]
        [ Table.table
            { options = []
            , thead =
                Table.thead []
                    [ Table.tr [ Table.rowAttr <| class "row" ]
                        [ Table.th [ cellClass "col-md-4" ] [ text "Document" ]
                        , Table.th [ cellClass "col-md-4" ] [ text "Participants" ]
                        , Table.th [ cellClass "col-md-4" ] [ text "Sender" ]
                        ]
                    ]
            , tbody =
                Table.tbody [] <|
                    case documents of
                        [] ->
                            [ Table.tr [ Table.rowAttr <| class "row" ]
                                [ Table.td
                                    [ Table.cellAttr <| colspan 3
                                    , cellClass "col-md-12 no-items"
                                    ]
                                    [ text noDocumentsText ]
                                ]
                            ]

                        _ ->
                            List.map
                                (\( document, mSignViewUrl ) ->
                                    Table.tr [ Table.rowAttr <| class "row" ]
                                        [ Table.td
                                            [ cellClass "col-md-4" ]
                                            [ maybeLink mSignViewUrl document.title ]
                                        , Table.td
                                            [ cellClass "col-md-4" ]
                                            [ text <| namesOfSignatories document ]
                                        , Table.td
                                            [ cellClass "col-md-4" ]
                                            [ text <| authorName document ]
                                        ]
                                )
                                documents
            }
        ]


rejectionButton : Html Msg
rejectionButton =
    div []
        [ p []
            [ a
                [ class "button enter-rejection-button"
                , onClick <| EnterRejectionClicked
                ]
                [ text "Reject the documents" ]
            ]
        ]


rejectionComponent : Model.Rejection -> Html Msg
rejectionComponent (Rejection { message }) =
    div [ class "rejection" ]
        [ p [ style "margin-bottom" "1em" ]
            [ text """Please let us know why you are rejecting the Flow.""" ]
        , textarea
            [ placeholder "Enter message..." -- TODO: Localisation
            , autocomplete False
            , value message
            , onInput UpdateTextarea
            ]
            []
        , div
            [ class "button-group right"
            ]
            [ a
                [ class "button button-reject"
                , onClick RejectButtonClicked
                ]
                [ text "Reject" ]
            , a
                [ class "button button-cancel"
                , onClick CancelButtonClicked
                ]
                [ text "Cancel" ]
            ]
        ]
