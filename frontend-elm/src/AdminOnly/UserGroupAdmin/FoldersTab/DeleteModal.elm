module AdminOnly.UserGroupAdmin.FoldersTab.DeleteModal exposing
    ( Model
    , Msg
    , init
    , show
    , update
    , view
    )

import AdminOnly.UserGroupAdmin.FoldersTab.Types exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import FlashMessage
import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Http.Detailed
import Util.APIError exposing (apiErrorMessage)
import Utils exposing (..)


type alias Model =
    { modalVisibility : Modal.Visibility
    , folder : Folder
    }


type Msg
    = CloseModal
    | SubmitForm
    | GotResponse (Result (Http.Detailed.Error String) ( Http.Metadata, String ))


init : Folder -> ( Model, Cmd msg )
init folder =
    let
        model =
            { modalVisibility = Modal.hidden
            , folder = folder
            }
    in
    ( model, Cmd.none )


update : (Msg -> msg) -> Globals msg -> Msg -> Cmd msg -> Model -> ( Model, Cmd msg )
update embed globals msg onSuccessCmd model =
    case msg of
        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }, Cmd.none )

        SubmitForm ->
            ( model
            , Http.post
                { url = "/api/frontend/folders/" ++ model.folder.id ++ "/delete"
                , body = formBody globals []
                , expect = Http.Detailed.expectString (embed << GotResponse)
                }
            )

        GotResponse result ->
            case result of
                Ok _ ->
                    ( { model | modalVisibility = Modal.hidden }
                    , Cmd.batch
                        [ globals.flashMessage <| FlashMessage.success "Folder was deleted"
                        , onSuccessCmd
                        ]
                    )

                Err error ->
                    ( model
                    , globals.flashMessage <|
                        FlashMessage.error (apiErrorMessage error "Error deleting the folder")
                    )


show : Model -> Model
show model =
    { model | modalVisibility = Modal.shown }


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    let
        name =
            folderDisplayName model.folder
    in
    Modal.config CloseModal
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Delete folder" ]
        |> Modal.body []
            [ p [] [ text <| "Are you sure that you want to delete folder " ++ name ++ "?" ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.secondary
                , Button.attrs
                    [ onClick <| CloseModal
                    , class "mr-auto"
                    ]
                ]
                [ text "Cancel" ]
            , Button.button
                [ Button.danger
                , Button.attrs [ onClick <| SubmitForm ]
                ]
                [ text "Delete folder" ]
            ]
        |> Modal.view model.modalVisibility
        |> Html.map embed
