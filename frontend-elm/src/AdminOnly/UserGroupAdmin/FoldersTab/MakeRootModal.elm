module AdminOnly.UserGroupAdmin.FoldersTab.MakeRootModal exposing
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
import Json.Encode as E
import Return exposing (..)
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


init : Folder -> Return msg Model
init folder =
    singleton
        { modalVisibility = Modal.hidden
        , folder = folder
        }


update : (Msg -> msg) -> Globals msg -> Msg -> Cmd msg -> Model -> Return msg Model
update embed globals msg onSuccessCmd model =
    case msg of
        CloseModal ->
            singleton { model | modalVisibility = Modal.hidden }

        SubmitForm ->
            let
                reqParams =
                    [ ( "folder_id", model.folder.id )
                    , ( "folder"
                      , E.encode 0 <|
                            E.object
                                [ ( "parent_id", E.null ) ]
                      )
                    ]
            in
            return model <|
                Http.post
                    { url = "/api/frontend/folders/" ++ model.folder.id ++ "/update"
                    , body = formBody globals reqParams
                    , expect = Http.Detailed.expectString (embed << GotResponse)
                    }

        GotResponse result ->
            case result of
                Ok _ ->
                    return { model | modalVisibility = Modal.hidden } <|
                        Cmd.batch
                            [ globals.flashMessage <| FlashMessage.success "Folder was made root"
                            , onSuccessCmd
                            ]

                Err error ->
                    return model <| globals.flashMessage <| FlashMessage.error <| apiErrorMessage error "Error making the folder root"


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
        |> Modal.h3 [] [ text "Make folder root" ]
        |> Modal.body []
            [ p [] [ text <| "Are you sure that you want to make folder " ++ name ++ " a root folder?" ]
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
                [ text "Make folder root" ]
            ]
        |> Modal.view model.modalVisibility
        |> Html.map embed
