module AdminOnly.UserGroupAdmin.FoldersTab.MoveModal exposing
    ( Model
    , Msg
    , init
    , show
    , update
    , view
    )

import AdminOnly.UserGroupAdmin.FoldersTab.Types exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import FlashMessage
import Html exposing (Html, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Http
import Http.Detailed
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Maybe as M
import Util.APIError exposing (apiErrorMessage)
import Utils exposing (..)


type alias Model =
    { modalVisibility : Modal.Visibility
    , folder : Folder
    , mOrigParentID : Maybe String
    , newParentID : String
    , sNewParentFolder : Status Folder
    }


type Msg
    = CloseModal
    | SubmitForm
    | GotResponse (Result (Http.Detailed.Error String) ( Http.Metadata, String ))
    | GotParentFolder (Result Http.Error Folder)
    | SetNewParentID String


init : Folder -> ( Model, Cmd msg )
init folder =
    let
        model =
            { modalVisibility = Modal.hidden
            , folder = folder
            , mOrigParentID = Nothing
            , newParentID = ""
            , sNewParentFolder = Loading
            }
    in
    ( model, Cmd.none )


getParentFolderCmd : String -> Cmd Msg
getParentFolderCmd fid =
    Http.get
        { url = "/api/frontend/folders/" ++ fid
        , expect = Http.expectJson GotParentFolder folderDecoder
        }


update : (Msg -> msg) -> Globals msg -> Msg -> Cmd msg -> Model -> ( Model, Cmd msg )
update embed globals msg onSuccessCmd model =
    case msg of
        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }, Cmd.none )

        SetNewParentID folderID ->
            ( { model | newParentID = folderID }
            , ite (isInteger folderID) (Cmd.map embed <| getParentFolderCmd folderID) Cmd.none
            )

        SubmitForm ->
            let
                reqParams =
                    [ ( "folder_id", model.folder.id )
                    , ( "folder"
                      , E.encode 0 <|
                            E.object
                                [ ( "parent_id", E.string model.newParentID ) ]
                      )
                    ]
            in
            ( model
            , Http.post
                { url = "/api/frontend/folders/" ++ model.folder.id ++ "/update"
                , body = formBody globals reqParams
                , expect = Http.Detailed.expectString (embed << GotResponse)
                }
            )

        GotResponse result ->
            case result of
                Ok _ ->
                    ( { model | modalVisibility = Modal.hidden }
                    , Cmd.batch
                        [ globals.flashMessage <| FlashMessage.success "Folder was moved"
                        , onSuccessCmd
                        ]
                    )

                Err error ->
                    ( model
                    , globals.flashMessage <|
                        FlashMessage.error (apiErrorMessage error "Error moving the folder")
                    )

        GotParentFolder result ->
            case result of
                Ok folder ->
                    ( { model | sNewParentFolder = Success folder }, Cmd.none )

                Err _ ->
                    ( { model | sNewParentFolder = Failure }, Cmd.none )


show : (Msg -> msg) -> Model -> ( Model, Cmd msg )
show embed model0 =
    let
        model =
            { model0 | modalVisibility = Modal.shown }

        cmd =
            case model.mOrigParentID of
                Just origParentID ->
                    Cmd.map embed (getParentFolderCmd origParentID)

                Nothing ->
                    Cmd.none
    in
    ( model, cmd )


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    let
        name =
            folderDisplayName model.folder

        ( newParentIDIsValid, inputValidity, feedback ) =
            if isInteger model.newParentID then
                case model.sNewParentFolder of
                    Loading ->
                        ( False, [], Form.help [] [ text "Loading parent folder ..." ] )

                    Failure ->
                        ( False
                        , [ Input.danger ]
                        , Form.invalidFeedback [] [ text "Parent folder doesn't exist." ]
                        )

                    Success newParentFolder ->
                        if model.mOrigParentID == Just newParentFolder.id then
                            ( False
                            , [ Input.danger ]
                            , Form.invalidFeedback [] [ text "Parent folder ID is the same as before." ]
                            )

                        else
                            ( True
                            , [ Input.success ]
                            , Form.validFeedback []
                                [ text <| "Parent folder: " ++ folderDisplayName newParentFolder ]
                            )

            else
                ( False
                , [ Input.danger ]
                , Form.invalidFeedback [] [ text "Parent folder ID can only contain numbers." ]
                )
    in
    Modal.config CloseModal
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text <| "Move folder " ++ name ]
        |> Modal.body []
            [ Form.row []
                [ Form.colLabel [ Col.sm4 ] [ text "Parent folder ID" ]
                , Form.col [ Col.sm8 ] <|
                    [ Input.text <|
                        [ Input.attrs
                            [ onInput <| SetNewParentID
                            , value model.newParentID
                            ]
                        ]
                            ++ inputValidity
                    , feedback
                    ]
                ]
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
                [ Button.success
                , Button.attrs [ onClick <| SubmitForm ]
                , Button.disabled <| not newParentIDIsValid
                ]
                [ text "Move folder" ]
            ]
        |> Modal.view model.modalVisibility
        |> Html.map embed
