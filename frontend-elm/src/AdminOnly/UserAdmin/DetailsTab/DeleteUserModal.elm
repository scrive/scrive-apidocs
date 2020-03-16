module AdminOnly.UserAdmin.DetailsTab.DeleteUserModal exposing
    ( Model
    , Msg
    , init
    , show
    , update
    , view
    )

import AdminOnly.UserAdmin.DetailsTab.User exposing (User)
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import FlashMessage
import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Utils exposing (..)


type alias Model =
    { modalVisibility : Modal.Visibility
    , response : Maybe (Result Http.Error String)
    , user : User
    }


type Msg
    = CloseModal
    | SubmitForm
    | GotResponse (Result Http.Error String)


init : User -> ( Model, Cmd msg )
init user =
    let
        model =
            { modalVisibility = Modal.hidden
            , response = Nothing
            , user = user
            }
    in
    ( model, Cmd.none )


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> ( Model, Cmd msg )
update embed globals msg model =
    case msg of
        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }, Cmd.none )

        SubmitForm ->
            ( { model | response = Nothing }
            , Http.post
                    { url = "/adminonly/useradmin/delete/" ++ model.user.id
                    , body = formBody globals []
                    , expect = Http.expectString (embed << GotResponse)
                    }
            )

        GotResponse result ->
            case result of
                Ok _ ->
                    ( { model
                        | response = Just result
                        , modalVisibility = Modal.hidden
                      }
                      -- redirect to list of company users
                    , Cmd.batch
                        [ globals.gotoUserGroupUsers model.user.userGroup.id
                        , globals.flashMessage <| FlashMessage.success "User was deleted"
                        ]
                    )

                Err _ ->
                    ( { model | response = Just result }
                    , Cmd.none
                    )


show : Model -> Model
show model =
    { model | modalVisibility = Modal.shown, response = Nothing }


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    Modal.config CloseModal
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Delete" ]
        |> Modal.body []
            [ p [] [ text "Are you sure that you want to delete this user?" ]
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
                [ text "Delete" ]
            ]
        |> Modal.view model.modalVisibility
        |> Html.map embed
