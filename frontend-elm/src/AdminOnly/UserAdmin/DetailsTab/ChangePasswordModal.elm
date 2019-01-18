module AdminOnly.UserAdmin.DetailsTab.ChangePasswordModal exposing
    ( Model
    , Msg
    , init
    , show
    , update
    , view
    )

import AdminOnly.UserAdmin.DetailsTab.User exposing (User)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Either exposing (Either(..))
import FlashMessage
import Html exposing (Html, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Utils exposing (..)


type alias Model =
    { modalVisibility : Modal.Visibility
    , response : Maybe (Result Http.Error Bool)
    , user : User
    , newPassword : String
    }


type Msg
    = CloseModal
    | SubmitForm
    | GotResponse (Result Http.Error Bool)
    | SetPassword String


init : User -> ( Model, Action msg Msg )
init user =
    let
        model =
            { modalVisibility = Modal.hidden
            , response = Nothing
            , user = user
            , newPassword = ""
            }
    in
    ( model, Cmd.none )


update : Globals msg -> Msg -> Model -> ( Model, Action msg Msg )
update globals msg model =
    case msg of
        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }, Cmd.none )

        SetPassword password ->
            ( { model | newPassword = password }, Cmd.none )

        GotResponse result ->
            case result of
                Ok changed ->
                    ( { model
                        | response = Just result
                        , modalVisibility = Modal.hidden
                      }
                    , Cmd.batch
                        [ outerCmd <| globals.setPageUrlFromModel -- reloads User Details
                        , outerCmd <|
                            globals.flashMessage <|
                                if changed then
                                    FlashMessage.success "Changed"

                                else
                                    FlashMessage.error "Failed"
                        ]
                    )

                Err _ ->
                    ( { model | response = Just result }, Cmd.none )

        SubmitForm ->
            ( { model | response = Nothing }
            , innerCmd <|
                Http.post
                    { url = "/adminonly/useradmin/changepassword/" ++ model.user.id
                    , body = formBody globals [ ( "password", model.newPassword ) ]
                    , expect =
                        Http.expectJson GotResponse
                            (D.field "changed" D.bool)
                    }
            )


show : Model -> Model
show model =
    { model | modalVisibility = Modal.shown, response = Nothing }


view : Model -> Html Msg
view model =
    Modal.config CloseModal
        --|> Modal.large
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Change user password" ]
        |> Modal.body []
            [ Form.row []
                [ Form.colLabel [ Col.sm4 ] [ text "Password" ]
                , Form.col [ Col.sm8 ] <|
                    [ Input.text <|
                        [ Input.attrs
                            [ onInput <| SetPassword
                            , value model.newPassword
                            ]
                        ]
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
                ]
                [ text "Change" ]
            ]
        |> Modal.view model.modalVisibility
