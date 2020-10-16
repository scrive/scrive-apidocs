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
import Return exposing (..)
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


init : User -> Return msg Model
init user =
    singleton
        { modalVisibility = Modal.hidden
        , response = Nothing
        , user = user
        , newPassword = ""
        }


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> Return msg Model
update embed globals msg model =
    case msg of
        CloseModal ->
            singleton { model | modalVisibility = Modal.hidden }

        SetPassword password ->
            singleton { model | newPassword = password }

        GotResponse result ->
            case result of
                Ok changed ->
                    return
                        { model
                            | response = Just result
                            , modalVisibility = Modal.hidden
                        }
                    <|
                        Cmd.batch
                            [ globals.setPageUrlFromModel -- reloads User Details
                            , globals.flashMessage <|
                                if changed then
                                    FlashMessage.success "Changed"

                                else
                                    FlashMessage.error "Failed"
                            ]

                Err _ ->
                    singleton { model | response = Just result }

        SubmitForm ->
            return { model | response = Nothing } <|
                Http.post
                    { url = "/adminonly/useradmin/changepassword/" ++ model.user.id
                    , body = formBody globals [ ( "password", model.newPassword ) ]
                    , expect =
                        Http.expectJson (embed << GotResponse)
                            (D.field "changed" D.bool)
                    }


show : Model -> Model
show model =
    { model | modalVisibility = Modal.shown, response = Nothing }


view : (Msg -> msg) -> Model -> Html msg
view embed model =
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
        |> Html.map embed
