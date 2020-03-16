module AdminOnly.UserAdmin.DetailsTab.MoveUserModal exposing
    ( Model
    , Msg
    , init
    , show
    , update
    , view
    )

import AdminOnly.UserAdmin.DetailsTab.User exposing (User)
import AdminOnly.UserAdmin.DetailsTab.UserGroup as UserGroup exposing (UserGroup)
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
import Utils exposing (..)


type alias Model =
    { modalVisibility : Modal.Visibility
    , response : Maybe (Result Http.Error String)
    , user : User
    , newUserGroupID : String
    , sNewUserGroupName : Status String
    }


type Msg
    = CloseModal
    | SubmitForm
    | GotResponse (Result Http.Error String)
    | SetUserGroupID String
    | GotUserGroup (Result Http.Error UserGroup)


init : (Msg -> msg) -> User -> ( Model, Cmd msg )
init embed user =
    let
        model =
            { modalVisibility = Modal.hidden
            , response = Nothing
            , user = user
            , newUserGroupID = user.userGroup.id
            , sNewUserGroupName = Success user.userGroup.name
            }
    in
    ( model, Cmd.map embed <| getUserGroupCmd model )


getUserGroupCmd : Model -> Cmd Msg
getUserGroupCmd model =
    Http.get
        { url = "/adminonly/companyadmin/details/" ++ model.newUserGroupID
        , expect = Http.expectJson GotUserGroup UserGroup.decoder
        }


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> ( Model, Cmd msg )
update embed globals msg model =
    case msg of
        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }, Cmd.none )

        SetUserGroupID userGroupID ->
            if isInteger userGroupID then
                let
                    model1 =
                        { model
                            | newUserGroupID = userGroupID
                            , sNewUserGroupName = Loading
                        }
                in
                ( model1, Cmd.map embed <| getUserGroupCmd model1 )

            else
                ( { model | newUserGroupID = userGroupID }, Cmd.none )

        GotResponse result ->
            case result of
                Ok _ ->
                    ( { model
                        | response = Just result
                        , modalVisibility = Modal.hidden
                      }
                    , Cmd.batch
                        [ globals.setPageUrlFromModel -- reloads User Details
                        , globals.flashMessage <| FlashMessage.success "User was moved"
                        ]
                    )

                Err _ ->
                    ( { model | response = Just result }
                    , Cmd.none
                    )

        GotUserGroup response ->
            case response of
                Err _ ->
                    ( { model | sNewUserGroupName = Failure }, Cmd.none )

                Ok userGroup ->
                    ( { model | sNewUserGroupName = Success userGroup.name }, Cmd.none )

        SubmitForm ->
            ( { model | response = Nothing }
            , Http.post
                    { url = "/adminonly/useradmin/move/" ++ model.user.id
                    , body = formBody globals [ ( "companyid", model.newUserGroupID ) ]
                    , expect = Http.expectString (embed << GotResponse)
                    }
            )


show : Model -> Model
show model =
    { model | modalVisibility = Modal.shown, response = Nothing }


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    let
        ( newUserGroupIDIsValid, inputValidity, feedback ) =
            if isInteger model.newUserGroupID then
                case model.sNewUserGroupName of
                    Loading ->
                        ( False, [], Form.help [] [ text "Loading company ..." ] )

                    Failure ->
                        ( False, [ Input.danger ], Form.invalidFeedback [] [ text "Company ID does not exist." ] )

                    Success userGroupName ->
                        if model.user.userGroup.id == model.newUserGroupID then
                            ( False, [ Input.danger ], Form.invalidFeedback [] [ text "Company ID is same as before." ] )

                        else
                            ( True, [ Input.success ], Form.validFeedback [] [ text <| "New company name: " ++ userGroupName ] )

            else
                ( False, [ Input.danger ], Form.invalidFeedback [] [ text "Company ID can only contain numbers." ] )
    in
    Modal.config CloseModal
        --|> Modal.large
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Move user to different company" ]
        |> Modal.body []
            [ Form.row []
                [ Form.colLabel [ Col.sm4 ] [ text "CompanyID" ]
                , Form.col [ Col.sm8 ] <|
                    [ Input.text <|
                        [ Input.attrs
                            [ onInput <| SetUserGroupID
                            , value model.newUserGroupID
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
                    [ onClick CloseModal
                    , class "mr-auto"
                    ]
                ]
                [ text "Cancel" ]
            , Button.button
                [ Button.danger
                , Button.attrs [ onClick SubmitForm ]
                , Button.disabled <| not newUserGroupIDIsValid
                ]
                [ text "Move" ]
            ]
        |> Modal.view model.modalVisibility
        |> Html.map embed
