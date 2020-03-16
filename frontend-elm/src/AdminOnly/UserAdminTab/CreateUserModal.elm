module AdminOnly.UserAdminTab.CreateUserModal exposing
    ( Config(..)
    , Model
    , Msg
    , UserCreated
    , init
    , show
    , update
    , view
    )

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import EnumExtra as Enum exposing (findEnumValue)
import Html exposing (Html, p, text)
import Html.Attributes exposing (checked, selected, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as D exposing (Decoder)
import Language exposing (Language, enumLanguage)
import List as L
import Maybe as M
import Utils exposing (..)


type alias Model =
    { modalVisibility : Modal.Visibility
    , config : Config
    , email : String
    , firstName : String
    , secondName : String
    , language : Language
    , isUserGroupAdmin : Bool
    }


type Config
    = CreateUserInUserGroup String
    | CreateUserWithRootUserGroup


type Msg
    = CloseModal
    | SubmitForm
    | SetEmail String
    | SetFirstName String
    | SetSecondName String
    | SetLanguage String
    | SetIsUserGroupAdmin Bool
    | GotResponse (Result Http.Error Response)


init : Config -> ( Model, Cmd msg )
init config =
    let
        model =
            { modalVisibility = Modal.hidden
            , config = config
            , email = ""
            , firstName = ""
            , secondName = ""
            , language = Language.English
            , isUserGroupAdmin = False
            }
    in
    ( model, Cmd.none )


type alias UserCreated
    = Maybe (Result String String)

update : (Msg -> msg) -> (UserCreated -> msg) -> Globals msg -> Msg -> Model -> ( Model, Cmd msg )
update embed userCreatedMsg globals msg model =
    case msg of
        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }, Cmd.none )

        SubmitForm ->
            let
                ( url, moreParams ) =
                    case model.config of
                        CreateUserWithRootUserGroup ->
                            ( "/adminonly/createuser", [ ( "add", "true" ) ] )

                        CreateUserInUserGroup ugid ->
                            ( "/adminonly/companyadmin/users/" ++ ugid
                            , ite model.isUserGroupAdmin [ ( "iscompanyadmin", "" ) ] []
                            )
            in
            ( model
            , Http.post
                { url = url
                , body =
                    formBody globals <|
                        [ ( "fstname", model.firstName )
                        , ( "sndname", model.secondName )
                        , ( "email", model.email )
                        , ( "lang", Enum.toString enumLanguage model.language )
                        ]
                            ++ moreParams
                , expect = Http.expectJson (embed << GotResponse) responseDecoder
                }
            )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        SetFirstName firstName ->
            ( { model | firstName = firstName }, Cmd.none )

        SetSecondName secondName ->
            ( { model | secondName = secondName }, Cmd.none )

        SetLanguage languageString ->
            ( case findEnumValue enumLanguage languageString of
                Err _ ->
                    model

                Ok language ->
                    { model | language = language }
            , Cmd.none
            )

        SetIsUserGroupAdmin isAdmin ->
            ( { model | isUserGroupAdmin = isAdmin }, Cmd.none )

        GotResponse result ->
            let callback = perform << userCreatedMsg
            in
            case result of
                Ok r ->
                    if r.success then
                        ( { model | modalVisibility = Modal.hidden }
                        , callback <| Just <| Ok "User created."
                        )

                    else
                        ( model
                        , callback <| Just <| Err <| M.withDefault "Error creating a user." r.error
                        )
                Err _ ->
                    ( model, callback <| Just <| Err "Error creating a user." )


type alias Response =
    { success : Bool
    , error : Maybe String
    }


responseDecoder : Decoder Response
responseDecoder =
    D.map2 Response
        (D.field "success" D.bool)
        (D.field "error_message" (D.nullable D.string))


show : Model -> Model
show model =
    { model | modalVisibility = Modal.shown, firstName = "", secondName = "", email = "", language = Language.English, isUserGroupAdmin = False }


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    let
        ( headingText, description, isAdminCheckbox ) =
            case model.config of
                CreateUserWithRootUserGroup ->
                    ( "Create user with empty company", "Create new account", [] )

                CreateUserInUserGroup _ ->
                    ( "Create user in company"
                    , "Create new user account in company"
                    , [ Form.row
                            []
                            [ Form.colLabel labelColAttrs [ text "Is company admin:" ]
                            , Form.col inputColAttrs
                                [ Checkbox.checkbox
                                    [ Checkbox.attrs <|
                                        [ checked model.isUserGroupAdmin
                                        , onCheck <| SetIsUserGroupAdmin
                                        ]
                                    ]
                                    ""
                                ]
                            ]
                      ]
                    )

        labelColAttrs =
            [ Col.sm2 ]

        inputColAttrs =
            [ Col.sm10 ]
    in
    Modal.config CloseModal
        |> Modal.large
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text headingText ]
        |> Modal.body []
            [ p [] [ text description ]
            , Form.form [ onSubmit <| SubmitForm ] <|
                [ Form.row []
                    [ Form.colLabel labelColAttrs [ text "First name" ]
                    , Form.col inputColAttrs
                        [ Input.text [ Input.attrs [ onInput <| SetFirstName, value model.firstName ] ]
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel labelColAttrs [ text "Second name" ]
                    , Form.col inputColAttrs
                        [ Input.text [ Input.attrs [ onInput <| SetSecondName, value model.secondName ] ]
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel labelColAttrs [ text "Email" ]
                    , Form.col inputColAttrs
                        [ Input.email [ Input.attrs [ onInput <| SetEmail, value model.email ] ]
                        , Form.help [] [ text "We'll never share your email with anyone else." ]
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel labelColAttrs [ text "Language" ]
                    , Form.col inputColAttrs
                        [ Select.select [ Select.onChange <| SetLanguage ] <|
                            L.map
                                (\lang ->
                                    Select.item
                                        [ value <| Enum.toString enumLanguage lang
                                        , selected <| lang == model.language
                                        ]
                                        [ text <| Enum.toHumanString enumLanguage lang ]
                                )
                            <|
                                Enum.allValues enumLanguage
                        ]
                    ]
                ]
                    ++ isAdminCheckbox
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.secondary
                , Button.attrs [ onClick <| CloseModal ]
                ]
                [ text "Cancel" ]
            , Button.button
                [ Button.success
                , Button.attrs [ onClick <| SubmitForm ]
                ]
                [ text "Create" ]
            ]
        |> Modal.view model.modalVisibility
        |> Html.map embed
