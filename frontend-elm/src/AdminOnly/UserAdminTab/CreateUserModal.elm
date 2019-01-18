module AdminOnly.UserAdminTab.CreateUserModal exposing
    ( Config(..)
    , Model
    , Msg
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
import Utils exposing (..)


type alias Model =
    { modalVisibility : Modal.Visibility
    , config : Config
    , response : Maybe (Result Http.Error Response)
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
            , response = Nothing
            , email = ""
            , firstName = ""
            , secondName = ""
            , language = Language.English
            , isUserGroupAdmin = False
            }
    in
    ( model, Cmd.none )


update : Globals msg -> Msg -> Model -> ( Model, Cmd Msg, Bool )
update globals msg model =
    case msg of
        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }, Cmd.none, False )

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
            ( { model | response = Nothing }
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
                , expect = Http.expectJson GotResponse responseDecoder
                }
            , False
            )

        SetEmail email ->
            ( { model | email = email }, Cmd.none, False )

        SetFirstName firstName ->
            ( { model | firstName = firstName }, Cmd.none, False )

        SetSecondName secondName ->
            ( { model | secondName = secondName }, Cmd.none, False )

        SetLanguage languageString ->
            ( case findEnumValue enumLanguage languageString of
                Err _ ->
                    model

                Ok language ->
                    { model | language = language }
            , Cmd.none
            , False
            )

        SetIsUserGroupAdmin isAdmin ->
            ( { model | isUserGroupAdmin = isAdmin }, Cmd.none, False )

        GotResponse result ->
            case result of
                Ok _ ->
                    ( { model
                        | response = Just result
                        , modalVisibility = Modal.hidden
                      }
                    , Cmd.none
                    , True
                    )

                Err _ ->
                    ( { model | response = Just result }, Cmd.none, False )


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
    { model | modalVisibility = Modal.shown, response = Nothing }


view : Model -> Html Msg
view model =
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
                        [ Input.text [ Input.attrs [ onInput <| SetFirstName ] ]
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel labelColAttrs [ text "Second name" ]
                    , Form.col inputColAttrs
                        [ Input.text [ Input.attrs [ onInput <| SetSecondName ] ]
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel labelColAttrs [ text "Email" ]
                    , Form.col inputColAttrs
                        [ Input.email [ Input.attrs [ onInput <| SetEmail ] ]
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
