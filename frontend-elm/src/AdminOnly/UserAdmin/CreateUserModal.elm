module AdminOnly.UserAdmin.CreateUserModal exposing
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
import FlashMessage
import Html exposing (Html, p, text)
import Html.Attributes exposing (checked, selected, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as D exposing (Decoder)
import Language exposing (Language, enumLanguage)
import List as L
import Maybe as M
import Return exposing (..)
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


type Msg
    = CloseModal
    | SubmitForm
    | SetEmail String
    | SetFirstName String
    | SetSecondName String
    | SetLanguage String
    | SetIsUserGroupAdmin Bool
    | GotResponse (Result Http.Error Response)


init : Config -> Return msg Model
init config =
    singleton
        { modalVisibility = Modal.hidden
        , config = config
        , email = ""
        , firstName = ""
        , secondName = ""
        , language = Language.English
        , isUserGroupAdmin = False
        }


type alias UserCreated =
    Maybe (Result String String)


update : (Msg -> msg) -> (UserCreated -> msg) -> Globals msg -> Msg -> Model -> Return msg Model
update embed userCreatedMsg globals msg model =
    case msg of
        CloseModal ->
            singleton { model | modalVisibility = Modal.hidden }

        SubmitForm ->
            let
                submit url params =
                    Http.post
                        { url = url
                        , body =
                            formBody globals <|
                                [ ( "fstname", model.firstName )
                                , ( "sndname", model.secondName )
                                , ( "email", model.email )
                                , ( "lang", Enum.toString enumLanguage model.language )
                                ]
                                    ++ params
                        , expect = Http.expectJson (embed << GotResponse) responseDecoder
                        }
            in
            case model.config of
                CreateUserInUserGroup ugid ->
                    return model <|
                        submit ("/adminonly/companyadmin/users/" ++ ugid)
                            (if model.isUserGroupAdmin then
                                [ ( "iscompanyadmin", "" ) ]

                             else
                                []
                            )

        SetEmail email ->
            singleton { model | email = email }

        SetFirstName firstName ->
            singleton { model | firstName = firstName }

        SetSecondName secondName ->
            singleton { model | secondName = secondName }

        SetLanguage languageString ->
            singleton <|
                case findEnumValue enumLanguage languageString of
                    Err _ ->
                        model

                    Ok language ->
                        { model | language = language }

        SetIsUserGroupAdmin isAdmin ->
            singleton { model | isUserGroupAdmin = isAdmin }

        GotResponse result ->
            let
                callback =
                    perform << userCreatedMsg
            in
            case result of
                Ok r ->
                    if r.success then
                        return { model | modalVisibility = Modal.hidden } <| callback <| Just <| Ok "User created."

                    else
                        return model <| callback <| Just <| Err <| M.withDefault "Error creating a user." r.error

                Err _ ->
                    return model <| callback <| Just <| Err "Error creating a user."


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
