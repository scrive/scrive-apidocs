module AdminOnly.UserAdmin.DetailsTab.DetailsTab exposing
    ( Model
    , Msg
    , getUserCmd
    , init
    , tabName
    , update
    , view
    )

import AdminOnly.UserAdmin.DetailsTab.ChangePasswordModal as ChangePasswordModal
import AdminOnly.UserAdmin.DetailsTab.DeleteUserModal as DeleteUserModal
import AdminOnly.UserAdmin.DetailsTab.MoveUserModal as MoveUserModal
import AdminOnly.UserAdmin.DetailsTab.User as User exposing (User, enumAccountType)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Spacing as Spacing
import Either exposing (Either(..))
import EnumExtra as Enum exposing (Enum, findEnumValue)
import FlashMessage
import Html exposing (Html, a, div, h4, text)
import Html.Attributes exposing (class, href, readonly, selected, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Language exposing (enumLanguage)
import List as L
import Maybe as M
import Monocle.Optional exposing (Optional)
import Time exposing (Month(..))
import Url.Parser exposing (map)
import Utils exposing (..)


type alias Model =
    { sUser : Status User
    , mDeleteUserModal : Maybe DeleteUserModal.Model
    , mMoveUserModal : Maybe MoveUserModal.Model
    , mChangePasswordModal : Maybe ChangePasswordModal.Model
    , sResend : Status String
    , sDisableTwoFA : Status String
    }


type Msg
    = GotUser (Result Http.Error User)
      -- EDIT USER
    | SetFirstName String
    | SetSecondName String
    | SetEmail String
    | SetAccountType String
    | SetLanguage String
    | SetPersonalNumber String
    | SetPhone String
    | SetCompanyPosition String
    | SetCallbackUrl String
    | SubmitForm
    | GotSaveResponse (Result Http.Error Bool)
      -- DELETE USER
    | DeleteUserModalMsg DeleteUserModal.Msg
    | DeleteUserClicked
      -- RESEND INVITATION
    | ResendInvitationClicked
    | GotResendInvitationResponse (Result Http.Error String)
      -- MOVE USER
    | MoveUserModalMsg MoveUserModal.Msg
    | MoveUserClicked
      -- CHANGE PASSWORD
    | ChangePasswordClicked
    | ChangePasswordModalMsg ChangePasswordModal.Msg
      -- DISABLE TWO-FACTOR AUTHENTICATION
    | DisableTwoFAClicked
    | GotDisableTwoFAResponse (Result Http.Error String)
    | SetTwoFAMandatory Bool


deleteUserModelLens : Optional Model DeleteUserModal.Model
deleteUserModelLens =
    Optional .mDeleteUserModal
        (\b a -> { a | mDeleteUserModal = Just b })


moveUserModelLens : Optional Model MoveUserModal.Model
moveUserModelLens =
    Optional .mMoveUserModal
        (\b a -> { a | mMoveUserModal = Just b })


changePasswordModelLens : Optional Model ChangePasswordModal.Model
changePasswordModelLens =
    Optional .mChangePasswordModal
        (\b a -> { a | mChangePasswordModal = Just b })


tabName : String
tabName =
    "details"


init : String -> ( Model, Cmd Msg )
init uid =
    let
        model =
            { sUser = Loading
            , sResend = Failure
            , sDisableTwoFA = Failure
            , mDeleteUserModal = Nothing
            , mMoveUserModal = Nothing
            , mChangePasswordModal = Nothing
            }
    in
    ( model, getUserCmd uid )


getUserCmd : String -> Cmd Msg
getUserCmd uid =
    Http.get
        { url = "/adminonly/useradmin/details/" ++ uid
        , expect = Http.expectJson GotUser User.decoder
        }


modifyUser : (User -> User) -> Model -> Model
modifyUser modify model =
    { model | sUser = statusMap modify model.sUser }


handleSetEnum : Model -> Enum a -> String -> (User -> a -> User) -> Model
handleSetEnum model enum str setter =
    case findEnumValue enum str of
        Err _ ->
            model

        Ok value ->
            modifyUser (\u -> setter u value) model


update : Globals msg -> Msg -> Model -> ( Model, Action msg Msg )
update globals msg model =
    let
        deleteUser =
            liftOptionalUpdateHandler
                deleteUserModelLens
                DeleteUserModalMsg
            <|
                DeleteUserModal.update globals

        moveUser =
            liftOptionalUpdateHandler
                moveUserModelLens
                MoveUserModalMsg
            <|
                MoveUserModal.update globals

        changePassword =
            liftOptionalUpdateHandler
                changePasswordModelLens
                ChangePasswordModalMsg
            <|
                ChangePasswordModal.update globals
    in
    case msg of
        GotUser result ->
            case result of
                Ok user ->
                    let
                        ( deleteUserModal, deleteUserModalCmd ) =
                            DeleteUserModal.init user

                        ( moveUserModal, moveUserModalCmd ) =
                            MoveUserModal.init user

                        ( changePasswordModal, changePasswordModalCmd ) =
                            ChangePasswordModal.init user
                    in
                    ( { model
                        | sUser = Success user
                        , mDeleteUserModal = Just deleteUserModal
                        , mMoveUserModal = Just moveUserModal
                        , mChangePasswordModal = Just changePasswordModal
                      }
                    , Cmd.batch
                        [ innerLiftCmd DeleteUserModalMsg deleteUserModalCmd
                        , innerLiftCmd MoveUserModalMsg moveUserModalCmd
                        , innerLiftCmd ChangePasswordModalMsg changePasswordModalCmd
                        ]
                    )

                Err _ ->
                    ( { model | sUser = Failure }, Cmd.none )

        -- FORM SETTERS
        SetEmail email ->
            ( modifyUser (\u -> { u | email = email }) model, Cmd.none )

        SetFirstName firstName ->
            ( modifyUser (\u -> { u | firstName = firstName }) model, Cmd.none )

        SetSecondName secondName ->
            ( modifyUser (\u -> { u | secondName = secondName }) model, Cmd.none )

        SetPersonalNumber personalNumber ->
            ( modifyUser (\u -> { u | personalNumber = personalNumber }) model, Cmd.none )

        SetPhone phone ->
            ( modifyUser (\u -> { u | phone = phone }) model, Cmd.none )

        SetCompanyPosition companyPosition ->
            ( modifyUser (\u -> { u | companyPosition = companyPosition }) model, Cmd.none )

        SetCallbackUrl callbackUrl ->
            ( modifyUser (\u -> { u | callbackUrl = callbackUrl }) model, Cmd.none )

        SetAccountType str ->
            ( handleSetEnum model enumAccountType str (\u v -> { u | accountType = v })
            , Cmd.none
            )

        SetLanguage str ->
            ( handleSetEnum model enumLanguage str (\u v -> { u | language = v })
            , Cmd.none
            )

        SubmitForm ->
            case fromStatus model.sUser of
                Just user ->
                    ( model
                    , innerCmd <|
                        Http.post
                            { url = "/adminonly/useradmin/" ++ user.id
                            , body = formBody globals (User.formValues user)
                            , expect =
                                Http.expectJson GotSaveResponse
                                    (D.field "changed" D.bool)
                            }
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotSaveResponse response ->
            case response of
                Err _ ->
                    ( model
                    , outerCmd <| globals.flashMessage <| FlashMessage.error "Request failed."
                    )

                Ok changed ->
                    let
                        flashMessage =
                            if changed then
                                FlashMessage.success "Saved"

                            else
                                FlashMessage.error "Failure. User already exists."
                    in
                    ( model
                    , outerCmd <| globals.flashMessage flashMessage
                    )

        -- DELETE USER
        DeleteUserClicked ->
            case model.mDeleteUserModal of
                Nothing ->
                    ( model, Cmd.none )

                Just deleteUserModal ->
                    ( { model | mDeleteUserModal = Just <| DeleteUserModal.show deleteUserModal }
                    , Cmd.none
                    )

        DeleteUserModalMsg modalMsg ->
            deleteUser modalMsg model

        -- RESEND STATUS
        ResendInvitationClicked ->
            case fromStatus model.sUser of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    case model.sResend of
                        Loading ->
                            ( model, Cmd.none )

                        _ ->
                            ( { model | sResend = Loading }
                            , innerCmd <|
                                Http.post
                                    { url = "/adminonly/useradmin/sendinviteagain"
                                    , body = formBody globals [ ( "userid", user.id ) ]
                                    , expect = Http.expectString GotResendInvitationResponse
                                    }
                            )

        GotResendInvitationResponse response ->
            case response of
                Ok str ->
                    ( { model | sResend = Success str }
                    , outerCmd <|
                        globals.flashMessage <|
                            FlashMessage.success "Invitation was sent"
                    )

                Err _ ->
                    ( { model | sResend = Failure }
                    , outerCmd <|
                        globals.flashMessage <|
                            FlashMessage.error "Resending invitation failed"
                    )

        -- MOVE USER
        MoveUserClicked ->
            model.mMoveUserModal
                |> M.map
                    (\modal -> ( { model | mMoveUserModal = Just <| MoveUserModal.show modal }, Cmd.none ))
                |> M.withDefault ( model, Cmd.none )

        MoveUserModalMsg modalMsg ->
            moveUser modalMsg model

        -- CHANGE PASSWORD
        ChangePasswordClicked ->
            model.mChangePasswordModal
                |> M.map
                    (\modal ->
                        ( { model | mChangePasswordModal = Just <| ChangePasswordModal.show modal }
                        , Cmd.none
                        )
                    )
                |> M.withDefault ( model, Cmd.none )

        ChangePasswordModalMsg modalMsg ->
            changePassword modalMsg model

        -- DISABLE TWO-FACTOR AUTHENTICATION
        DisableTwoFAClicked ->
            case fromStatus model.sUser of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    case model.sDisableTwoFA of
                        Loading ->
                            ( model, Cmd.none )

                        _ ->
                            ( { model | sDisableTwoFA = Loading }
                            , innerCmd <|
                                Http.post
                                    { url = "/adminonly/useradmin/disable2fa/" ++ user.id
                                    , body = formBody globals []
                                    , expect = Http.expectString GotDisableTwoFAResponse
                                    }
                            )

        GotDisableTwoFAResponse response ->
            case (fromStatus model.sUser, response) of
                (Just user, Ok str) ->
                    ( { model | sDisableTwoFA = Success str }
                    , Cmd.batch
                        [ innerCmd <| getUserCmd user.id
                        , outerCmd <|
                            globals.flashMessage <|
                                FlashMessage.success "Two-factor authentication was disabled."
                        ]
                    )

                (Nothing, Ok _) -> ( model, Cmd.none )

                (_, Err _) ->
                    ( { model | sDisableTwoFA = Failure }
                    , outerCmd <|
                        globals.flashMessage <|
                            FlashMessage.error "Error disabling two-factor authentication."
                    )

        SetTwoFAMandatory newMandatory ->
            ( modifyUser (\u -> { u | twoFAMandatory = newMandatory }) model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        deleteUser =
            liftOptionalViewHandler
                deleteUserModelLens
                DeleteUserModalMsg
                DeleteUserModal.view

        moveUser =
            liftOptionalViewHandler
                moveUserModelLens
                MoveUserModalMsg
                MoveUserModal.view

        changePassword =
            liftOptionalViewHandler
                changePasswordModelLens
                ChangePasswordModalMsg
                ChangePasswordModal.view
    in
    case model.sUser of
        Loading ->
            h4 [] [ text "Loading ..." ]

        Failure ->
            h4 [] [ text "Failure ..." ]

        Success user ->
            div [] <|
                [ viewUser user ]
                    ++ (L.filterMap identity <|
                            [ deleteUser model
                            , moveUser model
                            , changePassword model
                            ]
                       )


formTextRow :
    List (Col.Option Msg)
    -> List (Col.Option Msg)
    -> String
    -> String
    -> (String -> Msg)
    -> List (Html.Attribute Msg)
    -> Html Msg
formTextRow labelColAttrs inputColAttrs label val mkMsg attrs =
    Form.row
        []
        [ Form.colLabel labelColAttrs [ text label ]
        , Form.col inputColAttrs
            [ Input.text
                [ Input.attrs <|
                    [ value <| val
                    , onInput <| mkMsg
                    ]
                        ++ attrs
                ]
            ]
        ]


formSelectRow :
    List (Col.Option Msg)
    -> List (Col.Option Msg)
    -> Enum a
    -> String
    -> a
    -> (String -> Msg)
    -> Html Msg
formSelectRow labelColAttrs inputColAttrs enum label selectedVal mkMsg =
    Form.row []
        [ Form.colLabel labelColAttrs [ text label ]
        , Form.col inputColAttrs
            [ Select.select [ Select.onChange <| mkMsg ] <|
                L.map
                    (\optionVal ->
                        Select.item
                            [ value <| Enum.toString enum optionVal
                            , selected <| optionVal == selectedVal
                            ]
                            [ text <| Enum.toHumanString enum optionVal ]
                    )
                <|
                    Enum.allValues enum
            ]
        ]


viewUser : User -> Html Msg
viewUser user =
    let
        labelColAttrs =
            [ Col.sm5, Col.md4, Col.lg3 ]

        inputColAttrs =
            [ Col.sm7, Col.md6, Col.lg5 ]

        formTextRowM =
            formTextRow labelColAttrs inputColAttrs

        formSelectRowM =
            formSelectRow labelColAttrs inputColAttrs
    in
    Grid.container []
        [ Form.form [ onSubmit SubmitForm ]
            [ Form.row []
                [ Form.colLabel labelColAttrs [ text "User ID" ]
                , Form.col inputColAttrs
                    [ Input.text [ Input.attrs [ readonly True, value user.id ] ] ]
                ]
            , Form.row []
                [ Form.colLabel labelColAttrs [ text "Two-factor authentication" ]
                , Form.colLabel inputColAttrs <|
                    ite user.twoFAActive
                        [ text "Active"
                        , a [ onClick DisableTwoFAClicked, href <| "#", Spacing.ml2 ] [ text "Disable" ]
                        ]
                        [ text "Not active"]
                ]
            , Form.row []
                [ Form.colLabel labelColAttrs [ text "Two-factor authentication is mandatory" ]
                , Form.colLabel inputColAttrs
                    [ Checkbox.checkbox
                        [ Checkbox.checked user.twoFAMandatory, Checkbox.onCheck SetTwoFAMandatory ]
                        "2FA can also be enforced for all users in the company."
                    ]
                ]
            , formTextRowM "First name" user.firstName SetFirstName []
            , formTextRowM "Second name" user.secondName SetSecondName []
            , formTextRowM "Personal number" user.personalNumber SetPersonalNumber []
            , formTextRowM "Email" user.email SetEmail []
            , formTextRowM "Phone" user.phone SetPhone []
            , formTextRowM "Company position" user.companyPosition SetCompanyPosition []
            , formSelectRowM enumLanguage "Language" user.language SetLanguage
            , Form.row []
                [ Form.colLabel labelColAttrs [ text "Company" ]
                , Form.colLabel inputColAttrs
                    [ a [ href <| "/adminonly/page/companyadmin/" ++ user.userGroup.id ]
                        [ text "Link to company" ]
                    ]
                ]
            , formSelectRowM enumAccountType "Account type" user.accountType SetAccountType
            , formTextRowM "Callback URL" user.callbackUrl SetCallbackUrl [ readonly <| not user.callbackUrlIsEditable ]
            ]
        , Grid.row [ Row.leftSm ]
            [ Grid.col [ Col.sm12 ]
                [ Button.button
                    [ Button.danger
                    , Button.attrs [ onClick DeleteUserClicked ]
                    ]
                    [ text "Delete user" ]
                , Button.button
                    [ Button.primary
                    , Button.attrs [ class "ml-sm-2", onClick ResendInvitationClicked ]
                    ]
                    [ text "Resend invitation" ]
                , Button.button
                    [ Button.primary
                    , Button.attrs [ class "ml-sm-2", onClick MoveUserClicked ]
                    ]
                    [ text "Move to different company" ]
                , Button.button
                    [ Button.primary
                    , Button.attrs [ class "ml-sm-2", onClick ChangePasswordClicked ]
                    ]
                    [ text "Change password" ]
                , Button.button
                    [ Button.success
                    , Button.attrs [ class "ml-sm-2", onClick SubmitForm ]
                    ]
                    [ text "Save" ]
                ]
            ]
        ]
