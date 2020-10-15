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
import Maybe.Extra as M
import Return exposing (..)
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
    , resetPasswordLoading : Bool
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
      -- SEND RESET PASSWORD LINK
    | ResetPasswordClicked
    | GotResetPasswordResponse (Result Http.Error String)
      -- DISABLE TWO-FACTOR AUTHENTICATION
    | DisableTwoFAClicked
    | GotDisableTwoFAResponse (Result Http.Error String)
    | SetTwoFAMandatory Bool


tabName : String
tabName =
    "details"


init : (Msg -> msg) -> String -> Return msg Model
init embed uid =
    let
        model =
            { sUser = Loading
            , sResend = Failure
            , sDisableTwoFA = Failure
            , resetPasswordLoading = False
            , mDeleteUserModal = Nothing
            , mMoveUserModal = Nothing
            , mChangePasswordModal = Nothing
            }
    in
    return model <| Cmd.map embed <| getUserCmd uid


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


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> Return msg Model
update embed globals msg model =
    case msg of
        GotUser result ->
            case result of
                Ok user ->
                    let
                        ( deleteUserModal, deleteUserModalCmd ) =
                            DeleteUserModal.init user

                        ( moveUserModal, moveUserModalCmd ) =
                            MoveUserModal.init (embed << MoveUserModalMsg) user

                        ( changePasswordModal, changePasswordModalCmd ) =
                            ChangePasswordModal.init user
                    in
                    return
                        { model
                            | sUser = Success user
                            , mDeleteUserModal = Just deleteUserModal
                            , mMoveUserModal = Just moveUserModal
                            , mChangePasswordModal = Just changePasswordModal
                        }
                    <|
                        Cmd.batch
                            [ deleteUserModalCmd
                            , moveUserModalCmd
                            , changePasswordModalCmd
                            ]

                Err _ ->
                    singleton { model | sUser = Failure }

        -- FORM SETTERS
        SetEmail email ->
            singleton <| modifyUser (\u -> { u | email = email }) model

        SetFirstName firstName ->
            singleton <| modifyUser (\u -> { u | firstName = firstName }) model

        SetSecondName secondName ->
            singleton <| modifyUser (\u -> { u | secondName = secondName }) model

        SetPersonalNumber personalNumber ->
            singleton <| modifyUser (\u -> { u | personalNumber = personalNumber }) model

        SetPhone phone ->
            singleton <| modifyUser (\u -> { u | phone = phone }) model

        SetCompanyPosition companyPosition ->
            singleton <| modifyUser (\u -> { u | companyPosition = companyPosition }) model

        SetCallbackUrl callbackUrl ->
            singleton <| modifyUser (\u -> { u | callbackUrl = callbackUrl }) model

        SetAccountType str ->
            singleton <| handleSetEnum model enumAccountType str (\u v -> { u | accountType = v })

        SetLanguage str ->
            singleton <| handleSetEnum model enumLanguage str (\u v -> { u | language = v })

        SubmitForm ->
            case fromStatus model.sUser of
                Just user ->
                    return model <|
                        Http.post
                            { url = "/adminonly/useradmin/" ++ user.id
                            , body = formBody globals (User.formValues user)
                            , expect =
                                Http.expectJson (embed << GotSaveResponse)
                                    (D.field "changed" D.bool)
                            }

                Nothing ->
                    singleton model

        GotSaveResponse response ->
            case response of
                Err _ ->
                    return model <| globals.flashMessage <| FlashMessage.error "Request failed."

                Ok changed ->
                    let
                        flashMessage =
                            if changed then
                                FlashMessage.success "Saved"

                            else
                                FlashMessage.error "Failure. User already exists."
                    in
                    return model <| globals.flashMessage flashMessage

        -- DELETE USER
        DeleteUserClicked ->
            singleton <|
                case model.mDeleteUserModal of
                    Nothing ->
                        model

                    Just deleteUserModal ->
                        { model | mDeleteUserModal = Just <| DeleteUserModal.show deleteUserModal }

        DeleteUserModalMsg modalMsg ->
            let
                updateDeleteUserModal =
                    DeleteUserModal.update (embed << DeleteUserModalMsg) globals modalMsg

                ( newDeleteUserModal, cmd ) =
                    maybeUpdate updateDeleteUserModal model.mDeleteUserModal
            in
            return { model | mDeleteUserModal = newDeleteUserModal } cmd

        -- RESEND STATUS
        ResendInvitationClicked ->
            case fromStatus model.sUser of
                Nothing ->
                    singleton model

                Just user ->
                    case model.sResend of
                        Loading ->
                            singleton model

                        _ ->
                            return { model | sResend = Loading } <|
                                Http.post
                                    { url = "/adminonly/useradmin/sendinviteagain"
                                    , body = formBody globals [ ( "userid", user.id ) ]
                                    , expect = Http.expectString (embed << GotResendInvitationResponse)
                                    }

        GotResendInvitationResponse response ->
            case response of
                Ok str ->
                    return { model | sResend = Success str } <| globals.flashMessage <| FlashMessage.success "Invitation was sent"

                Err _ ->
                    return { model | sResend = Failure } <| globals.flashMessage <| FlashMessage.error "Resending invitation failed"

        -- MOVE USER
        MoveUserClicked ->
            model.mMoveUserModal
                |> M.map
                    (\modal -> { model | mMoveUserModal = Just <| MoveUserModal.show modal })
                |> M.withDefault model
                |> singleton

        MoveUserModalMsg modalMsg ->
            let
                updateMoveUserModal =
                    MoveUserModal.update (embed << MoveUserModalMsg) globals modalMsg

                ( newMoveUserModal, cmd ) =
                    maybeUpdate updateMoveUserModal model.mMoveUserModal
            in
            return { model | mMoveUserModal = newMoveUserModal } cmd

        -- CHANGE PASSWORD
        ChangePasswordClicked ->
            model.mChangePasswordModal
                |> M.map (\modal -> { model | mChangePasswordModal = Just <| ChangePasswordModal.show modal })
                |> M.withDefault model
                |> singleton

        ChangePasswordModalMsg modalMsg ->
            let
                updateChangePasswordModal =
                    ChangePasswordModal.update (embed << ChangePasswordModalMsg) globals modalMsg

                ( newChangePasswordModal, cmd ) =
                    maybeUpdate updateChangePasswordModal model.mChangePasswordModal
            in
            return { model | mChangePasswordModal = newChangePasswordModal } cmd

        -- SEND RESET PASSWORD LINK
        ResetPasswordClicked ->
            case ( fromStatus model.sUser, model.resetPasswordLoading ) of
                ( Just user, False ) ->
                    return { model | resetPasswordLoading = True } <|
                        Http.post
                            { url = "/api/frontend/sendpasswordresetmail"
                            , body = formBody globals [ ( "email", user.email ) ]
                            , expect = Http.expectString (embed << GotResetPasswordResponse)
                            }

                _ ->
                    singleton model

        GotResetPasswordResponse response ->
            return { model | resetPasswordLoading = False } <|
                case ( fromStatus model.sUser, response ) of
                    ( Just user, Ok _ ) ->
                        globals.flashMessage <|
                            FlashMessage.success "A password reset link was sent to the user's email address."

                    ( Nothing, Ok _ ) ->
                        Cmd.none

                    ( _, Err _ ) ->
                        globals.flashMessage <|
                            FlashMessage.error "Error sending the password reset link."

        -- DISABLE TWO-FACTOR AUTHENTICATION
        DisableTwoFAClicked ->
            case fromStatus model.sUser of
                Nothing ->
                    singleton model

                Just user ->
                    case model.sDisableTwoFA of
                        Loading ->
                            singleton model

                        _ ->
                            return { model | sDisableTwoFA = Loading } <|
                                Http.post
                                    { url = "/adminonly/useradmin/disable2fa/" ++ user.id
                                    , body = formBody globals []
                                    , expect = Http.expectString (embed << GotDisableTwoFAResponse)
                                    }

        GotDisableTwoFAResponse response ->
            case ( fromStatus model.sUser, response ) of
                ( Just user, Ok str ) ->
                    return { model | sDisableTwoFA = Success str } <|
                        Cmd.batch
                            [ Cmd.map embed <| getUserCmd user.id
                            , globals.flashMessage <| FlashMessage.success "Two-factor authentication was disabled."
                            ]

                ( Nothing, Ok _ ) ->
                    singleton model

                ( _, Err _ ) ->
                    return { model | sDisableTwoFA = Failure } <|
                        globals.flashMessage <|
                            FlashMessage.error "Error disabling two-factor authentication."

        SetTwoFAMandatory newMandatory ->
            singleton <|
                modifyUser (\u -> { u | twoFAMandatory = newMandatory }) model


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    case model.sUser of
        Loading ->
            h4 [] [ text "Loading ..." ]

        Failure ->
            h4 [] [ text "Failure ..." ]

        Success user ->
            div [] <|
                [ Html.map embed <| viewUser user ]
                    ++ M.values
                        [ Maybe.map (DeleteUserModal.view <| embed << DeleteUserModalMsg) model.mDeleteUserModal
                        , Maybe.map (MoveUserModal.view <| embed << MoveUserModalMsg) model.mMoveUserModal
                        , Maybe.map (ChangePasswordModal.view <| embed << ChangePasswordModalMsg) model.mChangePasswordModal
                        ]


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
                        [ text "Not active" ]
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
                        [ text <| "Link to company " ++ user.userGroup.entityName ]
                    ]
                ]
            , formSelectRowM enumAccountType "Account type" user.accountType SetAccountType
            , formTextRowM "Callback URL" user.callbackUrl SetCallbackUrl [ readonly <| not user.callbackUrlIsEditable ]
            ]
        , div [ class "d-inline-block" ]
            [ Grid.row [ Row.leftSm, Row.attrs [ class "mb-sm-2" ] ]
                [ Grid.col [ Col.sm12 ]
                    [ Button.button
                        [ Button.primary
                        , Button.attrs [ onClick ResendInvitationClicked ]
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
                        [ Button.primary
                        , Button.attrs [ class "ml-sm-2", onClick ResetPasswordClicked ]
                        ]
                        [ text "Send password reset link" ]
                    ]
                ]
            , Grid.row [ Row.leftSm ]
                [ Grid.col [ Col.sm12 ]
                    [ Button.button
                        [ Button.danger
                        , Button.attrs [ onClick DeleteUserClicked ]
                        ]
                        [ text "Delete user" ]
                    , Button.button
                        [ Button.success
                        , Button.attrs [ class "ml-sm-2 float-right", onClick SubmitForm ]
                        ]
                        [ text "Save" ]
                    ]
                ]
            ]
        ]
