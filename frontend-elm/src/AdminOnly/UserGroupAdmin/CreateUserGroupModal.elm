module AdminOnly.UserGroupAdmin.CreateUserGroupModal exposing
    ( Config(..)
    , Model
    , Msg
    , init
    , showWithConfig
    , update
    , view
    )

import AdminOnly.Types.UserGroup.Cmd as Cmd exposing (UserGroupCreated, UserGroupCreatedSuccess)
import AdminOnly.UserGroupAdmin.Subscription exposing (PaymentPlan(..), enumPaymentPlan)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Debug
import EnumExtra as Enum exposing (findEnumValue)
import Html exposing (Html, div, h3, p, text)
import Html.Attributes exposing (checked, class, selected, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as D exposing (Decoder)
import Language exposing (Language, enumLanguage)
import List as L
import Maybe as M
import Result as R
import Return exposing (..)
import Utils exposing (..)


type alias Model =
    { modalVisibility : Modal.Visibility
    , config : Config
    , name : String
    , mResult : Maybe UserGroupCreatedSuccess
    }


type Config
    = CreatePaidUserGroup
    | CreateFreeUserGroup
    | CreateRootUserGroup String String
    | CreateChildUserGroup String


type Msg
    = CloseModal
    | CloseModalWithCallback
    | SubmitForm
    | GotResponse (Result Http.Error UserGroupCreated)
    | SetName String


init : Model
init =
    { modalVisibility = Modal.hidden
    , config = CreatePaidUserGroup
    , name = ""
    , mResult = Nothing
    }


showWithConfig : Config -> Model
showWithConfig config =
    let
        model =
            { init | modalVisibility = Modal.shown, config = config }
    in
    case config of
        CreateRootUserGroup _ name ->
            { model | name = name }

        _ ->
            model


update : (Msg -> msg) -> (UserGroupCreated -> msg) -> Globals msg -> Msg -> Model -> Return msg Model
update embed userGroupCreatedMsg globals msg model =
    case msg of
        CloseModal ->
            singleton { model | modalVisibility = Modal.hidden }

        CloseModalWithCallback ->
            let
                cmd =
                    case model.mResult of
                        Just r ->
                            perform <| userGroupCreatedMsg <| Ok r

                        Nothing ->
                            Cmd.none
            in
            return { model | modalVisibility = Modal.hidden } cmd

        SubmitForm ->
            let
                groupType =
                    case model.config of
                        CreatePaidUserGroup ->
                            Cmd.CreatePaidUserGroup

                        CreateFreeUserGroup ->
                            Cmd.CreateFreeUserGroup

                        CreateChildUserGroup ugid ->
                            Cmd.CreateChildUserGroup ugid

                        CreateRootUserGroup ugid _ ->
                            Cmd.CreateRootUserGroup ugid
            in
            return model <|
                Cmd.createUserGroup (embed << GotResponse) globals model.name groupType

        GotResponse result ->
            let
                callback =
                    perform << userGroupCreatedMsg
            in
            case result of
                Ok r ->
                    let
                        warnDialogOnSuccess =
                            let
                                ( mResult, cmd ) =
                                    case r of
                                        Ok rr ->
                                            ( Just rr, Cmd.none )

                                        Err _ ->
                                            ( Nothing, callback <| r )
                            in
                            return { model | mResult = mResult } cmd

                        hideDialogOnSuccess =
                            let
                                visibility =
                                    case r of
                                        Ok _ ->
                                            Modal.hidden

                                        Err _ ->
                                            model.modalVisibility
                            in
                            return { model | modalVisibility = Modal.hidden } <|
                                callback r
                    in
                    case model.config of
                        CreateFreeUserGroup ->
                            hideDialogOnSuccess

                        CreateChildUserGroup _ ->
                            hideDialogOnSuccess

                        CreatePaidUserGroup ->
                            warnDialogOnSuccess

                        CreateRootUserGroup _ _ ->
                            warnDialogOnSuccess

                Err _ ->
                    return model <| callback <| Err "HTTP request failure"

        SetName name ->
            singleton { model | name = name }


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    Html.map embed <|
        case model.mResult of
            Just groupIds ->
                groupIds.mRootUserGroupId
                    |> M.map (viewSubscriptionWarning model)
                    |> M.withDefault (h3 [] [ text "Error ..." ])

            Nothing ->
                viewCreate model


viewCreate : Model -> Html Msg
viewCreate model =
    let
        ( headingText, fieldName, description ) =
            case model.config of
                CreatePaidUserGroup ->
                    let
                        desc =
                            p []
                                [ text "This will create two user groups with root group having billable flag." ]
                    in
                    ( "Create billable company", "Company name", desc )

                CreateFreeUserGroup ->
                    ( "Create company free company", "Company name", text "" )

                CreateChildUserGroup _ ->
                    ( "Create child user group", "User group name", text "" )

                CreateRootUserGroup _ _ ->
                    let
                        desc =
                            p [] [ text "This will create new parent user group with billable flag." ]
                    in
                    ( "Upgrade to billable company", "User group name", desc )
    in
    Modal.config CloseModal
        |> Modal.large
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text headingText ]
        |> Modal.body []
            [ description
            , Form.form [ onSubmit SubmitForm ]
                [ Form.row []
                    [ Form.colLabel [ Col.sm3 ] [ text fieldName ]
                    , Form.col [ Col.sm9 ]
                        [ Input.text [ Input.attrs [ onInput SetName, value model.name ] ]
                        ]
                    ]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.secondary
                , Button.attrs [ onClick CloseModal ]
                ]
                [ text "Cancel" ]
            , Button.button
                [ Button.success
                , Button.attrs [ onClick SubmitForm ]
                ]
                [ text "Create" ]
            ]
        |> Modal.view model.modalVisibility


viewSubscriptionWarning : Model -> String -> Html Msg
viewSubscriptionWarning model ugid =
    Modal.config CloseModalWithCallback
        |> Modal.large
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Salesforce action action required" ]
        |> Modal.body []
            [ p [ class "bg-warning", class "p-1", class "rounded" ]
                [ text "Copy and paste the Billable User Group ID into Salesforce."
                ]
            , h3 [ class "text-center", class "border", class "border-dark", class "rounded", class "p-2", class "w-50", class "m-auto" ] [ text ugid ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.secondary
                , Button.attrs [ onClick CloseModalWithCallback ]
                ]
                [ text "Close" ]
            ]
        |> Modal.view model.modalVisibility
