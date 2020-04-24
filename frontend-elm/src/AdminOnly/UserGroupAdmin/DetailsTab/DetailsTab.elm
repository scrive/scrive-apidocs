module AdminOnly.UserGroupAdmin.DetailsTab.DetailsTab exposing
    ( Model
    , Msg
    , init
    , setUserGroupID
    , tabName
    , update
    , view
    )

import AdminOnly.UserAdmin.DetailsTab.UserGroup as UserGroup exposing (Address, Settings, UserGroup)
import AdminOnly.UserGroupAdmin.DetailsTab.MergeUserGroupModal as MergeUserGroupModal
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import EnumExtra as Enum exposing (Enum)
import FlashMessage
import Html exposing (Html, a, div, h4, hr, text)
import Html.Attributes exposing (checked, class, disabled, href, readonly, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import List as L
import Maybe as M
import Maybe.Extra as M
import Time exposing (Month(..))
import Url.Parser exposing (map)
import Utils exposing (..)


type alias Model =
    { sUserGroup : Status UserGroup
    , origParentID : Maybe String
    , sNewParentName : Status String
    , mMergeUserGroupModal : Maybe MergeUserGroupModal.Model
    }


type Msg
    = GotUserGroup (Result Http.Error UserGroup)
      -- EDIT USER GROUP
    | SetStringField String String
    | SetBoolField String Bool
    | SetIntField String (Maybe Range) String
    | SetAddressIsInherited Bool
    | SetSettingsIsInherited Bool
    | SetParentID String
    | SetSfAccountID String
    | GotParentUserGroup (Result Http.Error UserGroup)
    | SubmitForm
    | GotSaveResponse (Result Http.Error String)
      -- MERGE TO DIFFERENT USER GROUP
    | MergeUserGroupModalMsg MergeUserGroupModal.Msg
    | MergeUserGroupClicked


type alias Range =
    { min : Int
    , max : Int
    }


tabName : String
tabName =
    "details"


init : (Msg -> msg) -> String -> ( Model, Cmd msg )
init embed ugid =
    let
        model =
            { sUserGroup = Loading
            , origParentID = Nothing
            , sNewParentName = Failure
            , mMergeUserGroupModal = Nothing
            }
    in
    ( model, Cmd.map embed <| getUserGroupCmd ugid model )


getUserGroupCmd : String -> Model -> Cmd Msg
getUserGroupCmd ugid _ =
    Http.get
        { url = "/adminonly/companyadmin/details/" ++ ugid
        , expect = Http.expectJson GotUserGroup UserGroup.decoder
        }


getParentUserGroupCmd : String -> Model -> Cmd Msg
getParentUserGroupCmd ugid _ =
    Http.get
        { url = "/adminonly/companyadmin/details/" ++ ugid
        , expect = Http.expectJson GotParentUserGroup UserGroup.decoder
        }


setUserGroupID : (Msg -> msg) -> String -> Model -> Cmd msg
setUserGroupID embed ugid =
    Cmd.map embed << getUserGroupCmd ugid


modifyUserGroup : (UserGroup -> UserGroup) -> Model -> Model
modifyUserGroup modify model =
    { model | sUserGroup = statusMap modify model.sUserGroup }


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> ( Model, Cmd msg )
update embed globals msg model =
    case msg of
        GotUserGroup result ->
            case result of
                Ok userGroup ->
                    let
                        ( mergeModal, mergeModalCmd ) =
                            MergeUserGroupModal.init (embed << MergeUserGroupModalMsg) userGroup
                    in
                    ( { model
                        | sUserGroup = Success userGroup
                        , origParentID = userGroup.parentID
                        , sNewParentName = Loading
                        , mMergeUserGroupModal = Just mergeModal
                      }
                    , mergeModalCmd
                    )

                Err _ ->
                    ( { model | sUserGroup = Failure }, Cmd.none )

        -- FORM SETTERS
        SetStringField name value ->
            ( modifyUserGroup (UserGroup.setStringField name value) model, Cmd.none )

        SetBoolField name value ->
            ( modifyUserGroup (UserGroup.setBoolField name value) model, Cmd.none )

        SetIntField name mRange value ->
            let
                mClamp = case mRange of
                    Just range -> clamp range.min range.max
                    Nothing -> identity

                mInt =
                    stringNonEmpty value
                        |> M.andThen String.toInt
                        |> M.map mClamp
            in
            ( modifyUserGroup (UserGroup.setIntField name mInt) model, Cmd.none )

        SetAddressIsInherited value ->
            case ( value, statusMap .inheritedAddress model.sUserGroup ) of
                ( True, Success Nothing ) ->
                    ( model, globals.flashMessage <| FlashMessage.error "Top level user group cannot inherit" )

                _ ->
                    ( modifyUserGroup (UserGroup.setBoolField "addressIsInherited" value) model, Cmd.none )

        SetSettingsIsInherited value ->
            case ( value, statusMap .inheritedSettings model.sUserGroup ) of
                ( True, Success Nothing ) ->
                    ( model, globals.flashMessage <| FlashMessage.error "Top level user group cannot inherit" )

                _ ->
                    ( modifyUserGroup (UserGroup.setBoolField "settingsIsInherited" value) model, Cmd.none )

        SetParentID newParentID ->
            let
                model1 =
                    { model
                        | sUserGroup = statusMap (\ug -> { ug | parentID = stringNonEmpty newParentID }) model.sUserGroup
                    }
            in
            if isInteger newParentID then
                let
                    model2 =
                        { model1 | sNewParentName = Loading }
                in
                ( model2, Cmd.map embed <| getParentUserGroupCmd newParentID model2 )

            else
                ( model1, Cmd.none )

        SetSfAccountID newSfAccountID ->
            let
                model1 =
                    { model
                        | sUserGroup = statusMap (UserGroup.setInternalTag "sf-account-id" <| stringNonEmpty newSfAccountID) model.sUserGroup
                    }
            in
            ( model1, Cmd.none )

        GotParentUserGroup response ->
            case response of
                Err _ ->
                    ( { model | sNewParentName = Failure }, Cmd.none )

                Ok userGroup ->
                    ( { model | sNewParentName = Success userGroup.name }, Cmd.none )

        SubmitForm ->
            case fromStatus model.sUserGroup of
                Just userGroup ->
                    ( model
                    , Http.post
                            { url = "/adminonly/companyadmin/" ++ userGroup.id
                            , body = formBody globals <| UserGroup.formValues userGroup
                            , expect = Http.expectString <| embed << GotSaveResponse
                            }
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotSaveResponse response ->
            case response of
                Err _ ->
                    ( model, globals.flashMessage <| FlashMessage.error "Request failed." )

                Ok _ ->
                    ( { model | sUserGroup = statusMap UserGroup.afterSaved model.sUserGroup }
                    , Cmd.batch
                            [ globals.flashMessage <| FlashMessage.success "Saved"
                            , globals.setPageUrlFromModel -- reloads UserGroup Details
                            ]
                    )

        -- MOVE USER
        MergeUserGroupClicked ->
            model.mMergeUserGroupModal
                |> M.map
                    (\modal -> ( { model | mMergeUserGroupModal = Just <| MergeUserGroupModal.show modal }, Cmd.none ))
                |> M.withDefault ( model, Cmd.none )

        MergeUserGroupModalMsg modalMsg ->
            let updateMergeUserGroupModal = MergeUserGroupModal.update (embed << MergeUserGroupModalMsg) globals modalMsg
                (newMergeUserGroupModal, cmd) = maybeUpdate updateMergeUserGroupModal model.mMergeUserGroupModal
            in ({ model | mMergeUserGroupModal = newMergeUserGroupModal}, cmd)


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    let
        mAddressSettings ug =
            ( ite ug.addressIsInherited ug.inheritedAddress (Just ug.address)
            , ite ug.settingsIsInherited ug.inheritedSettings (Just ug.settings)
            )
    in
    case model.sUserGroup of
        Loading ->
            h4 [] [ text "Loading ..." ]

        Failure ->
            h4 [] [ text "Failure ..." ]

        Success ug ->
            div [] <|
                [ case mAddressSettings ug of
                    ( Just address, Just settings ) ->
                        Html.map embed <| viewUserGroup model ug address settings

                    _ ->
                        text "Inheritance inconsistency. Report to bugs@scrive.com."
                ]
                    ++ M.values
                            [ M.map
                                (MergeUserGroupModal.view <| embed << MergeUserGroupModalMsg)
                                model.mMergeUserGroupModal
                            ]



formText :
    String
    -> (String -> Msg)
    -> List (Html.Attribute Msg)
    -> List (Html Msg)
formText val mkMsg attrs =
    [ Input.text
        [ Input.attrs <|
            [ value <| val
            , onInput mkMsg
            ]
                ++ attrs
        ]
    ]


formCheckbox :
    Bool
    -> (Bool -> Msg)
    -> List (Html.Attribute Msg)
    -> List (Html Msg)
formCheckbox val mkMsg attrs =
    [ Checkbox.checkbox
        [ Checkbox.attrs <|
            [ checked val
            , onCheck mkMsg
            ]
                ++ attrs
        ]
        ""
    ]


formRow :
    List (Col.Option msg)
    -> List (Col.Option msg)
    -> String
    -> String
    -> List (Html msg)
    -> Html msg
formRow labelColAttrs inputColAttrs label help html =
    Form.row
        []
        [ Form.colLabel labelColAttrs [ text label ]
        , Form.col inputColAttrs html
        , Form.col [] [ Form.helpInline [] [ text help ] ]
        ]


formSelect :
    Enum a
    -> a
    -> (String -> Msg)
    -> List (Html.Attribute Msg)
    -> List (Html Msg)
formSelect enum selectedVal mkMsg attrs =
    [ Select.select [ Select.onChange mkMsg ] <|
        (L.map <|
            \optionVal ->
                Select.item
                    ([ value <| Enum.toString enum optionVal
                     , selected <| optionVal == selectedVal
                     ]
                        ++ attrs
                    )
                    [ text <| Enum.toHumanString enum optionVal ]
        )
        <|
            Enum.allValues enum
    ]


viewUserGroup : Model -> UserGroup -> Address -> Settings -> Html Msg
viewUserGroup model ug address settings =
    let
        labelColAttrs =
            [ Col.sm4, Col.md3, Col.lg3 ]

        inputColAttrs =
            [ Col.sm4, Col.md4, Col.lg3 ]

        formTextRowM label help val toMsg attrs =
            formRow labelColAttrs inputColAttrs label help <| formText val toMsg attrs

        formIntRowM label help val toMsg attrs =
            formRow labelColAttrs inputColAttrs label help <| formText val toMsg (type_ "number" :: attrs)

        formSelectRowM label help enum val toMsg attrs =
            formRow labelColAttrs inputColAttrs label help <| formSelect enum val toMsg attrs

        formCheckboxRowM label help val toMsg attrs =
            formRow labelColAttrs inputColAttrs label help <| formCheckbox val toMsg attrs

        ( newParentIDIsValid, newParentIDInputValidity, newParentIDFeedback ) =
            if model.origParentID == ug.parentID then
                ( True, [], [] )

            else
                case M.map isInteger ug.parentID of
                    Nothing ->
                        ( True, [ Input.success ], [ Form.validFeedback [] [ text "No parent company." ] ] )

                    Just False ->
                        ( False, [ Input.danger ], [ Form.invalidFeedback [] [ text "Company ID can only contain numbers." ] ] )

                    Just True ->
                        case model.sNewParentName of
                            Loading ->
                                ( False, [], [ Form.help [] [ text "Loading company ..." ] ] )

                            Failure ->
                                ( False, [ Input.danger ], [ Form.invalidFeedback [] [ text "Company ID does not exist." ] ] )

                            Success userGroupName ->
                                ( True, [ Input.success ], [ Form.validFeedback [] [ text <| "Company with name: " ++ userGroupName ] ] )

        docTimeoutRange = Just (Range 1 365)
    in
    Grid.container []
        [ Form.form [ onSubmit SubmitForm ]
            [ Form.row []
                [ Form.colLabel labelColAttrs [ text "User group ID" ]
                , Form.col inputColAttrs
                    [ Input.text [ Input.attrs [ readonly True, value ug.id ] ] ]
                ]
            , formTextRowM "User group name"
                "This is the internal-use User Group name (for example \"HR\")"
                ug.name
                (SetStringField "name")
                []
            , Form.row
                []
                [ Form.colLabel labelColAttrs [ text "Parent user group ID" ]
                , Form.col inputColAttrs <|
                    [ Input.text <|
                        [ Input.attrs <|
                            [ value <| M.withDefault "" ug.parentID
                            , onInput SetParentID
                            ]
                        ]
                            ++ newParentIDInputValidity
                    ]
                        ++ newParentIDFeedback
                , Form.col [] [ Form.helpInline [] [ text "Leave is empty for Scrive. SF is 9197237133460633368. Only Partner Manager is allowed to make changes in this field." ] ]
                ]
            , Form.row []
                [ Form.colLabel labelColAttrs [ text "Parent group path" ]
                , Form.colLabel inputColAttrs
                    (ug.parentGroupPath
                        |> L.map
                            (\parent -> a [ href parent.id ] [ text <| parent.name ++ " (" ++ parent.id ++ ")" ])
                        |> L.intersperse (text " > ")
                    )
                ]
            , hr [] []
            , Form.row
                []
                [ Form.colLabel labelColAttrs [ text "SF Account ID" ]
                , Form.col inputColAttrs <|
                    [ Input.text <|
                        [ Input.attrs <|
                            [ value <| M.withDefault "" <| UserGroup.getInternalTag "sf-account-id" ug
                            , onInput SetSfAccountID
                            ]
                        ]
                    ]
                , Form.col [] [ Form.helpInline [] [ text "The purpose of this field is to match User Groups with Salesforce Accounts." ] ]
                ]
            , hr [] []
            , formCheckboxRowM "Inherit address"
                "If enabled, all address fields will be inherited from the parent user group."
                ug.addressIsInherited
                SetAddressIsInherited
                []
            , formTextRowM "Company name (Entity)"
                "\"Company\" field in documents"
                ug.entityName
                (SetStringField "entityName")
                []
            , formTextRowM "Company reg. no." "" address.companyNumber (SetStringField "companyNumber") [ readonly ug.addressIsInherited ]
            , formTextRowM "Address" "" address.address (SetStringField "address") [ readonly ug.addressIsInherited ]
            , formTextRowM "Zip" "" address.zip (SetStringField "zip") [ readonly ug.addressIsInherited ]
            , formTextRowM "City" "" address.city (SetStringField "city") [ readonly ug.addressIsInherited ]
            , formTextRowM "Country" "" address.country (SetStringField "country") [ readonly ug.addressIsInherited ]
            , hr [] []
            , formCheckboxRowM "Inherit settings"
                "If enabled, all settings will be inherited from the parent user group."
                ug.settingsIsInherited
                SetSettingsIsInherited
                []
            , formTextRowM "IP address mask"
                ""
                settings.ipAddressMaskList
                (SetStringField "ipAddressMaskList")
                [ readonly ug.settingsIsInherited ]
            , formTextRowM "CGI display name (BankID only)"
                "This has to be accepted by CGI. Otherwise, BankID will not work."
                (M.withDefault "" settings.cgiDisplayName)
                (SetStringField "cgiDisplayName")
                [ readonly ug.settingsIsInherited ]
            , formTextRowM "CGI service ID (BankID only)"
                "This has to be accepted by CGI. Otherwise, BankID will not work."
                (M.withDefault "" settings.cgiServiceID)
                (SetStringField "cgiServiceID")
                [ readonly ug.settingsIsInherited ]
            , formSelectRowM "SMS Provider"
                ""
                UserGroup.enumSmsProvider
                settings.smsProvider
                (SetStringField "smsProvider")
                [ disabled ug.settingsIsInherited ]
            , formSelectRowM "Pad application mode"
                ""
                UserGroup.enumPadAppMode
                settings.padAppMode
                (SetStringField "padAppMode")
                [ disabled ug.settingsIsInherited ]
            , formCheckboxRowM "Enable E-archive in Pad application"
                "Enable using E-archive in Pad applications."
                settings.padEarchiveEnabled
                (SetBoolField "padEarchiveEnabled")
                [ readonly ug.settingsIsInherited ]
            , formCheckboxRowM "Force hiding personal numbers in all documents"
                "Force hiding personal numbers in all documents."
                settings.forceHidePN
                (SetBoolField "forceHidePN")
                [ readonly ug.settingsIsInherited ]
            , formCheckboxRowM "Immediate trash"
                "If enabled, documents in trash will be deleted as soon as possible instead of waiting for 30 days."
                settings.immediateTrash
                (SetBoolField "immediateTrash")
                [ readonly ug.settingsIsInherited ]
            , Form.row []
                [ Form.colLabel [ Col.sm8, Col.md7, Col.lg6 ] [ text "Move idle documents to trash after X days" ]
                , Form.col [] [ Form.helpInline [] [ text "The following settings apply to all documents except pending documents and templates. If empty, documents will not be moved. Available values: 1 to 365" ] ]
                ]
            , formIntRowM "In preparation"
                ""
                (M.withDefault "" <| M.map String.fromInt settings.idleDocTimeoutPreparation)
                (SetIntField "idleDocTimeoutPreparation" docTimeoutRange)
                [ readonly ug.settingsIsInherited ]
            , formIntRowM "Closed"
                ""
                (M.withDefault "" <| M.map String.fromInt settings.idleDocTimeoutClosed)
                (SetIntField "idleDocTimeoutClosed" docTimeoutRange)
                [ readonly ug.settingsIsInherited ]
            , formIntRowM "Cancelled"
                ""
                (M.withDefault "" <| M.map String.fromInt settings.idleDocTimeoutCancelled)
                (SetIntField "idleDocTimeoutCancelled" docTimeoutRange)
                [ readonly ug.settingsIsInherited ]
            , formIntRowM "Timed out"
                ""
                (M.withDefault "" <| M.map String.fromInt settings.idleDocTimeoutTimeout)
                (SetIntField "idleDocTimeoutTimeout" docTimeoutRange)
                [ readonly ug.settingsIsInherited ]
            , formIntRowM "Rejected"
                ""
                (M.withDefault "" <| M.map String.fromInt settings.idleDocTimeoutRejected)
                (SetIntField "idleDocTimeoutRejected" docTimeoutRange)
                [ readonly ug.settingsIsInherited ]
            , formIntRowM "Error"
                ""
                (M.withDefault "" <| M.map String.fromInt settings.idleDocTimeoutError)
                (SetIntField "idleDocTimeoutError" docTimeoutRange)
                [ readonly ug.settingsIsInherited ]
            , formCheckboxRowM "Send timeout notifications"
                ""
                settings.sendTimeoutNotification
                (SetBoolField "sendTimeoutNotification")
                [ readonly ug.settingsIsInherited ]
            , formCheckboxRowM "2FA is mandatory"
                "If enabled, 2FA is mandatory/enforced for all users. 2FA can also be enforced for a single user."
                settings.twoFAMandatory
                (SetBoolField "twoFAMandatory")
                [ readonly ug.settingsIsInherited ]
            , formIntRowM "Session timeout"
                "If set, users cookie session expiry is set based on the provided seconds. Valid values are between 5 minutes to 30 days. Leave field empty to use the default session timeout."
                (M.withDefault "" <| M.map String.fromInt settings.sessionTimeout)
                (SetIntField "sessionTimeout" (Just <| Range 300 2592000)) -- 5 mins to 30 days
                [ readonly ug.settingsIsInherited ]
            , formIntRowM "Document session timeout"
                "A session timeout (in seconds) specific to document views. Leave this field empty to use the default session timeout."
                (M.withDefault "" <| M.map String.fromInt settings.documentSessionTimeout)
                (SetIntField "documentSessionTimeout" (Just <| Range 300 2592000)) -- 5 mins to 30 days
                [ readonly ug.settingsIsInherited ]
            , formTextRowM "Portal URL"
                ""
                (M.withDefault "" settings.portalUrl)
                (SetStringField "portalUrl")
                [ readonly ug.settingsIsInherited ]
            , formTextRowM "EID Hub token"
                "If set, then customer specific branding in EID Hub is used (must be also defined in EID Hub). If not set, then EID Hub Scrive branding is used."
                (M.withDefault "" settings.eidServiceToken)
                (SetStringField "eidServiceToken")
                [ readonly ug.settingsIsInherited ]
            , formCheckboxRowM "PAdES sealing"
                "If enabled, use PAdES instead of GuardTime sealing."
                (settings.sealingMethod == UserGroup.Pades)
                (\b -> SetStringField "sealingMethod"
                       <| Enum.toString UserGroup.enumSealingMethod
                       <| if b then UserGroup.Pades else UserGroup.GuardTime)
                [ readonly ug.settingsIsInherited ]
            , formCheckboxRowM "Has 'post signview'"
                "If set, then signatories that don't have an account will be shown a 'post signview' that asks them to sign up for our service."
                settings.hasPostSignview
                (SetBoolField "hasPostSignview")
                [ readonly ug.settingsIsInherited ]
            ]
        , Grid.row [ Row.leftSm ]
            [ Grid.col [ Col.sm12 ]
                [ Button.button
                    [ Button.primary
                    , Button.attrs [ class "ml-sm-2", onClick MergeUserGroupClicked ]
                    ]
                    [ text "Merge to different company" ]
                , Button.button
                    [ Button.success
                    , Button.attrs [ class "ml-sm-2", onClick SubmitForm ]
                    , Button.disabled <| not newParentIDIsValid
                    ]
                    [ text "Save" ]
                ]
            ]
        ]
