module AdminOnly.UserGroupAdmin.DetailsTab.DetailsTab exposing
    ( Model
    , Msg
    , init
    , tabName
    , update
    , view
    )

import AdminOnly.Types.UserGroup as UserGroup exposing (UserGroup)
import AdminOnly.Types.UserGroup.Address as Address exposing (Address)
import AdminOnly.Types.UserGroup.Cmd as Cmd
import AdminOnly.Types.UserGroup.DigitalSignatureMethod as DigitalSignatureMethod exposing (DigitalSignatureMethod)
import AdminOnly.Types.UserGroup.PadAppMode as PadAppMode exposing (PadAppMode)
import AdminOnly.Types.UserGroup.SEBankIDSigningProviderOverride as SEBankIDSigningProviderOverride
import AdminOnly.Types.UserGroup.Settings as Settings exposing (Settings)
import AdminOnly.Types.UserGroup.SmsProvider as SmsProvider exposing (SmsProvider)
import AdminOnly.UserGroupAdmin.DetailsTab.DeletionRequest as DeletionRequest
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
import Return exposing (..)
import Time exposing (Month(..))
import Url.Parser exposing (map)
import Utils exposing (..)


type alias Model =
    { sUserGroup : Status UserGroup
    , mOrigParentID : Maybe String
    , sNewParentHint : Status (Result NewParentError NewParentSuccess)
    , mMergeUserGroupModal : Maybe MergeUserGroupModal.Model
    , mDeletionRequest : Maybe DeletionRequest.State
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
      -- REQUEST USER GROUP DELETION
    | DeletionRequestMsg DeletionRequest.Msg


type alias Range =
    { min : Int
    , max : Int
    }


type NewParentError
    = RecursiveStructure
    | RootIsNotBillable


type alias NewParentSuccess =
    { name : String
    , warning : Maybe String
    }


tabName : String
tabName =
    "details"


init : (Msg -> msg) -> String -> Return msg Model
init embed ugid =
    let
        model =
            { sUserGroup = Loading
            , mOrigParentID = Nothing
            , sNewParentHint = Failure
            , mMergeUserGroupModal = Nothing
            , mDeletionRequest = Nothing
            }
    in
    return model <| Cmd.map embed <| Cmd.getUserGroup GotUserGroup ugid


modifyUserGroup : (UserGroup -> UserGroup) -> Model -> Model
modifyUserGroup modify model =
    { model | sUserGroup = statusMap modify model.sUserGroup }


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> Return msg Model
update embed globals msg model =
    case msg of
        GotUserGroup result ->
            case result of
                Ok userGroup ->
                    let
                        ( mergeModal, mergeModalCmd ) =
                            MergeUserGroupModal.init (embed << MergeUserGroupModalMsg) userGroup

                        deletionRequestParams =
                            { embed = embed << DeletionRequestMsg
                            , userGroupId = userGroup.id
                            , xtoken = globals.xtoken
                            , updateDeletionRequest = \_ -> globals.gotoUserGroup userGroup.id
                            , showFlashMessage = globals.flashMessage
                            }

                        ( deletionRequest, deletionRequestCmd ) =
                            DeletionRequest.init deletionRequestParams
                    in
                    return
                        { model
                            | sUserGroup = Success userGroup
                            , mOrigParentID = userGroup.parentID
                            , sNewParentHint = Loading
                            , mMergeUserGroupModal = Just mergeModal
                            , mDeletionRequest = Just deletionRequest
                        }
                    <|
                        Cmd.batch [ mergeModalCmd, deletionRequestCmd ]

                Err _ ->
                    singleton { model | sUserGroup = Failure }

        -- FORM SETTERS
        SetStringField name value ->
            singleton <| modifyUserGroup (UserGroup.setStringField name value) model

        SetBoolField name value ->
            singleton <| modifyUserGroup (UserGroup.setBoolField name value) model

        SetIntField name mRange value ->
            let
                mClamp =
                    case mRange of
                        Just range ->
                            clamp range.min range.max

                        Nothing ->
                            identity

                mInt =
                    stringNonEmpty value
                        |> M.andThen String.toInt
                        |> M.map mClamp
            in
            singleton <| modifyUserGroup (UserGroup.setIntField name mInt) model

        SetAddressIsInherited value ->
            case ( value, statusMap .inheritedAddress model.sUserGroup ) of
                ( True, Success Nothing ) ->
                    return model <| globals.flashMessage <| FlashMessage.error "Top level user group cannot inherit"

                _ ->
                    singleton <| modifyUserGroup (UserGroup.setBoolField "addressIsInherited" value) model

        SetSettingsIsInherited value ->
            case ( value, statusMap .inheritedSettings model.sUserGroup ) of
                ( True, Success Nothing ) ->
                    return model <| globals.flashMessage <| FlashMessage.error "Top level user group cannot inherit"

                _ ->
                    singleton <| modifyUserGroup (UserGroup.setBoolField "settingsIsInherited" value) model

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
                        { model1 | sNewParentHint = Loading }
                in
                return model2 <| Cmd.map embed <| Cmd.getUserGroup GotParentUserGroup newParentID

            else
                singleton model1

        SetSfAccountID newSfAccountID ->
            let
                model1 =
                    { model
                        | sUserGroup = statusMap (UserGroup.setInternalTag "sf-account-id" <| stringNonEmpty newSfAccountID) model.sUserGroup
                    }
            in
            singleton model1

        GotParentUserGroup response ->
            case response of
                Err _ ->
                    singleton { model | sNewParentHint = Failure }

                Ok parentGroup ->
                    let
                        isRecursive currentGroup =
                            parentGroup.id == currentGroup.id || L.any (\parentUserGroup -> parentUserGroup.id == currentGroup.id) parentGroup.parentGroupPath

                        hint =
                            model.sUserGroup
                                |> statusMap
                                    (\currentGroup ->
                                        if isRecursive currentGroup then
                                            Err RecursiveStructure

                                        else if not parentGroup.rootIsBillable then
                                            Err RootIsNotBillable

                                        else if model.mOrigParentID == Nothing then
                                            Ok <| NewParentSuccess parentGroup.name <| Just "Invoicing type for this group will be set to None."

                                        else
                                            Ok <| NewParentSuccess parentGroup.name Nothing
                                    )
                    in
                    singleton { model | sNewParentHint = hint }

        SubmitForm ->
            case fromStatus model.sUserGroup of
                Just userGroup ->
                    return model <|
                        Http.post
                            { url = "/adminonly/companyadmin/" ++ userGroup.id
                            , body = formBody globals <| UserGroup.formValues userGroup
                            , expect = Http.expectString <| embed << GotSaveResponse
                            }

                Nothing ->
                    singleton model

        GotSaveResponse response ->
            case response of
                Err _ ->
                    return model <| globals.flashMessage <| FlashMessage.error "Request failed."

                Ok _ ->
                    return { model | sUserGroup = statusMap UserGroup.afterSaved model.sUserGroup } <|
                        Cmd.batch
                            [ globals.flashMessage <| FlashMessage.success "Saved"
                            , globals.setPageUrlFromModel -- reloads UserGroup Details
                            ]

        -- MOVE USER
        MergeUserGroupClicked ->
            model.mMergeUserGroupModal
                |> M.map
                    (\modal -> { model | mMergeUserGroupModal = Just <| MergeUserGroupModal.show modal })
                |> M.withDefault model
                |> singleton

        MergeUserGroupModalMsg modalMsg ->
            let
                updateMergeUserGroupModal =
                    MergeUserGroupModal.update (embed << MergeUserGroupModalMsg) globals modalMsg

                ( newMergeUserGroupModal, cmd ) =
                    maybeUpdate updateMergeUserGroupModal model.mMergeUserGroupModal
            in
            return { model | mMergeUserGroupModal = newMergeUserGroupModal } cmd

        DeletionRequestMsg deletionRequestMsg ->
            case ( model.sUserGroup, model.mDeletionRequest ) of
                ( Success userGroup, Just deletionRequest ) ->
                    let
                        params =
                            { embed = embed << DeletionRequestMsg
                            , userGroupId = userGroup.id
                            , xtoken = globals.xtoken
                            , updateDeletionRequest = \_ -> globals.gotoUserGroup userGroup.id
                            , showFlashMessage = globals.flashMessage
                            }

                        ( newDeletionRequest, cmd ) =
                            DeletionRequest.update params deletionRequestMsg deletionRequest
                    in
                    return { model | mDeletionRequest = Just newDeletionRequest } cmd

                _ ->
                    singleton model


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
                        , M.map (Html.map <| embed << DeletionRequestMsg) <| M.andThen DeletionRequest.viewModal model.mDeletionRequest
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

        ( newParentIdIsValid, newParentIdInputValidity, newParentIdFeedback ) =
            if ug.isBillable then
                ( True, [ Input.disabled True ], [ Form.help [] [ text "Can't move billable user group." ] ] )

            else if model.mOrigParentID == ug.parentID then
                ( True, [], [] )

            else
                case M.map isInteger ug.parentID of
                    Nothing ->
                        if ug.isBillable then
                            ( True, [ Input.success ], [ Form.validFeedback [] [ text "No parent user group." ] ] )

                        else
                            ( False
                            , [ Input.danger ]
                            , [ Form.invalidFeedback []
                                    [ text "User group can't become root without billable flag. Use "
                                    , Html.i [] [ text "Upgrade to billable company" ]
                                    , text " button in the structure tab."
                                    ]
                              ]
                            )

                    Just False ->
                        ( False, [ Input.danger ], [ Form.invalidFeedback [] [ text "Company ID can only contain numbers." ] ] )

                    Just True ->
                        case model.sNewParentHint of
                            Loading ->
                                ( False, [], [ Form.help [] [ text "Loading user group ..." ] ] )

                            Failure ->
                                ( False, [ Input.danger ], [ Form.invalidFeedback [] [ text "User group does not exist." ] ] )

                            Success (Err errorType) ->
                                let
                                    errorMsg =
                                        case errorType of
                                            RecursiveStructure ->
                                                "Recursive user group structure is not allowed."

                                            RootIsNotBillable ->
                                                "Root of destination user group is not billable."
                                in
                                ( False, [ Input.danger ], [ Form.invalidFeedback [] [ text errorMsg ] ] )

                            Success (Ok newParent) ->
                                let
                                    warning =
                                        case newParent.warning of
                                            Just msg ->
                                                Html.div [ class "text-secondary" ] [ text msg ]

                                            Nothing ->
                                                text ""
                                in
                                ( True, [ Input.success ], [ Form.validFeedback [] [ text <| "User group with name: \"" ++ newParent.name ++ "\"", warning ] ] )

        docTimeoutRange =
            Just (Range 1 365)
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
                            ++ newParentIdInputValidity
                    ]
                        ++ newParentIdFeedback
                , Form.col [] [ Form.helpInline [] [ text "Leave is empty for Scrive. SF is 9197237133460633368. Only Partner Manager is allowed to make changes in this field." ] ]
                ]
            , Form.row []
                [ Form.colLabel labelColAttrs [ text "Parent group path" ]
                , Form.colLabel [ Col.smAuto, Col.mdAuto, Col.lgAuto ]
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
                SmsProvider.enum
                settings.smsProvider
                (SetStringField "smsProvider")
                [ disabled ug.settingsIsInherited ]
            , formSelectRowM "Pad application mode"
                ""
                PadAppMode.enum
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
            , formCheckboxRowM "Use folder list calls"
                "Use folder based list calls instead of legacy ones."
                settings.useFolderListCalls
                (SetBoolField "useFolderListCalls")
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
                (SetIntField "sessionTimeout" (Just <| Range 300 2592000))
                -- 5 mins to 30 days
                [ readonly ug.settingsIsInherited ]
            , formIntRowM "Document session timeout"
                "A session timeout (in seconds) specific to document views. Leave this field empty to use the default session timeout."
                (M.withDefault "" <| M.map String.fromInt settings.documentSessionTimeout)
                (SetIntField "documentSessionTimeout" (Just <| Range 300 2592000))
                -- 5 mins to 30 days
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
                (settings.digitalSignatureMethod == DigitalSignatureMethod.Pades)
                (\b ->
                    SetStringField "digitalSignatureMethod" <|
                        Enum.toString DigitalSignatureMethod.enum <|
                            if b then
                                DigitalSignatureMethod.Pades

                            else
                                DigitalSignatureMethod.GuardTime
                )
                [ readonly ug.settingsIsInherited ]
            , formTextRowM "Pades Credentials Label"
                "If set, use GlobalSign API Credentials assigned to this label for PAdES signing."
                (M.withDefault "" settings.padesCredentialsLabel)
                (SetStringField "padesCredentialsLabel")
                [ readonly True ]
            , formCheckboxRowM "Has 'post signview'"
                "If set, then signatories that don't have an account will be shown a 'post signview' that asks them to sign up for our service."
                settings.hasPostSignview
                (SetBoolField "hasPostSignview")
                [ readonly ug.settingsIsInherited ]
            , formCheckboxRowM "Uses EID Hub for SEBankID auth-to-view"
                "If set, then the EID Hub will be used to initiate the BankID transaction, rather than our internal implementation via CGI-GRP."
                settings.eidUseForSEView
                (SetBoolField "eidUseForSEView")
                [ readonly ug.settingsIsInherited ]
            , formSelectRowM "Force SEBankID Signing Provider"
                ""
                SEBankIDSigningProviderOverride.enum
                settings.seBankIDSigningOverride
                (SetStringField "seBankIDSigningOverride")
                [ disabled ug.settingsIsInherited ]
            , formCheckboxRowM "Uses new frontend"
                "If set, then the users of company will see new frontend by default after loging in."
                settings.appFrontend
                (SetBoolField "appFrontend")
                [ readonly ug.settingsIsInherited ]
            , formRow labelColAttrs
                inputColAttrs
                "SSO enabled"
                "If set, then there is an SSO configuration associated with this company"
              <|
                [ Checkbox.checkbox
                    [ Checkbox.attrs <| [ checked settings.usesSSO, readonly True, disabled True ] ]
                    ""
                ]
            ]
        , Grid.row [ Row.leftSm ] <|
            [ Grid.col [ Col.sm12 ]
                [ Button.button
                    [ Button.primary
                    , Button.attrs [ onClick MergeUserGroupClicked ]
                    ]
                    [ text "Merge to different company" ]
                , Button.button
                    [ Button.success
                    , Button.attrs [ class "ml-sm-2", onClick SubmitForm ]
                    , Button.disabled <| not newParentIdIsValid
                    ]
                    [ text "Save" ]
                ]
            ]
                ++ (case model.mDeletionRequest of
                        Just deletionRequest ->
                            [ DeletionRequest.viewButtons DeletionRequestMsg { deletionStatus = ug.deletionStatus } deletionRequest
                            ]

                        Nothing ->
                            []
                   )
        ]
