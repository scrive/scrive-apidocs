module AdminOnly.Types.UserGroup exposing
    ( UserGroup
    , afterSaved
    , decoder
    , formValues
    , getInternalTag
    , modifyAddress
    , modifySettings
    , setBoolField
    , setIntField
    , setInternalTag
    , setStringField
    )

import AdminOnly.Types.UserGroup.Address as Address exposing (Address)
import AdminOnly.Types.UserGroup.DeletionRequest as DeletionRequest exposing (DeletionRequest)
import AdminOnly.Types.UserGroup.DigitalSignatureMethod as DigitalSignatureMethod exposing (DigitalSignatureMethod)
import AdminOnly.Types.UserGroup.PadAppMode as PadAppMode exposing (PadAppMode)
import AdminOnly.Types.UserGroup.SEBankIDSigningProviderOverride as SEBankIDSigningProviderOverride exposing (SEBankIDSigningProviderOverride)
import AdminOnly.Types.UserGroup.Settings as Settings exposing (Settings)
import AdminOnly.Types.UserGroup.SmsProvider as SmsProvider exposing (SmsProvider)
import Dict as D
import EnumExtra as Enum
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP
import Json.Encode as JE
import Maybe as M
import Maybe.Extra as M
import Utils exposing (boolToJson, ite, stringNonEmpty)



-- USER GROUP
-- Note that `UserGroup` is a misnomer: this type represents a user group _along
-- with_ ad-hoc facts about the user group. We should split this type into its
-- conceptual components.


type alias UserGroup =
    { id : String
    , name : String
    , entityName : String
    , parentID : Maybe String
    , parentGroupPath : List ParentUserGroup
    , isBillable : Bool
    , rootIsBillable : Bool
    , address : Address
    , addressIsInherited : Bool
    , inheritedAddress : Maybe Address
    , settings : Settings
    , settingsIsInherited : Bool
    , inheritedSettings : Maybe Settings
    , internalTags : D.Dict String String
    , initialInternalTags : D.Dict String String
    , homeFolderID : Maybe String
    , deletionStatus : Maybe DeletionRequest
    }


decoder : Decoder UserGroup
decoder =
    JD.succeed UserGroup
        |> DP.required "companyid" JD.string
        |> DP.required "companyname" JD.string
        |> DP.required "entityname" JD.string
        |> DP.required "parentid" (JD.string |> JD.nullable)
        |> DP.required "parentgrouppath" (JD.list parentUserGroupDecoder)
        |> DP.required "companyisbillable" JD.bool
        |> DP.required "companyrootisbillable" JD.bool
        |> DP.custom Address.decoder
        |> DP.required "companyaddressisinherited" JD.bool
        |> DP.optional "companyinheritedaddress" (Address.decoder |> JD.nullable) Nothing
        |> DP.custom Settings.decoder
        |> DP.required "companysettingsisinherited" JD.bool
        |> DP.optional "companyinheritedsettings" (Settings.decoder |> JD.nullable) Nothing
        |> DP.optional "companyinternaltags" (JD.list tagDecoder |> JD.map D.fromList) D.empty
        -- initialInternalTags are exactly the same as internalTags
        |> DP.optional "companyinternaltags" (JD.list tagDecoder |> JD.map D.fromList) D.empty
        |> DP.required "companyhomefolderid" (JD.nullable JD.string)
        |> DP.optional "deletionrequest" (JD.nullable DeletionRequest.decoder) Nothing


type alias ParentUserGroup =
    { id : String
    , name : String
    }


parentUserGroupDecoder : Decoder ParentUserGroup
parentUserGroupDecoder =
    JD.map2 ParentUserGroup
        (JD.field "group_id" JD.string)
        (JD.field "group_name" JD.string)


tagDecoder : Decoder ( String, String )
tagDecoder =
    JD.map2 (\name value -> ( name, value ))
        (JD.field "name" JD.string)
        (JD.field "value" JD.string)



-- Accessors


modifySettings : (Settings -> Settings) -> UserGroup -> UserGroup
modifySettings modify ug =
    { ug | settings = modify ug.settings }


modifyAddress : (Address -> Address) -> UserGroup -> UserGroup
modifyAddress modify ug =
    { ug | address = modify ug.address }


setStringField : String -> String -> UserGroup -> UserGroup
setStringField name value ug =
    case name of
        "name" ->
            { ug | name = value }

        "entityName" ->
            { ug | entityName = value }

        "address" ->
            modifyAddress (\a -> { a | address = value }) ug

        "city" ->
            modifyAddress (\a -> { a | city = value }) ug

        "country" ->
            modifyAddress (\a -> { a | country = value }) ug

        "zip" ->
            modifyAddress (\a -> { a | zip = value }) ug

        "companyNumber" ->
            modifyAddress (\a -> { a | companyNumber = value }) ug

        "ipAddressMaskList" ->
            modifySettings (\s -> { s | ipAddressMaskList = value }) ug

        "cgiDisplayName" ->
            modifySettings (\s -> { s | cgiDisplayName = stringNonEmpty value }) ug

        "cgiServiceID" ->
            modifySettings (\s -> { s | cgiServiceID = stringNonEmpty value }) ug

        "portalUrl" ->
            modifySettings (\s -> { s | portalUrl = stringNonEmpty value }) ug

        "eidServiceToken" ->
            modifySettings (\s -> { s | eidServiceToken = stringNonEmpty value }) ug

        "padesCredentialsLabel" ->
            modifySettings (\s -> { s | padesCredentialsLabel = stringNonEmpty value }) ug

        "parentID" ->
            { ug | parentID = stringNonEmpty value }

        "padAppMode" ->
            modifySettings
                (\s ->
                    let
                        mNewPadAppMode =
                            Enum.fromString PadAppMode.enum value
                    in
                    { s | padAppMode = M.withDefault s.padAppMode mNewPadAppMode }
                )
                ug

        "smsProvider" ->
            modifySettings
                (\s ->
                    let
                        mNewSmsProvider =
                            Enum.fromString SmsProvider.enum value
                    in
                    { s | smsProvider = M.withDefault s.smsProvider mNewSmsProvider }
                )
                ug

        "digitalSignatureMethod" ->
            modifySettings
                (\s ->
                    let
                        mNewSealingMethod =
                            Enum.fromString DigitalSignatureMethod.enum value
                    in
                    { s | digitalSignatureMethod = M.withDefault s.digitalSignatureMethod mNewSealingMethod }
                )
                ug

        "seBankIDSigningOverride" ->
            modifySettings
                (\s ->
                    let
                        mNewSEBankIDSigningOverride =
                            Enum.fromString SEBankIDSigningProviderOverride.enum value
                    in
                    { s | seBankIDSigningOverride = M.withDefault s.seBankIDSigningOverride mNewSEBankIDSigningOverride }
                )
                ug

        _ ->
            ug


setInternalTag : String -> Maybe String -> UserGroup -> UserGroup
setInternalTag key mValue ug =
    case mValue of
        Nothing ->
            { ug | internalTags = D.remove key ug.internalTags }

        Just value ->
            { ug | internalTags = D.insert key value ug.internalTags }


getInternalTag : String -> UserGroup -> Maybe String
getInternalTag key ug =
    D.get key ug.internalTags


setBoolField : String -> Bool -> UserGroup -> UserGroup
setBoolField name value ug =
    case name of
        "padEarchiveEnabled" ->
            modifySettings (\s -> { s | padEarchiveEnabled = value }) ug

        "forceHidePN" ->
            modifySettings (\s -> { s | forceHidePN = value }) ug

        "useFolderListCalls" ->
            modifySettings (\s -> { s | useFolderListCalls = value }) ug

        "immediateTrash" ->
            modifySettings (\s -> { s | immediateTrash = value }) ug

        "sendTimeoutNotification" ->
            modifySettings (\s -> { s | sendTimeoutNotification = value }) ug

        "twoFAMandatory" ->
            modifySettings (\s -> { s | twoFAMandatory = value }) ug

        "addressIsInherited" ->
            { ug | addressIsInherited = value }

        "settingsIsInherited" ->
            { ug | settingsIsInherited = value }

        "hasPostSignview" ->
            modifySettings (\s -> { s | hasPostSignview = value }) ug

        "eidUseForSEView" ->
            modifySettings (\s -> { s | eidUseForSEView = value }) ug

        "appFrontend" ->
            modifySettings (\s -> { s | appFrontend = value }) ug

        _ ->
            ug


setIntField : String -> Maybe Int -> UserGroup -> UserGroup
setIntField name value ug =
    (\modify -> modifySettings modify ug) <|
        \s ->
            case name of
                "idleDocTimeoutPreparation" ->
                    { s | idleDocTimeoutPreparation = value }

                "idleDocTimeoutClosed" ->
                    { s | idleDocTimeoutClosed = value }

                "idleDocTimeoutCancelled" ->
                    { s | idleDocTimeoutCancelled = value }

                "idleDocTimeoutTimeout" ->
                    { s | idleDocTimeoutTimeout = value }

                "idleDocTimeoutRejected" ->
                    { s | idleDocTimeoutRejected = value }

                "idleDocTimeoutError" ->
                    { s | idleDocTimeoutError = value }

                "sessionTimeout" ->
                    { s | sessionTimeout = value }

                "documentSessionTimeout" ->
                    { s | documentSessionTimeout = value }

                _ ->
                    s



-- Helper functions


afterSaved : UserGroup -> UserGroup
afterSaved ug =
    { ug | initialInternalTags = ug.internalTags }


formValues : UserGroup -> List ( String, String )
formValues ug =
    [ ( "companyname", ug.name )
    , ( "entityname", ug.entityName )
    , ( "companysettingsisinherited", boolToJson ug.settingsIsInherited )
    , ( "companyaddressisinherited", boolToJson ug.addressIsInherited )
    ]
        ++ ite ug.settingsIsInherited [] (Settings.formValues ug.settings)
        ++ (ite ug.addressIsInherited [] <|
                [ ( "companynumber", ug.address.companyNumber )
                , ( "companyaddress", ug.address.address )
                , ( "companyzip", ug.address.zip )
                , ( "companycity", ug.address.city )
                , ( "companycountry", ug.address.country )
                ]
           )
        ++ (M.withDefault [] <| M.map (\parentID -> [ ( "companyparentid", parentID ) ]) ug.parentID)
        ++ [ ( "companyinternaltags", JE.encode 0 <| tagsToUpdatesJson ug.internalTags ug.initialInternalTags ) ]


tagsToUpdatesJson : D.Dict String String -> D.Dict String String -> JE.Value
tagsToUpdatesJson newTags oldTags =
    D.merge
        (\key newValue -> D.insert key <| Just newValue)
        (\key newValue oldValue -> ite (newValue == oldValue) identity (D.insert key <| Just newValue))
        (\key _ -> D.insert key Nothing)
        newTags
        oldTags
        D.empty
        |> D.toList
        |> JE.list
            (\( key, mValue ) ->
                JE.object
                    [ ( "name", JE.string key )
                    , ( "value", M.withDefault JE.null <| M.map JE.string mValue )
                    ]
            )
