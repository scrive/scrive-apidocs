module AdminOnly.UserAdmin.DetailsTab.UserGroup exposing
    ( Address
    , PadAppMode(..)
    , SealingMethod(..)
    , Settings
    , SmsProvider(..)
    , UserGroup
    , afterSaved
    , decoder
    , enumPadAppMode
    , enumSealingMethod
    , enumSmsProvider
    , formValues
    , getInternalTag
    , padAppModeDecoder
    , setBoolField
    , setIntField
    , setInternalTag
    , setStringField
    , smsProviderDecoder
    , DeletionStatus
    , DeletionRequest
    , deletionRequestDecoder
    )

import Dict as D
import EnumExtra as Enum exposing (Enum, findEnumValue, makeEnum)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP
import Json.Encode as JE
import List as L
import Maybe as M
import Utils exposing (boolToJson, ite, stringNonEmpty, flip)



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
    , address : Address
    , addressIsInherited : Bool
    , inheritedAddress : Maybe Address
    , settings : Settings
    , settingsIsInherited : Bool
    , inheritedSettings : Maybe Settings
    , internalTags : D.Dict String String
    , initialInternalTags : D.Dict String String
    , homeFolderID : Maybe String
    , deletionStatus : DeletionStatus
    }


decoder : Decoder UserGroup
decoder =
    JD.succeed UserGroup
        |> DP.required "companyid" JD.string
        |> DP.required "companyname" JD.string
        |> DP.required "entityname" JD.string
        |> DP.required "parentid" (JD.string |> JD.nullable)
        |> DP.required "parentgrouppath" (JD.list parentUserGroupDecoder)
        |> DP.custom addressDecoder
        |> DP.required "companyaddressisinherited" JD.bool
        |> DP.optional "companyinheritedaddress" (addressDecoder |> JD.nullable) Nothing
        |> DP.custom settingsDecoder
        |> DP.required "companysettingsisinherited" JD.bool
        |> DP.optional "companyinheritedsettings" (settingsDecoder |> JD.nullable) Nothing
        |> DP.optional "companyinternaltags" (JD.list tagDecoder |> JD.map D.fromList) D.empty
        -- initialInternalTags are exactly the same as internalTags
        |> DP.optional "companyinternaltags" (JD.list tagDecoder |> JD.map D.fromList) D.empty
        |> DP.required "companyhomefolderid" (JD.nullable JD.string)
        |> DP.optional "deletionrequest" (JD.nullable deletionRequestDecoder) Nothing

deletionRequestDecoder : Decoder DeletionRequest
deletionRequestDecoder =
    JD.succeed DeletionRequest
    |> DP.required "requested_by" JD.string
    |> DP.required "requested_deletion_date" JD.string
    |> DP.optional "signed_off_by" (JD.nullable JD.string) Nothing


type alias DeletionStatus = Maybe DeletionRequest

type alias DeletionRequest =
    { requestedBy : String
    , requestedDeletionDate : String
    , signedOffBy : Maybe String
    }

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

        "parentID" ->
            { ug | parentID = stringNonEmpty value }

        "padAppMode" ->
            modifySettings
                (\s ->
                    let
                        mNewPadAppMode =
                            Enum.fromString enumPadAppMode value
                    in
                    { s | padAppMode = M.withDefault s.padAppMode mNewPadAppMode }
                )
                ug

        "smsProvider" ->
            modifySettings
                (\s ->
                    let
                        mNewSmsProvider =
                            Enum.fromString enumSmsProvider value
                    in
                    { s | smsProvider = M.withDefault s.smsProvider mNewSmsProvider }
                )
                ug

        "sealingMethod" ->
            modifySettings
                (\s ->
                    let
                        mNewSealingMethod =
                            Enum.fromString enumSealingMethod value
                    in
                    { s | sealingMethod = M.withDefault s.sealingMethod mNewSealingMethod }
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



-- move tags to initial tags


afterSaved : UserGroup -> UserGroup
afterSaved ug =
    { ug | initialInternalTags = ug.internalTags }



-- ADDRESS


type alias Address =
    { address : String
    , city : String
    , country : String
    , zip : String
    , companyNumber : String
    }


addressDecoder : Decoder Address
addressDecoder =
    JD.succeed Address
        |> DP.required "address" JD.string
        |> DP.required "city" JD.string
        |> DP.required "country" JD.string
        |> DP.required "zip" JD.string
        |> DP.required "companynumber" JD.string



-- SETTINGS


type alias Settings =
    { ipAddressMaskList : String
    , cgiDisplayName : Maybe String
    , cgiServiceID : Maybe String
    , smsProvider : SmsProvider
    , padAppMode : PadAppMode
    , padEarchiveEnabled : Bool
    , forceHidePN : Bool
    , useFolderListCalls : Bool
    , idleDocTimeoutPreparation : Maybe Int
    , idleDocTimeoutClosed : Maybe Int
    , idleDocTimeoutCancelled : Maybe Int
    , idleDocTimeoutTimeout : Maybe Int
    , idleDocTimeoutRejected : Maybe Int
    , idleDocTimeoutError : Maybe Int
    , immediateTrash : Bool
    , sendTimeoutNotification : Bool
    , twoFAMandatory : Bool
    , sessionTimeout : Maybe Int
    , documentSessionTimeout : Maybe Int
    , portalUrl : Maybe String
    , eidServiceToken : Maybe String
    , sealingMethod : SealingMethod
    , hasPostSignview : Bool
    , eidUseForSEView : Bool
    , appFrontend : Bool
    }


settingsDecoder : Decoder Settings
settingsDecoder =
    JD.succeed Settings
        |> DP.required "ipaddressmasklist" JD.string
        |> DP.required "cgidisplayname" (JD.string |> JD.nullable)
        |> DP.required "cgiserviceid" (JD.string |> JD.nullable)
        |> DP.required "smsprovider" (JD.string |> JD.andThen smsProviderDecoder)
        |> DP.required "padappmode" (JD.string |> JD.andThen padAppModeDecoder)
        |> DP.required "padearchiveenabled" JD.bool
        |> DP.required "forcehidepn" JD.bool
        |> DP.required "usefolderlistcalls" JD.bool
        |> DP.required "idledoctimeoutpreparation" (JD.int |> JD.nullable)
        |> DP.required "idledoctimeoutclosed" (JD.int |> JD.nullable)
        |> DP.required "idledoctimeoutcanceled" (JD.int |> JD.nullable)
        |> DP.required "idledoctimeouttimedout" (JD.int |> JD.nullable)
        |> DP.required "idledoctimeoutrejected" (JD.int |> JD.nullable)
        |> DP.required "idledoctimeouterror" (JD.int |> JD.nullable)
        |> DP.required "immediatetrash" JD.bool
        |> DP.required "sendtimeoutnotification" JD.bool
        |> DP.required "totpismandatory" JD.bool
        |> DP.required "sessiontimeout" (JD.int |> JD.nullable)
        |> DP.required "documentsessiontimeout" (JD.int |> JD.nullable)
        |> DP.required "portalurl" (JD.string |> JD.nullable)
        |> DP.required "eidservicetoken" (JD.string |> JD.nullable)
        |> DP.required "sealingmethod" (JD.string |> JD.andThen sealingMethodDecoder)
        |> DP.required "haspostsignview" JD.bool
        |> DP.required "eiduseforseview" JD.bool
        |> DP.required "appfrontend" JD.bool


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



-- Sealing method


type SealingMethod
    = GuardTime
    | Pades


enumSealingMethod : Enum SealingMethod
enumSealingMethod =
    let
        toString sm =
            case sm of
                GuardTime ->
                    "guardtime"

                Pades ->
                    "pades"

        toHumanString sm =
            case sm of
                GuardTime ->
                    "GuardTime"

                Pades ->
                    "PAdES"

        allValues =
            [ GuardTime, Pades ]
    in
    makeEnum allValues toString toHumanString


sealingMethodDecoder : String -> Decoder SealingMethod
sealingMethodDecoder sealingMethodString =
    case findEnumValue enumSealingMethod sealingMethodString of
        Err _ ->
            JD.fail <| "Cannot parse sealing method: " ++ sealingMethodString

        Ok sealingMethod ->
            JD.succeed sealingMethod



-- PAD APP MODE


type PadAppMode
    = ListView
    | PinCode
    | QRCode


allPadAppModes : List PadAppMode
allPadAppModes =
    [ ListView, PinCode, QRCode ]


encodePadAppMode : PadAppMode -> String
encodePadAppMode padAppMode =
    case padAppMode of
        ListView ->
            "list_view"

        PinCode ->
            "pin_code"

        QRCode ->
            "qr_code"


fromPadAppMode : PadAppMode -> String
fromPadAppMode padAppMode =
    case padAppMode of
        ListView ->
            "ListView"

        PinCode ->
            "PinCode"

        QRCode ->
            "QRCode"


padAppModeDecoder : String -> Decoder PadAppMode
padAppModeDecoder padAppModeString =
    case findEnumValue enumPadAppMode padAppModeString of
        Err _ ->
            JD.fail <| "Cannot parse pad app mode: " ++ padAppModeString

        Ok padAppMode ->
            JD.succeed padAppMode


enumPadAppMode : Enum PadAppMode
enumPadAppMode =
    makeEnum allPadAppModes encodePadAppMode fromPadAppMode



-- SMS Provider


smsProviderDecoder : String -> Decoder SmsProvider
smsProviderDecoder smsProviderString =
    case findEnumValue enumSmsProvider smsProviderString of
        Err _ ->
            JD.fail <| "Cannot parse sms provider: " ++ smsProviderString

        Ok smsProvider ->
            JD.succeed smsProvider


allSmsProviders : List SmsProvider
allSmsProviders =
    [ SmsDefault, SmsTeliaCallGuide ]


encodeSmsProvider : SmsProvider -> String
encodeSmsProvider smsProvider =
    case smsProvider of
        SmsDefault ->
            "SMSDefault"

        SmsTeliaCallGuide ->
            "SMSTeliaCallGuide"


type SmsProvider
    = SmsDefault
    | SmsTeliaCallGuide


enumSmsProvider : Enum SmsProvider
enumSmsProvider =
    makeEnum allSmsProviders encodeSmsProvider encodeSmsProvider


formValues : UserGroup -> List ( String, String )
formValues ug =
    [ ( "companyname", ug.name )
    , ( "entityname", ug.entityName )
    , ( "companysettingsisinherited", boolToJson ug.settingsIsInherited )
    , ( "companyaddressisinherited", boolToJson ug.addressIsInherited )
    ]
        ++ ite ug.settingsIsInherited [] (formValuesSettings ug.settings)
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


formValuesSettings : Settings -> List ( String, String )
formValuesSettings settings =
    let
        fromIntWithEmpty mVal =
            M.map String.fromInt mVal |> M.withDefault ""
    in
    [ ( "companyipaddressmasklist", settings.ipAddressMaskList )
    , ( "companysmsprovider", encodeSmsProvider settings.smsProvider )
    , ( "companypadappmode", encodePadAppMode settings.padAppMode )
    , ( "companypadearchiveenabled", boolToJson settings.padEarchiveEnabled )
    , ( "companyforcehidepn", boolToJson settings.forceHidePN )
    , ( "companyusefolderlistcalls", boolToJson settings.useFolderListCalls )
    , ( "companyimmediatetrash", boolToJson settings.immediateTrash )
    , ( "companyidledoctimeoutpreparation", fromIntWithEmpty settings.idleDocTimeoutPreparation )
    , ( "companyidledoctimeoutclosed", fromIntWithEmpty settings.idleDocTimeoutClosed )
    , ( "companyidledoctimeoutcanceled", fromIntWithEmpty settings.idleDocTimeoutCancelled )
    , ( "companyidledoctimeouttimedout", fromIntWithEmpty settings.idleDocTimeoutTimeout )
    , ( "companyidledoctimeoutrejected", fromIntWithEmpty settings.idleDocTimeoutRejected )
    , ( "companyidledoctimeouterror", fromIntWithEmpty settings.idleDocTimeoutError )
    , ( "companysendtimeoutnotification", boolToJson settings.sendTimeoutNotification )
    , ( "companytotpismandatory", boolToJson settings.twoFAMandatory )
    , ( "companysessiontimeout", fromIntWithEmpty settings.sessionTimeout )
    , ( "companydocumentsessiontimeout", fromIntWithEmpty settings.documentSessionTimeout )
    , ( "companyportalurl", M.withDefault "" settings.portalUrl )
    , ( "companyeidservicetoken", M.withDefault "" settings.eidServiceToken )
    , ( "companysealingmethod", Enum.toString enumSealingMethod settings.sealingMethod )
    , ( "companyhaspostsignview", boolToJson settings.hasPostSignview )
    , ( "companyeiduseforseview", boolToJson settings.eidUseForSEView )
    , ( "companyappfrontend", boolToJson settings.appFrontend )
    , ( "companycgiserviceid", M.withDefault "" settings.cgiServiceID )
    , ( "companycgidisplayname", M.withDefault "" settings.cgiDisplayName )
    ]

mField : (value -> String) -> ( String, Maybe value ) -> Maybe ( String, String )
mField toString tuple =
    case tuple of
        ( _, Nothing ) ->
            Nothing

        ( name, Just value ) ->
            Just ( name, toString value )
