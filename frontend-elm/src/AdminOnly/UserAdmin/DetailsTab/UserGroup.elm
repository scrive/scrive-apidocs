module AdminOnly.UserAdmin.DetailsTab.UserGroup exposing
    ( Address
    , PadAppMode(..)
    , Settings
    , SmsProvider(..)
    , UserGroup
    , afterSaved
    , decoder
    , enumPadAppMode
    , enumSmsProvider
    , formValues
    , getInternalTag
    , padAppModeDecoder
    , setBoolField
    , setIntField
    , setInternalTag
    , setStringField
    , smsProviderDecoder
    )

import Dict as D
import EnumExtra exposing (Enum, findEnumValue, makeEnum)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP
import Json.Encode as JE
import List as L
import Maybe as M
import Set as S
import Utils exposing (boolToJson, ite, stringNonEmpty)



-- USER GROUP


type alias UserGroup =
    { id : String
    , name : String
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
    }


decoder : Decoder UserGroup
decoder =
    JD.succeed UserGroup
        |> DP.required "companyid" JD.string
        |> DP.required "companyname" JD.string
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

        "parentID" ->
            { ug | parentID = stringNonEmpty value }

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

        "immediateTrash" ->
            modifySettings (\s -> { s | immediateTrash = value }) ug

        "addressIsInherited" ->
            { ug | addressIsInherited = value }

        "settingsIsInherited" ->
            { ug | settingsIsInherited = value }

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
    , idleDocTimeoutPreparation : Maybe Int
    , idleDocTimeoutClosed : Maybe Int
    , idleDocTimeoutCancelled : Maybe Int
    , idleDocTimeoutTimeout : Maybe Int
    , idleDocTimeoutRejected : Maybe Int
    , idleDocTimeoutError : Maybe Int
    , immediateTrash : Bool
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
        |> DP.required "idledoctimeoutpreparation" (JD.int |> JD.nullable)
        |> DP.required "idledoctimeoutclosed" (JD.int |> JD.nullable)
        |> DP.required "idledoctimeoutcanceled" (JD.int |> JD.nullable)
        |> DP.required "idledoctimeouttimedout" (JD.int |> JD.nullable)
        |> DP.required "idledoctimeoutrejected" (JD.int |> JD.nullable)
        |> DP.required "idledoctimeouterror" (JD.int |> JD.nullable)
        |> DP.required "immediatetrash" JD.bool


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
    D.keys newTags
        ++ D.keys oldTags
        |> S.fromList
        |> S.toList
        |> L.filterMap
            (\key ->
                case ( D.get key newTags, D.get key oldTags ) of
                    -- this cannot happen
                    ( Nothing, Nothing ) ->
                        Nothing

                    ( Nothing, Just _ ) ->
                        Just <|
                            JE.object
                                [ ( "name", JE.string key )
                                , ( "value", JE.null )
                                ]

                    ( Just newValue, _ ) ->
                        Just <|
                            JE.object
                                [ ( "name", JE.string key )
                                , ( "value", JE.string newValue )
                                ]
            )
        |> JE.list identity


formValuesSettings : Settings -> List ( String, String )
formValuesSettings settings =
    [ ( "companyipaddressmasklist", settings.ipAddressMaskList )
    , ( "companysmsprovider", encodeSmsProvider settings.smsProvider )
    , ( "companypadappmode", encodePadAppMode settings.padAppMode )
    , ( "companypadearchiveenabled", boolToJson settings.padEarchiveEnabled )
    , ( "companyimmediatetrash", boolToJson settings.immediateTrash )
    ]
        ++ L.filterMap identity
            [ mField identity ( "companycgiserviceid", settings.cgiServiceID )
            , mField identity ( "companycgidisplayname", settings.cgiDisplayName )
            , mField String.fromInt ( "companyidledoctimeoutpreparation", settings.idleDocTimeoutPreparation )
            , mField String.fromInt ( "companyidledoctimeoutclosed", settings.idleDocTimeoutClosed )
            , mField String.fromInt ( "companyidledoctimeoutcanceled", settings.idleDocTimeoutCancelled )
            , mField String.fromInt ( "companyidledoctimeouttimedout", settings.idleDocTimeoutTimeout )
            , mField String.fromInt ( "companyidledoctimeoutrejected", settings.idleDocTimeoutRejected )
            , mField String.fromInt ( "companyidledoctimeouterror", settings.idleDocTimeoutError )
            ]


mField : (value -> String) -> ( String, Maybe value ) -> Maybe ( String, String )
mField toString tuple =
    case tuple of
        ( _, Nothing ) ->
            Nothing

        ( name, Just value ) ->
            Just ( name, toString value )
