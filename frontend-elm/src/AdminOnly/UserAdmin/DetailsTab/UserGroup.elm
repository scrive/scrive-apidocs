module AdminOnly.UserAdmin.DetailsTab.UserGroup exposing
    ( Address
    , PadAppMode(..)
    , Settings
    , SmsProvider(..)
    , UserGroup
    , decoder
    , enumPadAppMode
    , enumSmsProvider
    , formValues
    , padAppModeDecoder
    , setBoolField
    , setIntField
    , setStringField
    , smsProviderDecoder
    )

import EnumExtra exposing (Enum, findEnumValue, makeEnum)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP
import List as L
import Maybe as M
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
    }


decoder : Decoder UserGroup
decoder =
    D.succeed UserGroup
        |> DP.required "companyid" D.string
        |> DP.required "companyname" D.string
        |> DP.required "parentid" (D.string |> D.nullable)
        |> DP.required "parentgrouppath" (D.list parentUserGroupDecoder)
        |> DP.custom addressDecoder
        |> DP.required "companyaddressisinherited" D.bool
        |> DP.optional "companyinheritedaddress" (addressDecoder |> D.nullable) Nothing
        |> DP.custom settingsDecoder
        |> DP.required "companysettingsisinherited" D.bool
        |> DP.optional "companyinheritedsettings" (settingsDecoder |> D.nullable) Nothing


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
    D.succeed Address
        |> DP.required "address" D.string
        |> DP.required "city" D.string
        |> DP.required "country" D.string
        |> DP.required "zip" D.string
        |> DP.required "companynumber" D.string



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
    D.succeed Settings
        |> DP.required "ipaddressmasklist" D.string
        |> DP.required "cgidisplayname" (D.string |> D.nullable)
        |> DP.required "cgiserviceid" (D.string |> D.nullable)
        |> DP.required "smsprovider" (D.string |> D.andThen smsProviderDecoder)
        |> DP.required "padappmode" (D.string |> D.andThen padAppModeDecoder)
        |> DP.required "padearchiveenabled" D.bool
        |> DP.required "idledoctimeoutpreparation" (D.int |> D.nullable)
        |> DP.required "idledoctimeoutclosed" (D.int |> D.nullable)
        |> DP.required "idledoctimeoutcanceled" (D.int |> D.nullable)
        |> DP.required "idledoctimeouttimedout" (D.int |> D.nullable)
        |> DP.required "idledoctimeoutrejected" (D.int |> D.nullable)
        |> DP.required "idledoctimeouterror" (D.int |> D.nullable)
        |> DP.required "immediatetrash" D.bool


type alias ParentUserGroup =
    { id : String
    , name : String
    }


parentUserGroupDecoder : Decoder ParentUserGroup
parentUserGroupDecoder =
    D.map2 ParentUserGroup
        (D.field "group_id" D.string)
        (D.field "group_name" D.string)



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
            D.fail <| "Cannot parse pad app mode: " ++ padAppModeString

        Ok padAppMode ->
            D.succeed padAppMode


enumPadAppMode : Enum PadAppMode
enumPadAppMode =
    makeEnum allPadAppModes encodePadAppMode fromPadAppMode



-- SMS Provider


smsProviderDecoder : String -> Decoder SmsProvider
smsProviderDecoder smsProviderString =
    case findEnumValue enumSmsProvider smsProviderString of
        Err _ ->
            D.fail <| "Cannot parse sms provider: " ++ smsProviderString

        Ok smsProvider ->
            D.succeed smsProvider


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
