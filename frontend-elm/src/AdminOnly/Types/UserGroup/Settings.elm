module AdminOnly.Types.UserGroup.Settings exposing
    ( Settings
    , decoder
    , formValues
    )

import AdminOnly.Types.UserGroup.DigitalSignatureMethod as DigitalSignatureMethod exposing (DigitalSignatureMethod)
import AdminOnly.Types.UserGroup.PadAppMode as PadAppMode exposing (PadAppMode)
import AdminOnly.Types.UserGroup.SEBankIDSigningProviderOverride as SEBankIDSigningProviderOverride exposing (SEBankIDSigningProviderOverride)
import AdminOnly.Types.UserGroup.SmsProvider as SmsProvider exposing (SmsProvider)
import EnumExtra as Enum
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP
import Maybe as M
import Utils exposing (boolToJson)


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
    , digitalSignatureMethod : DigitalSignatureMethod
    , hasPostSignview : Bool
    , eidUseForSEView : Bool
    , appFrontend : Bool
    , seBankIDSigningOverride : SEBankIDSigningProviderOverride
    , padesCredentialsLabel : Maybe String
    }


decoder : Decoder Settings
decoder =
    JD.succeed Settings
        |> DP.required "ipaddressmasklist" JD.string
        |> DP.required "cgidisplayname" (JD.string |> JD.nullable)
        |> DP.required "cgiserviceid" (JD.string |> JD.nullable)
        |> DP.required "smsprovider" (JD.string |> JD.andThen SmsProvider.decoder)
        |> DP.required "padappmode" (JD.string |> JD.andThen PadAppMode.decoder)
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
        |> DP.required "sealingmethod" (JD.string |> JD.andThen DigitalSignatureMethod.decoder)
        |> DP.required "haspostsignview" JD.bool
        |> DP.required "eiduseforseview" JD.bool
        |> DP.required "appfrontend" JD.bool
        |> DP.required "seBankIDSigningOverride" SEBankIDSigningProviderOverride.decoder
        |> DP.required "padescredentialslabel" (JD.string |> JD.nullable)


formValues : Settings -> List ( String, String )
formValues settings =
    let
        fromIntWithEmpty mVal =
            M.map String.fromInt mVal |> M.withDefault ""
    in
    [ ( "companyipaddressmasklist", settings.ipAddressMaskList )
    , ( "companysmsprovider", Enum.toString SmsProvider.enum settings.smsProvider )
    , ( "companypadappmode", Enum.toString PadAppMode.enum settings.padAppMode )
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
    , ( "companysealingmethod", Enum.toString DigitalSignatureMethod.enum settings.digitalSignatureMethod )
    , ( "companyhaspostsignview", boolToJson settings.hasPostSignview )
    , ( "companyeiduseforseview", boolToJson settings.eidUseForSEView )
    , ( "companyappfrontend", boolToJson settings.appFrontend )
    , ( "companycgiserviceid", M.withDefault "" settings.cgiServiceID )
    , ( "companycgidisplayname", M.withDefault "" settings.cgiDisplayName )
    , ( "companysebankidsigningoverride", Enum.toString SEBankIDSigningProviderOverride.enum settings.seBankIDSigningOverride )
    , ( "companypadescredentialslabel", M.withDefault "" settings.padesCredentialsLabel )
    ]
