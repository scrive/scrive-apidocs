module AdminOnly.UserGroupAdmin.Subscription exposing
    ( FeatureFlags
    , Features
    , Invoicing(..)
    , InvoicingType(..)
    , PaymentPlan(..)
    , Subscription
    , enumInvoicingType
    , enumPaymentPlan
    , getFF
    , invoicingPaymentPlan
    , invoicingType
    , setFF
    , setFeaturesIsInherited
    , setFreeDocumentTokenData
    , setInvoicingType
    , setPaymentPlan
    , subscriptionDecoder
    , toPostJson
    )

import Dict as D
import EnumExtra exposing (Enum, findEnumValue, makeEnum)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List as L
import Maybe as M
import Result exposing (toMaybe)
import Time exposing (Month(..))
import Time.Date exposing (Date(..), date)
import Time.Iso8601 exposing (fromDate, toDate)
import Url.Parser exposing (map)
import Utils exposing (Status(..), isJust, ite)


type alias Subscription =
    { features : Features

    -- we do not have featuresIsInherited property to avoid invalid state,
    -- where featuresIsInherited == True and inheritableFeatures == Nothing
    , mInheritableFeatures : Maybe Features -- feature which we can inherit
    , mInheritedFeatures : Maybe Features -- features which we are inheriting
    , mInheritablePlan : Maybe PaymentPlan
    , numberOfUsers : Int
    , freeDocumentTokens : Int
    , freeDocumentTokensValidity : Date
    , invoicing : Invoicing
    }


setFeaturesIsInherited : Bool -> Subscription -> Maybe Subscription
setFeaturesIsInherited isInherited sub =
    if isInherited then
        M.map (\inheritableFeatures -> { sub | mInheritedFeatures = Just inheritableFeatures })
            sub.mInheritableFeatures

    else
        Just { sub | mInheritedFeatures = Nothing }


asFeaturesIn : Subscription -> Features -> Subscription
asFeaturesIn sub features =
    { sub | features = features }


asRegularUsersIn : Features -> FeatureFlags -> Features
asRegularUsersIn f ff =
    { f | regularUsers = ff }


asAdminUsersIn : Features -> FeatureFlags -> Features
asAdminUsersIn f ff =
    { f | adminUsers = ff }


setFF : Bool -> String -> Bool -> Subscription -> Subscription
setFF isAdmin ffKey ffValue subscription =
    let
        ( ff, asUsersIn ) =
            if isAdmin then
                ( subscription.features.adminUsers, asAdminUsersIn )

            else
                ( subscription.features.regularUsers, asRegularUsersIn )
    in
    case D.get ffKey (D.fromList ffKeyAccessPairs) of
        Nothing ->
            subscription

        Just ( _, setFlag ) ->
            setFlag ff ffValue
                |> asUsersIn subscription.features
                |> asFeaturesIn subscription


getFF : Bool -> String -> Features -> Maybe Bool
getFF isAdmin ffKey features =
    let
        ff =
            ite isAdmin features.adminUsers features.regularUsers
    in
    D.get ffKey (D.fromList ffKeyAccessPairs)
        |> M.map (\( getFlag, _ ) -> getFlag ff)


type alias Features =
    { adminUsers : FeatureFlags
    , regularUsers : FeatureFlags
    }


type alias FeatureFlags =
    { canUseSmsInvitations : Bool
    , canUseNOAuthenticationToView : Bool
    , canUseApiInvitations : Bool
    , canUseStandardAuthenticationToView : Bool
    , canUseSEAuthenticationToView : Bool
    , canUseEmailConfirmations : Bool
    , canUseStandardAuthenticationToSign : Bool
    , canUseEmailInvitations : Bool
    , canUseMassSendout : Bool
    , canUseNOAuthenticationToSign : Bool
    , canUseForwarding : Bool
    , canUseBranding : Bool
    , canUseTemplates : Bool
    , canUseShareableLinks : Bool
    , canUsePadInvitations : Bool
    , canUseDKCPRAuthenticationToSign : Bool
    , canUseDKPIDAuthenticationToSign : Bool
    , canUseDKCVRAuthenticationToSign : Bool
    , canUseAuthorAttachments : Bool
    , canUseSmsPinAuthenticationToSign : Bool
    , canUseSignatoryAttachments : Bool
    , canUseSmsPinAuthenticationToView : Bool
    , canUseSEAuthenticationToSign : Bool
    , canUseDKCPRAuthenticationToView : Bool
    , canUseDKPIDAuthenticationToView : Bool
    , canUseDKCVRAuthenticationToView : Bool
    , canUseSmsConfirmations : Bool
    , canUseFIAuthenticationToView : Bool
    , canUseFIAuthenticationToSign : Bool
    , canUseDocumentPartyNotifications : Bool
    , canUseVerimiAuthenticationToView : Bool
    , canUseVerimiQesToSign : Bool
    , canUseIdinAuthenticationToView : Bool
    , canUseIdinAuthenticationToSign : Bool
    , canUseOnfidoAuthenticationToSign : Bool
    , canUsePortal : Bool
    , canUseCustomSMSTexts : Bool
    , canUseArchiveToDropBox : Bool
    , canUseArchiveToGoogleDrive : Bool
    , canUseArchiveToOneDrive : Bool
    , canUseArchiveToSharePoint : Bool
    , canUseArchiveToSftp : Bool
    }



-- SUBSCRIPTION


subscriptionDecoder : Decoder Subscription
subscriptionDecoder =
    JD.map2 (\a b -> ( a, b ))
        (JD.field "features_is_inherited" JD.bool)
        (JD.maybe (JD.field "inherited_features" featuresDecoder))
        |> JD.andThen
            (\( featuresIsInherited, mInheritableFeatures ) ->
                JD.map8 Subscription
                    (currentFeaturesDecoder featuresIsInherited mInheritableFeatures)
                    -- inheritable features
                    (JD.succeed mInheritableFeatures)
                    -- actually inherited features
                    (inheritedFeaturesDecoder featuresIsInherited mInheritableFeatures)
                    (JD.maybe (JD.field "inherited_plan" paymentPlanDecoder))
                    (JD.field "number_of_users" JD.int)
                    (JD.field "free_document_tokens" JD.int)
                    (JD.map (M.withDefault (date 2020 1 1) << toMaybe << toDate) (JD.field "free_document_tokens_valid_till" JD.string))
                    invoicingDecoder
            )


toPostJson : Subscription -> JE.Value
toPostJson sub =
    JE.object
        ([ ( "invoicing_type"
           , sub.invoicing
                |> invoicingType
                |> encodeInvoicingType
                |> JE.string
           )
         , ( "features_is_inherited"
           , JE.bool <| isJust sub.mInheritedFeatures
           )
         , ( "payment_plan"
           , sub.invoicing
                |> invoicingPaymentPlan
                |> M.map (JE.string << encodePaymentPlan)
                |> M.withDefault JE.null
           )
         , ( "features"
           , fToJson sub.features
                |> ite (isJust sub.mInheritedFeatures) JE.null
           )
         ]
            ++ (case sub.invoicing of
                    InvoicingInvoice Free ->
                        [ ( "free_document_tokens", JE.int sub.freeDocumentTokens ), ( "free_document_tokens_valid_till", JE.string (fromDate sub.freeDocumentTokensValidity ++ "T23:59:59.000Z") ) ]

                    _ ->
                        []
               )
        )


setInvoicingType : InvoicingType -> Subscription -> Subscription
setInvoicingType invType sub =
    case ( invType, sub.invoicing, sub.mInheritablePlan ) of
        ( InvoicingTypeNone, _, Nothing ) ->
            sub

        ( InvoicingTypeNone, _, Just _ ) ->
            { sub | invoicing = InvoicingNone }

        ( InvoicingTypeBillItem, _, Nothing ) ->
            sub

        ( InvoicingTypeBillItem, invoicing, Just _ ) ->
            { sub | invoicing = InvoicingBillItem <| invoicingPaymentPlan invoicing }

        ( InvoicingTypeInvoice, invoicing, mPP ) ->
            { sub
                | invoicing =
                    invoicingPaymentPlan invoicing
                        |> M.map Just
                        |> M.withDefault mPP
                        |> M.withDefault Free
                        |> InvoicingInvoice
            }


setFreeDocumentTokenData : Int -> Date -> Subscription -> Subscription
setFreeDocumentTokenData ft ftd sub =
    { sub | freeDocumentTokens = ft, freeDocumentTokensValidity = ftd }


setPaymentPlan : Maybe PaymentPlan -> Subscription -> Subscription
setPaymentPlan mPP sub =
    case sub.invoicing of
        InvoicingNone ->
            sub

        InvoicingBillItem _ ->
            { sub | invoicing = InvoicingBillItem mPP }

        InvoicingInvoice _ ->
            case mPP of
                Nothing ->
                    sub

                Just pp ->
                    { sub | invoicing = InvoicingInvoice pp }



-- FEATURE FLAGS


defaultFeatureFlags : FeatureFlags
defaultFeatureFlags =
    { canUseSmsInvitations = False
    , canUseNOAuthenticationToView = False
    , canUseApiInvitations = False
    , canUseStandardAuthenticationToView = False
    , canUseSEAuthenticationToView = False
    , canUseEmailConfirmations = False
    , canUseStandardAuthenticationToSign = False
    , canUseEmailInvitations = False
    , canUseMassSendout = False
    , canUseNOAuthenticationToSign = False
    , canUseForwarding = False
    , canUseBranding = False
    , canUseTemplates = False
    , canUseShareableLinks = False
    , canUsePadInvitations = False
    , canUseDKCPRAuthenticationToSign = False
    , canUseDKPIDAuthenticationToSign = False
    , canUseDKCVRAuthenticationToSign = False
    , canUseAuthorAttachments = False
    , canUseSmsPinAuthenticationToSign = False
    , canUseSignatoryAttachments = False
    , canUseSmsPinAuthenticationToView = False
    , canUseSEAuthenticationToSign = False
    , canUseDKCPRAuthenticationToView = False
    , canUseDKPIDAuthenticationToView = False
    , canUseDKCVRAuthenticationToView = False
    , canUseSmsConfirmations = False
    , canUseFIAuthenticationToView = False
    , canUseFIAuthenticationToSign = False
    , canUseDocumentPartyNotifications = False
    , canUseVerimiAuthenticationToView = False
    , canUseVerimiQesToSign = False
    , canUseIdinAuthenticationToView = False
    , canUseIdinAuthenticationToSign = False
    , canUseOnfidoAuthenticationToSign = False
    , canUsePortal = False
    , canUseCustomSMSTexts = False
    , canUseArchiveToDropBox = False
    , canUseArchiveToGoogleDrive = False
    , canUseArchiveToOneDrive = False
    , canUseArchiveToSharePoint = False
    , canUseArchiveToSftp = False
    }


ffKeyAccessPairs : List ( String, ( FeatureFlags -> Bool, FeatureFlags -> Bool -> FeatureFlags ) )
ffKeyAccessPairs =
    [ ( "can_use_sms_invitations", ( .canUseSmsInvitations, \ff v -> { ff | canUseSmsInvitations = v } ) )
    , ( "can_use_no_authentication_to_view", ( .canUseNOAuthenticationToView, \ff v -> { ff | canUseNOAuthenticationToView = v } ) )
    , ( "can_use_api_invitations", ( .canUseApiInvitations, \ff v -> { ff | canUseApiInvitations = v } ) )
    , ( "can_use_standard_authentication_to_view", ( .canUseStandardAuthenticationToView, \ff v -> { ff | canUseStandardAuthenticationToView = v } ) )
    , ( "can_use_se_authentication_to_view", ( .canUseSEAuthenticationToView, \ff v -> { ff | canUseSEAuthenticationToView = v } ) )
    , ( "can_use_email_confirmations", ( .canUseEmailConfirmations, \ff v -> { ff | canUseEmailConfirmations = v } ) )
    , ( "can_use_standard_authentication_to_sign", ( .canUseStandardAuthenticationToSign, \ff v -> { ff | canUseStandardAuthenticationToSign = v } ) )
    , ( "can_use_email_invitations", ( .canUseEmailInvitations, \ff v -> { ff | canUseEmailInvitations = v } ) )
    , ( "can_use_mass_sendout", ( .canUseMassSendout, \ff v -> { ff | canUseMassSendout = v } ) )
    , ( "can_use_no_authentication_to_sign", ( .canUseNOAuthenticationToSign, \ff v -> { ff | canUseNOAuthenticationToSign = v } ) )
    , ( "can_use_forwarding", ( .canUseForwarding, \ff v -> { ff | canUseForwarding = v } ) )
    , ( "can_use_branding", ( .canUseBranding, \ff v -> { ff | canUseBranding = v } ) )
    , ( "can_use_templates", ( .canUseTemplates, \ff v -> { ff | canUseTemplates = v } ) )
    , ( "can_use_shareable_links", ( .canUseShareableLinks, \ff v -> { ff | canUseShareableLinks = v } ) )
    , ( "can_use_pad_invitations", ( .canUsePadInvitations, \ff v -> { ff | canUsePadInvitations = v } ) )
    , ( "can_use_dk_cpr_authentication_to_sign", ( .canUseDKCPRAuthenticationToSign, \ff v -> { ff | canUseDKCPRAuthenticationToSign = v } ) )
    , ( "can_use_dk_pid_authentication_to_sign", ( .canUseDKPIDAuthenticationToSign, \ff v -> { ff | canUseDKPIDAuthenticationToSign = v } ) )
    , ( "can_use_dk_cvr_authentication_to_sign", ( .canUseDKCVRAuthenticationToSign, \ff v -> { ff | canUseDKCVRAuthenticationToSign = v } ) )
    , ( "can_use_author_attachments", ( .canUseAuthorAttachments, \ff v -> { ff | canUseAuthorAttachments = v } ) )
    , ( "can_use_sms_pin_authentication_to_sign", ( .canUseSmsPinAuthenticationToSign, \ff v -> { ff | canUseSmsPinAuthenticationToSign = v } ) )
    , ( "can_use_signatory_attachments", ( .canUseSignatoryAttachments, \ff v -> { ff | canUseSignatoryAttachments = v } ) )
    , ( "can_use_sms_pin_authentication_to_view", ( .canUseSmsPinAuthenticationToView, \ff v -> { ff | canUseSmsPinAuthenticationToView = v } ) )
    , ( "can_use_se_authentication_to_sign", ( .canUseSEAuthenticationToSign, \ff v -> { ff | canUseSEAuthenticationToSign = v } ) )
    , ( "can_use_dk_cpr_authentication_to_view", ( .canUseDKCPRAuthenticationToView, \ff v -> { ff | canUseDKCPRAuthenticationToView = v } ) )
    , ( "can_use_dk_pid_authentication_to_view", ( .canUseDKPIDAuthenticationToView, \ff v -> { ff | canUseDKPIDAuthenticationToView = v } ) )
    , ( "can_use_dk_cvr_authentication_to_view", ( .canUseDKCVRAuthenticationToView, \ff v -> { ff | canUseDKCVRAuthenticationToView = v } ) )
    , ( "can_use_sms_confirmations", ( .canUseSmsConfirmations, \ff v -> { ff | canUseSmsConfirmations = v } ) )
    , ( "can_use_fi_authentication_to_view", ( .canUseFIAuthenticationToView, \ff v -> { ff | canUseFIAuthenticationToView = v } ) )
    , ( "can_use_fi_authentication_to_sign", ( .canUseFIAuthenticationToSign, \ff v -> { ff | canUseFIAuthenticationToSign = v } ) )
    , ( "can_use_document_party_notifications", ( .canUseDocumentPartyNotifications, \ff v -> { ff | canUseDocumentPartyNotifications = v } ) )
    , ( "can_use_verimi_authentication_to_view", ( .canUseVerimiAuthenticationToView, \ff v -> { ff | canUseVerimiAuthenticationToView = v } ) )
    , ( "can_use_verimi_qes_to_sign", ( .canUseVerimiQesToSign, \ff v -> { ff | canUseVerimiQesToSign = v } ) )
    , ( "can_use_idin_authentication_to_view", ( .canUseIdinAuthenticationToView, \ff v -> { ff | canUseIdinAuthenticationToView = v } ) )
    , ( "can_use_idin_authentication_to_sign", ( .canUseIdinAuthenticationToSign, \ff v -> { ff | canUseIdinAuthenticationToSign = v } ) )
    , ( "can_use_onfido_authentication_to_sign", ( .canUseOnfidoAuthenticationToSign, \ff v -> { ff | canUseOnfidoAuthenticationToSign = v } ) )
    , ( "can_use_portal", ( .canUsePortal, \ff v -> { ff | canUsePortal = v } ) )
    , ( "can_use_custom_sms_texts", ( .canUseCustomSMSTexts, \ff v -> { ff | canUseCustomSMSTexts = v } ) )
    , ( "can_use_archive_to_drop_box", ( .canUseArchiveToDropBox, \ff v -> { ff | canUseArchiveToDropBox = v } ) )
    , ( "can_use_archive_to_google_drive", ( .canUseArchiveToGoogleDrive, \ff v -> { ff | canUseArchiveToGoogleDrive = v } ) )
    , ( "can_use_archive_to_one_drive", ( .canUseArchiveToOneDrive, \ff v -> { ff | canUseArchiveToOneDrive = v } ) )
    , ( "can_use_archive_to_share_point", ( .canUseArchiveToSharePoint, \ff v -> { ff | canUseArchiveToSharePoint = v } ) )
    , ( "can_use_archive_to_sftp", ( .canUseArchiveToSftp, \ff v -> { ff | canUseArchiveToSftp = v } ) )
    ]


featureFlagsDecoder : Decoder FeatureFlags
featureFlagsDecoder =
    let
        decodeOne ffDict ( key, ( _, ffSet ) ) mFF =
            mFF
                |> M.andThen (\ff -> D.get key ffDict |> M.map (\value -> ffSet ff value))

        ffFromDict ffDict =
            case L.foldl (decodeOne ffDict) (Just defaultFeatureFlags) ffKeyAccessPairs of
                Nothing ->
                    JD.fail "Some of FeatureFlag keys were missing. Impossible !!! We already compared the keys"

                Just ff ->
                    JD.succeed ff
    in
    JD.dict JD.bool |> JD.andThen ffFromDict


currentFeaturesDecoder : Bool -> Maybe Features -> Decoder Features
currentFeaturesDecoder featuresIsInherited mInheritableFeatures =
    case ( featuresIsInherited, mInheritableFeatures ) of
        ( False, _ ) ->
            JD.field "features" featuresDecoder

        ( True, Nothing ) ->
            JD.fail "Features should be inherited, but there is nothing to inherit"

        -- initialize with what was inherited
        ( True, Just features ) ->
            JD.succeed features


featuresDecoder : Decoder Features
featuresDecoder =
    JD.map2 Features
        (JD.field "admin_users" featureFlagsDecoder)
        (JD.field "regular_users" featureFlagsDecoder)


ffToJson : FeatureFlags -> JE.Value
ffToJson ff =
    ffKeyAccessPairs
        |> L.map (\( name, ( getFlag, _ ) ) -> ( name, JE.bool <| getFlag ff ))
        |> JE.object


fToJson : Features -> JE.Value
fToJson features =
    JE.object
        [ ( "admin_users"
          , ffToJson features.adminUsers
          )
        , ( "regular_users", ffToJson features.regularUsers )
        ]


inheritedFeaturesDecoder : Bool -> Maybe Features -> Decoder (Maybe Features)
inheritedFeaturesDecoder featuresIsInherited mInheritableFeatures =
    case ( featuresIsInherited, mInheritableFeatures ) of
        ( True, Just features ) ->
            JD.succeed <| Just features

        ( False, _ ) ->
            JD.succeed <| Nothing

        _ ->
            JD.fail "Features should be inherited, but there is nothing to inherit"



-- INVOICING


invoicingDecoder : Decoder Invoicing
invoicingDecoder =
    let
        mk3 a b c =
            ( a, b, c )

        preDecoder : Decoder ( String, Maybe PaymentPlan, Maybe PaymentPlan )
        preDecoder =
            JD.map3 mk3
                (JD.field "invoicing_type" JD.string)
                (JD.maybe (JD.field "payment_plan" paymentPlanDecoder))
                (JD.maybe (JD.field "inherited_plan" paymentPlanDecoder))
    in
    preDecoder
        |> JD.andThen
            (\preResult ->
                case preResult of
                    ( "none", Nothing, _ ) ->
                        JD.succeed InvoicingNone

                    ( "billitem", Just pp, _ ) ->
                        JD.succeed <| InvoicingBillItem (Just pp)

                    ( "billitem", Nothing, Just _ ) ->
                        JD.succeed <| InvoicingBillItem Nothing

                    ( "invoice", Just pp, _ ) ->
                        JD.succeed <| InvoicingInvoice pp

                    _ ->
                        JD.fail "Invalid invoicing"
            )



-- PAYMENT PLAN


type PaymentPlan
    = Free
    | One
    | Team
    | Enterprise
    | Trial


paymentPlanDecoder : Decoder PaymentPlan
paymentPlanDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case findEnumValue enumPaymentPlan str of
                    Err _ ->
                        JD.fail <| "Cannot parse payment plan: " ++ str

                    Ok paymentPlan ->
                        JD.succeed paymentPlan
            )


encodePaymentPlan : PaymentPlan -> String
encodePaymentPlan paymentPlan =
    case paymentPlan of
        Free ->
            "free"

        One ->
            "one"

        Team ->
            "team"

        Enterprise ->
            "enterprise"

        Trial ->
            "trial"


fromPaymentPlan : PaymentPlan -> String
fromPaymentPlan paymentPlan =
    case paymentPlan of
        Free ->
            "Free"

        One ->
            "One"

        Team ->
            "Team"

        Enterprise ->
            "Enterprise"

        Trial ->
            "Trial"


enumPaymentPlan : Enum PaymentPlan
enumPaymentPlan =
    makeEnum [ Free, One, Team, Enterprise, Trial ] encodePaymentPlan fromPaymentPlan


type Invoicing
    = InvoicingNone
    | InvoicingBillItem (Maybe PaymentPlan)
    | InvoicingInvoice PaymentPlan


type InvoicingType
    = InvoicingTypeNone
    | InvoicingTypeBillItem
    | InvoicingTypeInvoice


encodeInvoicingType : InvoicingType -> String
encodeInvoicingType invType =
    case invType of
        InvoicingTypeNone ->
            "none"

        InvoicingTypeBillItem ->
            "billitem"

        InvoicingTypeInvoice ->
            "invoice"


fromInvoicingType : InvoicingType -> String
fromInvoicingType invType =
    case invType of
        InvoicingTypeNone ->
            "None"

        InvoicingTypeBillItem ->
            "BillItem"

        InvoicingTypeInvoice ->
            "Invoice"


enumInvoicingType : Enum InvoicingType
enumInvoicingType =
    makeEnum [ InvoicingTypeNone, InvoicingTypeBillItem, InvoicingTypeInvoice ]
        encodeInvoicingType
        fromInvoicingType


invoicingPaymentPlan : Invoicing -> Maybe PaymentPlan
invoicingPaymentPlan inv =
    case inv of
        InvoicingNone ->
            Nothing

        InvoicingBillItem mPP ->
            mPP

        InvoicingInvoice pp ->
            Just pp


invoicingType : Invoicing -> InvoicingType
invoicingType inv =
    case inv of
        InvoicingNone ->
            InvoicingTypeNone

        InvoicingBillItem _ ->
            InvoicingTypeBillItem

        InvoicingInvoice _ ->
            InvoicingTypeInvoice
