module AdminOnly.UserGroupAdmin.PaymentsTab exposing
    ( Model
    , Msg
    , init
    , setUserGroupID
    , tabName
    , update
    , view
    )

import AdminOnly.UserGroupAdmin.Subscription as Subscription
    exposing
        ( Features
        , Invoicing(..)
        , InvoicingType(..)
        , PaymentPlan(..)
        , Subscription
        , enumInvoicingType
        , enumPaymentPlan
        , invoicingPaymentPlan
        , invoicingType
        , setFeaturesIsInherited
        , setFreeDocumentTokenData
        , setInvoicingType
        , setPaymentPlan
        , subscriptionDecoder
        )
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import EnumExtra as Enum exposing (Enum, findEnumValue)
import FlashMessage
import Html exposing (Html, div, h4, hr, strong, text)
import Html.Attributes exposing (checked, class, disabled, readonly, selected, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Encode as JE
import List as L
import Maybe as M
import Result as Result
import Time exposing (Month(..))
import Time.Date as Date
import Time.Iso8601 as Time
import Url.Parser exposing (map)
import Utils exposing (..)


type alias FormData =
    { freeDocumentTokensInput : String
    , freeDocumentTokensValidityInput : String
    }


type alias Model =
    { sSubscription : Status Subscription
    , sFormData : Maybe FormData
    , ugid : String
    }


type Msg
    = GotSubscription (Result Http.Error Subscription)
      -- EDIT Subscription
    | SetFeatureFlag Bool String Bool
    | SetInvoicingType String
    | SetPaymentPlan String
    | SetFeaturesIsInherited Bool
    | FreeDocumentTokenInputChange String
    | FreeDocumentTokenInputValidityInputChange String
    | SubmitForm
    | GotSaveResponse (Result Http.Error String)


tabName : String
tabName =
    "payments"


init : (Msg -> msg) -> String -> ( Model, Cmd msg )
init embed ugid =
    let
        model =
            { sSubscription = Loading
            , sFormData = Nothing
            , ugid = ugid
            }
    in
    ( model, Cmd.map embed <| getSubscriptionCmd model )


getFormDataFromSubscription : Subscription -> FormData
getFormDataFromSubscription sub =
    { freeDocumentTokensInput = String.fromInt sub.freeDocumentTokens
    , freeDocumentTokensValidityInput = Time.fromDate sub.freeDocumentTokensValidity
    }


getSubscriptionCmd : Model -> Cmd Msg
getSubscriptionCmd model =
    Http.get
        { url = "/adminonly/companyadmin/getsubscription/" ++ model.ugid
        , expect = Http.expectJson GotSubscription subscriptionDecoder
        }


setUserGroupID : (Msg -> msg) -> String -> Model -> ( Model, Cmd msg )
setUserGroupID embed ugid model0 =
    let
        model =
            { model0 | ugid = ugid }
    in
    ( model, Cmd.map embed <| getSubscriptionCmd model )


modifySubscription : (Subscription -> Subscription) -> Model -> Model
modifySubscription modify model =
    { model | sSubscription = statusMap modify model.sSubscription }


setFreeFeatures : Features -> Features
setFreeFeatures features =
    let
        adminFeatures1 =
            features.adminUsers

        adminFeatures2 =
            { adminFeatures1
                | canUseDKAuthenticationToSign = False
                , canUseDKCPRAuthenticationToView = False
                , canUseDKPIDAuthenticationToView = False
                , canUseDKCVRAuthenticationToView = False
                , canUseFIAuthenticationToView = False
                , canUseFIAuthenticationToSign = False
                , canUseNOAuthenticationToView = False
                , canUseNOAuthenticationToSign = False
                , canUseSEAuthenticationToView = False
                , canUseSEAuthenticationToSign = False
                , canUseVerimiAuthenticationToView = False
                , canUseVerimiQesToSign = False
                , canUseIdinAuthenticationToView = False
                , canUseIdinAuthenticationToSign = False
                , canUseOnfidoAuthenticationToSign = False
            }

        userFeatures1 =
            features.regularUsers

        userFeatures2 =
            { userFeatures1
                | canUseDKCPRAuthenticationToView = False
                , canUseDKPIDAuthenticationToView = False
                , canUseDKCVRAuthenticationToView = False
                , canUseDKAuthenticationToSign = False
                , canUseFIAuthenticationToView = False
                , canUseFIAuthenticationToSign = False
                , canUseNOAuthenticationToView = False
                , canUseNOAuthenticationToSign = False
                , canUseSEAuthenticationToView = False
                , canUseSEAuthenticationToSign = False
                , canUseVerimiAuthenticationToView = False
                , canUseVerimiQesToSign = False
                , canUseIdinAuthenticationToView = False
                , canUseIdinAuthenticationToSign = False
                , canUseOnfidoAuthenticationToSign = False
                , canUsePortal = False
            }
    in
    { adminUsers = adminFeatures2
    , regularUsers = userFeatures2
    }


setPaidFeatures : Features -> Features
setPaidFeatures features =
    let
        adminFeatures1 =
            features.adminUsers

        adminFeatures2 =
            { adminFeatures1
                | canUseDKAuthenticationToSign = True
                , canUseDKCPRAuthenticationToView = True
                , canUseDKPIDAuthenticationToView = True
                , canUseDKCVRAuthenticationToView = True
                , canUseFIAuthenticationToView = True
                , canUseFIAuthenticationToSign = True
                , canUseNOAuthenticationToView = True
                , canUseNOAuthenticationToSign = True
                , canUseSEAuthenticationToView = True
                , canUseSEAuthenticationToSign = True
                , canUseVerimiAuthenticationToView = True
                , canUseVerimiQesToSign = True
                , canUseIdinAuthenticationToView = True
                , canUseIdinAuthenticationToSign = True
                , canUseOnfidoAuthenticationToSign = True
            }

        userFeatures1 =
            features.regularUsers

        userFeatures2 =
            { userFeatures1
                | canUseDKCPRAuthenticationToView = True
                , canUseDKPIDAuthenticationToView = True
                , canUseDKCVRAuthenticationToView = True
                , canUseDKAuthenticationToSign = True
                , canUseFIAuthenticationToView = True
                , canUseFIAuthenticationToSign = True
                , canUseNOAuthenticationToView = True
                , canUseNOAuthenticationToSign = True
                , canUseSEAuthenticationToView = True
                , canUseSEAuthenticationToSign = True
                , canUseVerimiAuthenticationToView = True
                , canUseVerimiQesToSign = True
                , canUseIdinAuthenticationToView = True
                , canUseIdinAuthenticationToSign = True
                , canUseOnfidoAuthenticationToSign = True
            }
    in
    { adminUsers = adminFeatures2
    , regularUsers = userFeatures2
    }


validateFreeTokenFields : Model -> Result String ( Int, Date.Date )
validateFreeTokenFields model =
    let
        mInt v =
            stringNonEmpty v |> M.andThen String.toInt

        mDate v =
            stringNonEmpty v |> M.andThen (Time.toDate >> Result.toMaybe)
    in
    case model.sFormData of
        Just fd ->
            case ( mInt fd.freeDocumentTokensInput, mDate fd.freeDocumentTokensValidityInput ) of
                ( Just tc, Just tv ) ->
                    Ok ( tc, tv )

                ( Just _, Nothing ) ->
                    Err "Incorrect free tokens validity!"

                ( Nothing, Just _ ) ->
                    Err "Incorrect amount of free tokens!"

                ( Nothing, Nothing ) ->
                    Err "Incorrect free tokens data!"

        Nothing ->
            Err "Validation in incorrect state. This should not happend!"


update : (Msg -> msg) -> Globals msg -> Msg -> Model -> ( Model, Cmd msg )
update embed globals msg model =
    case msg of
        GotSubscription result ->
            case result of
                Ok subscription ->
                    ( { model
                        | sSubscription = Success subscription
                        , sFormData = Just <| getFormDataFromSubscription subscription
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | sSubscription = Failure }, Cmd.none )

        -- FORM SETTERS
        SetFeatureFlag isAdmin name value ->
            ( modifySubscription (Subscription.setFF isAdmin name value) model, Cmd.none )

        SetFeaturesIsInherited value ->
            case statusMap (setFeaturesIsInherited value) model.sSubscription of
                Success Nothing ->
                    ( model, globals.flashMessage <| FlashMessage.error "Top level user group cannot inherit" )

                Success (Just subscription) ->
                    ( { model | sSubscription = Success subscription }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetInvoicingType value ->
            case ( findEnumValue enumInvoicingType value, model.sSubscription ) of
                ( Ok invType, Success sub ) ->
                    ( { model | sSubscription = Success <| setInvoicingType invType sub }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FreeDocumentTokenInputChange value ->
            case model.sFormData of
                Just fd ->
                    let
                        incorrectValue =
                            (M.withDefault 0 <| String.toInt value) < 0

                        newValue =
                            ite incorrectValue "0" value
                    in
                    ( { model | sFormData = Just <| { fd | freeDocumentTokensInput = newValue } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FreeDocumentTokenInputValidityInputChange value ->
            case model.sFormData of
                Just fd ->
                    ( { model | sFormData = Just <| { fd | freeDocumentTokensValidityInput = value } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SetPaymentPlan value ->
            case model.sSubscription of
                Success subscription ->
                    case ( findEnumValue enumPaymentPlan value, value ) of
                        ( Ok paymentPlan, _ ) ->
                            let
                                subscription2 =
                                    setPaymentPlan (Just paymentPlan) subscription

                                subscription3 =
                                    if paymentPlan == Free then
                                        { subscription2
                                            | features = setFreeFeatures subscription2.features
                                        }

                                    else
                                        { subscription2
                                            | features = setPaidFeatures subscription2.features
                                        }
                            in
                            ( { model | sSubscription = Success subscription3 }
                            , Cmd.none
                            )

                        ( _, "inherit" ) ->
                            ( { model
                                | sSubscription =
                                    Success <|
                                        setPaymentPlan Nothing subscription
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitForm ->
            case validateFreeTokenFields model of
                Err emsg ->
                    ( model, globals.flashMessage <| FlashMessage.error emsg )

                Ok ( tc, tv ) ->
                    fromStatus model.sSubscription
                        |> M.map
                            (\subscription ->
                                ( model
                                , Http.post
                                    { url = "/adminonly/companyadmin/updatesubscription/" ++ model.ugid
                                    , body =
                                        formBody globals
                                            [ ( "subscription"
                                              , JE.encode 0 <| Subscription.toPostJson <| setFreeDocumentTokenData tc tv subscription
                                              )
                                            ]
                                    , expect = Http.expectString (embed << GotSaveResponse)
                                    }
                                )
                            )
                        |> M.withDefault ( model, Cmd.none )

        GotSaveResponse response ->
            case response of
                Err _ ->
                    ( model, globals.flashMessage <| FlashMessage.error "Request failed." )

                Ok _ ->
                    ( model
                    , Cmd.batch
                        [ globals.flashMessage <| FlashMessage.success "Saved"
                        , globals.setPageUrlFromModel -- reloads UserGroup Details
                        ]
                    )


view : (Msg -> msg) -> Model -> Html msg
view embed model =
    case ( model.sSubscription, model.sFormData ) of
        ( Loading, _ ) ->
            h4 [] [ text "Loading ..." ]

        ( Failure, _ ) ->
            h4 [] [ text "Failure ..." ]

        ( _, Nothing ) ->
            h4 [] [ text "Failure ..." ]

        ( Success subscription, Just formData ) ->
            div [] <|
                [ Html.map embed <| viewSubscription subscription formData ]


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
    List (Col.Option Msg)
    -> List (Col.Option Msg)
    -> String
    -> String
    -> List (Html Msg)
    -> Html Msg
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


featureFlagsStructure : List ( String, List ( String, String ) )
featureFlagsStructure =
    [ ( "General features"
      , [ ( "Can use templates", "can_use_templates" )
        , ( "Can use shareable links", "can_use_shareable_links" )
        , ( "Can use branding", "can_use_branding" )
        , ( "Can use mass sendout", "can_use_mass_sendout" )
        , ( "Can use email invitations", "can_use_email_invitations" )
        , ( "Can use email confirmations", "can_use_email_confirmations" )
        , ( "Can use link (i.e. API) invitations", "can_use_api_invitations" )
        , ( "Can use in-person invitations", "can_use_pad_invitations" )
        , ( "Can use forwarding", "can_use_forwarding" )
        , ( "Can use document party notifications", "can_use_document_party_notifications" )
        ]
      )
    , ( "Attachments"
      , [ ( "Can use author attachments", "can_use_author_attachments" )
        , ( "Can use signatory attachments", "can_use_signatory_attachments" )
        ]
      )
    , ( "SMS"
      , [ ( "Can use sms invitations", "can_use_sms_invitations" )
        , ( "Can use sms confirmations", "can_use_sms_confirmations" )
        , ( "Can use SMS Pin authorization to view", "can_use_sms_pin_authentication_to_view" )
        , ( "Can use SMS Pin authorization to sign", "can_use_sms_pin_authentication_to_sign" )
        , ( "Can use custom SMS texts", "can_use_custom_sms_texts" )
        ]
      )
    , ( "eID to view and sign"
      , [ ( "Can use DK CPR authorization to view", "can_use_dk_cpr_authentication_to_view" )
        , ( "Can use DK PID authorization to view", "can_use_dk_pid_authentication_to_view" )
        , ( "Can use DK CVR authorization to view", "can_use_dk_cvr_authentication_to_view" )
        , ( "Can use DK authorization to sign", "can_use_dk_authentication_to_sign" )
        , ( "Can use FI authorization to view", "can_use_fi_authentication_to_view" )
        , ( "Can use FI authorization to sign", "can_use_fi_authentication_to_sign" )
        , ( "Can use NO authorization to view", "can_use_no_authentication_to_view" )
        , ( "Can use NO authorization to sign", "can_use_no_authentication_to_sign" )
        , ( "Can use SE authorization to view", "can_use_se_authentication_to_view" )
        , ( "Can use SE authorization to sign", "can_use_se_authentication_to_sign" )
        , ( "Can use Verimi authorization to view", "can_use_verimi_authentication_to_view" )
        , ( "Can use Verimi QES to sign", "can_use_verimi_qes_to_sign" )
        , ( "Can use IDIN Authentication to view", "can_use_idin_authentication_to_view" )
        , ( "Can use IDIN Authentication to sign", "can_use_idin_authentication_to_sign" )
        , ( "Can use Onfido Authentication to sign", "can_use_onfido_authentication_to_sign" )
        ]
      )
    , ( "Standard auth to view and sign (disable to force use of eID/SMS)"
      , [ ( "Can use Standard authorization to view", "can_use_standard_authentication_to_view" )
        , ( "Can use Standard authorization to sign", "can_use_standard_authentication_to_sign" )
        ]
      )
    , ( "Portal"
      , [ ( "Can use Portal", "can_use_portal" )
        ]
      )
    
    , ( "Integrations"
      , [ ( "Can use archive to DropBox", "can_use_archive_to_drop_box" )
        , ( "Can use archive to GoogleDrive", "can_use_archive_to_google_drive" )
        , ( "Can use archive to OneDrive", "can_use_archive_to_one_drive" )
        , ( "Can use archive to SharePoint", "can_use_archive_to_share_point" )
        , ( "Can use archive to SFTP", "can_use_archive_to_sftp" )
        ]
      )
    ]


viewSubscription : Subscription -> FormData -> Html Msg
viewSubscription sub formData =
    let
        labelColAttrs =
            [ Col.sm5, Col.md4, Col.lg4 ]

        labelSmallColAttrs =
            labelColAttrs ++ [ Col.attrs [ class "col-form-label-small" ] ]

        inputColAttrs =
            [ Col.sm6, Col.md6, Col.lg4 ]

        inputColHalfAttrs =
            [ Col.sm3, Col.md3, Col.lg2 ]

        formSelectRowM label help enum val toMsg attrs =
            formRow labelColAttrs inputColAttrs label help <|
                formSelect enum val toMsg attrs

        formCheckboxRowM label help val toMsg attrs =
            formRow labelColAttrs inputColAttrs label help <|
                formCheckbox val toMsg attrs

        invType =
            invoicingType sub.invoicing

        mPaymentPlan =
            invoicingPaymentPlan sub.invoicing

        txtInheritPP =
            "Inherit ("
                ++ (M.withDefault "ERROR" <|
                        M.map (Enum.toHumanString enumPaymentPlan) <|
                            sub.mInheritablePlan
                   )
                ++ ")"

        currentPPOption =
            M.withDefault "inherit" <|
                M.map (Enum.toString enumPaymentPlan) mPaymentPlan

        optionsPP =
            Enum.allValues enumPaymentPlan
                |> L.map
                    (\pp ->
                        ( Enum.toString enumPaymentPlan pp
                        , Enum.toHumanString enumPaymentPlan pp
                        )
                    )

        currentFeatureFlag isAdmin ffKey =
            sub.mInheritedFeatures
                |> M.andThen (Subscription.getFF isAdmin ffKey)
                |> M.map Just
                |> M.withDefault (Subscription.getFF isAdmin ffKey sub.features)
                |> M.withDefault False
    in
    Grid.container [] <|
        [ Form.form [ onSubmit SubmitForm ] <|
            [ Form.row []
                [ Form.colLabel labelColAttrs [ text "Number of users" ]
                , Form.colLabel inputColAttrs [ text <| String.fromInt sub.numberOfUsers ]
                ]
            , formSelectRowM "Invoicing type"
                ""
                Subscription.enumInvoicingType
                invType
                SetInvoicingType
                [ disabled <| not <| isJust <| sub.mInheritablePlan ]
            , Form.row
                []
                [ Form.colLabel labelColAttrs [ text "Price plan" ]
                , Form.col inputColAttrs
                    [ Select.select [ Select.onChange SetPaymentPlan ] <|
                        (L.map <|
                            \( optionVal, txt ) ->
                                Select.item
                                    [ value optionVal
                                    , selected <| optionVal == currentPPOption
                                    , disabled <| invType == InvoicingTypeNone
                                    ]
                                    [ text txt ]
                        )
                        <|
                            case invType of
                                InvoicingTypeNone ->
                                    [ ( "inherit", txtInheritPP ) ]

                                InvoicingTypeBillItem ->
                                    ( "inherit", txtInheritPP ) :: optionsPP

                                InvoicingTypeInvoice ->
                                    optionsPP
                    ]
                ]
            ]
                ++ (case sub.invoicing of
                        InvoicingInvoice Free ->
                            [ Form.row
                                []
                                [ Form.colLabel labelColAttrs [ text "Number of free tokens" ]
                                , Form.colLabel inputColAttrs [ Input.number [ Input.attrs [ value <| formData.freeDocumentTokensInput, onInput <| FreeDocumentTokenInputChange ] ] ]
                                ]
                            , Form.row
                                []
                                [ Form.colLabel labelColAttrs [ text "Validity of free tokens" ]
                                , Form.colLabel inputColAttrs [ Input.date [ Input.attrs [ value <| formData.freeDocumentTokensValidityInput, onInput <| FreeDocumentTokenInputValidityInputChange ] ] ]
                                ]
                            ]

                        _ ->
                            []
                   )
                ++ [ hr [] []
                   , formCheckboxRowM "Inherit feature flags"
                        "If enabled, all feature flags will be inherited from the parent user group."
                        (isJust sub.mInheritedFeatures)
                        SetFeaturesIsInherited
                        []
                   , Form.row [ Row.attrs [ class "form-group-small" ] ]
                        [ Form.colLabel labelSmallColAttrs []
                        , Form.colLabel inputColHalfAttrs [ strong [] <| [ text "Regular users" ] ]
                        , Form.colLabel inputColHalfAttrs [ strong [] <| [ text "Admin users" ] ]
                        ]
                   ]
                ++ ((\f -> L.concatMap f featureFlagsStructure) <|
                        \( heading, flags ) ->
                            Form.row [ Row.attrs [ class "form-group-small" ] ]
                                [ Form.colLabel labelColAttrs [ strong [] <| [ text heading ] ]
                                ]
                                :: ((\f -> L.map f flags) <|
                                        \( label, ffKey ) ->
                                            Form.row [ Row.attrs [ class "form-group-small" ] ]
                                                [ Form.colLabel labelSmallColAttrs [ text label ]
                                                , Form.col inputColHalfAttrs <|
                                                    formCheckbox
                                                        (currentFeatureFlag False ffKey)
                                                        (SetFeatureFlag False ffKey)
                                                        [ readonly <| isJust sub.mInheritedFeatures ]
                                                , Form.col inputColHalfAttrs <|
                                                    formCheckbox
                                                        (currentFeatureFlag True ffKey)
                                                        (SetFeatureFlag True ffKey)
                                                        [ readonly <| isJust sub.mInheritedFeatures ]
                                                ]
                                   )
                   )
        ]
            ++ [ Grid.row [ Row.leftSm ]
                    [ Grid.col [ Col.sm12 ]
                        [ Button.button
                            [ Button.success
                            , Button.attrs [ class "ml-sm-2", onClick SubmitForm ]
                            ]
                            [ text "Save" ]
                        ]
                    ]
               ]
