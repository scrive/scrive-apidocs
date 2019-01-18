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
        ( Invoicing(..)
        , InvoicingType(..)
        , PaymentPlan(..)
        , Subscription
        , enumInvoicingType
        , enumPaymentPlan
        , invoicingPaymentPlan
        , invoicingType
        , setFeaturesIsInherited
        , setInvoicingType
        , setPaymentPlan
        , subscriptionDecoder
        )
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import EnumExtra as Enum exposing (Enum, findEnumValue)
import FlashMessage
import Html exposing (Html, div, h4, hr, strong, text)
import Html.Attributes exposing (checked, class, disabled, readonly, selected, value)
import Html.Events exposing (onCheck, onClick, onSubmit)
import Http
import Json.Encode as JE
import List as L
import Maybe as M
import Time exposing (Month(..))
import Url.Parser exposing (map)
import Utils exposing (..)


type alias Model =
    { sSubscription : Status Subscription
    , ugid : String
    }


type Msg
    = GotSubscription (Result Http.Error Subscription)
      -- EDIT Subscription
    | SetFeatureFlag Bool String Bool
    | SetInvoicingType String
    | SetPaymentPlan String
    | SetFeaturesIsInherited Bool
    | SubmitForm
    | GotSaveResponse (Result Http.Error String)


tabName : String
tabName =
    "payments"


init : String -> ( Model, Cmd Msg )
init ugid =
    let
        model =
            { sSubscription = Loading
            , ugid = ugid
            }
    in
    ( model, getSubscriptionCmd model )


getSubscriptionCmd : Model -> Cmd Msg
getSubscriptionCmd model =
    Http.get
        { url = "/adminonly/companyadmin/getsubscription/" ++ model.ugid
        , expect = Http.expectJson GotSubscription subscriptionDecoder
        }


setUserGroupID : String -> Model -> ( Model, Cmd Msg )
setUserGroupID ugid model0 =
    let
        model =
            { model0 | ugid = ugid }
    in
    ( model, getSubscriptionCmd model )


modifySubscription : (Subscription -> Subscription) -> Model -> Model
modifySubscription modify model =
    { model | sSubscription = statusMap modify model.sSubscription }


update : Globals msg -> Msg -> Model -> ( Model, Action msg Msg )
update globals msg model =
    case msg of
        GotSubscription result ->
            case result of
                Ok subscription ->
                    ( { model
                        | sSubscription = Success subscription
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
                    ( model, outerCmd <| globals.flashMessage <| FlashMessage.error "Top level user group cannot inherit" )

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

        SetPaymentPlan value ->
            let
                mmPP =
                    case ( findEnumValue enumPaymentPlan value, value ) of
                        ( Ok pp, _ ) ->
                            Just (Just pp)

                        ( _, "inherit" ) ->
                            Just Nothing

                        _ ->
                            Nothing
            in
            case ( mmPP, model.sSubscription ) of
                ( Just mPP, Success sub ) ->
                    ( { model | sSubscription = Success <| setPaymentPlan mPP sub }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitForm ->
            fromStatus model.sSubscription
                |> M.map
                    (\subscription ->
                        ( model
                        , innerCmd <|
                            Http.post
                                { url = "/adminonly/companyadmin/updatesubscription/" ++ model.ugid
                                , body =
                                    formBody globals
                                        [ ( "subscription"
                                          , JE.encode 0 <| Subscription.toPostJson subscription
                                          )
                                        ]
                                , expect = Http.expectString GotSaveResponse
                                }
                        )
                    )
                |> M.withDefault ( model, Cmd.none )

        GotSaveResponse response ->
            case response of
                Err _ ->
                    ( model, outerCmd <| globals.flashMessage <| FlashMessage.error "Request failed." )

                Ok _ ->
                    ( model
                    , outerCmd <|
                        Cmd.batch
                            [ globals.flashMessage <| FlashMessage.success "Saved"
                            , globals.setPageUrlFromModel -- reloads UserGroup Details
                            ]
                    )


view : Model -> Html Msg
view model =
    case model.sSubscription of
        Loading ->
            h4 [] [ text "Loading ..." ]

        Failure ->
            h4 [] [ text "Failure ..." ]

        Success subscription ->
            div [] <|
                [ viewSubscription subscription ]


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
        ]
      )
    , ( "eID to view and sign"
      , [ ( "Can use DK authorization to view", "can_use_dk_authentication_to_view" )
        , ( "Can use DK authorization to sign", "can_use_dk_authentication_to_sign" )
        , ( "Can use FI authorization to view", "can_use_fi_authentication_to_view" )
        , ( "Can use NO authorization to view", "can_use_no_authentication_to_view" )
        , ( "Can use NO authorization to sign", "can_use_no_authentication_to_sign" )
        , ( "Can use SE authorization to view", "can_use_se_authentication_to_view" )
        , ( "Can use SE authorization to sign", "can_use_se_authentication_to_sign" )
        , ( "Can use Verimi authorization to view", "can_use_verimi_authentication_to_view" )
        , ( "Can use IDIN Authentication to view", "can_use_idin_authentication_to_view" )
        , ( "Can use IDIN Authentication to sign", "can_use_idin_authentication_to_sign" )
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
    ]


viewSubscription : Subscription -> Html Msg
viewSubscription sub =
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
            , hr [] []
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
