module Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.ActionColors exposing (ColorFields, Config, Init, Msg, State, UpdateHandler, ViewHandler, initialize, stateToColorFields, update, view)

import Component.BrandedDomain.Data exposing (ActionColors)
import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.ColorField as Color
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias ColorFields =
    ActionColors


type alias Config =
    ColorFields


type alias State =
    Pair.State Color.State (Pair.State Color.State (Pair.State Color.State (Pair.State Color.State (Pair.State Color.State (Pair.State Color.State (Pair.State Color.State Color.State))))))


type alias Msg =
    Pair.Msg Color.Msg (Pair.Msg Color.Msg (Pair.Msg Color.Msg (Pair.Msg Color.Msg (Pair.Msg Color.Msg (Pair.Msg Color.Msg (Pair.Msg Color.Msg Color.Msg))))))


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> List (Html Msg)


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize =
    let
        inInit =
            Pair.liftInit
                Color.initialize
            <|
                Pair.liftInit
                    Color.initialize
                <|
                    Pair.liftInit
                        Color.initialize
                    <|
                        Pair.liftInit
                            Color.initialize
                        <|
                            Pair.liftInit
                                Color.initialize
                            <|
                                Pair.liftInit
                                    Color.initialize
                                <|
                                    Pair.liftInit
                                        Color.initialize
                                    <|
                                        Color.initialize
    in
    \config ->
        let
            config1 =
                { id = "branding-color-draft"
                , title = "Draft, template"
                , initialColor = config.draftColor
                }

            config2 =
                { id = "branding-color-error"
                , title = "Error, withdrawn, cancelled and timed-out"
                , initialColor = config.cancelledColor
                }

            config3 =
                { id = "branding-color-initiated"
                , title = "Initiated"
                , initialColor = config.initatedColor
                }

            config4 =
                { id = "branding-color-sent"
                , title = "Sent"
                , initialColor = config.sentColor
                }

            config5 =
                { id = "branding-color-delivered"
                , title = "Delivered"
                , initialColor = config.deliveredColor
                }

            config6 =
                { id = "branding-color-opened"
                , title = "Email opened, prolonged"
                , initialColor = config.openedColor
                }

            config7 =
                { id = "branding-color-review"
                , title = "Reviewed online, Opened view to identify, and Identified online"
                , initialColor = config.reviewedColor
                }

            config8 =
                { id = "branding-color-signed"
                , title = "Signed, sealed"
                , initialColor = config.signedColor
                }
        in
        inInit
            ( config1
            , ( config2
              , ( config3
                , ( config4
                  , ( config5
                    , ( config6
                      , ( config7
                        , config8
                        )
                      )
                    )
                  )
                )
              )
            )


update : UpdateHandler
update =
    Pair.liftUpdate
        Color.update
    <|
        Pair.liftUpdate
            Color.update
        <|
            Pair.liftUpdate
                Color.update
            <|
                Pair.liftUpdate
                    Color.update
                <|
                    Pair.liftUpdate
                        Color.update
                    <|
                        Pair.liftUpdate
                            Color.update
                        <|
                            Pair.liftUpdate
                                Color.update
                            <|
                                Color.update


view : ViewHandler
view =
    Pair.consView
        Color.view
    <|
        Pair.consView
            Color.view
        <|
            Pair.consView
                Color.view
            <|
                Pair.consView
                    Color.view
                <|
                    Pair.consView
                        Color.view
                    <|
                        Pair.consView
                            Color.view
                        <|
                            Pair.consView
                                Color.view
                            <|
                                Handler.toListViewHandler Color.view


stateToColorFields : State -> ColorFields
stateToColorFields ( state1, ( state2, ( state3, ( state4, ( state5, ( state6, ( state7, state8 ) ) ) ) ) ) ) =
    { draftColor =
        Color.stateToColor state1
    , cancelledColor =
        Color.stateToColor state2
    , initatedColor =
        Color.stateToColor state3
    , sentColor =
        Color.stateToColor state4
    , deliveredColor =
        Color.stateToColor state5
    , openedColor =
        Color.stateToColor state6
    , reviewedColor =
        Color.stateToColor state7
    , signedColor =
        Color.stateToColor state8
    }
