module Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.ParticipantColors exposing (ColorFields, Config, Init, Msg, State, UpdateHandler, ViewHandler, initialize, stateToColorFields, update, view)

import Component.BrandedDomain.Data exposing (ParticipantColors)
import Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.ColorField as Color
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias ColorFields =
    ParticipantColors


type alias Config =
    ColorFields


type alias State =
    Pair.State Color.State (Pair.State Color.State (Pair.State Color.State (Pair.State Color.State (Pair.State Color.State Color.State))))


type alias Msg =
    Pair.Msg Color.Msg (Pair.Msg Color.Msg (Pair.Msg Color.Msg (Pair.Msg Color.Msg (Pair.Msg Color.Msg Color.Msg))))


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
                                Color.initialize
    in
    \config ->
        let
            config1 =
                { id = "branding-color-participant-1"
                , title = "Participant 1"
                , initialColor = config.participantColor1
                }

            config2 =
                { id = "branding-color-participant-2"
                , title = "Participant 2"
                , initialColor = config.participantColor2
                }

            config3 =
                { id = "branding-color-participant-3"
                , title = "Participant 3"
                , initialColor = config.participantColor3
                }

            config4 =
                { id = "branding-color-participant-4"
                , title = "Participant 4"
                , initialColor = config.participantColor4
                }

            config5 =
                { id = "branding-color-participant-5"
                , title = "Participant 5"
                , initialColor = config.participantColor5
                }

            config6 =
                { id = "branding-color-participant-6"
                , title = "Participant 6"
                , initialColor = config.participantColor6
                }
        in
        inInit
            ( config1
            , ( config2
              , ( config3
                , ( config4
                  , ( config5
                    , config6
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
                        Handler.toListViewHandler Color.view


stateToColorFields : State -> ColorFields
stateToColorFields ( state1, ( state2, ( state3, ( state4, ( state5, state6 ) ) ) ) ) =
    { participantColor1 = Color.stateToColor state1
    , participantColor2 = Color.stateToColor state2
    , participantColor3 = Color.stateToColor state3
    , participantColor4 = Color.stateToColor state4
    , participantColor5 = Color.stateToColor state5
    , participantColor6 = Color.stateToColor state6
    }
