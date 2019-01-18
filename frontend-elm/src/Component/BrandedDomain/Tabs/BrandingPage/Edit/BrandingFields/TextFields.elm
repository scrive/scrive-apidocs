module Component.BrandedDomain.Tabs.BrandingPage.Edit.BrandingFields.TextFields exposing (Config, Init, Msg, State, TextFields, UpdateHandler, ViewHandler, initialize, stateToTextFields, update, view)

import Component.Input.TextField as TextField
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias TextFields =
    { url : String
    , browserTitle : String
    , smsOriginator : String
    , emailOriginator : String
    }


type alias Config =
    TextFields


type alias State =
    Pair.State TextField.State
        -- URL
        (Pair.State TextField.State
            -- Browser Title
            (Pair.State TextField.State
                -- SMS Originator
                TextField.State
            )
        )



-- Email Originator


type alias Msg =
    Pair.Msg TextField.Msg
        -- URL
        (Pair.Msg TextField.Msg
            -- Browser Title
            (Pair.Msg TextField.Msg
                -- SMS Originator
                TextField.Msg
            )
        )



-- Email Originator


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
                TextField.initialize
            <|
                Pair.liftInit
                    TextField.initialize
                <|
                    Pair.liftInit
                        TextField.initialize
                    <|
                        TextField.initialize
    in
    \config1 ->
        let
            config2 =
                { title = "URL"
                , description =
                    "The full web address for the domain."
                , value = config1.url
                }

            config3 =
                { title = "Browser title"
                , description =
                    "The text at the top of the browser."
                , value = config1.browserTitle
                }

            config4 =
                { title = "SMS originator"
                , description =
                    "The name displayed to the recipient when receiving an SMS. "
                        ++ "Maximum 11 alpha-numeric characters."
                , value = config1.smsOriginator
                }

            config5 =
                { title = "Email originator"
                , description =
                    "The name displayed to the recipient when receiving emails."
                , value = config1.emailOriginator
                }

            config6 =
                ( config2
                , ( config3
                  , ( config4
                    , config5
                    )
                  )
                )
        in
        inInit config6


update : UpdateHandler
update =
    Pair.liftUpdate
        TextField.update
    <|
        Pair.liftUpdate
            TextField.update
        <|
            Pair.liftUpdate
                TextField.update
                TextField.update


view : ViewHandler
view =
    Pair.consView
        TextField.view
    <|
        Pair.consView
            TextField.view
        <|
            Pair.consView
                TextField.view
            <|
                Handler.toListViewHandler TextField.view


stateToTextFields : State -> TextFields
stateToTextFields ( url, ( browserTitle, ( smsOriginator, emailOriginator ) ) ) =
    { url = url.value
    , browserTitle = browserTitle.value
    , smsOriginator = smsOriginator.value
    , emailOriginator = emailOriginator.value
    }
