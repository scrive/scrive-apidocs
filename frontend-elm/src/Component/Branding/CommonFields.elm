module Component.Branding.CommonFields exposing (CommonFields, Config, Init, Msg, State, UpdateHandler, ViewHandler, browserTitle, initialize, smsOriginator, stateToCommonFields, update, view)

import Component.Branding.Data exposing (BrandingSettings)
import Component.Input.TextField as TextField
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias CommonFields =
    BrandingSettings


type alias Config =
    CommonFields


type alias State =
    Pair.State TextField.State TextField.State


type alias Msg =
    Pair.Msg TextField.Msg TextField.Msg


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
                TextField.initialize
    in
    \config1 ->
        let
            config2 =
                { title = "Browser title"
                , description =
                    "The text at the top of the browser."
                , value = config1.browserTitle
                }

            config3 =
                { title = "SMS originator"
                , description =
                    "The name displayed to the recipient when receiving an SMS. "
                        ++ "Maximum 11 alpha-numeric characters."
                , value = config1.smsOriginator
                }

            config4 =
                ( config2, config3 )
        in
        inInit config4


update : UpdateHandler
update =
    Pair.liftUpdate
        TextField.update
        TextField.update


view : ViewHandler
view =
    Pair.consView
        TextField.view
    <|
        Handler.toListViewHandler TextField.view


browserTitle : State -> String
browserTitle ( state, _ ) =
    state.value


smsOriginator : State -> String
smsOriginator ( _, state ) =
    state.value


stateToCommonFields : State -> CommonFields
stateToCommonFields state =
    { browserTitle = browserTitle state
    , smsOriginator = smsOriginator state
    }
