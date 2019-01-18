module Component.Theme.Edit.CurrentTheme exposing (Config, Msg, State, UpdateHandler, ViewHandler, currentTheme, currentThemeState, defaultView, initialize, selectThemeMsg, stateToThemes, themeIndex, themeStates, update, view)

import Component.Theme.Data exposing (Theme)
import Component.Theme.Single as Theme
import Compose.Cursor as Cursor
import Either exposing (Either(..))
import Html exposing (Html, div, p, strong, text)


type alias Config =
    { availableThemes : List Theme
    , initialThemeIndex : Maybe Int
    }


type alias Msg =
    Cursor.Msg Theme.Msg


type alias State =
    Cursor.State Theme.State


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> Html Msg


initialize : Config -> ( State, Cmd Msg )
initialize =
    let
        inInit =
            Cursor.liftInit Theme.initialize
    in
    \config1 ->
        let
            config2 =
                { initialIndex = config1.initialThemeIndex
                , itemsConfig = config1.availableThemes
                }
        in
        inInit config2


update : UpdateHandler
update =
    Cursor.liftUpdate Theme.update


view : ViewHandler
view =
    let
        maybeView =
            Cursor.liftView Theme.view
    in
    \state ->
        let
            mBody1 =
                maybeView state

            body2 =
                Maybe.withDefault defaultView mBody1
        in
        body2


defaultView : Html msg
defaultView =
    div []
        [ strong [] [ text "Did you know you can use your own logo and colours?" ]
        , p [] [ text "Customize Scrive to make it look like your brand by creating themes." ]
        ]


selectThemeMsg : Maybe Int -> Msg
selectThemeMsg =
    Cursor.SelectMsg


themeStates : State -> List Theme.State
themeStates state =
    state.itemsState


currentThemeState : State -> Maybe Theme.State
currentThemeState =
    Cursor.currentItemState


currentTheme : State -> Maybe Theme
currentTheme =
    Maybe.map Theme.stateToTheme << currentThemeState


themeIndex : State -> Maybe Int
themeIndex state =
    state.currentIndex


stateToThemes : State -> List Theme
stateToThemes =
    List.map Theme.stateToTheme << themeStates
