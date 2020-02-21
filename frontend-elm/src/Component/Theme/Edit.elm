module Component.Theme.Edit exposing (Config, InConfig, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, currentTheme, initialize, stateToThemes, themeSavedMsg, update, view)

import Component.Input.SaveButton as Button
import Component.Theme.Data as Data exposing (Theme)
import Component.Theme.Edit.CurrentTheme as CurrentTheme
import Component.Theme.Edit.SaveButton as SaveButton
import Component.Theme.Edit.DeleteButton as DeleteButton
import Component.Theme.Edit.SelectMenu as SelectMenu
import Component.Theme.Single as Theme
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html, div)


type alias Config =
    CurrentTheme.Config


type alias InConfig =
    Pair.Config CurrentTheme.Config
        (Pair.Config
            SaveButton.Config
            DeleteButton.Config)


type alias State =
    Pair.State CurrentTheme.State
        (Pair.State
            SaveButton.State
            DeleteButton.State)


type alias Msg =
    Pair.Msg CurrentTheme.Msg
        (Pair.Msg
            SaveButton.Msg
            DeleteButton.Msg)


type alias OutMsg =
    Data.OutMsg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize =
    let
        inInit =
            Pair.liftInit
                CurrentTheme.initialize <|
                Pair.liftInit
                    SaveButton.initialize
                    DeleteButton.initialize
    in
    \config1 ->
        let
            config2 =
                ( config1, ((), ()) )
        in
        inInit config2


update : UpdateHandler
update =
    Pair.liftUpdate
        (Handler.outerMapUpdate never CurrentTheme.update) <|
        Pair.liftUpdate
            (Handler.outerMapUpdate
                (\(Button.SaveMsg theme) -> Data.SaveThemeMsg theme)
                SaveButton.update)
            (Handler.outerMapUpdate
                (\(Button.SaveMsg theme) -> Data.DeleteThemeMsg theme)
                DeleteButton.update)


view : ViewHandler
view ( state1, (state2, state3) ) =
    let
        mState3 =
            CurrentTheme.currentThemeState state1

        mTheme =
            Maybe.map Theme.stateToTheme mState3

        mButtonBody =
            Maybe.map
                (\theme ->
                    [ Html.map (Pair.SecondMsg << Pair.FirstMsg) <|
                        SaveButton.view theme state2
                    , Html.map (Pair.SecondMsg << Pair.SecondMsg) <|
                        DeleteButton.view theme state3
                    ]
                )
                mTheme

        buttonBody =
            Maybe.withDefault [] mButtonBody

        currentThemeBody =
            CurrentTheme.view
                state1

        selectThemeBody =
            SelectMenu.view state1

        mainBody : Html Msg
        mainBody =
            div [] <|
                [ Html.map Pair.FirstMsg
                    selectThemeBody
                , Html.map Pair.FirstMsg
                    currentThemeBody
                ]
                    ++ buttonBody
    in
    mainBody


currentTheme : State -> Maybe Theme
currentTheme ( state, _ ) =
    CurrentTheme.currentTheme state


stateToThemes : State -> List Theme
stateToThemes ( state, _ ) =
    CurrentTheme.stateToThemes state


themeSavedMsg : Msg
themeSavedMsg =
    Pair.SecondMsg <| Pair.FirstMsg SaveButton.themeSavedMsg
