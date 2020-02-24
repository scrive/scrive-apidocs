module Component.Theme.Edit exposing (Config, InConfig, Init, Msg, OutMsg, State, UpdateHandler, ViewHandler, currentTheme, initialize, stateToThemes, doneSaveThemeMsg, doneDeleteThemeMsg, update, view)

import Component.Input.Button exposing (OutMsg (..))
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
import List.Extra as List


type alias Config =
    { inConfig : CurrentTheme.Config
    , currentThemes : List Theme
    }


type alias InConfig =
    Pair.Config CurrentTheme.Config
        (Pair.Config
            SaveButton.Config
            DeleteButton.Config)


type alias State =
    { inState :
        Pair.State CurrentTheme.State
            (Pair.State
                SaveButton.State
                DeleteButton.State)
    , currentThemes : List Theme
    }


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

        init : Init
        init config1 =
            let
                config2 =
                    ( config1.inConfig, ((), ()) )

                (state1, cmd1) =
                    inInit config2

                state2 =
                    { inState = state1
                    , currentThemes = config1.currentThemes
                    }
            in
            (state2, cmd1)
    in
    init


update : UpdateHandler
update =
    let
        inUpdate =
            Pair.liftUpdate
                (Handler.outerMapUpdate never CurrentTheme.update) <|
                Pair.liftUpdate
                    (Handler.outerMapUpdate
                        (\(ClickMsg theme) -> Data.SaveThemeMsg theme)
                        SaveButton.update)
                    (Handler.outerMapUpdate
                        (\(ClickMsg theme) -> Data.DeleteThemeMsg theme)
                        DeleteButton.update)
    in
    \msg state1 ->
        let
            (state2, cmd1) = inUpdate msg state1.inState
            state3 = { state1 | inState = state2 }
        in
        (state3, cmd1)

themeInThemeSet : Theme -> List Theme -> Bool
themeInThemeSet theme themes =
    List.any
        (\theme2 -> theme2.id == theme.id)
        themes

view : ViewHandler
view state =
    let
        currentThemes = state.currentThemes
        ( state1, (state2, state3) ) = state.inState

        mState3 =
            CurrentTheme.currentThemeState state1

        mTheme =
            Maybe.map Theme.stateToTheme mState3

        mButtonBody =
            Maybe.map
                (\theme ->
                    let
                        saveButton =
                            Html.map (Pair.SecondMsg << Pair.FirstMsg) <|
                                SaveButton.view theme state2

                        deleteButton =
                            Html.map (Pair.SecondMsg << Pair.SecondMsg) <|
                                DeleteButton.view theme state3
                    in
                    if themeInThemeSet theme currentThemes
                    then [ saveButton ]
                    else [ saveButton, deleteButton ]
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
currentTheme state1 =
    let ( state2, _ )  = state1.inState
    in
    CurrentTheme.currentTheme state2

stateToThemes : State -> List Theme
stateToThemes state1 =
    let ( state2, _ )  = state1.inState
    in
    CurrentTheme.stateToThemes state2

doneSaveThemeMsg : Msg
doneSaveThemeMsg =
    Pair.SecondMsg <| Pair.FirstMsg SaveButton.doneSaveThemeMsg

doneDeleteThemeMsg : Msg
doneDeleteThemeMsg =
    Pair.SecondMsg <| Pair.SecondMsg DeleteButton.doneDeleteThemeMsg
