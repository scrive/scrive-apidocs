module Component.Branding.CreateTheme exposing (Config, Init, Msg, NewTheme, OutMsg, State, Theme, UpdateHandler, ViewHandler, initialize, update, view)

import Component.Input.SaveButton as SaveButton
import Component.Input.Select as Select
import Component.Input.TextField as TextField
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html, div)


type alias Config =
    ()


type alias Theme =
    { id : String
    , name : String
    }


type alias NewTheme =
    { originalThemeId : String
    , newThemeName : String
    }


type alias Msg =
    Pair.Msg Select.Msg (Pair.Msg TextField.Msg (SaveButton.Msg NewTheme))


type alias OutMsg =
    SaveButton.OutMsg NewTheme


type alias State =
    Pair.State Select.State (Pair.State TextField.State SaveButton.State)


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
                Select.initialize
            <|
                Pair.liftInit
                    TextField.initialize
                    SaveButton.initialize
    in
    \_ ->
        let
            fieldConfig =
                { title = "New Theme Title"
                , description = "Create new theme based on selected theme"
                , value = ""
                }

            selectConfig =
                { fieldLabel = "Create New Theme From"
                , selectedIndex = Just 0
                }
        in
        inInit ( selectConfig, ( fieldConfig, "Create" ) )


update : UpdateHandler
update =
    Pair.liftUpdate
        (Handler.outerMapUpdate never Select.update)
    <|
        Pair.liftUpdate
            (Handler.outerMapUpdate never TextField.update)
            SaveButton.update


view : List Theme -> ViewHandler
view themes ( selectState, ( textState, buttonState ) ) =
    let
        themeNames =
            List.map .name themes

        mSelectedTheme =
            Select.getSelected themes selectState

        selectedThemeId =
            Maybe.withDefault
                "1"
                (Maybe.map .id mSelectedTheme)

        themeName =
            TextField.fieldValue textState

        themeData =
            { originalThemeId = selectedThemeId
            , newThemeName = themeName
            }

        selectBody =
            Html.map Pair.FirstMsg <|
                Select.view themeNames selectState

        textBody =
            Html.map (Pair.SecondMsg << Pair.FirstMsg) <|
                TextField.view textState

        buttonBody =
            Html.map (Pair.SecondMsg << Pair.SecondMsg) <|
                SaveButton.view themeData buttonState
    in
    div []
        [ selectBody
        , textBody
        , buttonBody
        ]
