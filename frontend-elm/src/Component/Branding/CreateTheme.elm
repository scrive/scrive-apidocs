module Component.Branding.CreateTheme exposing (Config, Init, Msg, NewTheme, OutMsg, State, Theme, UpdateHandler, ViewHandler, initialize, update, view)

import Component.Input.Button as Button
import Component.Input.Select as Select
import Component.Input.TextField as TextField
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html, div)
import Regex


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
    Pair.Msg Select.Msg (Pair.Msg TextField.Msg (Button.Msg NewTheme))


type alias OutMsg =
    Button.OutMsg NewTheme


type alias State =
    Pair.State Select.State (Pair.State TextField.State Button.State)


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
                    Button.initialize
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

            buttonConfig =
                { caption = "Create"
                , buttonType = Button.Ok
                }
        in
        inInit ( selectConfig, ( fieldConfig, buttonConfig ) )


update : UpdateHandler
update =
    Pair.liftUpdate
        (Handler.outerMapUpdate never Select.update)
    <|
        Pair.liftUpdate
            (Handler.outerMapUpdate never TextField.update)
            Button.update


emptyString : String -> Bool
emptyString =
    Regex.contains <|
        Maybe.withDefault Regex.never <|
            Regex.fromString "^\\s*$"


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

        buttonBody1 =
            if emptyString textState.value then
                Button.viewOverride themeData Button.Disabled buttonState

            else
                Button.view themeData buttonState

        buttonBody2 =
            Html.map (Pair.SecondMsg << Pair.SecondMsg) buttonBody1
    in
    div []
        [ selectBody
        , textBody
        , buttonBody2
        ]
