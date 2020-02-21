module Component.Branding.ThemeField exposing (Config, Init, Msg(..), OutMsg(..), State, UpdateHandler, ViewHandler, initialize, onSelect, stateToTheme, update, view, viewPreview)

import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Component.Theme.Data exposing (Theme)
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (selected, value)
import List.Extra as List
import Maybe.Extra as Maybe


type alias Config =
    State


type alias State =
    { fieldLabel : String
    , mDefaultTheme : Maybe Theme
    , selectedThemeIndex : Maybe Int
    , previewHandler : Theme -> Html Never
    }


type Msg
    = SelectThemeMsg (Maybe Int)


type OutMsg
    = ThemeChangedMsg


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either OutMsg Msg) )


type alias ViewHandler =
    State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize state =
    ( state, Cmd.none )


update : UpdateHandler
update msg1 state1 =
    case msg1 of
        SelectThemeMsg i ->
            let
                state2 =
                    { state1 | selectedThemeIndex = i }

                cmd =
                    Util.msgToCmd <| Left ThemeChangedMsg
            in
            ( state2, cmd )


onSelect : String -> Msg
onSelect str =
    SelectThemeMsg <| String.toInt str


view : List Theme -> ViewHandler
view availableThemes state =
    let
        selectedThemeIndex =
            state.selectedThemeIndex

        viewSelect i theme =
            Select.item
                [ value <| String.fromInt i
                , selected <| Just i == selectedThemeIndex
                ]
                [ text theme.name
                ]

        selectDefault =
            Select.item
                [ value "default"
                , selected <| Nothing == selectedThemeIndex
                ]
                [ text "Default"
                ]

        body1 =
            List.indexedMap viewSelect availableThemes

        body2 =
            case state.mDefaultTheme of
                Just _ ->
                    selectDefault :: body1

                Nothing ->
                    body1

        body3 =
            Form.row
                []
                [ Form.colLabel
                    [ Col.sm4, Col.md4, Col.lg4 ]
                    [ text state.fieldLabel ]
                , Form.col
                    [ Col.sm8, Col.md8, Col.lg8 ]
                    [ Select.select [ Select.onChange onSelect ]
                        body2
                    ]
                ]
    in
    body3


viewPreview : List Theme -> State -> Html Never
viewPreview availableThemes state =
    let
        mTheme1 =
            Maybe.andThen
                (\i -> List.getAt i availableThemes)
                state.selectedThemeIndex

        mTheme2 =
            Maybe.or
                mTheme1
                state.mDefaultTheme

        mBody1 =
            Maybe.map state.previewHandler mTheme2

        body2 =
            Maybe.withDefault
                (div []
                    [ text "Select a theme to preview" ]
                )
                mBody1
    in
    body2


stateToTheme :
    List Theme
    -> State
    -> Maybe Theme
stateToTheme availableThemes state =
    let
        mTheme1 =
            Maybe.andThen
                (\i ->
                    List.getAt
                        i
                        availableThemes
                )
                state.selectedThemeIndex

        mTheme2 =
            Maybe.or
                mTheme1
                state.mDefaultTheme
    in
    mTheme2
