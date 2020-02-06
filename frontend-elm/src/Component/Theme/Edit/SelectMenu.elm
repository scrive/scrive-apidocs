module Component.Theme.Edit.SelectMenu exposing (onSelect, view, viewFormSelect, viewSelectRow, viewSelectRows)

import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Component.Theme.Edit.CurrentTheme as CurrentTheme
import Component.Theme.Single as Theme
import Either exposing (Either(..))
import Html exposing (Html, text)
import Html.Attributes exposing (selected, value)
import String


onSelect : String -> CurrentTheme.Msg
onSelect str =
    CurrentTheme.selectThemeMsg <| String.toInt str


viewSelectRow :
    Maybe Int
    -> Maybe Int
    -> String
    -> Select.Item CurrentTheme.Msg
viewSelectRow selection i themeName =
    let
        idStr =
            Maybe.withDefault "" <|
                Maybe.map String.fromInt i

        isSelected =
            selection == i
    in
    Select.item
        [ value idStr
        , selected isSelected
        ]
        [ text themeName
        ]


viewSelectRows :
    Maybe Int
    -> List String
    -> List (Select.Item CurrentTheme.Msg)
viewSelectRows selection themeNames =
    List.indexedMap
        (\i themeName ->
            viewSelectRow selection (Just i) themeName
        )
        themeNames


viewFormSelect :
    List (Select.Item CurrentTheme.Msg)
    -> Html CurrentTheme.Msg
viewFormSelect rows =
    Form.row
        []
        [ Form.colLabel
            [ Col.sm3, Col.md3, Col.lg3 ]
            [ text "Current theme" ]
        , Form.col
            [ Col.sm9, Col.md9, Col.lg9 ]
            [ Select.select [ Select.onChange onSelect ]
                rows
            ]
        ]


view :
    CurrentTheme.State
    -> Html CurrentTheme.Msg
view state =
    let
        themeNames =
            List.map
                (.name << Theme.stateToTheme)
            <|
                CurrentTheme.themeStates state

        selection =
            CurrentTheme.themeIndex state

        themeRows =
            viewSelectRows selection themeNames
    in
    viewFormSelect themeRows
