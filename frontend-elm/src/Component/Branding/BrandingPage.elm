module Component.Branding.BrandingPage exposing (Config, State, brandingTab, goBackTab, initBrandedDomainTab, initTab, tabConfig, themeTab, updateBrandedDomainTabState, updateTabState, viewBrandedDomainBrandingPage, viewBrandingPage)

import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Component.Theme.Data exposing (Theme)
import Compose.Pair as Pair
import Compose.Util exposing (msgToCmd)
import Either exposing (Either(..))
import Html exposing (Html, text)


type alias State =
    Tab.State


type alias Config themeSet =
    { browserTitle : String
    , smsOriginator : String
    , defaultThemeSet : themeSet
    , currentThemeSet : themeSet
    , availableThemes : List Theme
    }


initTab : () -> ( Tab.State, Cmd msg )
initTab () =
    ( Tab.initialState, Cmd.none )


initBrandedDomainTab : () -> ( Tab.State, Cmd msg )
initBrandedDomainTab () =
    ( Tab.customInitialState "branding", Cmd.none )


updateTabState : Tab.State -> Tab.State -> ( Tab.State, Cmd msg )
updateTabState state _ =
    ( state, Cmd.none )


updateBrandedDomainTabState : msg -> Tab.State -> Tab.State -> ( Tab.State, Cmd msg )
updateBrandedDomainTabState goBackMsg state _ =
    if state == Tab.customInitialState "goback" then
        ( state, msgToCmd goBackMsg )

    else
        ( state, Cmd.none )


tabConfig : Tab.Config (Pair.Msg msg1 Tab.State)
tabConfig =
    Tab.config Pair.SecondMsg
        |> Tab.useHash False


goBackTab : Tab.Item msg
goBackTab =
    Tab.item
        { id = "goback"
        , link = Tab.link [] [ text "<" ]
        , pane = Tab.pane [ Spacing.mt3 ] []
        }


brandingTab : Html msg -> Tab.Item msg
brandingTab page =
    Tab.item
        { id = "branding"
        , link = Tab.link [] [ text "Branding" ]
        , pane =
            Tab.pane
                [ Spacing.mt3 ]
                [ page ]
        }


themeTab : Html msg -> Tab.Item msg
themeTab page =
    Tab.item
        { id = "theme"
        , link = Tab.link [] [ text "Theme" ]
        , pane =
            Tab.pane
                [ Spacing.mt3 ]
                [ page
                ]
        }


viewBrandingPage :
    Tab.State
    -> Html msg
    -> Html msg
    -> Html (Pair.Msg msg Tab.State)
viewBrandingPage tabState brandingPage1 themePage1 =
    let
        brandingPage2 =
            Html.map Pair.FirstMsg
                brandingPage1

        themePage2 =
            Html.map Pair.FirstMsg
                themePage1

        body1 =
            tabConfig
                |> Tab.items
                    [ brandingTab brandingPage2
                    , themeTab themePage2
                    ]
                |> Tab.view tabState
    in
    body1


viewBrandedDomainBrandingPage :
    Tab.State
    -> Html msg
    -> Html msg
    -> Html (Pair.Msg msg Tab.State)
viewBrandedDomainBrandingPage tabState brandingPage1 themePage1 =
    let
        brandingPage2 =
            Html.map Pair.FirstMsg
                brandingPage1

        themePage2 =
            Html.map Pair.FirstMsg
                themePage1

        body1 =
            tabConfig
                |> Tab.items
                    [ goBackTab
                    , brandingTab brandingPage2
                    , themeTab themePage2
                    ]
                |> Tab.view tabState
    in
    body1
