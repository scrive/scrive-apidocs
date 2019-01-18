module Component.BrandedDomain.Page.One exposing (Config, Init, Msg, OutMsg(..), State, UpdateHandler, ViewHandler, brandingSavedMsg, initialize, themeSavedMsg, update, view)

import Component.BrandedDomain.Data exposing (Branding, ThemeSet)
import Component.BrandedDomain.Tabs as Tabs
import Component.Branding.BrandingPage as BrandingPage
import Component.Theme.Data exposing (Theme)
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    { brandingInfo : Branding
    , defaultThemeSet : ThemeSet
    , currentThemeSet : ThemeSet
    , availableThemes : List Theme
    }


type alias State =
    Pair.State Tabs.State BrandingPage.State


type alias Msg =
    Pair.Msg Tabs.Msg BrandingPage.State


type OutMsg
    = TabsOutMsg Tabs.OutMsg
    | GoBack


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
                Tabs.initialize
                BrandingPage.initBrandedDomainTab
    in
    \config1 ->
        let
            branding =
                config1.brandingInfo

            config2 =
                { availableThemes = config1.availableThemes
                , defaultThemeSet = config1.defaultThemeSet
                , currentThemeSet = config1.currentThemeSet
                , brandingInfo = branding
                , brandingSettings =
                    { browserTitle = branding.browserTitle
                    , smsOriginator = branding.smsOriginator
                    }
                }
        in
        inInit ( config2, () )


update : UpdateHandler
update =
    Pair.liftUpdate
        (\msg state1 ->
            let
                ( state2, cmd ) =
                    Tabs.update msg state1
            in
            ( state2, Cmd.map (Either.mapLeft TabsOutMsg) cmd )
        )
        (BrandingPage.updateBrandedDomainTabState <| Left GoBack)


view : ViewHandler
view ( state1, state2 ) =
    let
        ( brandingPage1, themePage1 ) =
            Tabs.view state1
    in
    BrandingPage.viewBrandedDomainBrandingPage state2 brandingPage1 themePage1


brandingSavedMsg : Msg
brandingSavedMsg =
    Pair.FirstMsg Tabs.brandingSavedMsg


themeSavedMsg : Msg
themeSavedMsg =
    Pair.FirstMsg Tabs.themeSavedMsg
