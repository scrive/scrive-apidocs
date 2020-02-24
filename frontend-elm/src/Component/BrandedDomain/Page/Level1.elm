module Component.BrandedDomain.Page.Level1 exposing (Config, Init, Msg, OutMsg(..), State, UpdateHandler, ViewHandler, doneDeleteThemeMsg, doneSaveBrandingMsg, doneSaveThemeMsg, initialize, update, view)

import Component.BrandedDomain.Data exposing (Branding, ThemeSet)
import Component.BrandedDomain.Tabs as Tabs
import Component.BrandedDomain.Tabs.Data as TabsData
import Component.Branding.BrandingPage as BrandingPage
import Component.Theme.Data exposing (Theme)
import Compose.Handler as Handler
import Compose.Pair as Pair
import Either exposing (Either(..))
import Html exposing (Html)


type alias Config =
    { brandingInfo : Branding
    , mDefaultThemeSet : Maybe ThemeSet
    , currentThemeSet : ThemeSet
    , availableThemes : List Theme
    }


type State
    = State (Pair.State Tabs.State BrandingPage.State)


type alias Msg =
    Pair.Msg Tabs.Msg BrandingPage.State


type OutMsg
    = BaseOutMsg TabsData.OutMsg
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

        init : Init
        init config1 =
            let
                branding =
                    config1.brandingInfo

                config2 : Tabs.Config
                config2 =
                    { availableThemes = config1.availableThemes
                    , mDefaultThemeSet = config1.mDefaultThemeSet
                    , currentThemeSet = config1.currentThemeSet
                    , brandingInfo = branding
                    , brandingSettings =
                        { browserTitle = branding.browserTitle
                        , smsOriginator = branding.smsOriginator
                        }
                    }

                ( state, cmd ) =
                    inInit ( config2, () )
            in
            ( State state, cmd )
    in
    init


update : UpdateHandler
update =
    let
        inUpdate =
            Pair.liftUpdate
                (Handler.outerMapUpdate BaseOutMsg Tabs.update)
                (BrandingPage.updateBrandedDomainTabState <| Left GoBack)
    in
    \msg (State state1) ->
        let
            ( state2, cmd ) =
                inUpdate msg state1
        in
        ( State state2, cmd )


view : ViewHandler
view (State ( state1, state2 )) =
    let
        ( brandingPage1, themePage1 ) =
            Tabs.view state1
    in
    BrandingPage.viewBrandedDomainBrandingPage state2 brandingPage1 themePage1


doneSaveBrandingMsg : Msg
doneSaveBrandingMsg =
    Pair.FirstMsg Tabs.doneSaveBrandingMsg


doneSaveThemeMsg : Msg
doneSaveThemeMsg =
    Pair.FirstMsg Tabs.doneSaveThemeMsg


doneDeleteThemeMsg : Msg
doneDeleteThemeMsg =
    Pair.FirstMsg Tabs.doneDeleteThemeMsg
