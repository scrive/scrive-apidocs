module AdminOnly.BrandedDomain.BrandedDomainTab exposing (..)

import Either exposing (Either(..))
import Html exposing (Html, text, div)
import Html.Attributes exposing (class)
import Url.Parser as UP exposing (Parser, (</>))
import Utils exposing (..)
import Http
import FlashMessage exposing (FlashMessage)
import Json.Decode as JD
import Json.Encode as JE
import EnumExtra as Enum exposing (Enum)
import List.Extra as List
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Vendor.ColorPickerExtra as ColorPicker
import Vendor.Popover as Popover


import AdminOnly.BrandedDomain.Types exposing (..)
import AdminOnly.BrandedDomain.Json exposing (..)
import AdminOnly.BrandedDomain.EditBrandedDomain as EditBrandedDomain
import Lib.Theme.Json as Theme
import Lib.Theme.EditTheme as EditTheme
import Lib.Theme.PreviewTheme exposing (..)
import Lib.Theme.Types as Theme exposing (Theme, ThemeID)

type EditTab = EditBrandingTab | EditThemeTab
enumEditTab : Enum EditTab
enumEditTab =
  let allValues = [EditBrandingTab, EditThemeTab]
      toString t = case t of
        EditBrandingTab -> "branding"
        EditThemeTab -> "themes"
      toHumanString t = case t of
        EditBrandingTab -> "Edit Branding"
        EditThemeTab -> "Manage Themes"
  in Enum.makeEnum allValues toString toHumanString

type alias LoadingState =
  { domainThemesLoaded : Bool
  , domainBrandingLoaded : Bool }

type alias State =
  { bdID : Int
  , editTab : EditTab
  , editTabState : Tab.State
  , previewTabState : Tab.State
  , loadingState : LoadingState
  , availableThemes : List Theme
  , editBrandedDomainState :
    { brandedDomainBeingEdited : BrandedDomain
    , colorPickers : Enum.Dict ColorIdentifier ColorPicker.State
    , popovers : Enum.Dict ColorIdentifier Popover.State
    }
  , editThemeState :
    { themeBeingEdited : Theme
    , colorPickers : Enum.Dict Theme.ColorIdentifier ColorPicker.State
    , popovers : Enum.Dict Theme.ColorIdentifier Popover.State
    }
  }

type alias Page =
  { bdID : Int
  , editTab : EditTab }

type Msg = SetAvailableThemesMsg (List Theme)
         | PresentFlashMessage FlashMessage
         | SetBrandedDomainMsg BrandedDomain
         | DoSaveBrandedDomainMsg
         | EditBrandedDomainMsg EditBrandedDomain.Msg
         | EditThemeMsg EditTheme.Msg
         | SetEditTabStateMsg Tab.State
         | SetPreviewTabStateMsg Tab.State
         | DoSaveThemeMsg
         | DoDeleteThemeMsg
         | DoCreateThemeMsg Theme
         | SaveThemeMsg Theme
         | DeleteThemeCallbackMsg ThemeID
         | CreateThemeCallbackMsg Theme ThemeID

init : (Msg -> msg) -> Page -> (State, Cmd msg)
init embed page =
  let initialState =
        { bdID = page.bdID
        , editTab = page.editTab
        , editTabState = Tab.customInitialState <| Enum.toString enumEditTab page.editTab
        , previewTabState = Tab.customInitialState <| Enum.toString enumThemeKind EmailTheme
        , loadingState =
          { domainThemesLoaded = False
          , domainBrandingLoaded = False }
        , availableThemes = []
        , editBrandedDomainState =
          { brandedDomainBeingEdited = defaultBrandedDomain
          , colorPickers = Enum.fromList enumColorIdentifier
              <| List.map (\ident ->
                  (ident, ColorPicker.initWithId <| "colorpicker-" ++ Enum.toString enumColorIdentifier ident))
              <| Enum.allValues enumColorIdentifier
          , popovers = Enum.fromList enumColorIdentifier
              <| List.map (\ident -> (ident, Popover.initialState))
              <| Enum.allValues enumColorIdentifier
          }
        , editThemeState =
          { themeBeingEdited = Theme.errorTheme
          , colorPickers = Enum.fromList Theme.enumColorIdentifier
              <| List.map (\ident ->
                  (ident, ColorPicker.initWithId <| "colorpicker-" ++ Enum.toString Theme.enumColorIdentifier ident))
              <| Enum.allValues Theme.enumColorIdentifier
          , popovers = Enum.fromList Theme.enumColorIdentifier
              <| List.map (\ident -> (ident, Popover.initialState))
              <| Enum.allValues Theme.enumColorIdentifier
          }
        }
      loadingCmds = [getBrandedDomainCmd embed page.bdID, loadThemesCmd embed page.bdID]
  in
  ( initialState, Cmd.batch loadingCmds )

-- to do
fromPage : Page -> PageUrl
fromPage page =
  { path = [String.fromInt page.bdID]
  , query = []
  , fragment = Just <| Enum.toString enumEditTab page.editTab }

-- to do
routeParser : Parser (Page -> a) a
routeParser =
  let parseEditTab : Maybe String -> EditTab
      parseEditTab =
        Maybe.withDefault EditBrandingTab
        << Maybe.withDefault Nothing
        << Maybe.map (Enum.fromString enumEditTab)
  in UP.map (\bdid editTab -> { bdID = bdid, editTab = editTab } )
       <| UP.int </> UP.fragment parseEditTab

pageFromModel : State -> Maybe Page
pageFromModel state = Just { bdID = state.bdID, editTab = state.editTab }

update :
  (Msg -> msg) -> Globals msg -> Msg -> State -> (State, Cmd msg)
update embed globals msg state =
  let pure : State -> (State, Cmd msg)
      pure newState = (newState, Cmd.none)
  in case msg of
    SetAvailableThemesMsg availableThemes ->
      let loadingState = state.loadingState
          editThemeState = state.editThemeState
      in pure { state
              | availableThemes = availableThemes
              , loadingState = { loadingState | domainThemesLoaded = True }
              , editThemeState =
                { editThemeState
                | themeBeingEdited =
                    Maybe.withDefault editThemeState.themeBeingEdited
                      <| List.head availableThemes
                }
              }
    SetBrandedDomainMsg brandedDomain ->
      let loadingState = state.loadingState
          editBrandedDomainState = state.editBrandedDomainState
      in pure { state | editBrandedDomainState =
                        { editBrandedDomainState | brandedDomainBeingEdited = brandedDomain }
                      , loadingState = { loadingState | domainBrandingLoaded = True } }
    DoSaveBrandedDomainMsg -> (state, doSaveBrandedDomain embed (formBody globals) state)
    PresentFlashMessage flashMsg -> (state, globals.flashMessage flashMsg)
    EditBrandedDomainMsg msg_ ->
      let (newEditBrandedDomainState, cmd) =
            EditBrandedDomain.update (embed << EditBrandedDomainMsg)
              msg_ state.editBrandedDomainState
          themes = state.editBrandedDomainState.brandedDomainBeingEdited.themes
          newThemes = newEditBrandedDomainState.brandedDomainBeingEdited.themes
          mPreviewThemeKind =
            List.find (\kind -> Enum.get kind themes /= Enum.get kind newThemes)
            <| Enum.allValues enumThemeKind
          newPreviewTabState =
            Maybe.withDefault state.previewTabState
              <| Maybe.map (Tab.customInitialState << Enum.toString enumThemeKind) mPreviewThemeKind
      in ({ state | editBrandedDomainState = newEditBrandedDomainState
                  , previewTabState = newPreviewTabState }, cmd)
    SetEditTabStateMsg editTabState ->
      if editTabState == Tab.customInitialState "goback"
      then (state, globals.gotoBrandedDomainsTab)
      else pure {state | editTabState = editTabState}
    SetPreviewTabStateMsg previewTabState -> pure {state | previewTabState = previewTabState}
    EditThemeMsg msg_ ->
      let editThemeReadonly = { availableThemes = state.availableThemes }
          (newEditThemeState, cmd) =
            EditTheme.update (embed << EditThemeMsg) msg_ editThemeReadonly state.editThemeState
      in ({ state | editThemeState = newEditThemeState }, cmd)
    DoSaveThemeMsg -> doSaveTheme embed (formBody globals) state
    SaveThemeMsg theme ->
      let cmd = globals.flashMessage <| FlashMessage.success "Theme saved."
          newState =
            { state
            | availableThemes =
                List.map
                  (\theme_ -> if theme_.id == theme.id then theme else theme_)
                  state.availableThemes
              }
      in (newState, cmd)
    DoDeleteThemeMsg -> doDeleteTheme embed (formBody globals) state
    DeleteThemeCallbackMsg id ->
      let cmd = globals.flashMessage <| FlashMessage.success "Theme deleted."
          newAvailableThemes = List.filter (\theme -> theme.id /= id) state.availableThemes
          editThemeState = state.editThemeState
          newThemeBeingEdited = if editThemeState.themeBeingEdited.id /= id
            then editThemeState.themeBeingEdited
            else Maybe.withDefault Theme.errorTheme <| List.head newAvailableThemes
          newState =
            { state
            | availableThemes = newAvailableThemes
            , editThemeState = { editThemeState | themeBeingEdited = newThemeBeingEdited }
            }
      in (newState, cmd)
    DoCreateThemeMsg blueprint -> doCreateTheme embed (formBody globals) blueprint state
    CreateThemeCallbackMsg blueprint newThemeID ->
      let newTheme =
            { blueprint
            | id = newThemeID
            , name = "Copy of " ++ blueprint.name }
          editThemeState = state.editThemeState
          newState =
            { state
            | editThemeState = {editThemeState | themeBeingEdited = newTheme}
            , availableThemes = state.availableThemes ++ [newTheme]
            }
      in doSaveTheme embed (formBody globals) newState

doSaveTheme : (Msg -> msg) -> (List (String, String) -> Http.Body) -> State -> (State, Cmd msg)
doSaveTheme embed formBody state =
  let theme = state.editThemeState.themeBeingEdited
      callback : Result Http.Error () -> msg
      callback result = case result of
        Ok _ -> embed <| SaveThemeMsg theme
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to save theme."

      cmd = Http.post
        { url = "/adminonly/brandeddomain/updatetheme/"
                  ++ String.fromInt state.bdID ++ "/" ++ String.fromInt theme.id
        , body = formBody
                  [ ( "theme", JE.encode 0 <| Theme.encodeTheme theme ) ]
        , expect = Http.expectWhatever callback
        }
  in (state, cmd)

doDeleteTheme : (Msg -> msg) -> (List (String, String) -> Http.Body) -> State -> (State, Cmd msg)
doDeleteTheme embed formBody state =
  let callback : Result Http.Error () -> msg
      callback result = case result of
        Ok _ -> embed <| DeleteThemeCallbackMsg theme.id
        _ -> embed <| PresentFlashMessage
          <| FlashMessage.error "Failed to get delete theme. You probably tried to delete a theme that is still in use."

      theme = state.editThemeState.themeBeingEdited

      cmd = Http.post
        { url = "/adminonly/brandeddomain/deletetheme/"
                  ++ String.fromInt state.bdID ++ "/" ++ String.fromInt theme.id
        , body = formBody
                  [ ( "theme", JE.encode 0 <| Theme.encodeTheme theme ) ]
        , expect = Http.expectWhatever callback
        }
  in (state, cmd)

doCreateTheme : (Msg -> msg) -> (List (String, String) -> Http.Body) -> Theme -> State -> (State, Cmd msg)
doCreateTheme embed formBody blueprint state =
  let callback : Result Http.Error Theme -> msg
      callback result = case result of
        Ok new -> embed <| CreateThemeCallbackMsg blueprint new.id
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to create theme."
      cmd = Http.post
        { url = "/adminonly/brandeddomain/newtheme/"
                  ++ String.fromInt state.bdID ++ "/" ++ String.fromInt blueprint.id
        , body = formBody [ ( "name", "New theme" ) ]
        , expect = Http.expectJson callback Theme.themeDecoder
        }
  in (state, cmd)


updatePage : (Msg -> msg) -> Page -> State -> (State, Cmd msg)
updatePage embed page state =
  if page.bdID == state.bdID
  then ({ state | editTab = page.editTab }, Cmd.none)
  else init embed page

view : (Msg -> msg) -> State -> Html msg
view embed state =
  if state.loadingState.domainThemesLoaded && state.loadingState.domainBrandingLoaded
  then Html.map embed <| viewLoaded state
  else text "Loading"

viewLoaded : State -> Html Msg
viewLoaded state =
  let brandingTab =
        EditBrandedDomain.viewEditBrandedDomain
        { embed = EditBrandedDomainMsg
        , doSaveBrandedDomain = DoSaveBrandedDomainMsg
        , availableThemes = state.availableThemes
        } state.editBrandedDomainState

      themeTab = EditTheme.viewEditTheme
        { embed = EditThemeMsg
        , doSaveTheme = DoSaveThemeMsg
        , doDeleteTheme = DoDeleteThemeMsg
        , doCreateTheme = DoCreateThemeMsg
        }
        { availableThemes = state.availableThemes
        } state.editThemeState

      previewThemeSet : ThemeKind -> Theme
      previewThemeSet kind = case state.editTab of
        EditBrandingTab ->
          let selected = Maybe.withDefault -1
                <| Enum.get kind state.editBrandedDomainState.brandedDomainBeingEdited.themes
          in Maybe.withDefault Theme.errorTheme <|
              List.find (\theme_ -> theme_.id == selected) state.availableThemes
        EditThemeTab -> state.editThemeState.themeBeingEdited

      viewThemePreview kind = case kind of
            EmailTheme -> viewEmailThemePreview
            SignViewTheme -> viewSignViewThemePreview
            ServiceTheme -> viewServiceThemePreview
            LoginTheme -> viewLoginThemePreview

      themePreviewItems = List.map (\kind -> Tab.item
                  { id = Enum.toString enumThemeKind kind
                  , link = Tab.link [] [ text <| Enum.toHumanString enumThemeKind kind ]
                  , pane = Tab.pane [ Spacing.mt3 ]
                    [ viewThemePreview kind <| previewThemeSet kind ]
                  }) <| Enum.allValues enumThemeKind

      editTabConfig = Tab.useHash True <| Tab.config SetEditTabStateMsg
      previewTabConfig = Tab.useHash False <| Tab.config SetPreviewTabStateMsg
  in Grid.row []
      [ Grid.col [ Col.sm12, Col.md5 ]
        [ Tab.view state.editTabState <|
            Tab.items
              [ Tab.item
                  { id = "goback"
                  , link = Tab.link [] [ text "<" ]
                  , pane = Tab.pane [ Spacing.mt3 ] []
                  }
              , Tab.item
                  { id = Enum.toString enumEditTab EditBrandingTab
                  , link = Tab.link [] [ text "Branded Domain" ]
                  , pane = Tab.pane [ Spacing.mt3 ] [ brandingTab ]
                  }
              , Tab.item
                  { id = Enum.toString enumEditTab EditThemeTab
                  , link = Tab.link [] [ text "Manage Themes" ]
                  , pane = Tab.pane [ Spacing.mt3 ] [ themeTab ]
                  }
            ] editTabConfig
        ]
      , Grid.col [ Col.sm12, Col.md7 ]
        [ div [ class "scrive" ] [ Tab.view state.previewTabState <|
            Tab.items themePreviewItems previewTabConfig
        ]
      ] ]

loadThemesCmd : (Msg -> msg) -> Int -> Cmd msg
loadThemesCmd embed bdID =
  let callback : Result Http.Error (List Theme) -> msg
      callback result = case result of
        Ok availableThemes -> embed <| SetAvailableThemesMsg availableThemes
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to load domain themes."
  in Http.get
      { url = "/adminonly/brandeddomain/themes/" ++ String.fromInt bdID
      , expect = Http.expectJson callback <| JD.field "themes" <| JD.list Theme.themeDecoder
      }


getBrandedDomainCmd : (Msg -> msg) -> Int -> Cmd msg
getBrandedDomainCmd embed bdID =
  let callback : Result Http.Error BrandedDomain -> msg
      callback result = case result of
        Ok brandedDomain -> embed <| SetBrandedDomainMsg brandedDomain
        Err (Http.BadBody str) -> embed <| PresentFlashMessage <| FlashMessage.error str
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to load branded domain."
  in Http.get
      { url = "/adminonly/brandeddomain/details/" ++ String.fromInt bdID
      , expect = Http.expectJson callback brandedDomainDecoder
      }

doSaveBrandedDomain : (Msg -> msg) -> (List (String, String) -> Http.Body) -> State -> Cmd msg
doSaveBrandedDomain embed formBody state =
  let callback : Result Http.Error () -> msg
      callback result = case result of
        Ok _ -> embed <| PresentFlashMessage <| FlashMessage.success "Branded domain saved."
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to save branded domain."

      brandedDomain = state.editBrandedDomainState.brandedDomainBeingEdited
  in Http.post
      { url =
          "/adminonly/brandeddomain/details/change/" ++ String.fromInt brandedDomain.id
      , body = formBody [ ( "domain", JE.encode 0 <| encodeBrandedDomain brandedDomain ) ]
      , expect = Http.expectWhatever callback
      }
