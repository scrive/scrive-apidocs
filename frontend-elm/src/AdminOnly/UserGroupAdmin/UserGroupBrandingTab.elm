module AdminOnly.UserGroupAdmin.UserGroupBrandingTab exposing (..)

import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Types exposing (..)
import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Json exposing (..)
import Lib.Theme.EditTheme as EditTheme
import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.EditUserGroupBranding as EditUserGroupBranding
import Lib.Theme.PreviewTheme exposing (..)
import Lib.Theme.Types exposing (..)
import Lib.Theme.Json exposing (..)
import Bootstrap.Tab as Tab
import Json.Decode as JD
import Json.Encode as JE
import Result exposing (Result (..))
import Either exposing (Either(..))
import Http
import Html exposing (Html, text, div)
import Html.Attributes exposing (class)
import Bootstrap.Utilities.Spacing as Spacing
import Utils exposing (..)
import Util.Http as Http
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Vendor.ColorPickerExtra as ColorPicker
import Vendor.Popover as Popover
import Maybe exposing (withDefault)
import FlashMessage exposing (FlashMessage)
import List.Extra as List
import EnumExtra as Enum exposing (Enum)

tabName : String
tabName = "branding"

type Page = EditBrandingTab | EditThemeTab

enumEditTab : Enum Page
enumEditTab =
  let allValues = [EditBrandingTab, EditThemeTab]
      toString t = case t of
        EditBrandingTab -> "branding"
        EditThemeTab -> "theme"
      toHumanString = toString
  in Enum.makeEnum allValues toString toHumanString

type alias LoadingState =
  { domainThemesLoaded : Bool
  , userGroupBrandingLoaded : Bool
  , userGroupThemesLoaded : Bool }

type alias State =
  { loadingState : LoadingState
  , editTabPage : Page
  , editTabState : Tab.State
  , previewTabState : Tab.State
  , availableThemes : List Theme
  , domainThemes : Enum.Dict ThemeKind Theme
  , editThemeState :
    { themeBeingEdited : Theme
    , colorPickers : Enum.Dict ColorIdentifier ColorPicker.State
    , popovers : Enum.Dict ColorIdentifier Popover.State
    }
  , editUserGroupBrandingState : UserGroupBranding
  , ugid : String
  }

type Msg = EditThemeMsg EditTheme.Msg
         | EditUserGroupBrandingMsg EditUserGroupBranding.Msg
         | SetEditTabStateMsg Tab.State
         | SetPreviewTabStateMsg Tab.State
         | SetDomainThemesMsg (Enum.Dict ThemeKind Theme)
         | SetAvailableThemesMsg (List Theme)
         | SetUserGroupBrandingMsg UserGroupBranding
         | DeleteThemeCallbackMsg ThemeID
         | CreateThemeCallbackMsg Theme ThemeID
         | SaveThemeMsg Theme
         | DoCreateThemeMsg Theme
         | DoDeleteThemeMsg
         | DoSaveThemeMsg
         | DoSaveUserGroupBrandingMsg
         | PresentFlashMessage FlashMessage

update :
  (Msg -> msg) -> Globals msg -> Msg -> State -> (State, Cmd msg)
update embed globals msg = case msg of
  SetEditTabStateMsg tabState -> setEditTabState tabState
  SetPreviewTabStateMsg tabState -> setPreviewTabState tabState
  SetDomainThemesMsg domainThemes -> setDomainThemes domainThemes
  SetAvailableThemesMsg availableThemes -> setAvailableThemes availableThemes
  SetUserGroupBrandingMsg branding -> setUserGroupBranding branding
  DeleteThemeCallbackMsg id -> deleteThemeCallback globals.flashMessage id
  CreateThemeCallbackMsg blueprint newThemeID -> createThemeCallback embed (formBody globals) blueprint newThemeID
  SaveThemeMsg theme -> saveTheme globals.flashMessage theme
  DoCreateThemeMsg blueprint -> doCreateTheme embed (formBody globals) blueprint
  DoDeleteThemeMsg -> doDeleteTheme embed (formBody globals)
  DoSaveThemeMsg -> doSaveTheme embed (formBody globals)
  DoSaveUserGroupBrandingMsg -> doSaveUserGroupBranding embed (formBody globals)
  EditThemeMsg msg_ -> \state ->
    let editThemeReadonly =
          { availableThemes = Enum.values state.domainThemes ++ state.availableThemes
          }
        (newEditThemeState, cmd) =
          EditTheme.update (embed << EditThemeMsg) msg_ editThemeReadonly state.editThemeState
    in ({ state | editThemeState = newEditThemeState }, cmd)
  EditUserGroupBrandingMsg msg_ -> \state ->
    let branding = state.editUserGroupBrandingState
        (newBranding, cmd) = EditUserGroupBranding.update (embed << EditUserGroupBrandingMsg) msg_ state.editUserGroupBrandingState
        mPreviewThemeKind =
            List.find (\kind -> Enum.get kind newBranding.themes /= Enum.get kind branding.themes)
            <| Enum.allValues enumThemeKind
        newPreviewTabState =
            Maybe.withDefault state.previewTabState
              <| Maybe.map (Tab.customInitialState << Enum.toString enumThemeKind) mPreviewThemeKind
    in ({ state | editUserGroupBrandingState = newBranding, previewTabState = newPreviewTabState }, cmd)
  PresentFlashMessage message -> \state -> (state, globals.flashMessage message)

updatePage : (Msg -> msg) -> { ugid : String, page : Page } -> State -> (State, Cmd msg)
updatePage embed params state =
  if params.ugid == state.ugid
  then ({ state | editTabPage = params.page }, Cmd.none)
  else init embed { page = params.page, ugid = params.ugid }

pageFromModel : State -> Page
pageFromModel = .editTabPage

init : (Msg -> msg) -> { page : Page, ugid : String } -> (State, Cmd msg)
init embed {page, ugid} =
  let initialState =
        { loadingState =
          { domainThemesLoaded = False
          , userGroupBrandingLoaded = False
          , userGroupThemesLoaded = False }
        , editTabPage = page
        , editTabState = Tab.customInitialState <| Enum.toString enumEditTab page
        , previewTabState = Tab.customInitialState <| Enum.toString enumThemeKind EmailTheme
        , availableThemes = []
        , domainThemes = Enum.empty enumThemeKind
        , editThemeState =
          { themeBeingEdited = errorTheme
          , colorPickers = Enum.fromList enumColorIdentifier
              <| List.map (\ident ->
                  (ident, ColorPicker.initWithId <| "colorpicker-" ++ Enum.toString enumColorIdentifier ident))
              <| Enum.allValues enumColorIdentifier
          , popovers = Enum.fromList enumColorIdentifier
              <| List.map (\ident -> (ident, Popover.initialState))
              <| Enum.allValues enumColorIdentifier
          }
        , editUserGroupBrandingState = fallbackUserGroupBranding
        , ugid = ugid
        }
  in (initialState
     , Cmd.batch [ getDomainThemes embed, getUserGroupThemes embed ugid, getUserGroupBranding embed ugid ]
     )

setEditTabState : Tab.State -> State -> (State, Cmd msg)
setEditTabState tabState state = ({ state | editTabState = tabState }, Cmd.none)

setPreviewTabState : Tab.State -> State -> (State, Cmd msg)
setPreviewTabState tabState state = ({ state | previewTabState = tabState }, Cmd.none)

view : (Msg -> msg) -> State -> Html msg
view embed state =
  if state.loadingState.userGroupBrandingLoaded
    && state.loadingState.userGroupThemesLoaded
    && state.loadingState.domainThemesLoaded
  then Html.map embed <| viewLoaded state
  else text "Loading"

viewLoaded : State -> Html Msg
viewLoaded state =
  let editTabConfig = Tab.useHash True <| Tab.config SetEditTabStateMsg
      previewTabConfig = Tab.useHash False <| Tab.config SetPreviewTabStateMsg

      themeTab = EditTheme.viewEditTheme
        { embed = EditThemeMsg
        , doSaveTheme = DoSaveThemeMsg
        , doDeleteTheme = DoDeleteThemeMsg
        , doCreateTheme = DoCreateThemeMsg
        }
        { availableThemes = Enum.values state.domainThemes ++ state.availableThemes
        } state.editThemeState

      brandingTab = EditUserGroupBranding.viewEditUserGroupBranding
        { embed = EditUserGroupBrandingMsg
        , doSaveUserGroupBranding = DoSaveUserGroupBrandingMsg
        }
        { domainThemes = state.domainThemes
        , availableThemes = state.availableThemes
        }
        state.editUserGroupBrandingState

      previewThemeSet : ThemeKind -> Theme
      previewThemeSet kind = case state.editTabPage of
        EditThemeTab -> state.editThemeState.themeBeingEdited
        EditBrandingTab ->
          Maybe.withDefault errorTheme <|
            case Enum.get kind state.editUserGroupBrandingState.themes of
              Nothing -> Enum.get kind state.domainThemes
              Just id -> List.find (\theme -> theme.id == id) state.availableThemes

      viewThemePreview kind = case kind of
            EmailTheme -> viewEmailThemePreview
            SignViewTheme -> viewSignViewThemePreview
            ServiceTheme -> viewServiceThemePreview

      themePreviewItems = List.map (\kind -> Tab.item
                  { id = Enum.toString enumThemeKind kind
                  , link = Tab.link [] [ text <| Enum.toHumanString enumThemeKind kind ]
                  , pane = Tab.pane [ Spacing.mt3 ]
                    [ viewThemePreview kind <| previewThemeSet kind ]
                  }) <| Enum.allValues enumThemeKind
  in Grid.row []
      [ Grid.col [ Col.sm12, Col.md5 ]
        [ Tab.view state.editTabState <|
            Tab.items
              [ Tab.item
                  { id = Enum.toString enumEditTab EditBrandingTab
                  , link = Tab.link [] [ text "User Group Branding" ]
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

setDomainThemes : (Enum.Dict ThemeKind Theme) -> State -> (State, Cmd msg)
setDomainThemes domainThemes state =
  let editThemeState = state.editThemeState
      loadingState = state.loadingState
      newState =
        { state
        | domainThemes = domainThemes
        , loadingState = { loadingState | domainThemesLoaded = True }
        , editThemeState =
              { editThemeState
              | themeBeingEdited =
                  withDefault editThemeState.themeBeingEdited
                    <| Enum.get EmailTheme domainThemes
              }
        }
  in (newState, Cmd.none)

getDomainThemes : (Msg -> msg) -> Cmd msg
getDomainThemes embed =
  let callback : Result Http.Error (Enum.Dict ThemeKind Theme) -> msg
      callback result = case result of
        Ok domainThemes -> embed <| SetDomainThemesMsg domainThemes
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to get domain themes."

      -- Domain themes are the themes we implicitly get if we set a theme id to
      -- null in the branding. We manually assign negative ids as a hack to a)
      -- express that these should be read-only and b) disambiguate between
      -- them, since they might correspond to the same underlying theme.
      toDomainThemes : Theme -> Theme -> Theme -> (Enum.Dict ThemeKind Theme)
      toDomainThemes mailTheme signViewTheme serviceTheme =
        Enum.fromList enumThemeKind
          [ (EmailTheme, { mailTheme | name = "Domain Email Theme", id = -1 })
          , (SignViewTheme, { signViewTheme | name = "Domain Sign View Theme", id = -2 })
          , (ServiceTheme, { serviceTheme | name = "Domain Service Theme", id = -3 })
          ]

      decoder : JD.Decoder (Enum.Dict ThemeKind Theme)
      decoder = JD.field "themes" <|
        JD.map3 toDomainThemes (JD.index 0 themeDecoder)
        (JD.index 1 themeDecoder) (JD.index 2 themeDecoder)

  in Http.get
      { url = "/adminonly/companyadmin/branding/companybranding/domainthemes"
      , expect = Http.expectJson callback decoder
      }

setAvailableThemes : List Theme -> State -> (State, Cmd msg)
setAvailableThemes availableThemes state =
  let loadingState = state.loadingState
      newState =
        { state
        | availableThemes = availableThemes
        , loadingState = { loadingState | userGroupThemesLoaded = True }
        }
  in (newState, Cmd.none)

getUserGroupThemes : (Msg -> msg) -> String -> Cmd msg
getUserGroupThemes embed ugid =
  let callback : Result Http.Error (List Theme) -> msg
      callback result = case result of
        Ok availableThemes -> embed <| SetAvailableThemesMsg availableThemes
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to get user group themes."
  in Http.get
      { url = "/adminonly/companyadmin/branding/companybranding/themes/" ++ ugid
      , expect = Http.expectJson callback <| JD.field "themes" <| JD.list themeDecoder
      }

setUserGroupBranding : UserGroupBranding -> State -> (State, Cmd msg)
setUserGroupBranding branding state =
  let loadingState = state.loadingState
      newState =
        {state | loadingState = { loadingState | userGroupBrandingLoaded = True }
               , editUserGroupBrandingState = branding }
  in (newState, Cmd.none)

getUserGroupBranding : (Msg -> msg) -> String -> Cmd msg
getUserGroupBranding embed ugid =
  let callback : Result Http.Error UserGroupBranding -> msg
      callback result = case result of
        Ok branding -> embed <| SetUserGroupBrandingMsg branding
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to get user group branding."
  in Http.get
      { url = "/adminonly/companyadmin/branding/companybranding/" ++ ugid
      , expect = Http.expectJson callback brandingDecoder
      }

doSaveUserGroupBranding : (Msg -> msg) -> (List (String, String) -> Http.Body) -> State -> (State, Cmd msg)
doSaveUserGroupBranding embed formBody state =
  let callback : Result Http.Error () -> msg
      callback result = case result of
        Ok _ -> embed <| PresentFlashMessage <| FlashMessage.success "User group branding saved."
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to save user group branding."

      cmd = Http.post
        { url = "/adminonly/companyadmin/branding/companybranding/change/" ++ state.ugid
        , body = formBody
            [ ( "companyui", JE.encode 0 <| encodeUserGroupBranding state.ugid state.editUserGroupBrandingState ) ]
        , expect = Http.expectWhatever callback
        }
  in (state, cmd)

saveTheme : (FlashMessage -> Cmd msg) -> Theme -> State -> (State, Cmd msg)
saveTheme presentFlashMessage theme state =
  let newState =
        { state
        | availableThemes =
          List.map
            (\theme_ -> if theme_.id == theme.id then theme else theme_)
            state.availableThemes
        }
      cmd = presentFlashMessage <| FlashMessage.success "Theme saved."
  in (newState, cmd)

doSaveTheme : (Msg -> msg) -> (List (String, String) -> Http.Body) -> State -> (State, Cmd msg)
doSaveTheme embed formBody state =
  let theme = state.editThemeState.themeBeingEdited
      callback : Result Http.Error () -> msg
      callback result = case result of
        Ok _ -> embed <| SaveThemeMsg theme
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to save theme."

      cmd = Http.post
        { url = "/adminonly/companyadmin/branding/companybranding/updatetheme/"
                  ++ state.ugid ++ "/" ++ String.fromInt theme.id
        , body = formBody
                  [ ( "theme", JE.encode 0 <| encodeTheme theme ) ]
        , expect = Http.expectWhatever callback
        }
  in (state, cmd)

deleteThemeCallback : (FlashMessage -> Cmd msg) -> ThemeID -> State -> (State, Cmd msg)
deleteThemeCallback presentFlashMessage id state =
  let newAvailableThemes = List.filter (\theme -> theme.id /= id) state.availableThemes
      editThemeState = state.editThemeState
      newThemeBeingEdited = if editThemeState.themeBeingEdited.id /= id
        then editThemeState.themeBeingEdited
        else withDefault errorTheme <| List.head
          <| newAvailableThemes ++ Enum.values state.domainThemes
      newState =
        { state
        | availableThemes = newAvailableThemes
        , editThemeState = { editThemeState | themeBeingEdited = newThemeBeingEdited }
        }
      cmd = presentFlashMessage <| FlashMessage.success "Theme deleted."
  in (newState, cmd)

doDeleteTheme : (Msg -> msg) -> (List (String, String) -> Http.Body) -> State -> (State, Cmd msg)
doDeleteTheme embed formBody state =
  let callback : Result Http.Error () -> msg
      callback result = case result of
        Ok _ -> embed <| DeleteThemeCallbackMsg theme.id
        _ -> embed <| PresentFlashMessage
          <| FlashMessage.error "Failed to get delete theme. You probably tried to delete a theme that is still in use."

      theme = state.editThemeState.themeBeingEdited

      cmd = Http.post
        { url = "/adminonly/companyadmin/branding/companybranding/deletetheme/"
                  ++ state.ugid ++ "/" ++ String.fromInt theme.id
        , body = formBody
                  [ ( "theme", JE.encode 0 <| encodeTheme theme ) ]
        , expect = Http.expectWhatever callback
        }
  in (state, cmd)

-- we need to update the newly created theme to the correct blueprint
createThemeCallback : (Msg -> msg) -> (List (String, String) -> Http.Body) -> Theme -> ThemeID -> State -> (State, Cmd msg)
createThemeCallback embed formBody blueprint newThemeID state =
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
  in doSaveTheme embed formBody newState

doCreateTheme : (Msg -> msg) -> (List (String, String) -> Http.Body) -> Theme -> State -> (State, Cmd msg)
doCreateTheme embed formBody blueprint state =
  let callback : Result Http.Error Theme -> msg
      callback result = case result of
        Ok new -> embed <| CreateThemeCallbackMsg blueprint new.id
        _ -> embed <| PresentFlashMessage <| FlashMessage.error "Failed to create theme."
      cmd = Http.post
        { url = "/adminonly/companyadmin/branding/companybranding/newtheme/"
                  ++ state.ugid ++ "/mail"
        , body = formBody [ ( "name", "New theme" ) ]
        , expect = Http.expectJson callback themeDecoder
        }
  in (state, cmd)
