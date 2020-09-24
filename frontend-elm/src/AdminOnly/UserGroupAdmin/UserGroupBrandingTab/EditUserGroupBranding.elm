module AdminOnly.UserGroupAdmin.UserGroupBrandingTab.EditUserGroupBranding exposing (..)

import Html exposing (Html, text, div, img)
import String
import Maybe exposing (withDefault)
import List exposing (map)
import Utils exposing (..)
import Bootstrap.Form.Select as Select
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Grid.Row as Row
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Html.Attributes exposing (selected, value, class, style, src, type_, disabled)
import Html.Events exposing (onInput, onClick)
import Tuple exposing (..)
import EnumExtra as Enum
import Maybe.Extra as Maybe
import File exposing (File)
import File.Select as FileSelect
import Task

import Lib.Types.Theme exposing (..)
import AdminOnly.UserGroupAdmin.UserGroupBrandingTab.Types exposing (..)

type alias EditUserGroupBrandingReadonly =
  { userGroupThemes : List Theme
  , domainThemes : Enum.Dict ThemeKind Theme
  , inheritableBranding : Maybe UserGroupBranding
  , inheritedThemes : List Theme
  }

type alias EditUserGroupBrandingState =
  { brandingBeingEdited : UserGroupBranding
  , inherit : Bool
  }

brandingPreview : EditUserGroupBrandingReadonly -> EditUserGroupBrandingState -> UserGroupBranding
brandingPreview read state =
  if state.inherit
  then Maybe.withDefault defaultUserGroupBranding read.inheritableBranding
  else state.brandingBeingEdited

type Msg = SetThemeMsg ThemeKind (Maybe ThemeID)
         | SetBrowserTitleMsg String
         | SetSmsOriginatorMsg String
         | OpenFaviconFileSelectMsg
         | LoadFaviconFileMsg File
         | SetFaviconMsg String
         | SetInheritBrandingMsg Bool

update : (Msg -> msg) -> Msg -> EditUserGroupBrandingState -> (EditUserGroupBrandingState, Cmd msg)
update embed msg =
  let updateBranding m state =
        let (newBranding, cmd) = m state.brandingBeingEdited
        in ({ state | brandingBeingEdited = newBranding }, cmd)
  in
  case msg of
    SetThemeMsg kind mId -> updateBranding <| setTheme kind mId
    SetBrowserTitleMsg text -> updateBranding <| setBrowserTitle text
    SetSmsOriginatorMsg text -> updateBranding <| setSmsOriginator text
    OpenFaviconFileSelectMsg -> updateBranding <| openFaviconFileSelect embed
    LoadFaviconFileMsg file -> updateBranding <| loadFaviconFile embed file
    SetFaviconMsg contents -> updateBranding <| setFavicon contents
    SetInheritBrandingMsg inherit -> setInheritBranding inherit


{- Theme selector -}

-- implements SetThemeMsg
setTheme : ThemeKind -> Maybe ThemeID -> UserGroupBranding -> (UserGroupBranding, Cmd msg)
setTheme kind mId state =
  let newThemes = case mId of
        Just id -> Enum.insert kind id state.themes
        Nothing -> Enum.remove kind state.themes
  in ({ state | themes = newThemes }, Cmd.none)

-- The 'theme selector' is a drop-down list of theme names to select.
viewThemeSelector :
  (Msg -> msg) -> ThemeKind -> EditUserGroupBrandingReadonly -> EditUserGroupBrandingState -> Form.Col msg
viewThemeSelector embed kind read state =
  let branding = brandingPreview read state

      availableThemes = if state.inherit
        then read.inheritedThemes
        else read.userGroupThemes

      onSelect : String -> msg
      onSelect = embed << SetThemeMsg kind << String.toInt

      themeItem : Theme -> Select.Item msg
      themeItem theme = Select.item
        [ value <| if theme.id <= 0 then "null" else String.fromInt theme.id
        , selected <| case Enum.get kind branding.themes of
            Just id -> theme.id == id
            Nothing -> theme.id <= 0 ]
        [ text theme.name ]
  in  Form.col
        [ Col.sm8, Col.md8, Col.lg8 ]
        [ Select.select
          [ Select.onChange onSelect, Select.disabled state.inherit ]
          (List.map themeItem <| Maybe.cons (Enum.get kind read.domainThemes) availableThemes)
        ]


{- User group branding editor -}

-- implements SetBrowserTitleMsg
setBrowserTitle : String -> UserGroupBranding -> (UserGroupBranding, Cmd msg)
setBrowserTitle text state =
  ({ state | browserTitle = stringNonEmpty text }, Cmd.none)

-- implements SetSmsOriginatorMsg
setSmsOriginator : String -> UserGroupBranding -> (UserGroupBranding, Cmd msg)
setSmsOriginator text state =
  ({ state | smsOriginator = stringNonEmpty text }, Cmd.none)

-- implements OpenFaviconFileSelectMsg
openFaviconFileSelect : (Msg -> msg) -> UserGroupBranding -> (UserGroupBranding, Cmd msg)
openFaviconFileSelect embed state =
  let callback : File -> msg
      callback file = embed <| LoadFaviconFileMsg file
  in (state, FileSelect.file ["image/png"] callback)

-- implements LoadFaviconFileMsg
loadFaviconFile : (Msg -> msg) -> File -> UserGroupBranding -> (UserGroupBranding, Cmd msg)
loadFaviconFile embed file state =
  let callback : String -> msg
      callback contents = embed <| SetFaviconMsg contents
  in (state, Task.perform callback <| File.toUrl file)

-- implements SetFaviconMsg
setFavicon : String -> UserGroupBranding -> (UserGroupBranding, Cmd msg)
setFavicon content state =
  ( { state | favicon = stringNonEmpty content }, Cmd.none )

-- implements SetInheritBrandingMsg
setInheritBranding : Bool -> EditUserGroupBrandingState -> (EditUserGroupBrandingState, Cmd msg)
setInheritBranding inherit state =
  ( { state | inherit = inherit }, Cmd.none )

{- The user group branding editor consists of
- a 'browser title' text field
- an 'SMS originator' text field
- theme selectors for Email, Sign View and Service themes
-}
viewEditUserGroupBranding :
  { embed : Msg -> msg
  , doSaveUserGroupBranding : msg  -- does post request and pop-over
  } -> EditUserGroupBrandingReadonly -> EditUserGroupBrandingState -> Html msg
viewEditUserGroupBranding
  {embed, doSaveUserGroupBranding} read state =
  let branding = brandingPreview read state
  in
  div []
    [ Form.row []
      [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Inheritance" ]
      , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
          [ Checkbox.checkbox
            [ Checkbox.disabled <| Maybe.isNothing read.inheritableBranding
            , Checkbox.onCheck <| embed << SetInheritBrandingMsg
            , Checkbox.checked state.inherit ]
            "Inherit branding"
          , Form.help [] [ text <| "Inherit all branding options from the "
              ++ "parent user group. Discards any customisations made to the "
              ++ "branding (but keeps any custom themes)." ]
          ]
      ]
    , Form.row []
        [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Favicon" ]
        , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
          [ div [] [case branding.favicon of
              Just favicon -> img [ style "max-width" "50px", src favicon] []
              Nothing -> text "Not overridden"]
          , Button.button
              [ Button.secondary
              , Button.attrs
                [ onClick (embed OpenFaviconFileSelectMsg)
                , type_ "button"
                , disabled state.inherit
                ]
              ]
              [ text "Select image" ]
          , Button.button
              [ Button.secondary
              , Button.attrs
                [ onClick (embed <| SetFaviconMsg "")
                , type_ "button"
                , disabled state.inherit
                ]
              ]
              [ text "Reset" ]
          ]
        ]
    , Form.row []
      [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Browser title" ]
      , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
          [ Input.text
            [ Input.attrs
              [ value <| withDefault "" branding.browserTitle
              , onInput (embed << SetBrowserTitleMsg)
              , disabled state.inherit
              ]
            ]
          , Form.help [] [ text "The text at the top of the browser." ]
          ]
      ]
    , Form.row []
      [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "SMS Originator" ]
      , Form.col [ Col.sm8, Col.md8, Col.lg8 ]
          [ Input.text
            [ Input.attrs
              [ value <| withDefault "" branding.smsOriginator
              , onInput (embed << SetSmsOriginatorMsg)
              , disabled state.inherit
              ]
            ]
          , Form.help [] [ text <| "The name displayed to the recipient when "
          ++ "receiving an SMS. Maximum 11 alpha-numeric characters." ]
          ]
      ]
    , Form.row []
      [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Email theme" ]
      , viewThemeSelector embed EmailTheme read state ]
    , Form.row []
      [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Sign View theme" ]
      , viewThemeSelector embed SignViewTheme read state ]
    , Form.row []
      [ Form.colLabel [ Col.sm4, Col.md4, Col.lg4 ] [ text "Service theme" ]
      , viewThemeSelector embed ServiceTheme read state ]
    , Form.row [ Row.rightSm ]
      [ Form.col [ Col.sm12 ]
          [ Button.button
            [ Button.success
            , Button.attrs [ class "ml-sm-2", onClick doSaveUserGroupBranding ]
            ]
          [ text "Save changes" ] ]
      ]
    ]
