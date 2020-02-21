module Component.Branding.DeleteTheme.Modal exposing (..)

import Bootstrap.Button as Button
import Html exposing (Html, text, button)
import Either exposing (Either (..))
import Bootstrap.Modal as Modal
import Compose.Util as Util
import Component.Theme.Data exposing (Theme)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Component.Branding.DeleteTheme.Data as Data

type alias Config = Theme

type alias State =
  { theme : Theme
  , modalState : Modal.Visibility
  }

type Msg
  = ConfirmMsg
  | CancelMsg

type alias OutMsg = Data.OutMsg

initialize : Config -> (State, Cmd Msg)
initialize theme =
  ( { theme = theme
    , modalState = Modal.shown
    }
  , Cmd.none
  )

update : Msg -> State -> (State, Cmd (Either OutMsg Msg))
update msg state1 =
  case msg of
    ConfirmMsg ->
      let
          state2 = { state1 | modalState = Modal.hidden }

          cmd : Cmd (Either OutMsg Msg)
          cmd =
            Util.msgToCmd <| Left <|
              Data.ConfirmDeleteMsg state1.theme
      in
      (state2, cmd)


    CancelMsg ->
      let
          state2 = { state1 | modalState = Modal.hidden }


          cmd : Cmd (Either OutMsg Msg)
          cmd =
            Util.msgToCmd <| Left <|
              Data.CancelDeleteMsg
      in
      (state2, cmd)

view : State -> Html Msg
view state =
  Modal.config CancelMsg
    |> Modal.h5 [] [ text "Confirm Theme Deletion" ]
    |> Modal.body []
        [ text "Are you sure you want to delete the theme?"
        , Button.button
            [ Button.danger
            , Button.attrs
                [ class "ml-sm-2"
                , onClick ConfirmMsg
                ]
            ]
            [ text "Yes" ]
        , Button.button
            [ Button.success
            , Button.attrs
                [ class "ml-sm-2"
                , onClick CancelMsg
                ]
            ]
            [ text "No" ]
        ]
    |> Modal.view state.modalState
