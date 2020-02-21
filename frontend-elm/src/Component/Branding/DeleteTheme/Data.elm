module Component.Branding.DeleteTheme.Data exposing (..)

import Component.Theme.Data exposing (Theme)

type OutMsg
  = ConfirmDeleteMsg Theme
  | CancelDeleteMsg
