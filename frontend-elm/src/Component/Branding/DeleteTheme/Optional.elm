module Component.Branding.DeleteTheme.Optional exposing (..)

import Html exposing (Html)
import Either exposing (Either (..))
import Component.Theme.Data exposing (Theme)
import Component.Branding.DeleteTheme.Modal as Base
import Compose.Optional as Optional

type alias Config =
  Optional.Config Base.Config

type alias State =
  Optional.State Base.State

type alias Msg =
  Optional.Msg Base.Config Base.Msg

type alias OutMsg = Base.OutMsg

initialize : Config -> (State, Cmd Msg)
initialize = Optional.liftInit Base.initialize

update : Msg -> State -> (State, Cmd (Either OutMsg Msg))
update = Optional.liftUpdate True Base.initialize Base.update

view : State -> Maybe (Html Msg)
view = Optional.liftView Base.view

showModalMsg : Theme -> Msg
showModalMsg theme = Optional.InitMsg theme
