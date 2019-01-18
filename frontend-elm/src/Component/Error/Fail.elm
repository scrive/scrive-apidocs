module Component.Error.Fail exposing (Config, Init, Msg, State, UpdateHandler, ViewHandler, initialize, update, view)

import Either exposing (Either(..))
import Html exposing (Html, div, h2, text)


type alias Config =
    String


type alias Msg =
    Never


type alias State =
    String


type alias UpdateHandler =
    Msg -> State -> ( State, Cmd (Either Never Msg) )


type alias ViewHandler =
    State -> Html Msg


type alias Init =
    Config -> ( State, Cmd Msg )


initialize : Init
initialize state =
    ( state, Cmd.none )


update : UpdateHandler
update msg _ =
    never msg


view : ViewHandler
view errorMessage =
    div
        []
        [ h2 []
            [ text "Oops, something went wrong" ]
        , text errorMessage
        ]
