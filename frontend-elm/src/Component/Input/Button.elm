module Component.Input.Button exposing (ButtonStatus(..), ButtonType(..), Config, Init, Msg(..), OutMsg(..), State, UpdateHandler, ViewHandler, disableMsg, enableMsg, initialize, update, view, viewOverride)

import Bootstrap.Button as Button
import Compose.Util as Util
import Either exposing (Either(..))
import Html exposing (Html, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)


type ButtonType
    = Ok
    | Danger


type ButtonStatus
    = Enabled
    | Disabled


type OutMsg data
    = ClickMsg data


type alias Config =
    { caption : String
    , buttonType : ButtonType
    }


type Msg data
    = EnableMsg
    | DisableMsg
    | ButtonClickedMsg data


type alias State =
    { status : ButtonStatus
    , caption : String
    , buttonType : ButtonType
    }


type alias UpdateHandler data =
    Msg data
    -> State
    -> ( State, Cmd (Either (OutMsg data) (Msg data)) )


type alias ViewHandler data =
    State -> Html (Msg data)


type alias Init data =
    Config -> ( State, Cmd (Msg data) )


initialize : Init data
initialize config =
    ( { status = Enabled
      , caption = config.caption
      , buttonType = config.buttonType
      }
    , Cmd.none
    )


update : UpdateHandler data
update msg1 state1 =
    case msg1 of
        EnableMsg ->
            ( { state1 | status = Enabled }
            , Cmd.none
            )

        DisableMsg ->
            ( { state1 | status = Disabled }
            , Cmd.none
            )

        ButtonClickedMsg fields ->
            let
                cmd =
                    Util.msgToCmd <|
                        Left <|
                            ClickMsg fields
            in
            ( { state1 | status = Disabled }
            , cmd
            )


view : data -> ViewHandler data
view data state =
    let
        buttonClass =
            case state.buttonType of
                Ok ->
                    Button.success

                Danger ->
                    Button.danger
    in
    Button.button
        [ buttonClass
        , Button.attrs
            [ class "ml-sm-2"
            , onClick <|
                ButtonClickedMsg data
            , disabled <|
                state.status
                    == Disabled
            ]
        ]
        [ text state.caption ]


viewOverride : data -> ButtonStatus -> ViewHandler data
viewOverride data status state =
    view data { state | status = status }


enableMsg : Msg data
enableMsg =
    EnableMsg


disableMsg : Msg data
disableMsg =
    DisableMsg
