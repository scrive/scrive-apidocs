-- Rename to FlashAlert to avoid confusion with Msg?


module Lib.Components.FlashMessage exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Process
import Task
import Utils exposing (..)



{- Params / State -}


type alias Params msg =
    { embed : Msg -> msg
    }


type FlashMessageState
    = Visible -- 10 secs
    | TransitionToInvisible



-- | `Invisible` messages need to be rendered for 0.25s for the CSS transition
-- to finish.


type alias AlertID =
    Int


type alias State =
    { alerts :
        List
            { id : AlertID
            , message : FlashMessage
            , messageState : FlashMessageState
            }
    , nextId : AlertID
    }



{- Msg / Update -}


type Msg
    = AddFlashMessageMsg FlashMessage
    | StartFadeoutMsg AlertID
    | RemoveFlashMessageMsg AlertID


update : Params msg -> Msg -> State -> ( State, Cmd msg )
update { embed } msg state =
    case msg of
        -- | Schedule for fade-out in 10 seconds.
        AddFlashMessageMsg message ->
            let
                id =
                    state.nextId

                newState =
                    { state
                        | nextId = 1 + state.nextId
                        , alerts =
                            { id = id
                            , message = message
                            , messageState = Visible
                            }
                                :: state.alerts
                    }

                cmd =
                    Process.sleep 10000 |> Task.perform (\_ -> embed <| StartFadeoutMsg id)
            in
            ( newState, cmd )

        -- | Set state to `TransitionToInvisible` and schedule removal in 250ms.
        StartFadeoutMsg id ->
            let
                newAlerts =
                    List.map
                        (\message ->
                            if message.id == id then
                                { message | messageState = TransitionToInvisible }

                            else
                                message
                        )
                        state.alerts

                cmd =
                    Process.sleep 250 |> Task.perform (\_ -> embed <| RemoveFlashMessageMsg id)
            in
            ( { state | alerts = newAlerts }, cmd )

        RemoveFlashMessageMsg id_ ->
            ( { state | alerts = List.filter (\{ id } -> id_ /= id) state.alerts }, Cmd.none )


addFlashMessage : Params msg -> FlashMessage -> State -> ( State, Cmd msg )
addFlashMessage params =
    update params << AddFlashMessageMsg


initialState : State
initialState =
    { alerts = [], nextId = 1 }



{- View -}


view : Params msg -> State -> Html msg
view { embed } state =
    div [ class "flash" ] <|
        -- flash container
        flip List.map state.alerts
        <|
            \{ id, message, messageState } ->
                div
                    [ class "flash-content-wrapper"
                    , class <|
                        case message of
                            FlashSuccess _ ->
                                "success"

                            FlashError _ ->
                                "error"
                    , style "display" <|
                        case messageState of
                            Visible ->
                                "block"

                            TransitionToInvisible ->
                                "none"
                    ]
                    [ div [ class "flash-content" ]
                        [ div [ class "flash-body" ]
                            [ text <|
                                case message of
                                    FlashSuccess txt ->
                                        txt

                                    FlashError txt ->
                                        txt
                            ]
                        , div [ class "flash-close", onClick (embed <| StartFadeoutMsg id) ]
                            [ text "×"
                            ]
                        ]
                    ]



-- \00D7
