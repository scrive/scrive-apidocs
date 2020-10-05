module IdentifyView.Rejection.Rejection exposing (..)

import Html exposing (Html, a, b, div, input, p, text, textarea)
import Html.Attributes exposing (autocomplete, class, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error, expectWhatever, jsonBody, post)
import Json.Encode as JE
import Lib.Misc.Cmd exposing (perform)
import Lib.Misc.Http exposing (encodeError)
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Lib.Types.Localization exposing (Localization)


type alias Params msg =
    { embed : Msg -> msg
    , addFlashMessageMsg : FlashMessage -> msg
    , errorTraceMsg : List ( String, JE.Value ) -> msg
    , xtoken : String
    , localization : Localization
    , participantEmail : String
    , rejectUrl : String
    }



-- modifiable part of the model


type State
    = EnterMessage { message : String }
    | Complete
    | AlreadyRejected


type Msg
    = RejectButtonClickedMsg
    | RejectCallbackMsg (Result Error ())
    | UpdateTextareaMsg String


update : Params msg -> State -> Msg -> ( State, Cmd msg )
update params state msg =
    case msg of
        RejectButtonClickedMsg ->
            case state of
                EnterMessage { message } ->
                    let
                        postReq =
                            post
                                { url = params.rejectUrl
                                , body = jsonBody <| JE.object [ ( "message", JE.string message ) ]
                                , expect = expectWhatever <| params.embed << RejectCallbackMsg
                                }
                    in
                    ( state, postReq )

                _ ->
                    ( state, Cmd.none )

        RejectCallbackMsg res ->
            case res of
                Ok () ->
                    ( Complete, Cmd.none )

                Err err ->
                    let
                        flashMessage =
                            "Failed to reject the Flow"

                        errorFields =
                            [ ( "where", JE.string "IdentifyView.Rejection.update RejectCallbackMsg" )
                            , ( "what", JE.string flashMessage )
                            , ( "http_error", encodeError err )
                            ]
                    in
                    ( state
                    , Cmd.batch
                        [ perform <| params.addFlashMessageMsg <| FlashError flashMessage
                        , perform <| params.errorTraceMsg errorFields
                        ]
                    )

        UpdateTextareaMsg message ->
            case state of
                EnterMessage _ ->
                    ( EnterMessage { message = message }, Cmd.none )

                _ ->
                    ( state, Cmd.none )


viewContent : Params msg -> State -> Html msg
viewContent params state =
    case state of
        Complete ->
            div [ class "identify-box-content" ]
                [ div [ class "identify-box-button" ] [ text "Your rejection message has been sent. Thank you!" ]
                ]

        EnterMessage { message } ->
            div [ class "identify-box-content" ]
                [ p [ style "margin-bottom" "1em" ]
                    [ text """Please let us know why you reject this Flow.""" ]
                , textarea
                    [ style "margin-bottom" "1em"
                    , style "width" "100%"
                    , style "height" "6em"
                    , placeholder "Enter message..." -- TODO: Localisation
                    , autocomplete False
                    , value message
                    , onInput <| params.embed << UpdateTextareaMsg
                    ]
                    []
                , a
                    [ class "button"
                    , class "button-large"
                    , class "action"
                    , onClick <| params.embed RejectButtonClickedMsg
                    ]
                    [ text "Reject" ]
                ]

        AlreadyRejected ->
            div [ class "identify-box-content" ]
                [ p []
                    [ text """This Flow has been rejected by you or another participant.""" ]
                ]


viewFooter : Params msg -> Html msg
viewFooter { localization, participantEmail } =
    div []
        [ text localization.yourEmail
        , text " "
        , b [] [ text participantEmail ]
        ]
