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


type alias Params msg model =
    { embed : Msg -> msg
    , addFlashMessageMsg : FlashMessage -> msg
    , errorTraceMsg : List ( String, JE.Value ) -> msg
    , xtoken : String
    , localization : Localization
    , participantEmail : String
    , rejectUrl : String
    , previousProviderModel : Maybe model
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
    | BackButtonClickedMsg


update : Params msg model -> State -> Msg -> (model -> msg) -> ( State, Cmd msg )
update params state msg exitRejectionMsg =
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

        BackButtonClickedMsg ->
            case params.previousProviderModel of
                Just providerModel ->
                    ( state, perform <| exitRejectionMsg providerModel )

                Nothing ->
                    ( state, Cmd.none )


viewContent : Params msg model -> State -> Html msg
viewContent params state =
    case state of
        Complete ->
            div [ class "identify-box-content" ]
                [ div [ class "identify-box-button" ] [ text "Your rejection message has been sent. Thank you!" ]
                ]

        EnterMessage { message } ->
            -- TODO: Maybe put this form in Lib and share it with FlowOverview?
            div [ class "identify-box-content rejection" ]
                [ p [ style "margin-bottom" "1em" ]
                    [ text """Please let us know why you are rejecting the Flow.""" ]
                , textarea
                    [ placeholder "Enter message..." -- TODO: Localisation
                    , autocomplete False
                    , value message
                    , onInput <| params.embed << UpdateTextareaMsg
                    ]
                    []
                , div [ class "button-group" ]
                    [ a
                        [ class "button button-large button-cancel"
                        , onClick <| params.embed BackButtonClickedMsg
                        ]
                        [ text "Back" ]
                    , a
                        [ class "button button-large button-reject"
                        , onClick <| params.embed RejectButtonClickedMsg
                        ]
                        [ text "Reject" ]
                    ]
                ]

        AlreadyRejected ->
            div [ class "identify-box-content" ]
                [ p []
                    [ text """This Flow has been rejected by you or another participant.""" ]
                ]


viewFooter : Params msg model -> Html msg
viewFooter { localization, participantEmail } =
    div []
        [ text localization.yourEmail
        , text " "
        , b [] [ text participantEmail ]
        ]
