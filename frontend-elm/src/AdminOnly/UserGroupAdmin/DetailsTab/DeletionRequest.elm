module AdminOnly.UserGroupAdmin.DetailsTab.DeletionRequest exposing (..)

import AdminOnly.Types.UserGroup exposing (UserGroup)
import AdminOnly.Types.UserGroup.DeletionRequest as DeletionRequest exposing (DeletionRequest)
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid exposing (Column)
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import FlashMessage exposing (FlashMessage)
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as JD
import Maybe.Extra as Maybe
import Return exposing (..)
import Task
import Time
import Utils exposing (formBody, monthToInt, perform)


type alias Params msg =
    { embed : Msg -> msg
    , userGroupId : String
    , xtoken : String
    , updateDeletionRequest : Maybe DeletionRequest -> Cmd msg
    , showFlashMessage : FlashMessage -> Cmd msg
    }



-- Inherited from the parent component; can't be modified directly.


type alias ReadonlyState =
    { deletionStatus : Maybe DeletionRequest
    }


type State
    = Loading
    | Loaded { userId : String, modalState : ModalState }


type ModalState
    = Idle
    | RequestDeletionModal { date : String }
    | SignOffDeletionModal


type
    Msg
    -- Loading
    = GetProfileCallbackMsg (Result Http.Error { userId : String })
      -- Abort modal button
    | AbortModalMsg
      -- Request deletion
    | RequestDeletionClickedMsg
    | ShowRequestDeletionModalMsg { date : String }
    | UpdateDeletionDateMsg String
    | ConfirmRequestDeletionMsg { date : String }
    | RequestDeletionCallbackMsg (Result Http.Error (Maybe DeletionRequest))
      -- Cancel deletion
    | CancelDeletionClickedMsg
    | CancelDeletionCallbackMsg (Result Http.Error ())
      -- Sign off on deletion request
    | SignOffDeletionClickedMsg
    | ConfirmSignOffDeletionMsg
    | SignOffDeletionCallbackMsg (Result Http.Error DeletionRequest)


init : Params msg -> Return msg State
init params =
    let
        decodeUserId =
            JD.field "id" JD.string |> JD.map (\userId -> { userId = userId })
    in
    return Loading <|
        Http.get
            { url = "/api/v2/getprofile/"
            , expect = Http.expectJson (params.embed << GetProfileCallbackMsg) decodeUserId
            }


update : Params msg -> Msg -> State -> Return msg State
update params msg state =
    case state of
        Loading ->
            singleton <|
                case msg of
                    GetProfileCallbackMsg res ->
                        case res of
                            Ok { userId } ->
                                Loaded { userId = userId, modalState = Idle }

                            Err _ ->
                                state

                    _ ->
                        state

        Loaded { userId, modalState } ->
            let
                ( newModalState, cmd ) =
                    updateModal params msg modalState
            in
            return (Loaded { userId = userId, modalState = newModalState }) cmd


updateModal : Params msg -> Msg -> ModalState -> Return msg ModalState
updateModal params msg state =
    case msg of
        AbortModalMsg ->
            abortModal state

        RequestDeletionClickedMsg ->
            requestDeletionClicked params state

        ShowRequestDeletionModalMsg date ->
            singleton <| RequestDeletionModal date

        UpdateDeletionDateMsg date ->
            singleton <|
                case state of
                    RequestDeletionModal _ ->
                        RequestDeletionModal { date = date }

                    _ ->
                        state

        ConfirmRequestDeletionMsg { date } ->
            confirmRequestDeletion params { date = date }

        RequestDeletionCallbackMsg res ->
            requestDeletionCallback params state res

        CancelDeletionClickedMsg ->
            cancelDeletionClicked params

        CancelDeletionCallbackMsg res ->
            cancelDeletionCallback params state res

        SignOffDeletionClickedMsg ->
            signOffDeletionClicked state

        ConfirmSignOffDeletionMsg ->
            confirmSignOffDeletion params

        SignOffDeletionCallbackMsg res ->
            signOffDeletionCallback params state res

        GetProfileCallbackMsg _ ->
            singleton state


viewModal : State -> Maybe (Html Msg)
viewModal state =
    case state of
        Loading ->
            Nothing

        Loaded { modalState } ->
            case modalState of
                Idle ->
                    Nothing

                RequestDeletionModal s ->
                    Just <| viewRequestDeletionModal s

                SignOffDeletionModal ->
                    Just <| viewSignOffDeletionModal


viewButtons : (Msg -> msg) -> ReadonlyState -> State -> Column msg
viewButtons embed { deletionStatus } state =
    Grid.col [ Col.sm12 ] <|
        case state of
            Loading ->
                []

            Loaded { userId } ->
                case deletionStatus of
                    Nothing ->
                        [ Html.map embed viewRequestDeletionButton ]

                    Just deletionRequest ->
                        let
                            isSignedOff =
                                Maybe.isJust deletionRequest.signedOffBy
                        in
                        (if not isSignedOff && deletionRequest.requestedBy /= userId then
                            [ Html.map embed viewSignOffDeletionButton ]

                         else
                            []
                        )
                            ++ [ Html.map embed viewCancelDeletionButton
                               , text <|
                                    (if isSignedOff then
                                        "Scheduled deletion date: "

                                     else
                                        "Requested deletion date: "
                                    )
                                        ++ deletionRequest.requestedDeletionDate
                                        ++ (if not isSignedOff && deletionRequest.requestedBy == userId then
                                                ". Needs to be signed off by another Scrive admin."

                                            else
                                                ""
                                           )
                               ]



-- AbortModalMsg


abortModal : ModalState -> Return msg ModalState
abortModal _ =
    singleton Idle



{- Request deletion -}


viewRequestDeletionButton : Html Msg
viewRequestDeletionButton =
    Button.button
        [ Button.danger
        , Button.attrs [ class "mt-sm-2", onClick RequestDeletionClickedMsg ]
        ]
        [ text "Request deletion"
        ]



-- RequestDeletionClickedMsg


requestDeletionClicked : Params msg -> ModalState -> Return msg ModalState
requestDeletionClicked { embed } state =
    let
        posixToDate posix =
            { date =
                String.padLeft 4 '0' (String.fromInt <| Time.toYear Time.utc posix)
                    ++ "-"
                    ++ String.padLeft 2 '0' (String.fromInt <| monthToInt <| Time.toMonth Time.utc posix)
                    ++ "-"
                    ++ String.padLeft 2 '0' (String.fromInt <| Time.toDay Time.utc posix)
            }
    in
    return state <| Task.perform (embed << ShowRequestDeletionModalMsg << posixToDate) Time.now


viewRequestDeletionModal : { date : String } -> Html Msg
viewRequestDeletionModal { date } =
    Modal.config AbortModalMsg
        |> Modal.h1 [] [ text "Request deletion of user group" ]
        |> Modal.body []
            [ text <|
                "This will request the deletion of the current user group. "
                    ++ "Note that the request will need to be signed off by "
                    ++ "a second scrive admin for the deletion to be carried out. "
                    ++ "Desired deletion date: "
            , Input.date [ Input.value date, Input.onInput UpdateDeletionDateMsg ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.secondary
                , Button.attrs [ onClick AbortModalMsg ]
                ]
                [ text "Cancel"
                ]
            , Button.button
                [ Button.danger
                , Button.attrs [ onClick <| ConfirmRequestDeletionMsg { date = date } ]
                ]
                [ text "Request deletion"
                ]
            ]
        |> Modal.view Modal.shown



-- ConfirmRequestDeletionMsg


confirmRequestDeletion : Params msg -> { date : String } -> Return msg ModalState
confirmRequestDeletion params { date } =
    return Idle <|
        Http.post
            { url = "/adminonly/companyadmin/requestdeletion/" ++ params.userGroupId
            , body = formBody params [ ( "deletion_date", date ) ]
            , expect =
                Http.expectJson (params.embed << RequestDeletionCallbackMsg) (JD.nullable DeletionRequest.decoder)
            }


requestDeletionCallback : Params msg -> ModalState -> Result Http.Error (Maybe DeletionRequest) -> Return msg ModalState
requestDeletionCallback { showFlashMessage, updateDeletionRequest } state res =
    return state <|
        case res of
            Ok (Just deletionRequest) ->
                Cmd.batch
                    [ showFlashMessage <| FlashMessage.success "Deletion requested."
                    , updateDeletionRequest <| Just deletionRequest
                    ]

            Ok Nothing ->
                Cmd.batch
                    [ showFlashMessage <|
                        FlashMessage.error
                            "Failed to request deletion of non-leaf user group. Only user groups without child user groups can be deleted."
                    ]

            Err _ ->
                Cmd.batch
                    [ showFlashMessage <|
                        FlashMessage.error
                            "Failed to request deletion. Only full Scrive admins (not sales admins) can request the deletion of user groups!"
                    ]



{- Cancel scheduled deletion -}


viewCancelDeletionButton : Html Msg
viewCancelDeletionButton =
    Button.button
        [ Button.secondary
        , Button.attrs [ class "mr-sm-2", class "mt-sm-2", onClick CancelDeletionClickedMsg ]
        ]
        [ text "Cancel deletion request"
        ]



-- CancelDeletionClickedMsg. No modal?


cancelDeletionClicked : Params msg -> Return msg ModalState
cancelDeletionClicked params =
    return Idle <|
        Http.post
            { url = "/adminonly/companyadmin/canceldeletion/" ++ params.userGroupId
            , body = formBody params []
            , expect =
                Http.expectWhatever (params.embed << CancelDeletionCallbackMsg)
            }


cancelDeletionCallback : Params msg -> ModalState -> Result Http.Error () -> Return msg ModalState
cancelDeletionCallback { showFlashMessage, updateDeletionRequest } state res =
    return state <|
        case res of
            Ok () ->
                Cmd.batch
                    [ showFlashMessage <| FlashMessage.success "Deletion cancelled."
                    , updateDeletionRequest Nothing
                    ]

            Err _ ->
                Cmd.batch
                    [ showFlashMessage <|
                        FlashMessage.error
                            "Failed to cancel deletion. Only full Scrive admins (not sales admins) can cancel the deletion of a user group!"
                    ]



{- Sign off on deletion request -}


viewSignOffDeletionButton : Html Msg
viewSignOffDeletionButton =
    Button.button
        [ Button.danger
        , Button.attrs [ class "mr-sm-2", class "mt-sm-2", onClick SignOffDeletionClickedMsg ]
        ]
        [ text "Sign off on deletion request"
        ]



-- SignOffDeletionClickedMsg


signOffDeletionClicked : ModalState -> Return msg ModalState
signOffDeletionClicked _ =
    singleton SignOffDeletionModal


viewSignOffDeletionModal : Html Msg
viewSignOffDeletionModal =
    Modal.config AbortModalMsg
        |> Modal.h1 [] [ text "Sign off on deletion of user group" ]
        |> Modal.body []
            [ text <|
                "This will schedule the user group for permanent deletion on the requested date. "
                    ++ "Make sure you know what you are doing!"
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.secondary
                , Button.attrs [ onClick AbortModalMsg ]
                ]
                [ text "Cancel"
                ]
            , Button.button
                [ Button.danger
                , Button.attrs [ onClick ConfirmSignOffDeletionMsg ]
                ]
                [ text "Permanently delete user group"
                ]
            ]
        |> Modal.view Modal.shown


confirmSignOffDeletion : Params msg -> Return msg ModalState
confirmSignOffDeletion params =
    return Idle <|
        Http.post
            { url = "/adminonly/companyadmin/signoffdeletion/" ++ params.userGroupId
            , body = formBody params []
            , expect =
                Http.expectJson (params.embed << SignOffDeletionCallbackMsg) DeletionRequest.decoder
            }


signOffDeletionCallback : Params msg -> ModalState -> Result Http.Error DeletionRequest -> Return msg ModalState
signOffDeletionCallback { showFlashMessage, updateDeletionRequest } state res =
    return state <|
        case res of
            Ok deletionRequest ->
                Cmd.batch
                    [ showFlashMessage <| FlashMessage.success "Deletion signed off. The user group will be deleted."
                    , updateDeletionRequest <| Just deletionRequest
                    ]

            Err _ ->
                Cmd.batch
                    [ showFlashMessage <|
                        FlashMessage.error
                            "Failed to sign off on deletion. Only full Scrive admins (not sales admins) can sign off on user group deletion requests!"
                    ]
