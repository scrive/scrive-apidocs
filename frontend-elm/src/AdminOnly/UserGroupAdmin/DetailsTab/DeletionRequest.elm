module AdminOnly.UserGroupAdmin.DetailsTab.DeletionRequest exposing (..)

import Html exposing (Html, text)
import Http exposing (Error(..))
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid exposing (Column)
import Bootstrap.Grid.Col as Col
import Bootstrap.Form.Input as Input
import Bootstrap.Modal as Modal
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Task
import Time
import Json.Decode as JD

import Utils exposing (formBody, perform, monthToInt)
import AdminOnly.UserAdmin.DetailsTab.UserGroup exposing (UserGroup, DeletionStatus, DeletionRequest, deletionRequestDecoder)
import FlashMessage exposing (FlashMessage)

type alias Params msg =
  { embed : Msg -> msg
  , userGroupId : String
  , xtoken : String
  , updateDeletionStatus : DeletionStatus -> Cmd msg
  , showFlashMessage : FlashMessage -> Cmd msg
  }

-- Inherited from the parent component; can't be modified directly.
type alias ReadonlyState =
  { deletionStatus : DeletionStatus
  }

type State
  = Loading
  | Loaded { userId : String, modalState : ModalState }

type ModalState
  = Idle
  | RequestDeletionModal { date : String }
  | SignOffDeletionModal

type Msg
  -- Loading
  = GetProfileCallbackMsg (Result Http.Error {userId : String})
  -- Abort modal button
  | AbortModalMsg
  -- Request deletion
  | RequestDeletionClickedMsg
  | ShowRequestDeletionModalMsg {date : String}
  | UpdateDeletionDateMsg String
  | ConfirmRequestDeletionMsg {date : String}
  | RequestDeletionCallbackMsg (Result Http.Error (Maybe DeletionRequest))
  -- Cancel deletion
  | CancelDeletionClickedMsg
  | CancelDeletionCallbackMsg (Result Http.Error ())
  -- Sign off on deletion request
  | SignOffDeletionClickedMsg
  | ConfirmSignOffDeletionMsg
  | SignOffDeletionCallbackMsg (Result Http.Error DeletionRequest)

init : Params msg -> (State, Cmd msg)
init params =
  let decodeUserId = JD.field "id" JD.string |> JD.map (\userId -> {userId = userId})
      cmd = Http.get
        { url = "/api/v2/getprofile/"
        , expect =
            Http.expectJson (params.embed << GetProfileCallbackMsg) decodeUserId
        }
  in (Loading, cmd)

update : Params msg -> Msg -> State -> (State, Cmd msg)
update params msg state =
  case state of
    Loading -> case msg of
      GetProfileCallbackMsg res -> case res of
        Ok {userId} -> (Loaded {userId = userId, modalState = Idle}, Cmd.none)
        Err _ -> (state, Cmd.none)
      _ -> (state, Cmd.none)
    Loaded {userId, modalState} ->
      let (newModalState, cmd) = updateModal params msg modalState
      in (Loaded {userId = userId, modalState = newModalState}, cmd)


updateModal : Params msg -> Msg -> ModalState -> (ModalState, Cmd msg)
updateModal params msg state = case msg of
  AbortModalMsg -> abortModal state
  RequestDeletionClickedMsg -> requestDeletionClicked params state
  ShowRequestDeletionModalMsg date -> (RequestDeletionModal date, Cmd.none)
  UpdateDeletionDateMsg date -> case state of
    RequestDeletionModal _ -> (RequestDeletionModal {date = date}, Cmd.none)
    _ -> (state, Cmd.none)
  ConfirmRequestDeletionMsg {date} -> confirmRequestDeletion params {date = date}
  RequestDeletionCallbackMsg res -> requestDeletionCallback params state res
  CancelDeletionClickedMsg -> cancelDeletionClicked params
  CancelDeletionCallbackMsg res -> cancelDeletionCallback params state res
  SignOffDeletionClickedMsg -> signOffDeletionClicked state
  ConfirmSignOffDeletionMsg -> confirmSignOffDeletion params
  SignOffDeletionCallbackMsg res -> signOffDeletionCallback params state res
  GetProfileCallbackMsg _ -> (state, Cmd.none)


viewModal : State -> Maybe (Html Msg)
viewModal state = case state of
  Loading -> Nothing
  Loaded {modalState} -> case modalState of
    Idle -> Nothing
    RequestDeletionModal s -> Just <| viewRequestDeletionModal s
    SignOffDeletionModal -> Just <| viewSignOffDeletionModal

viewButtons : (Msg -> msg) -> ReadonlyState -> State -> Column msg
viewButtons embed {deletionStatus} state = Grid.col [ Col.sm12 ] <| case state of
  Loading -> []
  Loaded {userId} -> case deletionStatus of
    Nothing ->  [ Html.map embed viewRequestDeletionButton ]

    Just deletionRequest ->
      let isSignedOff = Maybe.isJust deletionRequest.signedOffBy
      in (if not isSignedOff && deletionRequest.requestedBy /= userId
          then [ Html.map embed viewSignOffDeletionButton ]
          else [])
         ++
         [ Html.map embed viewCancelDeletionButton
         , text <|
             (if isSignedOff
              then "Scheduled deletion date: "
              else "Requested deletion date: ")
             ++ deletionRequest.requestedDeletionDate
             ++ if not isSignedOff && deletionRequest.requestedBy == userId
                then ". Needs to be signed off by another Scrive admin."
                else ""
          ]

-- AbortModalMsg
abortModal : ModalState -> (ModalState, Cmd msg)
abortModal _ = (Idle, Cmd.none)

{- Request deletion -}
viewRequestDeletionButton : Html Msg
viewRequestDeletionButton =
  Button.button [ Button.danger
                , Button.attrs [ class "mt-sm-2", onClick RequestDeletionClickedMsg ] ] [
    text "Request deletion" ]

-- RequestDeletionClickedMsg
requestDeletionClicked : Params msg -> ModalState -> (ModalState, Cmd msg)
requestDeletionClicked {embed} state =
  let posixToDate posix =
        { date =
            String.padLeft 4 '0' (String.fromInt <| Time.toYear Time.utc posix)
            ++ "-"
            ++ String.padLeft 2 '0' (String.fromInt <| monthToInt <| Time.toMonth Time.utc posix)
            ++ "-"
            ++ String.padLeft 2 '0' (String.fromInt <| Time.toDay Time.utc posix)
        }
  in (state, Task.perform (embed << ShowRequestDeletionModalMsg << posixToDate) Time.now )

viewRequestDeletionModal : {date : String} -> Html Msg
viewRequestDeletionModal {date} =
  Modal.config AbortModalMsg
  |> Modal.h1 [] [ text "Request deletion of user group" ]
  |> Modal.body [] [
    text <| "This will request the deletion of the current user group. "
          ++ "Note that the request will need to be signed off by "
          ++ "a second scrive admin for the deletion to be carried out. "
          ++ "Desired deletion date: ",
    Input.date [ Input.value date, Input.onInput UpdateDeletionDateMsg ]]
  |> Modal.footer [] [
      Button.button [ Button.secondary
                    , Button.attrs [ onClick AbortModalMsg ] ] [
        text "Cancel" ],

      Button.button [ Button.danger
                    , Button.attrs [ onClick <| ConfirmRequestDeletionMsg {date = date} ] ] [
        text "Request deletion" ] ]

  |> Modal.view Modal.shown

-- ConfirmRequestDeletionMsg
confirmRequestDeletion : Params msg -> {date : String} -> (ModalState, Cmd msg)
confirmRequestDeletion params {date} =
  let cmd = Http.post
        { url = "/adminonly/companyadmin/requestdeletion/" ++ params.userGroupId
        , body = formBody params [("deletion_date", date) ]
        , expect =
            Http.expectJson (params.embed << RequestDeletionCallbackMsg) (JD.nullable deletionRequestDecoder)
        }
  in (Idle, cmd)

requestDeletionCallback : Params msg -> ModalState -> Result Http.Error (Maybe DeletionRequest) -> (ModalState, Cmd msg)
requestDeletionCallback {showFlashMessage, updateDeletionStatus} state res =
  case res of
    Ok (Just deletionRequest) ->
      let cmd1 = showFlashMessage <| FlashMessage.success "Deletion requested."
          cmd2 = updateDeletionStatus <| Just deletionRequest
      in (state, Cmd.batch [ cmd1, cmd2 ])

    Ok Nothing ->
      let cmd = showFlashMessage <| FlashMessage.error
             "Failed to request deletion of non-leaf user group. Only user groups without child user groups can be deleted."
      in (state, cmd)

    Err _ ->
      let cmd = showFlashMessage <| FlashMessage.error
                  "Failed to request deletion. Only full Scrive admins (not sales admins) can request the deletion of user groups!"
      in (state, cmd)


{- Cancel scheduled deletion -}
viewCancelDeletionButton : Html Msg
viewCancelDeletionButton =
  Button.button [ Button.secondary
                , Button.attrs [ class "mr-sm-2", class "mt-sm-2", onClick CancelDeletionClickedMsg ] ] [
    text "Cancel deletion request" ]

-- CancelDeletionClickedMsg. No modal?
cancelDeletionClicked : Params msg -> (ModalState, Cmd msg)
cancelDeletionClicked params =
  let cmd = Http.post
        { url = "/adminonly/companyadmin/canceldeletion/" ++ params.userGroupId
        , body = formBody params []
        , expect =
            Http.expectWhatever (params.embed << CancelDeletionCallbackMsg)
        }
  in (Idle, cmd)

cancelDeletionCallback : Params msg -> ModalState -> Result Http.Error () -> (ModalState, Cmd msg)
cancelDeletionCallback {showFlashMessage, updateDeletionStatus} state res =
  case res of
    Ok () ->
      let cmd1 = showFlashMessage <| FlashMessage.success "Deletion cancelled."
          cmd2 = updateDeletionStatus Nothing
      in (state, Cmd.batch [ cmd1, cmd2 ])

    Err _ ->
      let cmd = showFlashMessage <| FlashMessage.error
                  "Failed to cancel deletion. Only full Scrive admins (not sales admins) can cancel the deletion of a user group!"
      in (state, cmd)


{- Sign off on deletion request -}
viewSignOffDeletionButton : Html Msg
viewSignOffDeletionButton =
  Button.button [ Button.danger
                , Button.attrs [ class "mr-sm-2", class "mt-sm-2", onClick SignOffDeletionClickedMsg ] ] [
    text "Sign off on deletion request" ]


-- SignOffDeletionClickedMsg
signOffDeletionClicked : ModalState -> (ModalState, Cmd msg)
signOffDeletionClicked _ = (SignOffDeletionModal, Cmd.none)

viewSignOffDeletionModal : Html Msg
viewSignOffDeletionModal =
  Modal.config AbortModalMsg
  |> Modal.h1 [] [ text "Sign off on deletion of user group" ]
  |> Modal.body [] [
    text <| "This will schedule the user group for permanent deletion on the requested date. "
          ++ "Make sure you know what you are doing!" ]
  |> Modal.footer [] [
      Button.button [ Button.secondary
                    , Button.attrs [ onClick AbortModalMsg ] ] [
        text "Cancel" ],

      Button.button [ Button.danger
                    , Button.attrs [ onClick ConfirmSignOffDeletionMsg ] ] [
        text "Permanently delete user group" ] ]
  |> Modal.view Modal.shown

confirmSignOffDeletion : Params msg -> (ModalState, Cmd msg)
confirmSignOffDeletion params =
  let cmd = Http.post
        { url = "/adminonly/companyadmin/signoffdeletion/" ++ params.userGroupId
        , body = formBody params []
        , expect =
            Http.expectJson (params.embed << SignOffDeletionCallbackMsg) deletionRequestDecoder
        }
  in (Idle, cmd)

signOffDeletionCallback : Params msg -> ModalState -> Result Http.Error DeletionRequest -> (ModalState, Cmd msg)
signOffDeletionCallback {showFlashMessage, updateDeletionStatus} state res =
  case res of
    Ok deletionRequest ->
      let cmd1 = showFlashMessage <| FlashMessage.success "Deletion signed off. The user group will be deleted."
          cmd2 = updateDeletionStatus <| Just deletionRequest
      in (state, Cmd.batch [ cmd1, cmd2 ])

    Err _ ->
      let cmd = showFlashMessage <| FlashMessage.error
                  "Failed to sign off on deletion. Only full Scrive admins (not sales admins) can sign off on user group deletion requests!"
      in (state, cmd)
