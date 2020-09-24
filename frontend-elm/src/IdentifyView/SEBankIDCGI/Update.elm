module IdentifyView.SEBankIDCGI.Update exposing (..)

import Json.Decode as JD
import Json.Encode as JE
import Http exposing (expectJson)
import Browser.Navigation
import Url
import Time

import Lib.Types.ID exposing (showId)
import Lib.Types.SignatoryLink exposing (SignatoryLink(..))
import Lib.Misc.Cmd exposing (perform)
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Lib.Misc.SignatoryLink exposing (getPersonalNumber)
import Lib.Json.Enum exposing (enumDecoder)
import Lib.Misc.Http exposing (encodeError, formBody)

import IdentifyView.SEBankIDCGI.Model exposing (..)
import IdentifyView.SEBankIDCGI.BankID exposing (..)
import IdentifyView.SEBankIDCGI.TrackedRequests exposing (..)

update : Msg -> Params msg -> State -> (State, Cmd msg)
update msg = case msg of
  IdentifyButtonClickedMsg -> identifyButtonClicked
  CgiGrpAuthCallbackMsg res -> cgiGrpAuthCallback res
  UseBankIdCheckboxMsg -> \_ state -> ({ state | useBankIdLink = not state.useBankIdLink }, Cmd.none)
  CancelButtonClickedMsg -> \_ -> cancelButtonClicked
  StartBankIdAppButtonClickedMsg -> startBankIdAppButtonClicked
  ProblemAcknowledgedButtonClickedMsg -> \_ state -> ({ state | innerState = Idle }, Cmd.none)
  CheckCgiTransactionTickMsg -> checkCgiTransactionTick
  CheckCgiTransactionCallbackMsg res -> checkCgiTransactionCallback res
  SomeTimeHasPassedTickMsg -> someTimeHasPassedTick

subscriptions : Params msg -> State -> Sub msg
subscriptions params state =
  let pollTransactionStatus = Time.every 3000 (params.embed << always CheckCgiTransactionTickMsg)

      waitForSomeTimeToPass = Time.every 5000 (params.embed << always SomeTimeHasPassedTickMsg)

  in case state.innerState of
  StartClientViaBankIdUrl {someTimeHasPassed} ->
    if someTimeHasPassed
      then pollTransactionStatus
      else Sub.batch [ pollTransactionStatus, waitForSomeTimeToPass ]

  WaitForUserToStartClient _ -> pollTransactionStatus

  WaitForUserToSign _ -> pollTransactionStatus

  _ -> Sub.none

-- | POST /s/eid/cgi/grp/auth and transition to `WaitForCgiGrpAuthEndpoint`.
identifyButtonClicked : Params msg -> State -> (State, Cmd msg)
identifyButtonClicked params state =
  let decodeResponse =
        JD.oneOf
          [ JD.map2 (\a s -> Ok { autoStartToken = a, sessionId = s} )
              (JD.field "auto_start_token" JD.string)
              (JD.field "session_id" JD.string)
          , JD.map (\err -> Err err)
              (JD.field "grp_fault" <| enumDecoder enumBankIdError)
          ]

      uniqueTracker = Tracker "request from identifyButtonClicked"

      rawPersonalNumber = getPersonalNumber params.signatory

      normalisedPersonalNumber = Maybe.withDefault rawPersonalNumber
        <| normalisePersonalNumber {currentYear = params.currentYear, rawPersonalNumber = rawPersonalNumber}

      postReq = trackedPostRequest {
          url = String.join "/"
            [ "/s/eid/cgi/grp/auth"
            , showId params.documentId
            , showId <| case params.signatory of SignatoryLink sig -> sig.id],
          body = formBody params [("personal_number", normalisedPersonalNumber)],
          expect = expectJson (params.embed << CgiGrpAuthCallbackMsg) decodeResponse,
          tracker = uniqueTracker
        }

      newInnerState = WaitForCgiGrpAuthEndpoint {
                          requestInFlight = uniqueTracker
                        }
  in ({state | innerState = newInnerState}, postReq)

-- | If `useBankIdLink`, then transition to `StartClientViaBankIdUrl` and
-- redirect to `bankIdUrl`, otherwise transition to `WaitForUserToStartClient`.
cgiGrpAuthCallback :
  Result Http.Error (Result BankIdError { autoStartToken : String, sessionId : String }) -> Params msg -> State -> (State, Cmd msg)
cgiGrpAuthCallback res params state = case res of
  Ok (Ok {autoStartToken, sessionId}) ->
    if state.useBankIdLink
      then let newInnerState = StartClientViaBankIdUrl {
                   requestInFlight = Nothing,
                   bankIdState = BankIdOutstandingTransaction,
                   autoStartToken = autoStartToken,
                   sessionId = sessionId,
                   -- ^ See https://scriveab.atlassian.net/browse/AUTH-201.
                   someTimeHasPassed = False
                 }
           in ({state | innerState = newInnerState}
              , Browser.Navigation.load <| bankIdUrl params {autoStartToken = autoStartToken, sessionId = sessionId})
      else let newInnerState = WaitForUserToStartClient {
                   requestInFlight = Nothing,
                   bankIdState = BankIdOutstandingTransaction
                 }
           in ({state | innerState = newInnerState}, Cmd.none)

  Ok (Err bankIdError) ->
    ({ state | innerState = Problem {bankIdError = bankIdError} }, Cmd.none)

  Err err ->
    let flashMessage = if err == Http.Timeout || err == Http.NetworkError
          then params.localization.docsignview.networkIssueErrorMessage
          else "Failed to create BankID transaction."
        errorFields =
          [ ("where", JE.string "IdentifyView.SEBankCGI.update cgiGrpAuthCallback")
          , ("what", JE.string "failed to create BankID transaction")
          , ("http_error", encodeError err)
          ]
    in ( {state | innerState = Idle}
       , Cmd.batch [ perform <| params.addFlashMessageMsg <| FlashError flashMessage
                   , perform <| params.errorTraceMsg errorFields ] )

-- | Start the app by pointing the browser at `bankIdUrl`.
startBankIdAppButtonClicked : Params msg -> State -> (State, Cmd msg)
startBankIdAppButtonClicked params state = case state.innerState of
  StartClientViaBankIdUrl {autoStartToken, sessionId} ->
    (state, Browser.Navigation.load <| bankIdUrl params {autoStartToken = autoStartToken, sessionId = sessionId})
  _ -> (state, Cmd.none)

-- | Helper to construct the `bankid://` url. If we are on iOS, then we need to
-- add a redirect link; see AUTH-201.
bankIdUrl : Params msg -> {autoStartToken : String, sessionId : String} -> String
bankIdUrl params {autoStartToken, sessionId} =
  let redirectTarget = String.join "/"
        [ params.origin
        , "/s/eid/cgi/grp/checkcgiauthstatuswithredirect"
        , showId params.documentId
        , showId <| case params.signatory of SignatoryLink sig -> sig.id ]
        ++ "?session_id=" ++ sessionId ++ "&url=" ++ Url.percentEncode params.location
        -- What's the deal with documentUrl? is this page ever embedded?
  in String.concat
      [ "bankid:///"
      , "?autostarttoken="
      , autoStartToken
      , "&redirect="
      , if params.browserNeedsSEBankIDRedirect
          then Url.percentEncode redirectTarget
          else "null"
      ]

-- | Cancel any ongoing requests and return to `Idle` state.
cancelButtonClicked : State -> (State, Cmd msg)
cancelButtonClicked state =
  let maybeCancel = Maybe.withDefault Cmd.none << Maybe.map cancelRequest
      cmd = case state.innerState of
        WaitForCgiGrpAuthEndpoint {requestInFlight} -> cancelRequest requestInFlight
        StartClientViaBankIdUrl {requestInFlight} -> maybeCancel requestInFlight
        WaitForUserToStartClient {requestInFlight} -> maybeCancel requestInFlight
        WaitForUserToSign {requestInFlight} -> maybeCancel requestInFlight
        _ -> Cmd.none
  in ({state | innerState = Idle}, cmd)

-- | Every second we fire off a GET request to
-- "/s/eid/cgi/grp/checkcgiauthstatus/".
checkCgiTransactionTick : Params msg -> State -> (State, Cmd msg)
checkCgiTransactionTick params state =
  let decodeResponse =
          ( JD.maybe <| JD.field "grp_fault" <| enumDecoder enumBankIdError)
          |> JD.andThen (\mf -> case mf of
               Just fault -> JD.succeed <| Err fault
               Nothing -> (JD.field "progress_status" <| enumDecoder enumBankIdState)
                         |> JD.andThen (JD.succeed << Ok)
             )

      uniqueTracker = Tracker "request from checkCgiTransactionTick"

      getReq = trackedGetRequest {
          url = String.join "/"
            [ "/s/eid/cgi/grp/checkcgiauthstatus"
            , showId params.documentId
            , showId <| case params.signatory of SignatoryLink sig -> sig.id ],
          expect = expectJson (params.embed << CheckCgiTransactionCallbackMsg) decodeResponse,
          tracker = uniqueTracker
        }
  in case state.innerState of
    StartClientViaBankIdUrl { requestInFlight, bankIdState, autoStartToken
                            , sessionId, someTimeHasPassed }
      -> case requestInFlight of
        Nothing -> let newInnerState = StartClientViaBankIdUrl {
                          requestInFlight = Just uniqueTracker,
                          bankIdState = bankIdState,
                          autoStartToken = autoStartToken,
                          sessionId = sessionId,
                          someTimeHasPassed = someTimeHasPassed
                        }
                  in ({ state | innerState = newInnerState }, getReq)
        Just _ -> (state, Cmd.none)

    WaitForUserToStartClient {requestInFlight, bankIdState} -> case requestInFlight of
      Nothing -> let newInnerState = WaitForUserToStartClient {
                        requestInFlight = Just uniqueTracker,
                        bankIdState = bankIdState
                        }
                in ({ state | innerState = newInnerState }, getReq)
      Just _ -> (state, Cmd.none)

    WaitForUserToSign {requestInFlight} -> case requestInFlight of
      Nothing -> let newInnerState = WaitForUserToSign {
                        requestInFlight = Just uniqueTracker
                        }
                in ({ state | innerState = newInnerState }, getReq)
      Just _ -> (state, Cmd.none)

    _ -> (state, Cmd.none)

-- | Update the state according to the response from
-- GET "/s/eid/cgi/grp/checkcgiauthstatus/".
checkCgiTransactionCallback :
  Result Http.Error (Result BankIdError BankIdState) -> Params msg -> State -> (State, Cmd msg)
checkCgiTransactionCallback res params state = case res of
  Ok (Ok newBankIdState) -> case newBankIdState of
    BankIdComplete -> (state, Browser.Navigation.reload)  -- reload to view document
    BankIdUserSign ->
      let newInnerState = WaitForUserToSign {requestInFlight = Nothing}
      in ({ state | innerState = newInnerState }, Cmd.none)
    _ -> case state.innerState of
      StartClientViaBankIdUrl { autoStartToken, sessionId, someTimeHasPassed } ->
        let newInnerState = StartClientViaBankIdUrl {
                autoStartToken = autoStartToken
              , sessionId = sessionId
              , someTimeHasPassed = someTimeHasPassed
              , requestInFlight = Nothing
              , bankIdState = newBankIdState
              }
        in ({ state | innerState = newInnerState }, Cmd.none)

      WaitForUserToStartClient _ ->
        let newInnerState = WaitForUserToStartClient {
                bankIdState = newBankIdState
              , requestInFlight = Nothing
              }
        in ({ state | innerState = newInnerState }, Cmd.none)

      WaitForUserToSign _ ->
        let newInnerState = WaitForUserToSign {
                requestInFlight = Nothing
              }
        in ({ state | innerState = newInnerState }, Cmd.none)

      _ ->
        -- Drop quietly, in case we got a straggling message.
        (state, Cmd.none)

  Ok (Err bankIdFault) ->
    let newInnerState = Problem { bankIdError = bankIdFault }
    in ({ state | innerState = newInnerState }, Cmd.none)

  Err err ->
    let flashMessage = if err == Http.Timeout || err == Http.NetworkError
          then params.localization.docsignview.networkIssueErrorMessage
          else "Failed to check transaction status."

        errorFields =
          [ ("where", JE.string "IdentifyView.SEBankCGI.update checkCgiTransactionCallback")
          , ("what", JE.string "failed to check transaction status")
          , ("http_error", encodeError err)
          ]

        newInnerState = case state.innerState of
          WaitForUserToStartClient args -> WaitForUserToStartClient { args | requestInFlight = Nothing }
          WaitForUserToSign args -> WaitForUserToSign { args | requestInFlight = Nothing }
          StartClientViaBankIdUrl args -> StartClientViaBankIdUrl { args | requestInFlight = Nothing }
          _ -> state.innerState

    in ( { state | innerState = newInnerState }
       , Cmd.batch [ perform <| params.addFlashMessageMsg <| FlashError flashMessage
                   , perform <| params.errorTraceMsg errorFields ] )

someTimeHasPassedTick : Params msg -> State -> (State, Cmd msg)
someTimeHasPassedTick params state = case state.innerState of
  StartClientViaBankIdUrl st ->
    let newInnerState = StartClientViaBankIdUrl {st | someTimeHasPassed = True}
    in ({state | innerState = newInnerState}, Cmd.none)
  _ ->
    let errorFields =
          [ ("where", JE.string "IdentifyView.SEBankCGI.update someTimeHasPassedTick")
          , ("what", JE.string "received 'some time has passed' tick in incompatible state")
          ]
    in ( state, perform <| params.errorTraceMsg errorFields )
