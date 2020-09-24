module IdentifyView.SEBankIDCGI.Model exposing (..)

import Http
import Json.Encode

import Lib.Types.Localization exposing (Localization)
import Lib.Types.FlashMessage exposing (FlashMessage)
import Lib.Types.ID exposing (ID)
import Lib.Types.Document exposing (Document)
import Lib.Types.SignatoryLink exposing (SignatoryLink)

import IdentifyView.SEBankIDCGI.BankID exposing (..)
import IdentifyView.SEBankIDCGI.TrackedRequests exposing (Tracker)

type Msg = IdentifyButtonClickedMsg
         | CgiGrpAuthCallbackMsg (Result Http.Error (Result BankIdError { autoStartToken : String, sessionId : String }))
         | UseBankIdCheckboxMsg
         | CancelButtonClickedMsg
         | StartBankIdAppButtonClickedMsg
         | ProblemAcknowledgedButtonClickedMsg
         | CheckCgiTransactionTickMsg
         | CheckCgiTransactionCallbackMsg (Result Http.Error (Result BankIdError BankIdState))
         | SomeTimeHasPassedTickMsg

type alias Params msg =
  { embed : Msg -> msg
  , addFlashMessageMsg : FlashMessage -> msg
  , errorTraceMsg : List (String, Json.Encode.Value) -> msg
  , documentId : ID Document
  , signatory : SignatoryLink
  , xtoken : String
  , localization : Localization
  , currentYear : Int
  , browserNeedsSEBankIDRedirect : Bool
  , origin : String
  , location : String
  , cdnBaseUrl : String
  }

type alias State = {
  useBankIdLink : Bool
  -- ^ Whether to open the app locally by pointing the browser at a `bankid://`
  -- url, or have the user open the app by themselves (possibly on a different
  -- device).
  , innerState : InnerState
  }

type InnerState =
    Idle
  -- | Wait for answer from "/s/eid/cgi/grp/auth/<..>".
  | WaitForCgiGrpAuthEndpoint {
      requestInFlight : Tracker
      -- ^ We don't really need to save the requestID (since it's constant), but
      -- having it explicit makes it easier to reason about. See
      -- `cancelButtonClicked`.
    }
  -- | Wait for local client to start after pointing browser at bankid://<..>.
  | StartClientViaBankIdUrl {
      requestInFlight : Maybe Tracker,
      -- ^ Every second we check "/s/eid/cgi/grp/checkcgiauthstatus/".
      bankIdState : BankIdState,
      autoStartToken : String,
      sessionId : String,
      -- ^ See https://scriveab.atlassian.net/browse/AUTH-201.
      someTimeHasPassed : Bool
      -- ^ After some time (5s) has passed, allow the user to start the app
      -- manually by clicking on a bankid:// link (since starting the app
      -- automatically by setting window.location doesn't seem to have worked).
    }
  -- | Wait for user to start their client of their own accord, presumably on
  -- another device.
  | WaitForUserToStartClient {
      requestInFlight : Maybe Tracker,
      -- ^ Every second we check "/s/eid/cgi/grp/checkcgiauthstatus/".
      bankIdState : BankIdState
    }
  -- | Reload page when signed. The corresponding BankIdState is `BankIdUserSign`
  | WaitForUserToSign {
      requestInFlight : Maybe Tracker
      -- ^ Every second we check "/s/eid/cgi/grp/checkcgiauthstatus/".
    }
  | Problem {
      bankIdError : BankIdError
    }

