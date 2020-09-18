port module IdentifyView.FITupasNets.Update exposing (..)

import Enum

import IdentifyView.FITupasNets.Model exposing (..)
import IdentifyView.FITupasNets.Tupas exposing (..)
import IdentifyView.FITupasNets.View exposing (..)

import Lib.Misc.SignatoryLink exposing (getPersonalNumber)
import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Lib.Misc.Cmd exposing (perform)

-- | Receives events via `window.addEventListener("message",_)`.
port messageEventPort : (String -> msg) -> Sub msg

subscriptions : Params msg -> Sub msg
subscriptions params = messageEventPort (params.embed << ErrorMsg)

update : Params msg -> State -> Msg -> (State, Cmd msg)
update params state msg = case msg of
  IdentifyButtonClickedMsg ->
    case state of
      Inquiry {personalNumber} ->
        if isPersonalNumberValid personalNumber
        then (Processing {personalNumber = personalNumber}, Cmd.none)
        else (state, Cmd.none)
      _ -> (Processing {personalNumber = getPersonalNumber params.signatory}, Cmd.none)

  SetPersonalNumberInquiryMsg pn -> (Inquiry {personalNumber = pn}, Cmd.none)

  ErrorMsg errorString ->
    let errorMessage = case Enum.findEnumValue enumNetsErrorMessage errorString of
          Ok err -> err
          _ -> None
    in (Idle, perform <| params.addFlashMessageMsg <| FlashError <| netsErrorMessageTranslated params errorMessage)
