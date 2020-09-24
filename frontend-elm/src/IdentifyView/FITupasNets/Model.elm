module IdentifyView.FITupasNets.Model exposing (..)

import Json.Encode

import Lib.Types.Document exposing (Document)
import Lib.Types.SignatoryLink exposing (SignatoryLink)
import Lib.Types.FlashMessage exposing (FlashMessage)
import Lib.Types.Localization exposing (Localization)
import Lib.Misc.SignatoryLink exposing (getPersonalNumber)

type alias NetsConf = {
    netsTrustedDomain : String
  , netsIdentifyUrl : String
  , netsMerchantIdentifier : String
  }

type alias Params msg =
  { embed : Msg -> msg
  , addFlashMessageMsg : FlashMessage -> msg
  , document : Document
  , signatory : SignatoryLink
  , xtoken : String
  , location : String
  , origin : String
  -- ^ Note that we can't just parse location and synthesise origin; we
  -- need the exact substring.
  --       case (decodeNetsTarget =<< mnt, mart) of
  --         (Just nt, _) | domainUrl /= netsTransactionDomain nt -> do
  , localization : Localization
  , netsConf : NetsConf
  , cdnBaseUrl : String
  }

initialState : Params msg -> State
initialState {signatory} =
  case getPersonalNumber signatory of
    "" -> Inquiry {personalNumber = ""}
    personalNumber -> Processing {personalNumber = personalNumber}  -- skip `Idle`

type State
  = Idle  -- Only used after an error.
  | Inquiry { personalNumber : String }
  | Processing { personalNumber : String }

type Msg = IdentifyButtonClickedMsg
         | SetPersonalNumberInquiryMsg String
         | ErrorMsg String
