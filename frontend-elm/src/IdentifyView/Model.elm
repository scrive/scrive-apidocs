module IdentifyView.Model exposing (..)

import Html exposing (Html)
import Http
import Json.Encode

import Lib.Types.Document exposing (Document(..))
import Lib.Types.SignatoryLink exposing (SignatoryLink, AuthenticationToViewMethod)
import Lib.Types.ID exposing (ID)
import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.FlashMessage exposing (FlashMessage)
import Lib.Types.Localization exposing (Localization)

import IdentifyView.SMSPin.SMSPin as SMSPin
import IdentifyView.GenericEidService.GenericEidService as GenericEidService

type Msg
  = AddFlashMessageMsg FlashMessage
  | ErrorTraceMsg (List (String, Json.Encode.Value))
  -- embedded messages
  | SMSPinMsg SMSPin.Msg
  | FlashMessageMsg FlashMessage.Msg
  | GenericEidServiceMsg GenericEidService.Msg

type alias Flags =
  { flashMessageFromCookie : Maybe FlashMessage
  , xtoken : String
  , localization : Localization
  , cdnBaseUrl : String
  , location : String
  , currentYear : Int
  , origin : String
  , logoUrl : String
  , welcomeText : String
  , entityTypeLabel : String
  , entityTitle : String
  , authenticationMethod : AuthenticationToViewMethod
  , authorName : String
  , participantEmail : String
  , participantMaskedMobile : String
  , genericEidServiceStartUrl : String
  , smsPinSendUrl : String
  , smsPinVerifyUrl : String
  }

type alias Model =
  { flashMessages : FlashMessage.State
  , state : State
  }

type State
  = Loading Flags
  | Error { errorView : Html Msg }  -- better: semantic error view?
  | IdentifyView { flags : Flags, innerModel : ProviderModel }

type ProviderModel
  = IdentifySMSPin SMSPin.State
  | IdentifyGenericEidService GenericEidService.Provider GenericEidService.State

toSMSPinParams : Flags -> SMSPin.Params Msg
toSMSPinParams f =
  { embed = SMSPinMsg
  , addFlashMessageMsg = AddFlashMessageMsg
  , errorTraceMsg = ErrorTraceMsg
  , xtoken = f.xtoken
  , localization = f.localization
  , participantMaskedMobile = f.participantMaskedMobile
  , sendUrl = f.smsPinSendUrl
  , verifyUrl = f.smsPinVerifyUrl
  }

toGenericEidServiceParams : Flags -> GenericEidService.Provider -> GenericEidService.Params Msg
toGenericEidServiceParams f provider =
  { embed = GenericEidServiceMsg
  , addFlashMessageMsg = AddFlashMessageMsg
  , errorTraceMsg = ErrorTraceMsg
  , provider = provider
  , xtoken = f.xtoken
  , location = f.location
  , localization = f.localization
  , cdnBaseUrl = f.cdnBaseUrl
  , participantEmail = f.participantEmail
  , startUrl = f.genericEidServiceStartUrl
  }

