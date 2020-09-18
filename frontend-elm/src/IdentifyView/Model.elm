module IdentifyView.Model exposing (..)

import Html exposing (Html)
import Http
import Json.Encode

import Lib.Types.Document exposing (Document(..))
import Lib.Types.SignatoryLink exposing (SignatoryLink)
import Lib.Types.ID exposing (ID)
import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.FlashMessage exposing (FlashMessage)
import Lib.Types.Localization exposing (Localization)

import IdentifyView.SMSPin.SMSPin as SMSPin
import IdentifyView.GenericEidService.GenericEidService as GenericEidService
import IdentifyView.SEBankIDCGI.Model as SEBankIDCGI
import IdentifyView.FITupasNets.Model as FITupasNets

type Msg
  = GetDocumentCallbackMsg (Result Http.Error Document)
  | AddFlashMessageMsg FlashMessage
  | ErrorTraceMsg (List (String, Json.Encode.Value))
  -- embedded messages
  | SMSPinMsg SMSPin.Msg
  | FlashMessageMsg FlashMessage.Msg
  | GenericEidServiceMsg GenericEidService.Msg
  | SEBankIDCGIMsg SEBankIDCGI.Msg
  | FITupasNetsMsg FITupasNets.Msg

type alias Flags =
  { flashMessageFromCookie : Maybe FlashMessage
  , xtoken : String
  , localization : Localization
  , signatoryLinkId : ID SignatoryLink
  , documentId : ID Document
  , brandingDomainId : String
  , brandingHash : String
  , cdnBaseUrl : String
  , location : String
  , currentYear : Int
  , browserNeedsSEBankIDRedirect : Bool
  , origin : String
  , netsConf :
    { netsTrustedDomain : String
    , netsIdentifyUrl : String
    , netsMerchantIdentifier : String
    }
  , eidServiceConf :
    { eidUseForFIView : Bool
    , eidUseForSEView : Bool
    }
  }

type alias Model =
  { flashMessages : FlashMessage.State
  , state : State
  }

type State
  = Loading Flags
  | Error { errorView : Html Msg }  -- better: semantic error view?
  | IdentifyView { params : Params, innerModel : ProviderModel }

type ProviderModel
  = IdentifySMSPin SMSPin.State
  | IdentifyGenericEidService GenericEidService.Provider GenericEidService.State
  | IdentifySEBankIDCGI SEBankIDCGI.State
  | IdentifyFITupasNets FITupasNets.State

-- | The static part of the IdentifyView model; provider `Params` are projected
-- out.
type alias Params =
  { xtoken : String
  , document : Document
  , signatory : SignatoryLink
  , localization : Localization
  , brandingDomainId : String  -- should be a proper ID?
  , brandingHash : String
  , cdnBaseUrl : String
  , location : String  -- window.location.href
  , currentYear : Int  -- (new Date).getFullYear()
  , browserNeedsSEBankIDRedirect : Bool  -- True on iOS
  , origin : String  -- window.location.origin
  , netsConf :
    { netsTrustedDomain : String
    , netsIdentifyUrl : String
    , netsMerchantIdentifier : String
    }
  }

toSMSPinParams : Params -> SMSPin.Params Msg
toSMSPinParams p =
  { embed = SMSPinMsg
  , addFlashMessageMsg = AddFlashMessageMsg
  , errorTraceMsg = ErrorTraceMsg
  , documentId = case p.document of Document doc -> doc.id
  , signatory = p.signatory
  , xtoken = p.xtoken
  , localization = p.localization
  }

toGenericEidServiceParams : Params -> GenericEidService.Provider -> GenericEidService.Params Msg
toGenericEidServiceParams p provider =
  { embed = GenericEidServiceMsg
  , addFlashMessageMsg = AddFlashMessageMsg
  , errorTraceMsg = ErrorTraceMsg
  , provider = provider
  , documentId = case p.document of Document doc -> doc.id
  , signatory = p.signatory
  , xtoken = p.xtoken
  , location = p.location
  , localization = p.localization
  , cdnBaseUrl = p.cdnBaseUrl
  }

toSEBankIDCGIParams : Params -> SEBankIDCGI.Params Msg
toSEBankIDCGIParams p =
  { embed = SEBankIDCGIMsg
  , addFlashMessageMsg = AddFlashMessageMsg
  , errorTraceMsg = ErrorTraceMsg
  , documentId = case p.document of Document doc -> doc.id
  , signatory = p.signatory
  , xtoken = p.xtoken
  , localization = p.localization
  , currentYear = p.currentYear
  , browserNeedsSEBankIDRedirect = p.browserNeedsSEBankIDRedirect
  , location = p.location
  , origin = p.origin
  , cdnBaseUrl = p.cdnBaseUrl
  }

toFITupasNetsParams : Params -> FITupasNets.Params Msg
toFITupasNetsParams p =
  { embed = FITupasNetsMsg
  , addFlashMessageMsg = AddFlashMessageMsg
  , document = p.document
  , signatory = p.signatory
  , xtoken = p.xtoken
  , location = p.location
  , origin = p.origin
  , localization = p.localization
  , netsConf = p.netsConf
  , cdnBaseUrl = p.cdnBaseUrl
  }
