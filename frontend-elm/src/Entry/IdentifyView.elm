module Entry.IdentifyView exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Browser
import Http
import Html

import Utils exposing (perform)
import Lib.Types.ID exposing (showId)
import Lib.Json.ID exposing (idDecoder)
import Lib.Json.Document exposing (documentDecoder)
import Lib.Json.FlashMessage exposing (flashMessageDecoder)
import Lib.Json.Localization exposing (localisationDecoder)
import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.FlashMessage exposing (FlashMessage(..))

import IdentifyView.View exposing (..)
import IdentifyView.Model exposing (..)
import IdentifyView.Update exposing (..)

decodeFlags : JD.Decoder Flags
decodeFlags =
  JD.succeed Flags
  |> JDP.required "flashMessageFromCookie" (JD.maybe flashMessageDecoder)
  |> JDP.required "xtoken" JD.string
  |> JDP.required "localization" localisationDecoder
  |> JDP.required "signatoryLinkId" idDecoder
  |> JDP.required "documentId" idDecoder
  |> JDP.required "brandingDomainId" JD.string
  |> JDP.required "brandingHash" JD.string
  |> JDP.required "cdnBaseUrl" JD.string
  |> JDP.required "location" JD.string
  |> JDP.required "currentYear" JD.int
  |> JDP.required "browserNeedsSEBankIDRedirect" JD.bool
  |> JDP.required "origin" JD.string
  |> JDP.required "netsConf" (
      JD.succeed (\td iu mi ->
        { netsTrustedDomain = td
        , netsIdentifyUrl = iu
        , netsMerchantIdentifier = mi
        })
      |> JDP.required "netsTrustedDomain" JD.string
      |> JDP.required "netsIdentifyUrl" JD.string
      |> JDP.required "netsMerchantIdentifier" JD.string
    )
  |> JDP.required "eidServiceConf" (
      JD.succeed (\fi se ->
        { eidUseForFIView = fi
        , eidUseForSEView = se
        })
      |> JDP.required "eidUseForFIView" JD.bool
      |> JDP.required "eidUseForSEView" JD.bool
    )

init : JD.Value -> (Model, Cmd Msg)
init value = case JD.decodeValue decodeFlags value of
  Ok flags ->
    let model = { flashMessages = FlashMessage.initialState
                , state = Loading flags
                }
        cmd = Http.get
                { url = "/api/v2/documents/" ++ showId flags.documentId
                      ++ "/get?signatory_id=" ++ showId flags.signatoryLinkId
                , expect = Http.expectJson GetDocumentCallbackMsg documentDecoder
                }
    in (model, cmd)
  Err err ->
    let model = { flashMessages = FlashMessage.initialState
                , state = Error {errorView = Html.text <| JD.errorToString err}
                }
        cmd = perform <| AddFlashMessageMsg (FlashError "Failed to decode flags")
    in (model, cmd)

main : Program JD.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
