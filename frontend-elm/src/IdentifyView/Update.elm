port module IdentifyView.Update exposing (..)

import Html
import Json.Encode as JE

import Lib.Types.FlashMessage exposing (FlashMessage(..))
import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.Document exposing (Document(..), DocumentStatus(..))
import Lib.Types.SignatoryLink exposing (SignatoryLink(..), AuthenticationToViewMethod(..))
import Lib.Types.ID exposing (showId)
import Lib.Misc.Http as Http

import IdentifyView.Model exposing (..)
import IdentifyView.SMSPin.SMSPin as SMSPin
import IdentifyView.GenericEidService.GenericEidService as GenericEidService
import IdentifyView.SEBankIDCGI.Update as SEBankIDCGI
import IdentifyView.SEBankIDCGI.Model as SEBankIDCGI
import IdentifyView.FITupasNets.Update as FITupasNets
import IdentifyView.FITupasNets.Model as FITupasNets

subscriptions : Model -> Sub Msg
subscriptions model = case model.state of
  IdentifyView {params, innerModel} -> case innerModel of
    IdentifySEBankIDCGI inner -> SEBankIDCGI.subscriptions (toSEBankIDCGIParams params) inner
    IdentifyFITupasNets _ -> FITupasNets.subscriptions (toFITupasNetsParams params)
    _ -> Sub.none
  _ -> Sub.none

port errorTraceMsg : JE.Value -> Cmd msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let errorFlashMessage str =
        update (AddFlashMessageMsg <| FlashError str) model

      criticalError str fields =
        let (newFlashMessages, cmd) =
              FlashMessage.addFlashMessage {embed = FlashMessageMsg} (FlashError str) model.flashMessages
            newModel = { flashMessages = newFlashMessages, state = Error { errorView = Html.text str } }
            (newModel2, cmd2) = update (ErrorTraceMsg fields) newModel
        in (newModel2, Cmd.batch [cmd, cmd2])

  in case msg of

  ErrorTraceMsg fields ->
    let object = case model.state of
          Loading flags ->
            JE.object <|
              [ ("document_id", JE.string <| showId flags.documentId)
              , ("signatory_link_id", JE.string <| showId flags.signatoryLinkId) ]
              ++ fields

          IdentifyView {params} ->
            JE.object <|
              [ ("document_id", JE.string <| showId <| case params.document of Document doc -> doc.id)
              , ("signatory_link_id", JE.string <| showId <| case params.signatory of SignatoryLink sig -> sig.id) ]
              ++ fields

          Error _ -> JE.object fields

    in (model, errorTraceMsg object)

  GetDocumentCallbackMsg res ->
    case model.state of
      Loading flags -> case res of
        Ok (Document document) ->
          let msignatory = document.parties
                          |> List.filter (\(SignatoryLink sl) -> sl.id == flags.signatoryLinkId)
                          |> List.head
          in case msignatory of
            Just (SignatoryLink signatory) ->
              let method = if document.status == Closed
                            then signatory.authenticationMethodToViewArchived
                            else signatory.authenticationMethodToView
                  params = { xtoken = flags.xtoken
                            , document = Document document
                            , signatory = SignatoryLink signatory
                            , localization = flags.localization
                            , brandingDomainId = flags.brandingDomainId
                            , brandingHash = flags.brandingHash
                            , cdnBaseUrl = flags.cdnBaseUrl
                            , location = flags.location
                            , currentYear = flags.currentYear
                            , browserNeedsSEBankIDRedirect = flags.browserNeedsSEBankIDRedirect
                            , origin = flags.origin
                            , netsConf = flags.netsConf
                            }
                  mInnerModel = case method of
                    StandardAuthenticationToView -> Nothing
                    SEBankIDAuthenticationToView ->
                      if flags.eidServiceConf.eidUseForSEView
                      then Just <| IdentifyGenericEidService GenericEidService.SEBankID GenericEidService.Idle
                      else Just <| IdentifySEBankIDCGI {useBankIdLink = False, innerState = SEBankIDCGI.Idle}
                    NOBankIDAuthenticationToView -> Just <| IdentifyGenericEidService GenericEidService.NOBankID GenericEidService.Idle
                    DKNemIDAuthenticationToView -> Just <| IdentifyGenericEidService GenericEidService.DKNemID GenericEidService.Idle
                    SMSPinAuthenticationToView -> Just <| IdentifySMSPin SMSPin.Idle
                    FITupasAuthenticationToView ->
                      if flags.eidServiceConf.eidUseForFIView
                      then Just <| IdentifyGenericEidService GenericEidService.FITupas GenericEidService.Idle
                      else Just <| IdentifyFITupasNets <| FITupasNets.initialState (toFITupasNetsParams params)
                    VerimiAuthenticationToView -> Just <| IdentifyGenericEidService GenericEidService.Verimi GenericEidService.Idle
                    IDINAuthenticationToView -> Just <| IdentifyGenericEidService GenericEidService.IDIN GenericEidService.Idle
                in case mInnerModel of
                    Just innerModel -> ({model | state = IdentifyView { params = params, innerModel = innerModel } }, Cmd.none)
                    Nothing ->
                      let flashMessage = "Internal error: IdentifyView not used for 'Standard' authentication to view."
                          errorFields =
                            [ ("where", JE.string "IdentifyView.update GetDocumentCallbackMsg")
                            , ("what", JE.string "authentication method is 'standard' (shouldn't use IdentifyView)")
                            ]
                      in criticalError flashMessage errorFields

            Nothing ->
              let flashMessage = "Internal error: a signatory with the given id does not exist."
                  errorFields =
                    [ ("where", JE.string "IdentifyView.update GetDocumentCallbackMsg")
                    , ("what", JE.string "signatory does not exist")
                    ]
              in criticalError flashMessage errorFields

        Err err ->
          let flashMessage = "Failed to get document."
              errorFields =
                [ ("where", JE.string "IdentifyView.update GetDocumentCallbackMsg")
                , ("what", JE.string "failed to get document")
                , ("http_error", Http.encodeError err)
                ]
          in criticalError flashMessage errorFields

      _ -> errorFlashMessage "Internal error: got GetDocumentCallbackMsg in incompatible state."

  AddFlashMessageMsg flashMessage ->
    let (newFlashMessages, cmd) = FlashMessage.addFlashMessage {embed = FlashMessageMsg} flashMessage model.flashMessages
    in ({ model | flashMessages = newFlashMessages}, cmd)

  SMSPinMsg msg_ -> case model.state of
    IdentifyView {params, innerModel} -> case innerModel of
      IdentifySMSPin state ->
        let (newInnerState, cmd) = SMSPin.update (toSMSPinParams params) state msg_
            newState = IdentifyView {params = params, innerModel = IdentifySMSPin newInnerState}
        in ( {model | state = newState}, cmd )

      _ -> errorFlashMessage "Internal error: got SMSPin message while in an incompatible state."
    _ -> errorFlashMessage "Internal error: got SMSPin message while in an incompatible state."

  GenericEidServiceMsg msg_ -> case model.state of
    IdentifyView {params, innerModel} -> case innerModel of
      IdentifyGenericEidService provider state ->
        let (newInnerState, cmd) = GenericEidService.update (toGenericEidServiceParams params provider) state msg_
            newState = IdentifyView
                        { params = params
                        , innerModel = IdentifyGenericEidService provider newInnerState
                        }
        in ( {model | state = newState}, cmd )

      _ -> errorFlashMessage "Internal error: got GenericEidService message while in an incompatible state."
    _ -> errorFlashMessage "Internal error: got GenericEidService message while in an incompatible state."

  SEBankIDCGIMsg msg_ -> case model.state of
    IdentifyView {params, innerModel} -> case innerModel of
      IdentifySEBankIDCGI state ->
        let (newInnerState, cmd) = SEBankIDCGI.update msg_ (toSEBankIDCGIParams params) state
            newState = IdentifyView {params = params, innerModel = IdentifySEBankIDCGI newInnerState}
        in ( {model | state = newState}, cmd )

      _ -> errorFlashMessage "Internal error: got SEBankIDCGI message while in an incompatible state."
    _ -> errorFlashMessage "Internal error: got SEBankIDCGI message while in an incompatible state."

  FITupasNetsMsg msg_ -> case model.state of
    IdentifyView {params, innerModel} -> case innerModel of
      IdentifyFITupasNets state ->
        let (newInnerState, cmd) = FITupasNets.update (toFITupasNetsParams params) state msg_
            newState = IdentifyView {params = params, innerModel = IdentifyFITupasNets newInnerState}
        in ( {model | state = newState}, cmd )

      _ -> errorFlashMessage "Internal error: got FITupasNets message while in an incompatible state."
    _ -> errorFlashMessage "Internal error: got FITupasNets message while in an incompatible state."

  FlashMessageMsg msg_ ->
    let (newFlashMessages, cmd) = FlashMessage.update {embed = FlashMessageMsg} msg_ model.flashMessages
    in ( {model | flashMessages = newFlashMessages}, cmd )
