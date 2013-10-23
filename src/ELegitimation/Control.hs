{-# LANGUAGE ExtendedDefaultRules #-}
module ELegitimation.Control
    ( generateBankIDTransaction
    , generateBankIDTransactionForAuthor
    , verifySignatureAndGetSignInfo
    , verifySignatureAndGetSignInfoForAuthor
    , VerifySignatureResult(..)
    , initiateMobileBankID
    , initiateMobileBankIDForAuthor
    , collectMobileBankID
    , collectMobileBankIDForAuthor
    , verifySignatureAndGetSignInfoMobile
    , verifySignatureAndGetSignInfoMobileForAuthor
    , verifyTransactionForAuthor
    )
    where

import DB
import qualified Log
import Control.Logic
import Control.Monad.State
import Doc.DocStateData
import Doc.Tokens.Model
import ELegitimation.ELegTransaction.Model
import Happstack.Server
import Doc.SignatoryLinkID
import Doc.Model
import Kontra
import Doc.DocumentID
import MagicHash (MagicHash)
import MinutesTime
import Happstack.Fields
import Text.XML.HaXml.XmlContent.Parser
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Util.MonadUtils
import Doc.DocStateQuery
import Text.JSON
import ELegitimation.BankIDUtils
import ELegitimation.BankIDRequests
import Text.JSON.Gen as J
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F
import Data.List
import Data.Maybe

{-
  There are two versions of almost everything for historical reasons.
  Before, the document was not saved before the document was sent or signed.
  So the complete document was not available to Haskell. The Text to be Signed
  (which shows up in the BankID plugin) had to be generated on the client in JS
  for the author. It contains a list of all signatories and other info.
  Also, the Text to be Signed was not available in JS for the signatory because
  we did not have all of that information in sign view in JS.

  We should clean this up because it is a mess! But it needs to be a part of the ajaxification
  of document creation/sending/signing.

 -}
{- |
   Handle the Ajax request for initiating a BankID transaction for a non-author.
 -}
generateBankIDTransaction :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
generateBankIDTransaction docid signid = do
  Context{ctxtime,ctxlogicaconf} <- getContext

  magic    <- guardJustM $ dbQuery $ GetDocumentSessionToken signid
  provider <- guardJustM $ readField "provider"
  let seconds = toSeconds ctxtime

  -- sanity check
  document <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash docid signid magic

  nonceresponse <- liftIO $ generateChallenge ctxlogicaconf provider
  case nonceresponse of
        Left (ImplStatus _a _b code msg) -> generationFailed "Generate Challenge failed" code msg
        Right (nonce, transactionid) -> do
            -- encode the text to be signed
            tbs <- getTBS document
            encodetbsresponse <- liftIO $ encodeTBS ctxlogicaconf provider tbs transactionid
            case encodetbsresponse of
                Left (ImplStatus _a _b code msg) -> generationFailed "EncodeTBS failed" code msg
                Right txt -> do
                    -- store in session
                    dbUpdate . MergeELegTransaction $ ELegTransaction
                                            { transactiontransactionid   = transactionid
                                            , transactiontbs             = tbs
                                            , transactionencodedtbs      = Just txt
                                            , transactionsignatorylinkid = Just signid
                                            , transactiondocumentid      = docid
                                            , transactionmagichash       = Just magic
                                            , transactionnonce           = Just nonce
                                            , transactionstatus          = Left "Only necessary for mobile bankid"
                                            , transactionoref            = Nothing
                                            }
                    Log.eleg "Eleg challenge generation sucessfull"
                    return $ runJSONGen $ do
                        J.value "status"        (0::Int)
                        J.value "servertime" $  show seconds
                        J.value "nonce"         nonce
                        J.value "tbs"           txt
                        J.value "transactionid" transactionid

generateBankIDTransactionForAuthor :: Kontrakcja m => DocumentID -> m JSValue
generateBankIDTransactionForAuthor  docid = do
    provider   <- guardJustM $ readField "provider"
    author     <- guardJustM $ ctxmaybeuser <$> getContext
    time       <- ctxtime <$> getContext
    logicaconf <- ctxlogicaconf <$> getContext
    document   <- getDocByDocID docid
    tbs <- case documentstatus document of
        Preparation    -> getDataFnM $ look "tbs" -- tbs will be sent as post param
        _              -> internalError
    unless (isAuthor (document, author)) internalError -- necessary because someone other than author cannot initiate eleg

    nonceresponse <- liftIO $ generateChallenge logicaconf provider
    case nonceresponse of
        Left (ImplStatus _a _b code msg) -> generationFailed "Generate Challenge failed" code msg
        Right (nonce, transactionid) -> do
            -- encode the text to be signed

            encodetbsresponse <- liftIO $ encodeTBS logicaconf provider tbs transactionid
            case encodetbsresponse of
                Left (ImplStatus _a _b code msg) -> generationFailed "EncodeTBS failed" code msg
                Right txt -> do
                    -- store in session
                    dbUpdate . MergeELegTransaction $ ELegTransaction
                                        { transactiontransactionid   = transactionid
                                        , transactiontbs             = tbs
                                        , transactionencodedtbs      = Just txt
                                        , transactionsignatorylinkid = Nothing
                                        , transactiondocumentid      = docid
                                        , transactionmagichash       = Nothing
                                        , transactionnonce           = Just nonce
                                        , transactionstatus = Left "Only necessary for mobile bankid"
                                        , transactionoref = Nothing
                                        }
                    return $ runJSONGen $ do
                        J.value "status" (0::Int)
                        J.value "servertime"  $ show $ toSeconds time
                        J.value "nonce" nonce
                        J.value "tbs" txt
                        J.value "transactionid"  transactionid


generationFailed:: Kontrakcja m => String -> Int -> String -> m JSValue
generationFailed desc code msg = do
  Log.eleg $ desc ++  " | code: " ++ show code ++" msg: "++ msg ++ " |"
  return $ runJSONGen $ do
    J.value "status" code
    J.value "msg"    msg

{- |
   Validating eleg-data passed when signing
 -}

data VerifySignatureResult  = Problem String
                            | Mismatch String String String String
                            | Sign SignatureInfo

-- Just a note: we should not pass these in the url; these url parameters should be passed as JSON
verifySignatureAndGetSignInfo ::  Kontrakcja m =>
                                   DocumentID
                                   -> SignatoryLinkID
                                   -> MagicHash
                                   -> [(FieldType, String)]
                                   -> SignatureProvider
                                   -> String
                                   -> String
                                   -> m VerifySignatureResult
verifySignatureAndGetSignInfo docid signid magic fields provider signature transactionid = do
    ELegTransaction{..} <- guardJustM  $ dbQuery $ GetELegTransaction transactionid
    document            <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash docid signid magic
    siglink             <- guardJust   $ getSigLinkFor document signid
    logicaconf          <-               ctxlogicaconf <$> getContext

    -- valid transaction?
    unless (transactiondocumentid == docid) internalError -- Error if document does not match

    if (isJust transactionsignatorylinkid && isJust transactionmagichash)
       -- Either we have siglink and magichash in transaction and it matches current one
       then unless (transactionsignatorylinkid == Just signid && transactionmagichash == Just magic) internalError
       -- Or we have don't have it, and current one is the author
       else unless (isAuthor siglink) internalError

    -- our encodedtbs should be a Just at this point
    etbs <- guardJust transactionencodedtbs

     -- end validation
    Log.eleg $ "Successfully found eleg transaction: " ++ show transactionid
    -- send signature to ELeg
    res <- liftIO $ verifySignature logicaconf provider
                etbs
                signature
                ( transactionnonce <|  provider == BankIDProvider |> Nothing )
                transactionid
    case res of
        -- error state
        Left (ImplStatus _a _b code msg) -> do
            Log.eleg $ "verifySignature failed: code " ++  show code ++ " message: "++  msg
            prob <- renderTemplate "bankidVerificationFailed" $ return ()
            return $ Problem prob
        -- successful request
        Right (cert, attrs) -> do
            Log.eleg "Successfully identified using eleg. (Omitting private information)."
            -- compare information from document (and fields) to that obtained from BankID
            info <- compareSigLinkToElegData siglink fields attrs
            case info of
                -- either number or name do not match
                Left (msg, sfn, sln, spn) -> do
                    return $ Mismatch msg sfn sln spn
                -- we have merged the info!
                Right (bn, bpn) -> do
                    Log.eleg $ "Successfully merged info for transaction: " ++ show transactionid
                    let mocsp = lookup "Validation.ocsp.response" attrs
                    return $ Sign $ SignatureInfo    { signatureinfotext         = transactiontbs
                                                     , signatureinfosignature    = signature
                                                     , signatureinfocertificate  = cert
                                                     , signatureinfoprovider     = provider
                                                     , signaturefstnameverified  = bn
                                                     , signaturelstnameverified  = bn
                                                     , signaturepersnumverified  = bpn
                                                     , signatureinfoocspresponse = mocsp
                                                    }

{- |
    Handle the ajax request for an eleg signature for the author.
    URL: /d/{provider}/{docid}
    Method: GET

    Validation:
    the document absolutely must exist. If the docid is not found, 404
    the tbs text must exist
-}

{- |
   Handle POST when author wants to sign+issue document
   URL: /d/{docid}
   Method: POST
   params
     provider -- the eleg provider (bankid, nordea, telia)
     signature -- signature generated by BankID plugin
     transactionid -- the id of the transaction from the BankID Ajax request

    Validation:
    the document absolutely must exist. If the docid is not found, 404
    the author should be logged in, otherwise 404
    provider, signature, transactionid must exist (or 404)
    there must be a valid eleg transaction
        the transaction must use the same docid

 -}


verifySignatureAndGetSignInfoForAuthor :: Kontrakcja m => DocumentID -> SignatureProvider -> String -> String -> m VerifySignatureResult
verifySignatureAndGetSignInfoForAuthor docid provider signature transactionid = do
    Log.eleg $ ("Document " ++ show docid ) ++ ": Author is signing with eleg for document "
    author   <- guardJustM  $ ctxmaybeuser <$> getContext
    doc <- getDocByDocID docid
    logicaconf <- ctxlogicaconf <$> getContext

    unless (isAuthor (doc, author)) internalError -- necessary because someone other than author cannot initiate eleg
    Log.eleg $ ("Document " ++ show docid ) ++ ": Author verified"
    ELegTransaction { transactiondocumentid
                    , transactiontbs
                    , transactionencodedtbs = Just etbs
                    , transactionnonce
                    } <- guardJustM $ dbQuery $ GetELegTransaction transactionid

    unless (transactiondocumentid == docid) internalError
    Log.eleg $ ("Document " ++ show docid ) ++ ": Transaction validated"
    Log.eleg $ ("Document " ++ show docid ) ++ ": Document matched"

    Log.eleg $ ("Document " ++ show docid ) ++ ": Document allows eleg"
    res <- liftIO $ verifySignature logicaconf provider
                      etbs
                      signature
                      ( transactionnonce <|  provider == BankIDProvider |> Nothing )
                      transactionid

    case res of
      Left (ImplStatus _a _b code msg) -> do
        Log.eleg $ ("Document " ++ show docid ) ++ "verifySignature failed: code " ++  show code ++ " message: "++  msg
        prob <- renderTemplate "bankidVerificationFailed" $ return ()
        return $ Problem prob
      Right (cert, attrs) -> do
        authorsl <- guardJust $ getAuthorSigLink doc
        -- compare information from document (and fields) to that obtained from BankID
        info <- compareSigLinkToElegData authorsl [] attrs
        case info of
          Left (msg, _, _, _) -> do
            Log.eleg $ "merge failed: " ++ msg
            prob <- renderTemplate "bankidPersInfoMismatch" $ F.value "message" msg
            return $ Problem prob
          -- we have merged the info!
          Right (bn, bpn) -> do
            Log.eleg "author merge succeeded. (details omitted)"
            return $ Sign $ SignatureInfo    { signatureinfotext        = transactiontbs
                                             , signatureinfosignature    = signature
                                             , signatureinfocertificate  = cert
                                             , signatureinfoprovider     = provider
                                             , signaturefstnameverified  = bn
                                             , signaturelstnameverified  = bn
                                             , signaturepersnumverified  = bpn
                                             , signatureinfoocspresponse = lookup "Validation.ocsp.response" attrs
                                             }


-- MobileBankID

initiateMobileBankID :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
initiateMobileBankID docid slid = do
    magic <- guardJustM $ dbQuery $ GetDocumentSessionToken slid
    logicaconf <- ctxlogicaconf <$> getContext

    -- sanity check
    document <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash docid slid magic

    mpn <- getField "personnummer"

    sl <- guardJust $ getSigLinkFor document slid
    let pn = fromMaybe (getPersonalNumber sl) mpn

    tbs <- getTBS document

    eresponse <- liftIO $ mbiRequestSignature logicaconf pn tbs
    case eresponse of
      Left e -> mobileBankIDErrorJSON e pn
      Right (tid, oref) -> do
        dbUpdate . MergeELegTransaction $ ELegTransaction {
            transactiontransactionid   = tid
          , transactiondocumentid      = docid
          , transactionsignatorylinkid = Just slid
          , transactionmagichash       = Just magic
          , transactionoref            = Just oref
          , transactionstatus          = Right $ CROutstanding tid
          , transactiontbs             = tbs
          , transactionencodedtbs      = Nothing
          , transactionnonce           = Nothing
        }
        return $ runJSONGen $ J.value "transactionid" tid

initiateMobileBankIDForAuthor :: Kontrakcja m => DocumentID -> m JSValue
initiateMobileBankIDForAuthor docid = do
    logicaconf <- ctxlogicaconf <$> getContext
    -- sanity check
    document <- getDocByDocIDForAuthor docid

    tbs <-getTBS document
    sl <- guardJust $ getAuthorSigLink document
    let pn = getPersonalNumber sl
    eresponse <- liftIO $ mbiRequestSignature logicaconf pn tbs
    case eresponse of
      Left e -> mobileBankIDErrorJSON e pn
      Right (tid, oref) -> do
        dbUpdate . MergeELegTransaction $ ELegTransaction {
            transactiontransactionid   = tid
          , transactiondocumentid      = docid
          , transactionsignatorylinkid = Just $ signatorylinkid sl
          , transactionmagichash       = Just $ signatorymagichash sl
          , transactionoref            = Just oref
          , transactionstatus          = Right $ CROutstanding tid
          , transactiontbs             = tbs
          , transactionencodedtbs      = Nothing
          , transactionnonce           = Nothing
        }
        msg <- renderTemplate "bankidStartApp" $ return ()
        return $ runJSONGen $ do
          J.value "transactionid" tid
          J.value "status" "outstanding"
          J.value "message" msg

collectMobileBankID :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
collectMobileBankID docid slid = do
  magic <- guardJustM $ dbQuery $ GetDocumentSessionToken slid
  tid <- guardJustM $ getField "transactionid"
  logicaconf <- ctxlogicaconf <$> getContext
  -- sanity check
  document <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash docid slid magic

  trans@ELegTransaction {transactionsignatorylinkid
                        ,transactionmagichash
                        ,transactionoref
                        ,transactionstatus
                        ,transactiondocumentid} <- guardJustM $ dbQuery $ GetELegTransaction tid
  unless (transactionsignatorylinkid == Just slid &&
          transactionmagichash       == Just magic &&
          transactiondocumentid      == docid) internalError

  sl <- guardJust $ getSigLinkFor document slid
  let pn = getPersonalNumber sl

  case transactionstatus of
    Left e -> mobileBankIDErrorJSON e pn
    Right (CRComplete _ _ _) ->
      return $ runJSONGen $ J.value "status" "complete"
    _ -> do
      oref <- guardJust transactionoref
      eresponse <- liftIO $ mbiRequestCollect logicaconf tid oref
      case eresponse of
        Left e -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Left e }
          return $ runJSONGen $ do
            J.value "error" e
            J.value "personalNumber" pn
        Right s@(CROutstanding _) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          msg <- renderTemplate "bankidStartApp" $ return ()
          return $ runJSONGen $ do
            J.value "status" "outstanding"
            J.value "message" msg
            J.value "personalNumber" pn
        Right s@(CRUserSign _) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          msg <- renderTemplate "bankidEnterCode" $ return ()
          return $ runJSONGen $ do
            J.value "status" "usersign"
            J.value "message" msg
            J.value "personalNumber" pn
        Right s@(CRComplete _ _signature _attrs) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          -- save signature and attrs somewhere;
          return $ runJSONGen $ do
            J.value "status" "complete"
            J.value "personalNumber" pn

collectMobileBankIDForAuthor :: Kontrakcja m => DocumentID -> m JSValue
collectMobileBankIDForAuthor docid = do
  tid <- guardJustM $ getField "transactionid"
  logicaconf <- ctxlogicaconf <$> getContext
  -- sanity check
  document <- getDocByDocIDForAuthor docid

  trans@ELegTransaction {transactionoref
                        ,transactionstatus
                        ,transactiondocumentid} <- guardJustM $ dbQuery $ GetELegTransaction tid
  oref <- guardJust transactionoref
  unless (transactiondocumentid == docid) internalError
  sl <- guardJust $ getAuthorSigLink document
  let pn = getPersonalNumber sl

  case transactionstatus of
    Left e -> mobileBankIDErrorJSON e pn
    Right (CRComplete _ _ _) ->
      return $ runJSONGen $ do
        J.value "status" "complete"
        J.value "personalNumber" pn
    _ -> do
      eresponse <- liftIO $ mbiRequestCollect logicaconf tid oref
      case eresponse of
        Left e -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Left e }
          return $ runJSONGen $ do
            J.value "error" e
            J.value "personalNumber" pn
        Right s@(CROutstanding _) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          msg <- renderTemplate "bankidStartApp" $ return ()
          return $ runJSONGen $ do
            J.value "status" "outstanding"
            J.value "message" msg
            J.value "personalNumber" pn
        Right s@(CRUserSign _) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          msg <- renderTemplate "bankidEnterCode" $ return ()
          return $ runJSONGen $ do
            J.value "status" "usersign"
            J.value "message" msg
            J.value "personalNumber" pn
        Right s@(CRComplete _ _signature _attrs) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          -- save signature and attrs somewhere;
          return $ runJSONGen $ do
            J.value "status" "complete"
            J.value "personalNumber" pn

verifySignatureAndGetSignInfoMobile :: Kontrakcja m
                                       => DocumentID
                                       -> SignatoryLinkID
                                       -> MagicHash
                                       -> [(FieldType, String)]
                                       -> String
                                       -> m VerifySignatureResult
verifySignatureAndGetSignInfoMobile docid signid magic fields transactionid = do
    document <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash docid signid magic
    siglink <- guardJust $ getSigLinkFor document signid
    -- valid transaction?
    ELegTransaction { transactionsignatorylinkid = mtsignid
                    , transactionmagichash       = mtmagic
                    , transactiondocumentid      = tdocid
                    , transactionstatus          = status
                    , transactiontbs
                    } <- guardJustM $ dbQuery $ GetELegTransaction transactionid


    -- valid transaction?
    unless (tdocid == docid) internalError -- Error if document does not match

    if (isJust mtsignid && isJust mtmagic)
       -- Either we have siglink and magichash in transaction and it matches current one
       then unless (mtsignid == Just signid && mtmagic == Just magic) internalError
       -- Or we have don't have it, and current one is the author
       else unless (isAuthor siglink) internalError

    case status of
      Right (CRComplete _ signature attrs) -> do
            Log.eleg "Successfully identified using eleg. (Omitting private information)."
            -- compare information from document (and fields) to that obtained from BankID
            info <- compareSigLinkToElegData siglink fields attrs
            case info of
                -- either number or name do not match
                Left (msg, sfn, sln, spn) -> do
                    return $ Mismatch msg sfn sln spn
                -- we have merged the info!
                Right (bn, bpn) -> do
                    Log.eleg $ "Successfully merged info for transaction: " ++ show transactionid
                    return $ Sign $ SignatureInfo    { signatureinfotext         = transactiontbs
                                                     , signatureinfosignature    = signature
                                                     , signatureinfocertificate  = "" -- it appears that Mobile BankID returns no certificate
                                                     , signatureinfoprovider     = MobileBankIDProvider
                                                     , signaturefstnameverified  = bn
                                                     , signaturelstnameverified  = bn
                                                     , signaturepersnumverified  = bpn
                                                     , signatureinfoocspresponse = lookup "Validation.ocsp.response" attrs
                                                    }
      Left s -> return $ Problem s
      _ -> return $ Problem "Signature is not yet complete."

verifySignatureAndGetSignInfoMobileForAuthor :: Kontrakcja m
                                             => DocumentID
                                             -> String
                                             -> m VerifySignatureResult
verifySignatureAndGetSignInfoMobileForAuthor docid transactionid = do
    document <- getDocByDocIDForAuthor docid
    siglink <- guardJust $ getAuthorSigLink document
    -- valid transaction?
    ELegTransaction { transactiondocumentid      = tdocid
                    , transactionstatus          = status
                    , transactiontbs
                    } <- guardJustM $ dbQuery $ GetELegTransaction transactionid
    unless (tdocid == docid) internalError
    case status of
      Right (CRComplete _ signature attrs) -> do
            Log.eleg "Successfully identified using eleg. (Omitting private information)."
            -- compare information from document (and fields) to that obtained from BankID
            info <- compareSigLinkToElegData siglink [] attrs
            case info of
                -- either number or name do not match
                Left (msg, sfn, sln, spn) -> do
                    return $ Mismatch msg sfn sln spn
                -- we have merged the info!
                Right (bn, bpn) -> do
                    Log.eleg $ "Successfully merged info for transaction: " ++ show transactionid
                    return $ Sign $ SignatureInfo    { signatureinfotext         = transactiontbs
                                                     , signatureinfosignature    = signature
                                                     , signatureinfocertificate  = "" -- it appears that Mobile BankID returns no certificate
                                                     , signatureinfoprovider     = MobileBankIDProvider
                                                     , signaturefstnameverified  = bn
                                                     , signaturelstnameverified  = bn
                                                     , signaturepersnumverified  = bpn
                                                     , signatureinfoocspresponse = lookup "Validation.ocsp.response" attrs
                                                    }
      Left s -> return $ Problem s
      _ -> return $ Problem "Signature is not yet complete."


{- Precheck for transaction - so we can tell if signing will succeed -}
verifyTransactionForAuthor :: Kontrakcja m
                                    => DocumentID
                                    -> m JSValue
verifyTransactionForAuthor docid = do
  mprovider  <- readField "eleg"
  transactionid <- getField'  "transactionid"
  signature <- getField'  "signature"
  vr <- case mprovider of
                Nothing ->  return $ Problem "No provider"
                Just MobileBankIDProvider -> verifySignatureAndGetSignInfoMobileForAuthor docid transactionid
                Just provider ->  verifySignatureAndGetSignInfoForAuthor docid provider signature transactionid
  case vr of
      Sign _ -> runJSONGenT $ J.value "verified" True
      _    -> runJSONGenT $ J.value "verified" False



mobileBankIDErrorJSON :: TemplatesMonad m => String -> String -> m JSValue
mobileBankIDErrorJSON e pn = do
  errormessage <- errortemplate
  runJSONGenT $ do
    J.value "error" errormessage
    J.value "personalNumber" pn
  where errortemplate = case e of
          _ | "INVALID_PARAMETERS"     `isInfixOf` e -> renderTemplate_ "bankidInvalidParameters"
            | "SIGN_VALIDATION_FAILED" `isInfixOf` e -> renderTemplate_ "bankidInternalError"
            | "RETRY"                  `isInfixOf` e -> renderTemplate_ "bankidInternalError"
            | "INTERNAL_ERROR"         `isInfixOf` e -> renderTemplate_ "bankidInternalError"
            | "UNKNOWN_USER"           `isInfixOf` e -> renderTemplate_ "bankidUnknownUser"
            | "EXPIRED_TRANSACTION"    `isInfixOf` e -> renderTemplate_ "bankidExpiredTransaction"
            | "INVALID_DEVICES"        `isInfixOf` e -> renderTemplate_ "bankidInvalidDevices"
            | "ALREADY_IN_PROGRESS"    `isInfixOf` e -> renderTemplate_ "bankidAlreadyInProgress"
            | "USER_CANCEL"            `isInfixOf` e -> renderTemplate_ "bankidUserCancel"
            | otherwise                              -> return e
