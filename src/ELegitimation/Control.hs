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
    )
    where

import DB
import Redirect
import qualified Log
import Control.Logic
import Control.Monad.State
import Doc.DocStateData as D
import Doc.DocUtils
import Doc.Tokens.Model
import ELegitimation.ELegTransaction.Model
import Happstack.Server
import Kontra
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
import Templates.Templates
import qualified Templates.Fields as F
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
  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid signid magic

  unless (document `allowsAuthMethod` ELegAuthentication) internalError
    -- request a nonce

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
    logicaconf <-ctxlogicaconf <$> getContext
    document   <- guardRightM $ getDocByDocID docid
    tbs <- case documentstatus document of
        Preparation    -> getDataFnM $ look "tbs" -- tbs will be sent as post param
        _ | canAuthorSignNow document -> getTBS document -- tbs is stored in document
        _              -> internalError
    let asl = case documentstatus document of
                Preparation    -> Nothing
                _ -> getAuthorSigLink document
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
                                        , transactionsignatorylinkid = signatorylinkid <$> asl
                                        , transactiondocumentid      = docid
                                        , transactionmagichash       = signatorymagichash <$> asl
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
    ELegTransaction{..} <- guardJustM404  $ dbQuery $ GetELegTransaction transactionid
    document            <- guardRightM    $ getDocByDocIDSigLinkIDAndMagicHash docid signid magic
    siglink             <- guardJust404   $ getSigLinkFor document signid
    logicaconf          <-               ctxlogicaconf <$> getContext

    -- valid transaction?
    unless (transactiondocumentid == docid && transactionsignatorylinkid == Just signid && transactionmagichash == Just magic )
           internalError
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
                Right (bfn, bln, bpn) -> do
                    Log.eleg $ "Successfully merged info for transaction: " ++ show transactionid
                    let mocsp = lookup "Validation.ocsp.response" attrs
                    return $ Sign $ SignatureInfo    { signatureinfotext         = transactiontbs
                                                     , signatureinfosignature    = signature
                                                     , signatureinfocertificate  = cert
                                                     , signatureinfoprovider     = provider
                                                     , signaturefstnameverified  = bfn
                                                     , signaturelstnameverified  = bln
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
    doc <- guardRightM $ getDocByDocID docid
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
    unless (doc `allowsAuthMethod` ELegAuthentication) internalError
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
          Right (bfn, bln, bpn) -> do
            Log.eleg "author merge succeeded. (details omitted)"
            return $ Sign $ SignatureInfo    { signatureinfotext        = transactiontbs
                                             , signatureinfosignature    = signature
                                             , signatureinfocertificate  = cert
                                             , signatureinfoprovider     = provider
                                             , signaturefstnameverified  = bfn
                                             , signaturelstnameverified  = bln
                                             , signaturepersnumverified  = bpn
                                             , signatureinfoocspresponse = lookup "Validation.ocsp.response" attrs
                                             }


-- MobileBankID

initiateMobileBankID :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
initiateMobileBankID docid slid = do
    magic <- guardJustM $ dbQuery $ GetDocumentSessionToken slid
    logicaconf <- ctxlogicaconf <$> getContext

    -- sanity check
    document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid slid magic
    unless (document `allowsAuthMethod` ELegAuthentication) internalError

    mpn <- getField "personnummer"

    sl <- guardJust $ getSigLinkFor document slid
    let pn = fromMaybe (getPersonalNumber sl) mpn
        
    tbs <- getTBS document

    eresponse <- liftIO $ mbiRequestSignature logicaconf pn tbs
    case eresponse of
      Left e -> mobileBankIDErrorJSON e
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
    document <- guardRightM $ getDocByDocIDForAuthor docid
    unless (document `allowsAuthMethod` ELegAuthentication) internalError
    tbs <-getTBS document
    sl <- guardJust $ getAuthorSigLink document
    let pn = getPersonalNumber sl
    eresponse <- liftIO $ mbiRequestSignature logicaconf pn tbs
    case eresponse of
      Left e -> mobileBankIDErrorJSON e
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
  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid slid magic
  unless (document `allowsAuthMethod` ELegAuthentication) internalError

  trans@ELegTransaction {transactionsignatorylinkid
                        ,transactionmagichash
                        ,transactionoref
                        ,transactionstatus
                        ,transactiondocumentid} <- guardJustM $ dbQuery $ GetELegTransaction tid
  unless (transactionsignatorylinkid == Just slid &&
          transactionmagichash       == Just magic &&
          transactiondocumentid      == docid) internalError

  case transactionstatus of
    Left e -> mobileBankIDErrorJSON e
    Right (CRComplete _ _ _) -> 
      return $ runJSONGen $ J.value "status" "complete"
    _ -> do
      oref <- guardJust transactionoref
      eresponse <- liftIO $ mbiRequestCollect logicaconf tid oref
      case eresponse of
        Left e -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Left e }
          return $ runJSONGen $ J.value "error" e
        Right s@(CROutstanding _) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          msg <- renderTemplate "bankidStartApp" $ return ()
          return $ runJSONGen $ do
            J.value "status" "outstanding"
            J.value "message" msg
        Right s@(CRUserSign _) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          msg <- renderTemplate "bankidEnterCode" $ return ()
          return $ runJSONGen $ do
            J.value "status" "usersign"
            J.value "message" msg
        Right s@(CRComplete _ _signature _attrs) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          -- save signature and attrs somewhere; 
          return $ runJSONGen $ do
            J.value "status" "complete"
  
collectMobileBankIDForAuthor :: Kontrakcja m => DocumentID -> m JSValue
collectMobileBankIDForAuthor docid = do
  tid <- guardJustM $ getField "transactionid"
  logicaconf <- ctxlogicaconf <$> getContext
  -- sanity check
  document <- guardRightM $ getDocByDocIDForAuthor docid
  unless (document `allowsAuthMethod` ELegAuthentication) internalError
  trans@ELegTransaction {transactionoref
                        ,transactionstatus
                        ,transactiondocumentid} <- guardJustM $ dbQuery $ GetELegTransaction tid
  oref <- guardJust transactionoref
  unless (transactiondocumentid == docid) internalError
  case transactionstatus of
    Left e -> mobileBankIDErrorJSON e
    Right (CRComplete _ _ _) ->
      return $ runJSONGen $ J.value "status" "complete"
    _ -> do
      eresponse <- liftIO $ mbiRequestCollect logicaconf tid oref
      case eresponse of
        Left e -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Left e }
          return $ runJSONGen $ J.value "error" e
        Right s@(CROutstanding _) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          msg <- renderTemplate "bankidStartApp" $ return ()
          return $ runJSONGen $ do
            J.value "status" "outstanding"
            J.value "message" msg
        Right s@(CRUserSign _) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}      
          msg <- renderTemplate "bankidEnterCode" $ return ()
          return $ runJSONGen $ do
            J.value "status" "usersign"
            J.value "message" msg
        Right s@(CRComplete _ _signature _attrs) -> do
          dbUpdate . MergeELegTransaction $ trans { transactionstatus = Right s}
          -- save signature and attrs somewhere; 
          return $ runJSONGen $ do
            J.value "status" "complete"

verifySignatureAndGetSignInfoMobile :: Kontrakcja m 
                                       => DocumentID
                                       -> SignatoryLinkID
                                       -> MagicHash
                                       -> [(FieldType, String)]
                                       -> String
                                       -> m VerifySignatureResult
verifySignatureAndGetSignInfoMobile docid signid magic fields transactionid = do
    document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid signid magic
    siglink <- guardJust $ getSigLinkFor document signid
    -- valid transaction?
    ELegTransaction { transactionsignatorylinkid = mtsignid
                    , transactionmagichash       = mtmagic
                    , transactiondocumentid      = tdocid
                    , transactionstatus          = status
                    , transactiontbs
                    } <- guardJustM $ dbQuery $ GetELegTransaction transactionid
    unless (tdocid == docid && mtsignid == Just signid && mtmagic == Just magic )
           internalError

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
                Right (bfn, bln, bpn) -> do
                    Log.eleg $ "Successfully merged info for transaction: " ++ show transactionid
                    return $ Sign $ SignatureInfo    { signatureinfotext         = transactiontbs
                                                     , signatureinfosignature    = signature
                                                     , signatureinfocertificate  = "" -- it appears that Mobile BankID returns no certificate
                                                     , signatureinfoprovider     = MobileBankIDProvider
                                                     , signaturefstnameverified  = bfn
                                                     , signaturelstnameverified  = bln
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
    document <- guardRightM $ getDocByDocIDForAuthor docid
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
                Right (bfn, bln, bpn) -> do
                    Log.eleg $ "Successfully merged info for transaction: " ++ show transactionid
                    return $ Sign $ SignatureInfo    { signatureinfotext         = transactiontbs
                                                     , signatureinfosignature    = signature
                                                     , signatureinfocertificate  = "" -- it appears that Mobile BankID returns no certificate
                                                     , signatureinfoprovider     = MobileBankIDProvider
                                                     , signaturefstnameverified  = bfn
                                                     , signaturelstnameverified  = bln
                                                     , signaturepersnumverified  = bpn
                                                     , signatureinfoocspresponse = lookup "Validation.ocsp.response" attrs                                                                                 
                                                    }
      Left s -> return $ Problem s
      _ -> return $ Problem "Signature is not yet complete."

mobileBankIDErrorJSON :: TemplatesMonad m => String -> m JSValue
mobileBankIDErrorJSON e = do
  errormessage <- renderTemplate errortemplate $ return ()
  runJSONGenT $ J.value "error" errormessage
  where errortemplate = case e of
          _ | "INVALID_PARAMETERS"     `isInfixOf` e -> "bankidInvalidParameters"
            | "SIGN_VALIDATION_FAILED" `isInfixOf` e -> "bankidInternalError"
            | "RETRY"                  `isInfixOf` e -> "bankidInternalError"
            | "INTERNAL_ERROR"         `isInfixOf` e -> "bankidInternalError"
            | "UNKNOWN_USER"           `isInfixOf` e -> "bankidUnknownUser"
            | "EXPIRED_TRANSACTION"    `isInfixOf` e -> "bankidExpiredTransaction"
            | "INVALID_DEVICES"        `isInfixOf` e -> "bankidInvalidDevices"
            | "ALREADY_IN_PROGRESS"    `isInfixOf` e -> "bankidAlreadyInProgress"
            | "USER_CANCEL"            `isInfixOf` e -> "bankidUserCancel"
            | otherwise                              -> e
