module ELegitimation.BankID
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

import Redirect
import qualified Log
import Control.Logic
import Control.Monad.State
import Doc.DocInfo
import Doc.DocStateData as D
import Doc.DocUtils
import ELegitimation.ELegTransaction
import Happstack.Server
import Kontra
import MagicHash (MagicHash)
import MinutesTime
import Misc
import Text.XML.HaXml.XmlContent.Parser
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Util.MonadUtils
import Doc.DocStateQuery
import Text.JSON
import ELegitimation.BankIDUtils
import ELegitimation.BankIDRequests
import Text.JSON.Gen as J

{- |
   Handle the Ajax request for initiating a BankID transaction.
 -}

generateBankIDTransaction :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
generateBankIDTransaction docid signid = do
    magic <- guardJustM $ readField "magichash"
    provider <- guardJustM $ readField "provider"
    Context{ctxtime,ctxlogicaconf} <- getContext
    let seconds = toSeconds ctxtime

    -- sanity check
    document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid signid magic

    unless (document `allowsIdentification` ELegitimationIdentification) internalError
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
                    addELegTransaction ELegTransaction
                                            { transactiontransactionid = transactionid
                                            , transactiontbs = tbs
                                            , transactionencodedtbs = Just txt
                                            , transactionsignatorylinkid = Just signid
                                            , transactiondocumentid = docid
                                            , transactionmagichash = Just magic
                                            , transactionnonce = Just nonce
                                            , transactionstatus = Left "Only necessary for mobile bankid"
                                            , transactionoref = Nothing
                                            }
                    Log.eleg "Eleg chalenge generation sucessfull"
                    return $ runJSONGen $ do
                        J.value "status" (0::Int)
                        J.value "servertime" $ show seconds
                        J.value "nonce" nonce
                        J.value "tbs" txt
                        J.value "transactionid" transactionid

generateBankIDTransactionForAuthor :: Kontrakcja m => DocumentID -> m JSValue
generateBankIDTransactionForAuthor  docid = do
    provider <- guardJustM $ readField "provider"
    author <- guardJustM $ ctxmaybeuser <$> getContext
    time <- ctxtime <$> getContext
    logicaconf <-ctxlogicaconf <$> getContext
    document <- guardRightM $ getDocByDocID docid
    tbs <- case documentstatus document of
        Preparation    -> getDataFnM $ look "tbs" -- tbs will be sent as post param
        _ | canAuthorSignLast document -> getTBS document -- tbs is stored in document
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
                    addELegTransaction ELegTransaction
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
    J.value "msg" msg

{- |
   Validating eleg-data passed when signing
 -}

data VerifySignatureResult  = Problem String
                            | Mismatch String String String String
                            | Sign SignatureInfo

verifySignatureAndGetSignInfo ::  Kontrakcja m =>
                                   DocumentID
                                   -> SignatoryLinkID
                                   -> MagicHash
                                   -> SignatureProvider
                                   -> String
                                   -> String
                                   -> m VerifySignatureResult
verifySignatureAndGetSignInfo docid signid magic provider signature transactionid = do
    elegtransactions  <- ctxelegtransactions <$> getContext
    document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid signid magic
    siglink <- guardJust $ getSigLinkFor document signid
    logicaconf <- ctxlogicaconf <$> getContext
    -- valid transaction?
    ELegTransaction { transactionsignatorylinkid = mtsignid
                    , transactionmagichash       = mtmagic
                    , transactiondocumentid      = tdocid
                    , transactiontbs
                    , transactionencodedtbs      = Just etbs
                    , transactionnonce
                    } <- findTransactionByIDOrFail elegtransactions transactionid

    unless (tdocid   == docid && mtsignid == Just signid && mtmagic  == Just magic )
           internalError
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
            return $ Problem "E-legitimationstjänsten misslyckades att verifiera din signatur"
        -- successful request
        Right (cert, attrs) -> do
            Log.eleg "Successfully identified using eleg. (Omitting private information)."
            -- compare information from document (and fields) to that obtained from BankID
            case compareSigLinkToElegData siglink attrs of
                -- either number or name do not match
                Left (msg, sfn, sln, spn) -> do
                    return $ Mismatch msg sfn sln spn
                -- we have merged the info!
                Right (bfn, bln, bpn) -> do
                    Log.eleg $ "Successfully merged info for transaction: " ++ show transactionid
                    return $ Sign $ SignatureInfo    { signatureinfotext         = transactiontbs
                                                     , signatureinfosignature    = signature
                                                     , signatureinfocertificate  = cert
                                                     , signatureinfoprovider     = provider
                                                     , signaturefstnameverified  = bfn
                                                     , signaturelstnameverified  = bln
                                                     , signaturepersnumverified  = bpn
                                                     , signatureinfoocspresponse = lookup "Validation.ocsp.response" attrs                                                                                 
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
    elegtransactions  <- ctxelegtransactions <$> getContext
    author   <- guardJustM  $ ctxmaybeuser <$> getContext
    doc <- guardRightM $ getDocByDocID docid
    logicaconf <- ctxlogicaconf <$> getContext

    unless (isAuthor (doc, author)) internalError -- necessary because someone other than author cannot initiate eleg
    Log.eleg $ ("Document " ++ show docid ) ++ ": Author verified"
    ELegTransaction { transactiondocumentid
                    , transactiontbs
                    , transactionencodedtbs = Just etbs
                    , transactionnonce
                    } <- findTransactionByIDOrFail elegtransactions transactionid

    unless (transactiondocumentid == docid) internalError
    Log.eleg $ ("Document " ++ show docid ) ++ ": Transaction validated"
    Log.eleg $ ("Document " ++ show docid ) ++ ": Document matched"
    unless (doc `allowsIdentification` ELegitimationIdentification) internalError
    Log.eleg $ ("Document " ++ show docid ) ++ ": Document allows eleg"
    res <- liftIO $ verifySignature logicaconf provider 
                      etbs
                      signature
                      ( transactionnonce <|  provider == BankIDProvider |> Nothing )
                      transactionid

    case res of
        Left (ImplStatus _a _b code msg) -> do
                    Log.eleg $ ("Document " ++ show docid ) ++ "verifySignature failed: code " ++  show code ++ " message: "++  msg
                    return $ Problem $ "E-legitimationstjänsten misslyckades att verifiera din signatur"
        Right (cert, attrs) -> do
                    authorsl <- guardJust $ getAuthorSigLink doc
                    -- compare information from document (and fields) to that obtained from BankID
                    case compareSigLinkToElegData authorsl attrs of
                        Left (msg, _, _, _) -> do
                            Log.eleg $ "merge failed: " ++ msg
                            return $ Problem $ "Dina personuppgifter matchade inte informationen från e-legitimationsservern: " ++ msg
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
    magic <- guardJustM $ readField "magichash"
    logicaconf <- ctxlogicaconf <$> getContext

    -- sanity check
    document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid slid magic
    unless (document `allowsIdentification` ELegitimationIdentification) internalError

    sl <- guardJust $ getSigLinkFor document slid
    let pn = getPersonalNumber sl

    tbs <- getTBS document

    eresponse <- liftIO $ mbiRequestSignature logicaconf pn tbs
    case eresponse of
      Left e -> return $ runJSONGen $ J.value "error" e
      Right (tid, oref) -> do
        addELegTransaction ELegTransaction { transactiontransactionid   = tid
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
    unless (document `allowsIdentification` ELegitimationIdentification) internalError
    
    tbs <-getTBS document
    
    sl <- guardJust $ getAuthorSigLink document
    let pn = getPersonalNumber sl
    Log.debug $ pn
    Log.debug $ tbs
    eresponse <- liftIO $ mbiRequestSignature logicaconf pn tbs
    case eresponse of
      Left e -> return $ runJSONGen $ J.value "error" e
      Right (tid, oref) -> do
        addELegTransaction ELegTransaction { transactiontransactionid   = tid
                                           , transactiondocumentid      = docid
                                           , transactionsignatorylinkid = Nothing
                                           , transactionmagichash       = Nothing
                                           , transactionoref            = Just oref
                                           , transactionstatus          = Right $ CROutstanding tid
                                           , transactiontbs             = tbs
                                           , transactionencodedtbs      = Nothing
                                           , transactionnonce           = Nothing
                                           }
        return $ runJSONGen $ do
          J.value "transactionid" tid
          J.value "status" "outstanding"
          J.value "message" "Starta din BankID säkerhetsapp."

collectMobileBankID :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
collectMobileBankID docid slid = do
  magic <- guardJustM $ readField "magichash"
  tid <- guardJustM $ readField "transactionid"
  logicaconf <- ctxlogicaconf <$> getContext
  elegtransactions  <- ctxelegtransactions <$> getContext
  -- sanity check
  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid slid magic
  unless (document `allowsIdentification` ELegitimationIdentification) internalError

  trans@ELegTransaction {transactionsignatorylinkid
                        ,transactionmagichash
                        ,transactionoref = Just oref
                        ,transactiondocumentid} <- findTransactionByIDOrFail elegtransactions tid
  unless (transactionsignatorylinkid == Just slid &&
          transactionmagichash       == Just magic &&
          transactiondocumentid      == docid) internalError
  eresponse <- liftIO $ mbiRequestCollect logicaconf tid oref
  case eresponse of
    Left e -> do
      addELegTransaction $ trans { transactionstatus = Left e }
      return $ runJSONGen $ J.value "error" e
    Right s@(CROutstanding _) -> do
      addELegTransaction $ trans { transactionstatus = Right s}
      return $ runJSONGen $ do
        J.value "status" "outstanding"
        J.value "message" "Starta din BankID säkerhetsapp."
    Right s@(CRUserSign _) -> do
      addELegTransaction $ trans { transactionstatus = Right s}      
      return $ runJSONGen $ do
        J.value "status" "usersign"
        J.value "message" "Skriv in säkerhetskoden till ditt BankID i din mobiltelefon eller surfplatta och välj Legitimera."
    Right s@(CRComplete _ _signature _attrs) -> do
      addELegTransaction $ trans { transactionstatus = Right s}
      -- save signature and attrs somewhere; 
      return $ runJSONGen $ do
        J.value "status" "complete"
  
collectMobileBankIDForAuthor :: Kontrakcja m => DocumentID -> m JSValue
collectMobileBankIDForAuthor docid = do
  Log.debug "beginning"
  tid <- guardJustM $ getField "transactionid"
  Log.debug tid
  logicaconf <- ctxlogicaconf <$> getContext
  elegtransactions  <- ctxelegtransactions <$> getContext
  -- sanity check
  document <- guardRightM $ getDocByDocIDForAuthor docid
  unless (document `allowsIdentification` ELegitimationIdentification) internalError
  Log.debug "got here"
  trans@ELegTransaction {transactionoref = Just oref
                        ,transactiondocumentid} <- findTransactionByIDOrFail elegtransactions tid
  Log.debug $ show trans
  unless (transactiondocumentid == docid) internalError
  eresponse <- liftIO $ mbiRequestCollect logicaconf tid oref
  case eresponse of
    Left e -> do
      addELegTransaction $ trans { transactionstatus = Left e }
      return $ runJSONGen $ J.value "error" e
    Right s@(CROutstanding _) -> do
      addELegTransaction $ trans { transactionstatus = Right s}
      return $ runJSONGen $ do
        J.value "status" "outstanding"
        J.value "message" "Starta din BankID säkerhetsapp."
    Right s@(CRUserSign _) -> do
      addELegTransaction $ trans { transactionstatus = Right s}      
      return $ runJSONGen $ do
        J.value "status" "usersign"
        J.value "message" "Skriv in säkerhetskoden till ditt BankID i din mobiltelefon eller surfplatta och välj Legitimera."
    Right s@(CRComplete _ _signature _attrs) -> do
      addELegTransaction $ trans { transactionstatus = Right s}
      -- save signature and attrs somewhere; 
      return $ runJSONGen $ do
        J.value "status" "complete"

verifySignatureAndGetSignInfoMobile :: Kontrakcja m 
                                       => DocumentID
                                       -> SignatoryLinkID
                                       -> MagicHash
                                       -> String
                                       -> m VerifySignatureResult
verifySignatureAndGetSignInfoMobile docid signid magic transactionid = do
    elegtransactions  <- ctxelegtransactions <$> getContext
    document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid signid magic
    siglink <- guardJust $ getSigLinkFor document signid
    -- valid transaction?
    ELegTransaction { transactionsignatorylinkid = mtsignid
                    , transactionmagichash       = mtmagic
                    , transactiondocumentid      = tdocid
                    , transactionstatus          = status
                    , transactiontbs
                    } <- findTransactionByIDOrFail elegtransactions transactionid
    unless (tdocid   == docid && mtsignid == Just signid && mtmagic  == Just magic )
           internalError
    case status of
      Right (CRComplete _ signature attrs) -> do
            Log.eleg "Successfully identified using eleg. (Omitting private information)."
            -- compare information from document (and fields) to that obtained from BankID
            case compareSigLinkToElegData siglink attrs of
                -- either number or name do not match
                Left (msg, sfn, sln, spn) -> do
                    return $ Mismatch msg sfn sln spn
                -- we have merged the info!
                Right (bfn, bln, bpn) -> do
                    Log.eleg $ "Successfully merged info for transaction: " ++ show transactionid
                    return $ Sign $ SignatureInfo    { signatureinfotext         = transactiontbs
                                                     , signatureinfosignature    = signature
                                                     , signatureinfocertificate  = ""
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
    elegtransactions  <- ctxelegtransactions <$> getContext
    document <- guardRightM $ getDocByDocIDForAuthor docid
    siglink <- guardJust $ getAuthorSigLink document
    -- valid transaction?
    ELegTransaction { transactiondocumentid      = tdocid
                    , transactionstatus          = status
                    , transactiontbs
                    } <- findTransactionByIDOrFail elegtransactions transactionid
    unless (tdocid == docid) internalError
    case status of
      Right (CRComplete _ signature attrs) -> do
            Log.eleg "Successfully identified using eleg. (Omitting private information)."
            -- compare information from document (and fields) to that obtained from BankID
            case compareSigLinkToElegData siglink attrs of
                -- either number or name do not match
                Left (msg, sfn, sln, spn) -> do
                    return $ Mismatch msg sfn sln spn
                -- we have merged the info!
                Right (bfn, bln, bpn) -> do
                    Log.eleg $ "Successfully merged info for transaction: " ++ show transactionid
                    return $ Sign $ SignatureInfo    { signatureinfotext         = transactiontbs
                                                     , signatureinfosignature    = signature
                                                     , signatureinfocertificate  = ""
                                                     , signatureinfoprovider     = MobileBankIDProvider
                                                     , signaturefstnameverified  = bfn
                                                     , signaturelstnameverified  = bln
                                                     , signaturepersnumverified  = bpn
                                                     , signatureinfoocspresponse = lookup "Validation.ocsp.response" attrs                                                                                 
                                                    }
      Left s -> return $ Problem s
      _ -> return $ Problem "Signature is not yet complete."
