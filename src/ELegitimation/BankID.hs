module ELegitimation.BankID
    ( generateBankIDTransaction
    , generateBankIDTransactionForAuthor
    , verifySignatureAndGetSignInfo
    , verifySignatureAndGetSignInfoForAuthor
    , VerifySignatureResult(..)
    )
    where

import Redirect
import qualified Log
import Control.Monad.State
import Doc.DocStateData as D
import Doc.DocUtils
import ELegitimation.ELegTransaction
import Happstack.Server
import Kontra
import KontraError (internalError)
import MagicHash (MagicHash)
import MinutesTime
import Misc
import Text.XML.HaXml.XmlContent.Parser
import qualified Data.ByteString as BS
import Util.SignatoryLinkUtils
import Util.MonadUtils
import Doc.DocStateQuery
import Text.JSON
import Text.JSON.Fields
import ELegitimation.BankIDUtils
import ELegitimation.BankIDRequests




{- |
   Handle the Ajax request for initiating a BankID transaction.
 -}

generateBankIDTransaction :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
generateBankIDTransaction docid signid = do
    magic <- guardJustM $ readField "magichash"
    provider <- guardJustM $ readField "provider"
    Context{ctxtime} <- getContext
    let seconds = toSeconds ctxtime

    -- sanity check
    document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid signid magic

    unless (document `allowsIdentification` ELegitimationIdentification) internalError
    -- request a nonce

    nonceresponse <- generateChallenge provider
    case nonceresponse of
        Left (ImplStatus _a _b code msg) -> generationFailed "Generate Challenge failed" code msg
        Right (nonce, transactionid) -> do
            -- encode the text to be signed
            tbs <- getTBS document
            encodetbsresponse <- encodeTBS provider tbs transactionid
            case encodetbsresponse of
                Left (ImplStatus _a _b code msg) -> generationFailed "EncodeTBS failed" code msg
                Right txt -> do
                    -- store in session
                    addELegTransaction ELegTransaction
                                            { transactionservertime = ctxtime
                                            , transactiontransactionid = transactionid
                                            , transactiontbs = tbs
                                            , transactionencodedtbs = txt
                                            , transactionsignatorylinkid = Just signid
                                            , transactiondocumentid = docid
                                            , transactionmagichash = Just magic
                                            , transactionnonce = nonce
                                            }
                    Log.eleg "Eleg chalenge generation sucessfull"
                    liftIO $ json $ do
                        field "status" (0::Int)
                        field "servertime"  $ show seconds
                        field "nonce" nonce
                        field "tbs" txt
                        field "transactionid"  transactionid

generateBankIDTransactionForAuthor :: Kontrakcja m => DocumentID -> m JSValue
generateBankIDTransactionForAuthor  docid = do
    provider <- guardJustM $ readField "provider"
    author <- guardJustM $ ctxmaybeuser <$> getContext
    time <- ctxtime <$> getContext
    document <- guardRightM $ getDocByDocID docid
    tbs <- case documentstatus document of
        Preparation    -> getDataFnM $ look "tbs" -- tbs will be sent as post param
        AwaitingAuthor -> getTBS document         -- tbs is stored in document
        _              -> internalError

    unless (isAuthor (document, author)) internalError -- necessary because someone other than author cannot initiate eleg

    nonceresponse <- generateChallenge provider
    case nonceresponse of
        Left (ImplStatus _a _b code msg) -> generationFailed "Generate Challenge failed" code msg
        Right (nonce, transactionid) -> do
            -- encode the text to be signed

            encodetbsresponse <- encodeTBS provider tbs transactionid
            case encodetbsresponse of
                Left (ImplStatus _a _b code msg) -> generationFailed "EncodeTBS failed" code msg
                Right txt -> do
                    -- store in session
                    addELegTransaction ELegTransaction
                                        { transactionservertime      = time
                                        , transactiontransactionid   = transactionid
                                        , transactiontbs             = tbs
                                        , transactionencodedtbs      = txt
                                        , transactionsignatorylinkid = Nothing
                                        , transactiondocumentid      = docid
                                        , transactionmagichash       = Nothing
                                        , transactionnonce           = nonce
                                        }
                    liftIO $ json $ do
                        field "status" (0::Int)
                        field "servertime"  $ show $ toSeconds time
                        field "nonce" nonce
                        field "tbs" txt
                        field "transactionid"  transactionid


generationFailed:: Kontrakcja m => String -> Int -> String -> m JSValue
generationFailed desc code msg = do
  Log.eleg $ desc ++  " | code: " ++ show code ++" msg: "++ msg ++ " |"
  liftIO $ json $ do
                field "status" code
                field "msg" msg
{- |
   Validating eleg-data passed when signing
 -}

data VerifySignatureResult  = Problem String
                            | Mismatch String BS.ByteString BS.ByteString BS.ByteString
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
    -- valid transaction?
    ELegTransaction { transactionsignatorylinkid = mtsignid
                    , transactionmagichash       = mtmagic
                    , transactiondocumentid      = tdocid
                    , transactiontbs
                    , transactionencodedtbs
                    , transactionnonce
                    } <- findTransactionByIDOrFail elegtransactions transactionid

    unless (tdocid   == docid && mtsignid == Just signid && mtmagic  == Just magic )
           internalError
     -- end validation
    Log.eleg $ "Successfully found eleg transaction: " ++ show transactionid
    -- send signature to ELeg
    res <- verifySignature provider
                transactionencodedtbs
                signature
                ( Just transactionnonce <|  provider == BankIDProvider |> Nothing )
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
                    return $ Sign $ SignatureInfo    { signatureinfotext        = transactiontbs
                                                    , signatureinfosignature   = signature
                                                    , signatureinfocertificate = cert
                                                    , signatureinfoprovider    = provider
                                                    , signaturefstnameverified = bfn
                                                    , signaturelstnameverified = bln
                                                    , signaturepersnumverified = bpn
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


verifySignatureAndGetSignInfoForAuthor :: Kontrakcja m => DocumentID -> SignatureProvider -> String  -> String -> m VerifySignatureResult
verifySignatureAndGetSignInfoForAuthor docid provider signature transactionid = do
    Log.eleg $ ("Document " ++ show docid ) ++ ": Author is signing with eleg for document "
    elegtransactions  <- ctxelegtransactions <$> getContext
    author   <- guardJustM  $ ctxmaybeuser <$> getContext
    doc <- guardRightM $ getDocByDocID docid

    unless (isAuthor (doc, author)) internalError -- necessary because someone other than author cannot initiate eleg
    Log.eleg $ ("Document " ++ show docid ) ++ ": Author verified"
    ELegTransaction { transactiondocumentid
                    , transactiontbs
                    , transactionencodedtbs
                    , transactionnonce
                    } <- findTransactionByIDOrFail elegtransactions transactionid

    unless (transactiondocumentid == docid) internalError
    Log.eleg $ ("Document " ++ show docid ) ++ ": Transaction validated"
    Log.eleg $ ("Document " ++ show docid ) ++ ": Document matched"
    unless (doc `allowsIdentification` ELegitimationIdentification) internalError
    Log.eleg $ ("Document " ++ show docid ) ++ ": Document allows eleg"
    res <- verifySignature provider
                    transactionencodedtbs
                    signature
                    ( Just transactionnonce <|  provider == BankIDProvider |> Nothing )
                    transactionid

    case res of
        Left (ImplStatus _a _b code msg) -> do
                    Log.eleg $ ("Document " ++ show docid ) ++ "verifySignature failed: code " ++  show code ++ " message: "++  msg
                    return $ Problem $ "E-legitimationstjänsten misslyckades att verifiera din signatur"
        Right (cert, attrs) -> do
                    Log.debug $ show attrs
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
                                                            , signatureinfosignature   = signature
                                                            , signatureinfocertificate = cert
                                                            , signatureinfoprovider    = provider
                                                            , signaturefstnameverified = bfn
                                                            , signaturelstnameverified = bln
                                                            , signaturepersnumverified = bpn
                                                            }


