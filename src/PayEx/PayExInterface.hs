{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  PayEx.PayExInterface
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
--  Wrapper around PeyEx webservices
-----------------------------------------------------------------------------
module PayEx.PayExInterface(payexTest,paymentInfo,startPaymentForDocument,checkSendPayments) where
import Control.Monad.State hiding (state)
import PayEx.PayExState
import PayEx.PayExRequest
import PayEx.PayExResponse
import PayEx.PayExView
import SOAP.SOAP
import Kontra
import Happstack.Server hiding (result)
import AppView
import Doc.DocState
import Doc.DocUtils
import Payments.PaymentsState
import Happstack.State (update,query)
import Text.XML.HaXml.XmlContent.Parser (XmlContent)
import KontraLink
import Misc
import Data.Maybe
import Data.List (isInfixOf)
import Mails.SendMail
import MinutesTime
import Redirect
import Happstack.Util.Common

payexTest::Maybe String -> Kontra Response    
payexTest Nothing = do
             ctx <- get
             payments <- case (ctxmaybeuser ctx) of 
                          Just user -> do
                                         invoice <- getField "invoice"  
                                         if (isJust invoice) 
                                           then generateInvoice user
                                           else return ()
                                         liftIO $ query $ GetUserPayments $ userid user
                          Nothing -> return []
             content <- liftIO $ viewPayments  (ctxtemplates ctx)  payments             
             renderFromBody TopNone kontrakcja $ content
             
payexTest (Just pid) =
                   do
                    ctx <- get 
                    mpayment <- sequenceMM $ fmap (liftIO . query . GetPayment . PaymentId) $ readM pid
                    case mpayment of
                     Just payment1 -> do
                                      payment2 <- if (Send == paymentState payment1) 
                                       then liftIO $ checkIfPaymentIsComplete payment1
                                       else return payment1
                                      agreement <- getField "agreement"  
                                      initial <- getField "initial"  
                                      payment3 <- if (isJust initial) 
                                       then do
                                             muser <- query $ GetUserByUserID $ userId payment2
                                             case muser of
                                              Just user ->  processPayment payment2 user (isJust agreement)
                                              Nothing -> return payment2
                                       else return payment2 
                                      unfail <- getField "unfail"  
                                      payment4 <- if (isJust  unfail) 
                                       then case (paymentState payment3) of
                                             (Failed _ state) -> do
                                                                 let npayment = payment3 {paymentState = state}
                                                                 liftIO $ update $ UpdatePayment $ npayment 
                                                                 return $ npayment
                                             _ ->  return payment3
                                       else return payment3
                                      content <- liftIO $ viewPayment (ctxtemplates ctx) payment4
                                      renderFromBody TopNone kontrakcja $ content
                     Nothing -> sendRedirect $ LinkPayExView Nothing

{-| Ajax info about how much we will chage a user for this document |-}
paymentInfo::DocumentID -> Kontra Response
paymentInfo documentid = do
                  ctx <- get
                  signatoriesCount <- fmap (fromMaybe 0) $ readField "signatoriesCount"
                  allowedidtypes <- fmap (fromMaybe "") $  getField  "allowedidtypes"
                  let allowedIdentification  =  (if "Email" `isInfixOf` allowedidtypes  then [EmailIdentification]  else []) ++  (if "ELeg" `isInfixOf` allowedidtypes   then [ELegitimationIdentification]   else [])
                  mdocument <- liftIO $ query $ GetDocumentByDocumentID $ documentid    
                  case (mdocument,ctxmaybeuser ctx) of
                    (Just document,Just user) -> if (isUserAuthor document user)
                                                  then do 
                                                        _ <- getCostOfSigning user document signatoriesCount allowedIdentification
                                                        simpleResponse ""
                                                  else  simpleResponse ""
                                                    
                    _-> simpleResponse ""
                                         
                  
getCostOfSigning::User -> Document -> Integer -> [IdentificationType] -> Kontra Money
getCostOfSigning _u _d sc _its = return $ Money sc

makePayExSoapCall::(PayExRequest a,XmlContent (PC a),XmlContent (PX b)) => PC a ->  IO (PX b)
makePayExSoapCall rq = do
                        res <- makeSoapCallINSECURE (actionURL rq) (soapActionName rq) rq
                        return $ joinPayExError res

sendInitRequest::Payment -> (Maybe String)-> Kontra (PX InitResponse)
sendInitRequest payment agreement =  do
                 request <- askRq
                 ctx <- get
                 rq <- liftIO $ toPC $ PayExInit request (ctxtemplates ctx) agreement payment
                 liftIO $ makePayExSoapCall rq 
                 

sendCompleteRequest::Payment -> IO (PX CompleteResponse)
sendCompleteRequest payment =  do
                 rq <- toPC $ PayExComplete payment
                 makePayExSoapCall rq 

{- not called, comment for compiler
sendCaptureRequest::Payment -> IO (PX CaptureResponse)
sendCaptureRequest payment = do
                           rq <- toPC $ PayExCapture payment
                           makePayExSoapCall rq
-} 

{- not called, comment for compiler
sendCancelRequest::Payment -> IO (PX CancelResponse)
sendCancelRequest payment = do
                           rq <- toPC $ PayExCancel payment
                           makePayExSoapCall rq 
-}

sendAutopayRequest::String -> Payment -> IO (PX AutopayResponse)
sendAutopayRequest agreement payment = do
                           rq <- toPC $ PayExAutopay agreement payment
                           makePayExSoapCall rq 

sendCreateAgreementRequest::String->Money->IO (PX AgreementResponse)
sendCreateAgreementRequest s m =  do
                                  rq <- toPC $ PayExAgreement {
                                          agreementDescription = s,
                                          agreementMax = m }
                                  makePayExSoapCall rq 

tryCreateAgreement::User -> IO (Maybe String)
tryCreateAgreement user = do
                           response <- sendCreateAgreementRequest "Creating agreement" (Money 10000)
                           case response of
                            PX (Left _) -> return Nothing
                            PX (Right res) -> do
                              _ <- update $ SetUserPaymentAccount (userid user) $ (userpaymentaccount user) {paymentAgreementRef= Just $ agreementRef res}
                              return $ Just $ agreementRef res

processAutopay::Payment -> String -> IO Payment
processAutopay payment agreement = runWhenState payment Waiting $ 
                                   do
                                    result <- liftIO $ sendAutopayRequest agreement payment
                                    let npayment = updatePayment result payment 
                                    liftIO $ update $ UpdatePayment npayment 
                                    return $ npayment
    
initPayment::Payment -> (Maybe String)-> Kontra Payment
initPayment payment agreement = runWhenState payment Waiting $ 
                           do
                            result <- sendInitRequest payment agreement  
                            let npayment = updatePayment result payment 
                            liftIO $ update $ UpdatePayment npayment 
                            return $ npayment

startPaymentForDocument::Context -> User -> Document -> IO ()
startPaymentForDocument ctx user document = 
                                          do
                                           payment <- createPaymentForDocument user document
                                           when (paymentState payment == Waiting) $
                                            if (takeImmediatelyPayment user)
                                             then 
                                             case (paymentAgreementRef $ userpaymentaccount user) of
                                              Just agreement -> do
                                                                 _ <- processAutopay payment agreement
                                                                 return ()
                                              Nothing -> sendPaymentMail ctx user payment
                                             else return () 
                                            
sendPaymentMail::Context -> User -> Payment -> IO ()
sendPaymentMail ctx user payment = do
    mail <- mailNewPayment ctx user payment
    scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress { fullname = userfullname user
                                                                        , email = unEmail $ useremail $ userinfo user}
                                                           ]
                                                    }

processPayment::Payment -> User -> Bool -> Kontra Payment
processPayment payment user tryToCreateAgreement= runWhenState payment Waiting $ 
                           do 
                            if (tryToCreateAgreement)
                             then 
                              do
                                v <- liftIO $ tryCreateAgreement user
                                initPayment payment v
                             else 
                               case (paymentAgreementRef $ userpaymentaccount user) of
                                 Just agreement -> liftIO $ processAutopay payment agreement
                                 Nothing ->  initPayment payment Nothing



checkIfPaymentIsComplete::Payment -> IO Payment
checkIfPaymentIsComplete payment = runWhenState payment Send $ 
                                    do
                                    result <- sendCompleteRequest payment     
                                    putStrLn $ show result
                                    let npayment = updatePayment result payment 
                                    now <- getMinutesTime    
                                    update $ UpdatePayment npayment {completeAttempts = now:(completeAttempts npayment)}
                                    return $ npayment

createPaymentForDocument:: User -> Document -> IO Payment
createPaymentForDocument user doc = do
                                       paymentscheme <- getUserPaymentSchema user
                                       let val =  getDocumentPaymentValue paymentscheme doc
                                       payment <- update $ NewPayment emptyPayment {
                                            userId = userid user,
                                            positions=[(PaymentForSigning $ documentid doc,val)],
                                            avaiblePaymentMethods=[CreditCard],
                                            paymentState = if (val > free) then Waiting else Finished  
                                            }        
                                       update $ UpdatePayment payment 
                                       return $ payment


getDocumentPaymentValue::PaymentScheme -> Document -> Money
getDocumentPaymentValue paymentschema doc  =  let persig = forEmailSignature  $$ (paymentForSignature paymentschema)
                                                  (sigcount :: Money) = fromIntegral $ length $ documentsignatorylinks doc
                                              in  sigcount * persig
                                                     

checkSendPayments::MinutesTime -> IO ()
checkSendPayments now = do
                         payments <- query $ GetPaymentsThatNeedCheck now  
                         sequence_ $ map checkIfPaymentIsComplete payments
                         return ()

generateInvoice::User -> Kontra ()
generateInvoice user = do
                        payment <- liftIO $ update $ MergeForUser (userid user)
                        result <- sendInitRequest payment Nothing
                        liftIO $ update $ UpdatePayment $ updatePayment result payment 
                        return ()
--Short utils
runWhenState :: (Monad m) => Payment -> PaymentState -> m Payment -> m Payment
runWhenState payment state a = if (state == (paymentState payment))
                            then a 
                            else return payment
