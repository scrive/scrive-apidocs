-----------------------------------------------------------------------------
-- |
-- Module      :  PayEx.PayExView
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
--  View module for payments.
-----------------------------------------------------------------------------
module PayEx.PayExView(viewPayment,viewPayments,mailNewPayment) where
import PayEx.PayExState
import Kontra
import Doc.DocState
import Payments.PaymentsState
import Happstack.State (query)
import Templates.Templates
import KontraLink
import qualified Data.ByteString.UTF8 as BS
import Data.Typeable
import Data.Data
import Mails.SendMail

positionInfo :: TemplatesMonad m => (PaymentPosition, Money) -> m String
positionInfo (PaymentForSigning did, _money) = do
    mtitle <- fmap (fmap $ BS.toString . documenttitle) $ query $ GetDocumentByDocumentID did
    renderTemplateFM "paymentForSigningPosition" $ do
        field "documenttitle" mtitle
positionInfo _ = return "" -- FIXME: do something better here

data PaymentView = PaymentView
                      { pvId::String,
                        pvPositions:: [String],
                        pvIsSend::Bool,
                        pvIsWaiting::Bool,
                        pvIsFinished::Bool,
                        pvIsFailed::Bool,
                        pvIsDropped::Bool,
                        pvIsInvoiceable::Bool,
                        pvValue::String,
                        pvLink::String,
                        pvPayExUrl::String
                      } deriving (Data, Typeable)

toPaymentView :: TemplatesMonad m => Payment -> m PaymentView
toPaymentView payment = do
    ps <- sequence $ map positionInfo (positions payment)
    return $ PaymentView {
          pvId = show $ paymentId payment
        , pvPositions = ps
        , pvIsSend = Send == paymentState payment
        , pvIsWaiting = Waiting == paymentState payment
        , pvIsFinished = Finished == paymentState payment
        , pvIsDropped = False
        , pvIsInvoiceable = False
        , pvIsFailed=  isFailed $ payment
        , pvValue= show $ paymentValue payment
        , pvLink= show $ LinkPayExView $ Just $ paymentId payment
        , pvPayExUrl = redirectUrl payment
    }

viewPayment :: TemplatesMonad m => Payment -> m String
viewPayment payment = do
    _pm <- toPaymentView payment
    renderTemplateFM "paymentView" $ do
        field "payment" True--pm

viewPayments :: TemplatesMonad m => [Payment] -> m String
viewPayments payments = do
    _pms <- sequence $ map toPaymentView payments
    renderTemplateFM "paymentsView" $ do
        field "payments" True--pms

mailNewPayment :: TemplatesMonad m => Context -> User -> Payment -> m Mail
mailNewPayment ctx _user payment = do
    _pm <- toPaymentView payment
    title <- renderTemplateM "mailNewPaymentTitle" ()
    content <- renderTemplateFM "mailNewPaymentContent" $ do
        field "payment" True--pm
        field "ctxhostpart" $ ctxhostpart ctx
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}
