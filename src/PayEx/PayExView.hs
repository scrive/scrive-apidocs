{-# LANGUAGE CPP, OverloadedStrings, TupleSections , OverlappingInstances , ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
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
import Control.Monad.State
import PayEx.PayExState
import PayEx.PayExRequest
import PayEx.PayExResponse
import SOAP.SOAP
import Kontra 
import Happstack.Server
import Doc.DocState
import Payments.PaymentsState
import Happstack.State (update,query)
import Templates.Templates
import Text.XML.HaXml.XmlContent.Parser (XmlContent)
import KontraLink
import Misc
import qualified Data.ByteString.UTF8 as BS
import Data.Typeable
import Data.Data
import Mails.SendMail

positionInfo::KontrakcjaTemplates -> (PaymentPosition,Money) -> IO String
positionInfo templates (PaymentForSigning did,money) = do
                                        mtitle <- fmap (fmap $ BS.toString . documenttitle) $ query $ GetDocumentByDocumentID did
                                        renderTemplate templates "paymentForSigningPosition" 
                                                                     (setAttribute "documenttitle" mtitle)


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

toPaymentView::KontrakcjaTemplates -> Payment -> IO PaymentView
toPaymentView templates payment = do 
                                   ps <- sequence $ map (positionInfo templates) (positions payment)
                                   return $ PaymentView
                                             {
                                              pvId = show $ paymentId payment, 
                                              pvPositions = ps,
                                              pvIsSend =  Send == paymentState payment ,
                                              pvIsWaiting=  Waiting == paymentState payment ,  
                                              pvIsFinished=  Finished == paymentState payment ,  
                                              pvIsDropped= False ,  
                                              pvIsInvoiceable = False ,  
                                              pvIsFailed=  isFailed $ payment ,   
                                              pvValue= show $ paymentValue payment,
                                              pvLink= show $ LinkPayExView $ Just $ paymentId payment,
                                              pvPayExUrl = redirectUrl payment
                                             }
viewPayment::KontrakcjaTemplates -> Payment -> IO String
viewPayment templates payment = do
                                 pm <- toPaymentView templates payment
                                 renderTemplate templates "paymentView" (setAttribute "payment" pm)

viewPayments::KontrakcjaTemplates -> [Payment] -> IO String
viewPayments templates payments = do
                                   pms <- sequence $ map (toPaymentView templates) payments
                                   renderTemplate templates "paymentsView" (setAttribute "payments" pms ) 


mailNewPayment::Context -> User -> Payment -> IO Mail
mailNewPayment ctx user payment = do
                                        pm <- toPaymentView (ctxtemplates ctx) payment 
                                        title <- renderTemplate (ctxtemplates ctx) "mailNewPaymentTitle" () 
                                        content <- renderTemplate (ctxtemplates ctx) "mailNewPaymentContent" $ 
                                                                    (setAttribute "payment" pm ) .
                                                                    (setAttribute "ctxhostpart" $ ctxhostpart ctx)
                                        return $ emptyMail {title = BS.fromString title, content = BS.fromString content}


