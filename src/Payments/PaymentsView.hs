{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Payments.PaymentsView
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
-- Payment admin view - editable and not . 
-- Main templates are in payments.st . 
-- 'PaymentChangeView' export data structure for payments change in templates (for admin backend)
-----------------------------------------------------------------------------
module Payments.PaymentsView(adminView,adminViewForSuperuser,getModelView,getChangeView, PaymentChangeView) where

import KontraLink
import Templates.Templates 
import Payments.PaymentsState
import Payments.PaymentsUtils
import Data.Typeable
import Data.Data
import Text.StringTemplate.GenericStandard()

{- | Payments models view. Not-editable -}
adminView::KontrakcjaTemplates-> [PaymentAccountModel] -> IO String                                      
adminView templates models = renderTemplate templates "paymentsadminpage" $ do
                                 field "models" $ map getModelView models
{-| Payments models view . Editable -}                                         
adminViewForSuperuser::KontrakcjaTemplates-> [PaymentAccountModel] -> IO String          
adminViewForSuperuser templates models = renderTemplate templates "paymentsadminpagesuperuser" $ do
                                         field "models" $ map getModelView models
                                         field "changeaction" $ show LinkPaymentsAdmin
                                         
{- | Nice view for 'PaymentAccountModel'. It can be easyly handled by templates -}                                                     
data PaymentAccountModelView  = PaymentAccountModelView {
                                      modelname::String,
                                      modelfieldsprefix::String,  
                                      foraccount::String,
                                      forsubaccount::String,   
                                      foremailsignature::String,
                                      forelegsignature::String,  
                                      formobiledignature::String,
                                      forcrediteardsignature::String,
                                      foripadsignature ::String,
                                      foramazon::String,
                                      fortrustweaver::String,
                                      fortemplate::String,
                                      fordraft::String   
} deriving (Data, Typeable)
{- | Nice view for 'PaymentChange'. It can be easyly handled by templates.
     To be used by user admin view -}              
data PaymentChangeView  = PaymentChangeView {
                                      changeforaccount::Maybe String,
                                      changeforsubaccount::Maybe String,   
                                      changeforemailsignature::Maybe String,
                                      changeforelegsignature::Maybe String,  
                                      changeformobiledignature::Maybe String,
                                      changeforcrediteardsignature::Maybe String,
                                      changeforipadsignature::Maybe String,
                                      changeforamazon::Maybe String,
                                      changefortrustweaver::Maybe String,
                                      changefortemplate::Maybe String,
                                      changefordraft::Maybe String   
} deriving (Data, Typeable)

{-Conversion 'PaymentAccountModel' to 'PaymentAccountModelView' -}
getModelView::PaymentAccountModel -> PaymentAccountModelView
getModelView  (PaymentAccountModel {modelAccountType,
                                    modelPaymentForAccounts,
                                    modelPaymentForSignature,                          
                                    modelPaymentForSignedStorage,       
                                    modelPaymentForOtherStorage                                           
                                               }) = PaymentAccountModelView {
                                                      modelname=show modelAccountType ,
                                                      modelfieldsprefix =show modelAccountType,
                                                      foraccount = showMoney $ forAccount modelPaymentForAccounts,
                                                      forsubaccount = showMoney $ forSubaccount modelPaymentForAccounts,     
                                                      foremailsignature=showMoney $ forEmailSignature modelPaymentForSignature,
                                                      forelegsignature= showMoney $ forElegSignature modelPaymentForSignature,
                                                      formobiledignature= showMoney $ forMobileSignature modelPaymentForSignature,
                                                      forcrediteardsignature= showMoney $ forCreditCardSignature modelPaymentForSignature,
                                                      foripadsignature = showMoney $ forIPadSignature modelPaymentForSignature,
                                                      foramazon= showMoney $ forAmazon modelPaymentForSignedStorage,
                                                      fortrustweaver=  showMoney $ forTrustWeaver modelPaymentForSignedStorage,
                                                      fortemplate=  showMoney $ forTemplate   modelPaymentForOtherStorage ,
                                                      fordraft= showMoney $ forDraft  modelPaymentForOtherStorage }
                                                      

{-Conversion 'PaymentChange' to 'PaymentChangeView' -}
getChangeView::PaymentChange -> PaymentChangeView
getChangeView  (PaymentChange  {changePaymentForAccounts,
                                        changePaymentForSignature,                          
                                        changePaymentForSignedStorage,       
                                        changePaymentForOtherStorage                                           
                                               }) =  PaymentChangeView {
                                                      changeforaccount = fmap showMoney $ forAccount changePaymentForAccounts,
                                                      changeforsubaccount = fmap showMoney $ forSubaccount changePaymentForAccounts,     
                                                      changeforemailsignature=fmap showMoney $ forEmailSignature changePaymentForSignature,
                                                      changeforelegsignature= fmap showMoney $ forElegSignature changePaymentForSignature,
                                                      changeformobiledignature= fmap showMoney $ forMobileSignature changePaymentForSignature,
                                                      changeforcrediteardsignature= fmap showMoney $ forCreditCardSignature changePaymentForSignature,
                                                      changeforipadsignature = fmap showMoney $ forIPadSignature changePaymentForSignature,
                                                      changeforamazon= fmap showMoney $ forAmazon changePaymentForSignedStorage,
                                                      changefortrustweaver= fmap showMoney $ forTrustWeaver changePaymentForSignedStorage,
                                                      changefortemplate= fmap showMoney $ forTemplate   changePaymentForOtherStorage ,
                                                      changefordraft= fmap showMoney $ forDraft  changePaymentForOtherStorage }

             