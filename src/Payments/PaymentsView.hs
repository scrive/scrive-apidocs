{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TupleSections, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Payments.PaymentsView
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
-- Payment admin view - for standard user (not editable) and superuser 
--
-----------------------------------------------------------------------------
module Payments.PaymentsView(adminView,adminViewForSuperuser) where

import KontraLink
import Templates
import Payments.PaymentsState
import Data.Typeable
import Data.Data
import Text.StringTemplate.GenericStandard()

{- | Payments models view for normal user. -}
adminView::KontrakcjaTemplates-> [PaymentAccountModel] -> IO String                                      
adminView templates models = renderTemplateComplex templates "paymentsadminpage" $  
                                         (setAttribute "models" $ map getModelView models) 
{-| Payments models view for superadmin.
   Allows him to change values.
 -}                                         
adminViewForSuperuser::KontrakcjaTemplates-> [PaymentAccountModel] -> IO String          
adminViewForSuperuser templates models = renderTemplateComplex templates "paymentsadminpagesuperuser" $  
                                         (setAttribute "models" $ map getModelView models) .
                                         (setAttribute "changeaction" $ show LinkPaymentsAdmin) 
{- | Datastructure to pack model. It can be easyly handle by templates -}                                                     
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

{-Conversion from data storage structure to template friendly structure-}
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

{-Money printer. Since show instance is derived by storage module, we provided nicer version -}
showMoney::Money->String  
showMoney (Money i) = (show $ i `div` 100)++"."++(show $ (i `div` 10) `mod` 10) ++ (show $ i `mod` 10)
             