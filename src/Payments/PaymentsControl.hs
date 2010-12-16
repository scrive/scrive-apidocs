{-# LANGUAGE CPP, OverloadedStrings, TupleSections#-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Payments.PaymentsControl
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
-- Payment admin view with edit handlers, with permissions checker. 
-- 'getPaymentChangeChange' is used to parse user payments change in admin backend.
-----------------------------------------------------------------------------
module Payments.PaymentsControl(handlePaymentsModelForViewView, handlePaymentsModelForEditView ,handleAccountModelsChange,readMoneyField,getPaymentChangeChange) where
import "mtl" Control.Monad.State
import AppView
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update)
import KontraLink
import Misc
import User
import HSP (cdata)
import Payments.PaymentsState
import Payments.PaymentsView
import Payments.PaymentsUtils

{- | View of payment models (not editable) -}
handlePaymentsModelForViewView::Kontra Response
handlePaymentsModelForViewView = onlySuperUser $
                                 do
                                  ctx<- get
                                  models <- update $ GetPaymentModels
                                  content <- liftIO $ adminView (ctxtemplates ctx) models 
                                  renderFromBody ctx TopEmpty kontrakcja $ cdata content
                                  
{- | View of payment models (editable) -}
handlePaymentsModelForEditView ::Kontra Response
handlePaymentsModelForEditView =  onlySuperUser $
                                  do
                                   ctx<- get
                                   models <- update $ GetPaymentModels
                                   content <- liftIO $ adminViewForSuperuser (ctxtemplates ctx) models
                                   renderFromBody ctx TopEmpty kontrakcja $ cdata content                                  
{- | Handle change of models values request.
     Supports full and partial upgrade.
     Fields names like in PaymentModelView (see PaymentsView) with PaymentAccountType suffix.
 -} 
handleAccountModelsChange::Kontra KontraLink
handleAccountModelsChange= do
                            ctx<- get
                            if (isSuperUser $ ctxmaybeuser ctx)
                             then do   
                                  mapM_ getAndApplyAccountModelChange (allValues::[PaymentAccountType])
                                  return $ LinkPaymentsAdmin 
                             else do
                                  return $ LinkPaymentsAdmin 
                                  
getAndApplyAccountModelChange :: PaymentAccountType -> Kontra ()
getAndApplyAccountModelChange accountType = do
                                    f <- getAccountModelChange  accountType
                                    model <- update $ GetPaymentModel accountType
                                    update $ UpdateAccountModel accountType (f model)
                                    
-- | For selected account type we read request params and retur a function for updating a structure
getAccountModelChange::PaymentAccountType->Kontra (PaymentAccountModel -> PaymentAccountModel)    
getAccountModelChange accountType = 
                           do
                            mforaccount <- readMoneyField $ withAccountType "foraccount" 
                            mforsubaccount <- readMoneyField $ withAccountType "forsubaccount"
                            mforemailsignature <- readMoneyField $ withAccountType "foremailsignature"
                            mforelegsignature <- readMoneyField $ withAccountType "forelegsignature"
                            mformobiledignature <- readMoneyField $ withAccountType "formobiledignature"
                            mforcrediteardsignature <- readMoneyField $ withAccountType "forcrediteardsignature"
                            mforipadsignature <- readMoneyField $ withAccountType "foripadsignature"
                            mforamazon <- readMoneyField $ withAccountType "foramazon"
                            mfortrustweaver <- readMoneyField $ withAccountType "fortrustweaver"
                            mfortemplate <- readMoneyField $ withAccountType "fortemplate"
                            mfordraft <- readMoneyField $ withAccountType "fordraft"
                            return (\PaymentAccountModel {modelAccountType = _,
                                               modelPaymentForAccounts = PaymentForAccounts {
                                                                                  forAccount,
                                                                                  forSubaccount
                                                                          },
                                               modelPaymentForSignature = PaymentForSignature {
                                                                                  forEmailSignature,
                                                                                  forElegSignature,
                                                                                  forMobileSignature,
                                                                                  forCreditCardSignature,
                                                                                  forIPadSignature
                                                                          },                         
                                               modelPaymentForSignedStorage = PaymentForSignedStorage {
                                                                                  forAmazon,
                                                                                  forTrustWeaver
                                                                          },       
                                               modelPaymentForOtherStorage = PaymentForOtherStorage {
                                                                                  forTemplate,
                                                                                  forDraft
                                                                          }                                            
                                               } ->  PaymentAccountModel {modelAccountType = accountType,
                                               modelPaymentForAccounts = PaymentForAccounts {
                                                                                  forAccount=maybe' forAccount mforaccount,
                                                                                  forSubaccount=maybe' forSubaccount mforsubaccount
                                                                          },
                                               modelPaymentForSignature = PaymentForSignature {
                                                                                  forEmailSignature=maybe' forEmailSignature mforemailsignature,
                                                                                  forElegSignature=maybe' forElegSignature mforelegsignature,
                                                                                  forMobileSignature= maybe' forMobileSignature  mformobiledignature,
                                                                                  forCreditCardSignature= maybe' forCreditCardSignature mforcrediteardsignature,
                                                                                  forIPadSignature=maybe' forIPadSignature mforipadsignature
                                                                          },                         
                                               modelPaymentForSignedStorage = PaymentForSignedStorage {
                                                                                  forAmazon=maybe' forAmazon mforamazon,
                                                                                  forTrustWeaver=maybe' forTrustWeaver mfortrustweaver
                                                                          },       
                                               modelPaymentForOtherStorage = PaymentForOtherStorage {
                                                                                  forTemplate=maybe' forTemplate mfortemplate,
                                                                                  forDraft=maybe' forDraft mfordraft
                                                                          }                                            
                                               }
                                )    
                          where 
                             withAccountType s = s ++ (show accountType)


{- | We read paymentchange from Kontra Params. It takes field suffix so we can use it for both, custom and temporary changes -}
getPaymentChangeChange::String -> Kontra (PaymentChange -> PaymentChange)    
getPaymentChangeChange fieldSuffix = 
                           do
                            mforaccount <- readMoneyField $ withSuffix "foraccount" 
                            mforsubaccount <- readMoneyField $ withSuffix "forsubaccount"
                            mforemailsignature <- readMoneyField $  withSuffix "foremailsignature"
                            mforelegsignature <- readMoneyField $  withSuffix "forelegsignature"
                            mformobiledignature <- readMoneyField $  withSuffix "formobiledignature"
                            mforcrediteardsignature <- readMoneyField $  withSuffix "forcrediteardsignature"
                            mforipadsignature <- readMoneyField $  withSuffix "foripadsignature"
                            mforamazon <- readMoneyField $  withSuffix "foramazon"
                            mfortrustweaver <- readMoneyField $  withSuffix "fortrustweaver"
                            mfortemplate <- readMoneyField $  withSuffix "fortemplate"
                            mfordraft <- readMoneyField $  withSuffix "fordraft"
                            return (\_ -> PaymentChange {
                                               changePaymentForAccounts = PaymentForAccounts {
                                                                                  forAccount= mforaccount,
                                                                                  forSubaccount= mforsubaccount
                                                                          },
                                               changePaymentForSignature = PaymentForSignature {
                                                                                  forEmailSignature=mforemailsignature,
                                                                                  forElegSignature= mforelegsignature,
                                                                                  forMobileSignature=  mformobiledignature,
                                                                                  forCreditCardSignature= mforcrediteardsignature,
                                                                                  forIPadSignature= mforipadsignature
                                                                          },                         
                                               changePaymentForSignedStorage = PaymentForSignedStorage {
                                                                                  forAmazon= mforamazon,
                                                                                  forTrustWeaver=mfortrustweaver
                                                                          },       
                                               changePaymentForOtherStorage = PaymentForOtherStorage {
                                                                                  forTemplate= mfortemplate,
                                                                                  forDraft= mfordraft
                                                                          }                                            
                                               }
                                )    
                          where 
                             withSuffix s = s ++  fieldSuffix
                             
{-| Utils for reading money fields -}
readMoneyField::String -> Kontra (Maybe Money)
readMoneyField name =  fmap (join . (fmap readMoney)) $ getDataFn' (look name)                                    
