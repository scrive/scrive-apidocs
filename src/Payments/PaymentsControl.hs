{-# LANGUAGE CPP, OverloadedStrings, TupleSections#-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Payments.PaymentsControl
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
-- Payment admin view with edit handlers, with permissions checker 
--
-----------------------------------------------------------------------------
module Payments.PaymentsControl(handleAdminView,handleAccountModelsChange) where
import "mtl" Control.Monad.State
import AppView
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update)
import KontraLink
import Misc
import User
import HSP (cdata)
import Payments.PaymentsState
import Payments.PaymentsView

{- | Admin view of payments accounts -}
handleAdminView::Kontra Response
handleAdminView = do
                    ctx<- get
                    models <- update $ GetPaymentModels
                    case (ctxmaybeuser ctx) of 
                      Nothing -> sendRedirect LinkLogin
                      _ ->
                            do
                             content <- if (isSuperUser $ ctxmaybeuser ctx)
                                         then liftIO $ adminViewForSuperuser (ctxtemplates ctx) models
                                         else liftIO $ adminView (ctxtemplates ctx) models
                             renderFromBody ctx TopEmpty kontrakcja $ cdata content
                                    
{- | Handle change of models values request.
     Supports full and partial upgrade.
     Fields names like in PaymentModelView (see PaymentsView) with AccountType suffix.
 -} 
handleAccountModelsChange::Kontra KontraLink
handleAccountModelsChange= do
                            ctx<- get
                            if (isSuperUser $ ctxmaybeuser ctx)
                             then do   
                                  mapM_ getAndApplyAccountModelChange accountTypes 
                                  return $ LinkPaymentsAdmin 
                             else do
                                  return $ LinkPaymentsAdmin 
                                  
getAndApplyAccountModelChange :: AccountType -> Kontra ()
getAndApplyAccountModelChange accountType = do
                                    f <- getAccountModelChange  accountType
                                    model <- update $ GetPaymentModel accountType
                                    update $ UpdateAccountModel accountType (f model)
                                    
-- | For selected account type we read request params and retur a function for updating a structure
getAccountModelChange::AccountType->Kontra (PaymentAccountModel -> PaymentAccountModel)    
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
                             maybe' a ma = maybe a id ma
                           

{-| Utils for reading money fields -}
readMoneyField::String -> Kontra (Maybe Money)
readMoneyField name =  fmap (join . (fmap readMoney)) $ getDataFn' (look name)                                    

readMoney::String->Maybe Money  
readMoney s = do
               let (m,r) = break (== '.') s 
               main<-maybeRead m
               rest<-(maybeRead $ drop 1 r) `mplus` (return 0)
               return $ Money (main * 100 + rest)


---I belive that this should be in prelude!!
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
