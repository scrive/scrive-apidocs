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
import Control.Monad.State
import Data.Maybe
import AppView
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update, query)
import KontraLink
import Misc
import Kontra
import Payments.PaymentsState
import Payments.PaymentsView
import Payments.PaymentsUtils

{- | View of payment models (not editable) -}
handlePaymentsModelForViewView :: Kontrakcja m => m Response
handlePaymentsModelForViewView = onlySuperUser $ do
    models <- query $ GetPaymentModels
    content <- adminView models
    renderFromBody TopEmpty kontrakcja content

{- | View of payment models (editable) -}
handlePaymentsModelForEditView :: Kontrakcja m => m Response
handlePaymentsModelForEditView =  onlySuperUser $ do
    models <- query $ GetPaymentModels
    content <- adminViewForSuperuser models
    renderFromBody TopEmpty kontrakcja content

{- | Handle change of models values request.
     Supports full and partial upgrade.
     Fields names like in PaymentModelView (see PaymentsView) with PaymentAccountType suffix.
 -}
handleAccountModelsChange :: Kontrakcja m => m KontraLink
handleAccountModelsChange= do
                            ctx<- getContext
                            if isSuperUser (ctxadminaccounts ctx) (ctxmaybeuser ctx)
                             then do
                                  mapM_ getAndApplyAccountModelChange (allValues::[PaymentAccountType])
                                  return $ LinkPaymentsAdmin
                             else do
                                  return $ LinkPaymentsAdmin

getAndApplyAccountModelChange :: Kontrakcja m => PaymentAccountType -> m ()
getAndApplyAccountModelChange accountType = do
                                    f <- getAccountModelChange  accountType
                                    model <- query $ GetPaymentModel accountType
                                    update $ UpdateAccountModel accountType (f model)

-- | For selected account type we read request params and retur a function for updating a structure
getAccountModelChange :: Kontrakcja m => PaymentAccountType -> m (PaymentAccountModel -> PaymentAccountModel)
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
                                                                                  forAccount=fromMaybe forAccount mforaccount,
                                                                                  forSubaccount=fromMaybe forSubaccount mforsubaccount
                                                                          },
                                               modelPaymentForSignature = PaymentForSignature {
                                                                                  forEmailSignature=fromMaybe forEmailSignature mforemailsignature,
                                                                                  forElegSignature=fromMaybe forElegSignature mforelegsignature,
                                                                                  forMobileSignature= fromMaybe forMobileSignature  mformobiledignature,
                                                                                  forCreditCardSignature= fromMaybe forCreditCardSignature mforcrediteardsignature,
                                                                                  forIPadSignature=fromMaybe forIPadSignature mforipadsignature
                                                                          },
                                               modelPaymentForSignedStorage = PaymentForSignedStorage {
                                                                                  forAmazon=fromMaybe forAmazon mforamazon,
                                                                                  forTrustWeaver=fromMaybe forTrustWeaver mfortrustweaver
                                                                          },
                                               modelPaymentForOtherStorage = PaymentForOtherStorage {
                                                                                  forTemplate=fromMaybe forTemplate mfortemplate,
                                                                                  forDraft=fromMaybe forDraft mfordraft
                                                                          }
                                               }
                                )
                          where
                             withAccountType s = s ++ (show accountType)


{- | We read paymentchange from Kontra Params. It takes field suffix so we can use it for both, custom and temporary changes -}
getPaymentChangeChange :: Kontrakcja m => String -> m (PaymentChange -> PaymentChange)
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
readMoneyField :: Kontrakcja m => String -> m (Maybe Money)
readMoneyField name =  fmap (join . (fmap readMoney)) $ getDataFn' (look name)
