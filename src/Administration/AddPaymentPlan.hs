module Administration.AddPaymentPlan(
            addManualPricePlan
          , addCompanyPlanManual
) where
import Data.Functor
import IPAddress ()
import Kontra
import Company.Model
import DB
import User.Model
import Payments.Model
import qualified Payments.Stats



addCompanyPlanManual :: Kontrakcja m => CompanyID -> PricePlan -> PaymentPlanStatus -> m ()
addCompanyPlanManual companyid pp status = do
  case pp of
    FreePricePlan -> do
      mpaymentplan <- dbQuery $ GetPaymentPlan (Right companyid)
      case mpaymentplan of
        Nothing -> return ()
        Just PaymentPlan {ppPaymentPlanProvider = NoProvider} -> do
          _ <- dbUpdate $ DeletePaymentPlan (Right companyid)
          return ()
        Just _ -> return ()
    plan -> do
      mpaymentplan <- dbQuery $ GetPaymentPlan (Right companyid)
      quantity <- dbQuery $ GetCompanyQuantity companyid
      time <- ctxtime <$> getContext
      case mpaymentplan of
        Nothing -> do
          ac <- dbUpdate $ GetAccountCode
          let paymentplan = PaymentPlan { ppAccountCode         = ac
                                        , ppID                  = Right companyid
                                        , ppPricePlan           = plan
                                        , ppPendingPricePlan    = plan
                                        , ppStatus              = status
                                        , ppPendingStatus       = status
                                        , ppQuantity            = quantity
                                        , ppPendingQuantity     = quantity
                                        , ppPaymentPlanProvider = NoProvider
                                        , ppDunningStep         = Nothing
                                        , ppDunningDate         = Nothing
                                        , ppBillingEndDate      = time
                                        }
          _ <- dbUpdate $ SavePaymentPlan paymentplan time
          _ <- Payments.Stats.record time Payments.Stats.SignupAction NoProvider quantity plan (Right companyid) ac
          return ()
        Just paymentplan | ppPaymentPlanProvider paymentplan == NoProvider -> do
          let paymentplan' = paymentplan { ppPricePlan        = plan
                                         , ppPendingPricePlan = plan
                                         , ppStatus           = status
                                         , ppPendingStatus    = status
                                         , ppQuantity         = quantity
                                         , ppPendingQuantity  = quantity
                                         }
          _ <- dbUpdate $ SavePaymentPlan paymentplan' time
          _ <- Payments.Stats.record time Payments.Stats.ChangeAction NoProvider quantity plan (Right companyid) (ppAccountCode paymentplan')
          return ()
        Just _ -> do -- must be a Recurly payment plan; maybe flash message?
          return ()

 where


addManualPricePlan :: Kontrakcja m => UserID -> PricePlan -> PaymentPlanStatus -> m ()
addManualPricePlan uid priceplan status =
  case priceplan of
    FreePricePlan -> do
      mpaymentplan <- dbQuery $ GetPaymentPlan $ Left uid
      case mpaymentplan of
        Just PaymentPlan{ppPaymentPlanProvider = NoProvider} -> do
          _ <- dbUpdate $ DeletePaymentPlan $ Left uid
          return ()
        _ -> do
          return ()
    plan -> do
      mpaymentplan <- dbQuery $ GetPaymentPlan $ Left uid
      let quantity = 1
      time <- ctxtime <$> getContext
      case mpaymentplan of
        Nothing -> do
          ac <- dbUpdate $ GetAccountCode
          let paymentplan = PaymentPlan { ppAccountCode         = ac
                                        , ppID                  = Left uid
                                        , ppPricePlan           = plan
                                        , ppPendingPricePlan    = plan
                                        , ppStatus              = status
                                        , ppPendingStatus       = status
                                        , ppQuantity            = quantity
                                        , ppPendingQuantity     = quantity
                                        , ppPaymentPlanProvider = NoProvider
                                        , ppDunningStep         = Nothing
                                        , ppDunningDate         = Nothing
                                        , ppBillingEndDate      = time
                                        }
          _ <- dbUpdate $ SavePaymentPlan paymentplan time
          _ <- Payments.Stats.record time Payments.Stats.SignupAction NoProvider quantity plan (Left uid) ac
          return ()
        Just paymentplan | ppPaymentPlanProvider paymentplan == NoProvider -> do
          let paymentplan' = paymentplan { ppPricePlan        = plan
                                         , ppPendingPricePlan = plan
                                         , ppStatus           = status
                                         , ppPendingStatus    = status
                                         , ppQuantity         = quantity
                                         , ppPendingQuantity  = quantity
                                         }
          _ <- dbUpdate $ SavePaymentPlan paymentplan' time
          _ <- Payments.Stats.record time Payments.Stats.ChangeAction NoProvider quantity plan (Left uid) (ppAccountCode paymentplan')
          return ()
        Just _ -> do -- must be a Recurly payment plan; maybe flash message?
            return ()
 
