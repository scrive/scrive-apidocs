module Administration.AddPaymentPlan(
            addCompanyPlanManual
) where
import Data.Functor

import Company.Model
import DB
import IPAddress ()
import Kontra
import KontraPrelude
import Payments.Model
import qualified Payments.Stats

addCompanyPlanManual :: Kontrakcja m => CompanyID -> PricePlan -> PaymentPlanStatus -> m ()
addCompanyPlanManual companyid pp status = do
  case pp of
    FreePricePlan -> do
      mpaymentplan <- dbQuery $ GetPaymentPlan companyid
      case mpaymentplan of
        Nothing -> return ()
        Just PaymentPlan {ppPaymentPlanProvider = NoProvider} -> do
          _ <- dbUpdate $ DeletePaymentPlan companyid
          return ()
        Just _ -> return ()
    plan -> do
      mpaymentplan <- dbQuery $ GetPaymentPlan companyid
      quantity <- dbQuery $ GetCompanyQuantity companyid
      time <- ctxtime <$> getContext
      case mpaymentplan of
        Nothing -> do
          ac <- dbUpdate $ GetAccountCode
          let paymentplan = PaymentPlan { ppAccountCode         = ac
                                        , ppCompanyID           = companyid
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
          _ <- Payments.Stats.record time Payments.Stats.SignupAction NoProvider quantity plan companyid ac
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
          _ <- Payments.Stats.record time Payments.Stats.ChangeAction NoProvider quantity plan companyid (ppAccountCode paymentplan')
          return ()
        Just _ -> do -- must be a Recurly payment plan; maybe flash message?
          return ()

