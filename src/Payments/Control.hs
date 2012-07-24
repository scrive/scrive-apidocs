{-# OPTIONS_GHC -fno-warn-orphans #-}
module Payments.Control where

import Control.Monad.State
--import Data.Convertible
import Data.Functor
--import Data.List
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)

--import MinutesTime
--import Templates.Templates
import AppView
import Company.Model
import DB hiding (update, query)
import Kontra
import KontraLink
import Misc
import Recurly
import Recurly.JS
import Text.JSON
import Text.JSON.Gen hiding (value)
import User.Model
import User.Utils
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils
import qualified Log
import qualified Text.JSON.Gen as J
import ListUtil

import Payments.Model
import Payments.Rules
import Payments.View
import Payments.Config (RecurlyConfig(..))

-- bootstrap the payments dashboard
handleSubscriptionDashboard :: Kontrakcja m => m (Either KontraLink Response)
handleSubscriptionDashboard = checkUserTOSGet $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  showSubscriptionDashboard user >>= renderFromBody kontrakcja
  
handleSubscriptionDashboardInfo :: Kontrakcja m => m JSValue
handleSubscriptionDashboardInfo = do
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext  
  user <- guardJustM $ ctxmaybeuser <$> getContext
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany user
  quantity <- (maybe (return 1) (dbQuery . GetCompanyQuantity) $ usercompany user)
  mplan <- dbQuery $ GetPaymentPlan (maybe (Left (userid user)) Right (usercompany user))
  --let currency = "SEK" -- we only support SEK for now
  plan <- case mplan of
    Nothing -> do
          freesig    <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]",    "free")]          
          basicsig    <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]",    "basic")]
          brandingsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "branding")]
          advancedsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "advanced")]
          code <- dbUpdate GetAccountCode
          return $ J.object "signup" $ do
            J.value "code" $ show code
            J.value "currency" "SEK"
            J.value "quantity" quantity
            J.object "signatures" $ do
              J.value "free"     freesig
              J.value "basic"    basicsig
              J.value "branding" brandingsig
              J.value "advanced" advancedsig
    Just plan -> do
          billingsig  <- liftIO $ genSignature recurlyPrivateKey [("account[account_code]", show $ ppAccountCode plan)]
          -- we should be syncing somewhere else
          esub <- liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show $ ppAccountCode plan
          einvoices <- liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey (show $ ppAccountCode plan)
          msub <- case esub of
            Left e -> do
              Log.debug $ "When fetching subscriptions for payment plan: " ++ e
              return Nothing
            Right (sub:_) -> return $ Just sub
            Right _ -> do
              Log.debug $ "No subscriptions returned."
              return Nothing
          minvoices <- case einvoices of
            Left e -> do
              Log.debug $ "When fetching invoices for payment plan: " ++ e
              return Nothing
            Right i -> return $ Just i
          return $ J.object "plan" $ do
            J.value "subscription" $ msub
            J.value "invoices" $ minvoices
            J.value "code"   $ show $ ppAccountCode plan
            J.value "plan"   $ show $ ppPricePlan plan
            J.value "status" $ show $ ppStatus plan
            J.object "signatures" $ do
              J.value "billing"  billingsig          
            case plan of
              UserPaymentPlan    {} -> J.value "userid"    $ show $ ppUserID plan
              CompanyPaymentPlan {} -> J.value "companyid" $ show $ ppCompanyID plan
  runJSONGenT $ do
    J.object "contact" $ do
      J.value "first_name"   $ getFirstName user
      J.value "last_name"    $ getLastName user
      J.value "email"        $ getEmail user
      J.value "company_name" $ getCompanyName (user, mcompany)
      J.value "country"      $ "SE" -- only one supported for now? Not important
    J.object "server" $ do
      J.value "subdomain"    $ recurlySubdomain
    plan
    
handleGetInvoices :: Kontrakcja m => m JSValue
handleGetInvoices = do
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext  
  params <- getListParamsNew
  user <- guardJustM $ ctxmaybeuser <$> getContext
  plan <- guardRightM' $ toEither "No plan for logged in user." <$> (dbQuery $ GetPaymentPlan (maybe (Left (userid user)) Right (usercompany user)))
  invoices <- guardRightM' $ liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey (show $ ppAccountCode plan)
  runJSONGenT $ do
    J.value "list" $ for invoices $ runJSONGen . J.value "fields"
    J.value "paging" $ pagingParamsJSON $ PagedList { list     = invoices
                                                    , params   = params
                                                    , pageSize = 12
                                                    }

-- to call this, user must not have an account code yet (no payment plan in table)
handleSyncNewSubscriptionWithRecurly :: Kontrakcja m => m ()
handleSyncNewSubscriptionWithRecurly = do
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  ac <- guardJustM $ readField "account_code"
  user <- guardJustM $ ctxmaybeuser <$> getContext
  subscriptions <- guardRightM' $ liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show ac
  subscription <- guardRight' $ toEither "No subscription." $ listToMaybe subscriptions
  let eid = maybe (Left $ userid user) Right $ usercompany user
  cachePlan ac subscription eid

handleChangePlan :: Kontrakcja m => m ()
handleChangePlan = do
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  newplan :: PricePlan <- guardJustM $ readField "plan"
  Log.debug $ show newplan
  user <- guardJustM $ ctxmaybeuser <$> getContext
  plan <- guardRightM' $ toEither "No plan for logged in user." <$> (dbQuery $ GetPaymentPlan (maybe (Left (userid user)) Right (usercompany user)))
  subscriptions <- guardRightM' $ liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show $ ppAccountCode plan
  subscription <- guardRight' $ toEither "No subscriptions for Recurly account." $ listToMaybe subscriptions
  quantity <- maybe (return 1) (dbQuery . GetCompanyQuantity) $ usercompany user
  let eid = maybe (Left $ userid user) Right $ usercompany user
      ac  = ppAccountCode plan
  subinfo <- guardRight' $ subInfo subscription
  Log.debug $ show subinfo
  case syncAction (quantity, newplan) subinfo of
    RNoAction -> do
      Log.debug "no action"
      return ()
    RUpdateNow -> do
      Log.debug "renewing now"
      s <- guardRightM' $ liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity True
      cachePlan ac s eid
    RUpdateRenewal -> do
      Log.debug $ "renewing at end of term"
      s <- guardRightM' $ liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity False
      cachePlan ac s eid
    RReactivateNow -> do
      Log.debug $ "renewing now"
      _ <- liftIO $ reactivateSubscription curl_exe recurlyAPIKey (subID subscription)
      s <- guardRightM' $ liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity True
      cachePlan ac s eid
    RReactivateRenewal -> do
      Log.debug $ "renewing later"
      _ <- liftIO $ reactivateSubscription curl_exe recurlyAPIKey (subID subscription)
      s <- guardRightM' $ liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity False
      cachePlan ac s eid
    RCancel -> do
      Log.debug "canceling"
      _ <- guardRightM' $ liftIO $ cancelSubscription curl_exe recurlyAPIKey $ subID subscription
      cachePlan ac subscription { subState = "canceled" } eid

cachePlan :: Kontrakcja m => AccountCode -> Subscription -> Either UserID CompanyID -> m ()
cachePlan ac subscription eid = do
  time <- ctxtime <$> getContext
  let p       = fromRecurlyPricePlan $ subPricePlan subscription
      (s, sp) = fromRecurlyStatus    $ subState     subscription
      q       = subQuantity subscription
      pp      = maybe p (fromRecurlyPricePlan . penPricePlan) $ subPending subscription
      qp      = maybe q penQuantity                           $ subPending subscription
  let paymentplan = case eid of
        Left  uid -> UserPaymentPlan { ppAccountCode      = ac
                                     , ppUserID           = uid
                                     , ppPricePlan        = p
                                     , ppPendingPricePlan = pp
                                     , ppStatus           = s
                                     , ppPendingStatus    = sp
                                     , ppQuantity         = q
                                     , ppPendingQuantity  = qp }
        Right cid -> CompanyPaymentPlan { ppAccountCode      = ac
                                        , ppCompanyID        = cid
                                        , ppPricePlan        = p
                                        , ppPendingPricePlan = pp
                                        , ppStatus           = s
                                        , ppPendingStatus    = sp
                                        , ppQuantity         = q
                                        , ppPendingQuantity  = qp }
  r <- dbUpdate $ SavePaymentPlan paymentplan time
  when (not r) $ do
    Log.debug "Could not save payment plan."
    internalError
  return ()

fromRecurlyStatus :: String -> (PaymentPlanStatus, PaymentPlanStatus)
fromRecurlyStatus "active"   = (ActiveStatus, ActiveStatus)
fromRecurlyStatus "canceled" = (ActiveStatus, CanceledStatus)
fromRecurlyStatus "expired"  = (CanceledStatus, CanceledStatus)
fromRecurlyStatus "future"   = (DeactivatedStatus, ActiveStatus)
fromRecurlyStatus _          = (ActiveStatus, ActiveStatus)

fromRecurlyPricePlan :: String -> PricePlan
fromRecurlyPricePlan "free"     = FreePricePlan
fromRecurlyPricePlan "basic"    = BasicPricePlan
fromRecurlyPricePlan "branded"  = BrandingPricePlan
fromRecurlyPricePlan "advanced" = AdvancedPricePlan
fromRecurlyPricePlan _          = AdvancedPricePlan
  
instance ToJSValue Subscription where
  toJSValue (Subscription{..}) = runJSONGen $ do
    case subState of
      "expired" -> do
        J.value "plan_name" "Free"
        J.value "plan_code" "free"
        J.value "unit_amount_in_cents" (0 :: Int)
      _ -> do
        J.value "plan_name" subName
        J.value "plan_code" subPricePlan
        J.value "unit_amount_in_cents" subUnitAmountInCents
    J.value "quantity"  subQuantity
    J.value "currency"  subCurrency
    J.value "activated" subActivateDate
    J.value "cancelled" subCancelledDate
    J.value "billing_started" subCurrentBillingStarted
    J.value "billing_ends" subCurrentBillingEnds
    case subState of
      "canceled" -> J.value "pending" $ PendingSubscription { penName = "Free"
                                                            , penPricePlan = "free"
                                                            , penUnitAmountInCents = 0
                                                            , penQuantity = subQuantity }
      _ -> J.value "pending" $ subPending
    
instance ToJSValue PendingSubscription where
  toJSValue (PendingSubscription{..}) = runJSONGen $ do
    J.value "plan_name" penName
    J.value "plan_code" penPricePlan
    J.value "unit_amount_in_cents" penUnitAmountInCents
    J.value "quantity" penQuantity

instance ToJSValue Invoice where
  toJSValue (Invoice{..}) = runJSONGen $ do
    J.value "invoice_number" inNumber
    J.value "total_in_cents" inTotalInCents
    J.value "currency"       inCurrency
    J.value "state"          inState
    J.value "account_code"   inAccount
    J.value "date"           inDate
    
