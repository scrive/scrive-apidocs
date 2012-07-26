{-# OPTIONS_GHC -fno-warn-orphans #-}
module Payments.Control (handleSubscriptionDashboard
                        ,handleSubscriptionDashboardInfo
                        ,handleSyncNewSubscriptionWithRecurly
                        ,handleChangePlan
                        ,switchPlanToCompany
                        ,handleSyncWithRecurly)
       where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Control.Monad.Base

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
import qualified Log (payments)
import qualified Text.JSON.Gen as J
import MinutesTime
import Util.MonadUtils (when_)

import Payments.Model
import Payments.Rules
import Payments.View
import Payments.Config (RecurlyConfig(..))
import qualified Payments.Stats as Stats

-- bootstrap the payments dashboard
handleSubscriptionDashboard :: Kontrakcja m => m (Either KontraLink Response)
handleSubscriptionDashboard = checkUserTOSGet $ do
  user <- pguardM' "handleSubscriptionDashboardInfo: No user logged in." $ 
          ctxmaybeuser <$> getContext
  showSubscriptionDashboard user >>= renderFromBody kontrakcja
  
handleSubscriptionDashboardInfo :: Kontrakcja m => m JSValue
handleSubscriptionDashboardInfo = do
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext  
  user <- pguardM' "handleSubscriptionDashboardInfo: No user logged in." $ 
          ctxmaybeuser <$> getContext
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
    Just plan | ppPaymentPlanProvider plan == RecurlyProvider -> do
          billingsig  <- liftIO $ genSignature recurlyPrivateKey [("account[account_code]", show $ ppAccountCode plan)]
          -- we should be syncing somewhere else
          esub <- liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show $ ppAccountCode plan
          einvoices <- liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey (show $ ppAccountCode plan)
          msub <- case esub of
            Left e -> do
              Log.payments $ "handleSubscriptionDashboardInfo: When fetching subscriptions for payment plan: " ++ e
              return Nothing
            Right (sub:_) -> return $ Just sub
            Right _ -> do
              Log.payments $ "handleSubscriptionDashboardInfo: No subscriptions returned."
              return Nothing
          minvoices <- case einvoices of
            Left e -> do
              Log.payments $ "handleSubscriptionDashboardInfo: When fetching invoices for payment plan: " ++ e
              return Nothing
            Right i -> return $ Just i
          return $ J.object "plan" $ do
            J.value "subscription" $ msub
            J.value "invoices" $ minvoices
            J.value "code"   $ show $ ppAccountCode plan
            J.value "plan"   $ show $ ppPricePlan plan
            J.value "status" $ show $ ppStatus plan
            J.value "provider" "recurly"
            J.object "signatures" $ do
              J.value "billing"  billingsig          
            either (J.value "userid" . show) (J.value "companyid" . show) $ ppID plan              
    Just plan -> do -- no provider
      return $ J.object "plan" $ do
        J.value "code"   $ show $ ppAccountCode plan
        J.value "plan"   $ show $ ppPricePlan plan
        J.value "status" $ show $ ppStatus plan
        J.value "provider" "none"
        either (J.value "userid" . show) (J.value "companyid" . show) $ ppID plan
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
    
-- to call this, user must not have an account code yet (no payment plan in table)
handleSyncNewSubscriptionWithRecurly :: Kontrakcja m => m ()
handleSyncNewSubscriptionWithRecurly = do
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  time <- ctxtime <$> getContext
  ac <- pguardM' "handleSyncNewSubscriptionWithRecurly: account_code must exist and be an integer." $ 
        readField "account_code"
  user <- pguardM' "handleSyncNewSubscriptionWithRecurly: No user logged in." $ 
          ctxmaybeuser <$> getContext
  subscriptions <- pguardM "handleSyncNewSubscriptionWithRecurly" $ 
                   liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show ac
  subscription <- pguard' "handleSyncNewSubscriptionWithRecurly: No subscription." $ 
                  listToMaybe subscriptions
  let eid = maybe (Left $ userid user) Right $ usercompany user
  _ <- cachePlan time Stats.SignupAction ac subscription eid
  return ()

handleChangePlan :: Kontrakcja m => m ()
handleChangePlan = do
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  time <- ctxtime <$> getContext
  newplan :: PricePlan <- pguardM' "handleChangePlan: field plan must exist and be a recurly plan code (String)" $ 
                          readField "plan"
  user <- pguardM' "handleChangePlan: No user logged in." $ 
          ctxmaybeuser <$> getContext
  plan <- pguardM' "handleChangePlan: No plan for logged in user." $ 
          dbQuery $ GetPaymentPlan (maybe (Left (userid user)) Right (usercompany user))
  subscriptions <- pguardM "handleChangePlan" $ 
                   liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show $ ppAccountCode plan
  subscription <- pguard' "handleChangePlan: No subscriptions for Recurly account." $ 
                  listToMaybe subscriptions
  quantity <- maybe (return 1) (dbQuery . GetCompanyQuantity) $ usercompany user
  let eid = maybe (Left $ userid user) Right $ usercompany user
      ac  = ppAccountCode plan
  subinfo <- pguard "handleChangePlan" $ subInfo subscription
  case syncAction (quantity, newplan) subinfo of
    RNoAction -> do
      return ()
    RUpdateNow -> do
      s <- pguardM "handleChangePlan" $ 
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity True
      _ <- cachePlan time Stats.ChangeAction ac s eid
      return ()
    RUpdateRenewal -> do
      s <- pguardM "handleChangePlan" $ 
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity False
      _ <- cachePlan time Stats.ChangeAction ac s eid
      return ()
    RReactivateNow -> do
      _ <- liftIO $ reactivateSubscription curl_exe recurlyAPIKey (subID subscription)
      s <- pguardM "handleChangePlan" $ 
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity True
      _ <- cachePlan time Stats.ReactivateAction ac s eid
      return ()
    RReactivateRenewal -> do
      _ <- liftIO $ reactivateSubscription curl_exe recurlyAPIKey (subID subscription)
      s <- pguardM "handleChangePlan" $ 
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity False
      _ <- cachePlan time Stats.ReactivateAction ac s eid
      return ()
    RCancel -> do
      _ <- pguardM "handleChangePlan" $ 
           liftIO $ cancelSubscription curl_exe recurlyAPIKey $ subID subscription
      _ <- cachePlan time Stats.CancelAction ac subscription { subState = "canceled" } eid
      return ()

cachePlan :: (MonadDB m, MonadIO m) => MinutesTime -> Stats.PaymentsAction -> AccountCode -> Subscription -> Either UserID CompanyID -> m Bool
cachePlan time pa ac subscription eid = do
  let p       = fromRecurlyPricePlan $ subPricePlan subscription
      (s, sp) = fromRecurlyStatus    $ subState     subscription
      q       = subQuantity subscription
      pp      = maybe p (fromRecurlyPricePlan . penPricePlan) $ subPending subscription
      qp      = maybe q penQuantity                           $ subPending subscription
  let paymentplan = PaymentPlan { ppAccountCode      = ac
                                , ppID               = eid
                                , ppPricePlan        = p
                                , ppPendingPricePlan = pp
                                , ppStatus           = s
                                , ppPendingStatus    = sp
                                , ppQuantity         = q
                                , ppPendingQuantity  = qp 
                                , ppPaymentPlanProvider = RecurlyProvider }
  r <- dbUpdate $ SavePaymentPlan paymentplan time
  if r 
    then Stats.record time pa RecurlyProvider (subQuantity subscription) (fromRecurlyPricePlan $ subPricePlan subscription) eid ac
    else do
    Log.payments "cachePlan: Could not save payment plan."
    return False
  
switchPlanToCompany :: Kontrakcja m => UserID -> CompanyID -> m Bool
switchPlanToCompany uid cid = do
  time <- ctxtime <$> getContext
  mplan <- dbQuery $ GetPaymentPlan (Left uid)
  case mplan of
    Just pp | isLeft $ ppID pp -> do
      let pp' = pp { ppID = Right cid }
      b <- dbUpdate $ SavePaymentPlan pp' time
      _ <- Stats.record time Stats.CompanySwitchAction RecurlyProvider (ppQuantity pp') (ppPricePlan pp') (Right cid) (ppAccountCode pp)
      return b
    _ -> return False

{- Should be run once per day, preferably at night -}
handleSyncWithRecurly :: (MonadIO m, MonadDB m) => String -> MinutesTime -> m ()
handleSyncWithRecurly recurlyapikey time = do
  Log.payments "Syncing with Recurly."
  plans <- dbQuery $ PaymentPlansRequiringSync time
  Log.payments $ "Found " ++ show (length plans) ++ " plans requiring sync."
  forM_ plans $ \plan -> do
    esubscriptions <- liftIO $ getSubscriptionsForAccount curl_exe recurlyapikey $ show $ ppAccountCode plan
    case esubscriptions of
      Left s ->
        Log.payments $ "syncing: " ++ s
      Right [] ->
        Log.payments $ "syncing: no subscriptions for Recurly account; skipping."
      Right (subscription:_) -> case subInfo subscription of
        Right subinfo@(_,_,_,newplan) -> do
          let eid = ppID plan
              ac = ppAccountCode plan
          _ <- cachePlan time Stats.SyncAction ac subscription eid              
          quantity <- maybe (return 1) (dbQuery . GetCompanyQuantity) $ toMaybe $ ppID plan
          Log.payments $ "Here is the db quantity: " ++ show quantity
          case syncAction (quantity, newplan) subinfo of
            RUpdateNow -> do
              ms <- liftIO $ changeAccount curl_exe recurlyapikey (subID subscription) (show newplan) quantity True
              when_ (isRight ms) $
                cachePlan time Stats.ChangeAction ac (fromRight ms) eid
            RUpdateRenewal -> do
              ms <- liftIO $ changeAccount curl_exe recurlyapikey (subID subscription) (show newplan) quantity False
              when_ (isRight ms) $
                cachePlan time Stats.ChangeAction ac (fromRight ms) eid
            _ -> do
              _ <- cachePlan time Stats.SyncAction ac subscription eid
              return ()
        _ -> Log.payments $ "Could not parse subscription from Recurly."
        

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
    
-- factor out error logging
pguard :: (MonadBase IO m, MonadIO m) => String -> Either String a -> m a
pguard _   (Right v) = return v
pguard pre (Left msg) = do
  Log.payments $ pre ++ ": " ++ msg
  internalError
  
pguardM :: (MonadBase IO m, MonadIO m) => String -> m (Either String a) -> m a
pguardM pre action = pguard pre =<< action

pguard' :: (MonadBase IO m, MonadIO m) => String -> Maybe a -> m a
pguard' _ (Just v) = return v
pguard' msg Nothing = do
  Log.payments msg
  internalError
  
pguardM' :: (MonadBase IO m, MonadIO m) => String -> m (Maybe a) -> m a
pguardM' msg action = pguard' msg =<< action