{-# OPTIONS_GHC -fno-warn-orphans #-}
module Payments.Control (handleSubscriptionDashboard
                        ,handleSubscriptionDashboardInfo
                        ,handleSyncNewSubscriptionWithRecurly
                        ,handleChangePlan
                        ,switchPlanToCompany
                        ,handleSyncWithRecurly
                        ,handleRecurlyPostBack
                        ,handlePricePageJSON)
       where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Control.Monad.Base
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Mails.MailsConfig

import AppView
import Company.Model
import DB hiding (update, query)
import Kontra
import KontraLink
import Crypto.RNG
import Recurly
import Recurly.JS
import Recurly.Push
--import Templates.Templates
import Templates.TemplatesLoader
import User.Locale
import Templates.Trans
import Text.JSON
import Text.JSON.Gen hiding (value)
import User.Model
import User.Utils
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Happstack.Fields
import Utils.Read
import Utils.Either
import Utils.IO
import qualified Log (payments)
import qualified Text.JSON.Gen as J
import MinutesTime
import Mails.SendMail
import Utils.Monad

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
  
-- information JSON for dashboard
handleSubscriptionDashboardInfo :: Kontrakcja m => m JSValue
handleSubscriptionDashboardInfo = do
  Log.payments $ "Json for payments dashboard"
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext  
  user <- pguardM' "handleSubscriptionDashboardInfo: No user logged in." $ 
          ctxmaybeuser <$> getContext
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany user
  quantity <- (maybe (return 1) (dbQuery . GetCompanyQuantity) $ usercompany user)
  mplan <- dbQuery $ GetPaymentPlan (maybe (Left (userid user)) Right (usercompany user))
  --let currency = "SEK" -- we only support SEK for now
  plan <- case mplan of
    Nothing -> do
          teamsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "team")]
          formsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "form")]
          code <- dbUpdate GetAccountCode
          return $ J.object "signup" $ do
            J.value "code" $ show code
            J.value "currency" "SEK"
            J.value "quantity" quantity
            J.object "signatures" $ do
              J.value "team" teamsig
              J.value "form" formsig
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
        J.value "quantity" quantity
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
  --mcompany <- case usercompany user of
  --  Nothing -> return Nothing
  --  Just cid -> dbQuery $ GetCompany cid
  subscriptions <- pguardM "handleSyncNewSubscriptionWithRecurly" $ 
                   liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show ac
  subscription <- pguard' "handleSyncNewSubscriptionWithRecurly: No subscription." $ 
                  listToMaybe subscriptions
  invoices <- pguardM "handleChangePlan" $ 
              liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey $ show ac
  let is = maybe "collected" inState $ listToMaybe invoices
  let eid = maybe (Left $ userid user) Right $ usercompany user
  _ <- cachePlan time Stats.SignupAction ac subscription is eid Nothing Nothing
  -- now we send email with push notification
  -- sendInvoiceEmail user mcompany subscription
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
  invoices <- pguardM "handleChangePlan" $ 
              liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey $ show $ ppAccountCode plan
  let is = maybe "collected" inState $ listToMaybe invoices
  case syncAction (quantity, newplan) subinfo of
    RNoAction -> do
      return ()
    RUpdateNow -> do
      s <- pguardM "handleChangePlan" $ 
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity True
      _ <- cachePlan time Stats.ChangeAction ac s is eid (ppDunningStep plan) (ppDunningDate plan)
      return ()
    RUpdateRenewal -> do
      s <- pguardM "handleChangePlan" $ 
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity False
      _ <- cachePlan time Stats.ChangeAction ac s is eid (ppDunningStep plan) (ppDunningDate plan)
      return ()
    RReactivateNow -> do
      _ <- liftIO $ reactivateSubscription curl_exe recurlyAPIKey (subID subscription)
      s <- pguardM "handleChangePlan" $ 
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity True
      _ <- cachePlan time Stats.ReactivateAction ac s is eid (ppDunningStep plan) (ppDunningDate plan)
      return ()
    RReactivateRenewal -> do
      _ <- liftIO $ reactivateSubscription curl_exe recurlyAPIKey (subID subscription)
      s <- pguardM "handleChangePlan" $ 
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity False
      _ <- cachePlan time Stats.ReactivateAction ac s is eid (ppDunningStep plan) (ppDunningDate plan)
      return ()
    RCancel -> do
      _ <- pguardM "handleChangePlan" $ 
           liftIO $ cancelSubscription curl_exe recurlyAPIKey $ subID subscription
      _ <- cachePlan time Stats.CancelAction ac subscription { subState = "canceled" } is eid (ppDunningStep plan) (ppDunningDate plan)
      return ()

cachePlan :: (MonadDB m) => MinutesTime -> Stats.PaymentsAction -> AccountCode -> Subscription -> String -> Either UserID CompanyID -> Maybe Int -> Maybe MinutesTime -> m Bool
cachePlan time pa ac subscription invoicestatus eid mds mdd = do
  let p       = fromRecurlyPricePlan $ subPricePlan subscription
      (s, sp) = if invoicestatus == "failed" 
                then (OverdueStatus, OverdueStatus)
                else fromRecurlyStatus $ subState subscription
      q       = subQuantity subscription
      pp      = maybe p (fromRecurlyPricePlan . penPricePlan) $ subPending subscription
      qp      = maybe q penQuantity                           $ subPending subscription
  let paymentplan = PaymentPlan { ppAccountCode         = ac
                                , ppID                  = eid
                                , ppPricePlan           = p
                                , ppPendingPricePlan    = pp
                                , ppStatus              = s
                                , ppPendingStatus       = sp
                                , ppQuantity            = q
                                , ppPendingQuantity     = qp 
                                , ppPaymentPlanProvider = RecurlyProvider 
                                , ppDunningStep         = mds
                                , ppDunningDate         = mdd}
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
handleSyncWithRecurly :: (MonadIO m, MonadDB m, CryptoRNG m) => String -> MailsConfig -> KontrakcjaGlobalTemplates -> String -> MinutesTime -> m ()
handleSyncWithRecurly hostpart mailsconfig templates recurlyapikey time = do
  Log.payments "Syncing with Recurly."
  plans <- dbQuery $ PaymentPlansRequiringSync time
  Log.payments $ "Found " ++ show (length plans) ++ " plans requiring sync."
  forM_ plans $ \plan -> do
    esubscriptions <- liftIO $ getSubscriptionsForAccount curl_exe recurlyapikey $ show $ ppAccountCode plan
    case esubscriptions of
      Left s -> do
        Log.payments $ "syncing: " ++ s
      Right [] ->
        Log.payments $ "syncing: no subscriptions for Recurly account; skipping."
      Right (subscription:_) -> case subInfo subscription of
        Right subinfo@(_,_,_,newplan) -> do
          einvoices <- liftIO $ getInvoicesForAccount curl_exe recurlyapikey $ show $ ppAccountCode plan
          case einvoices of
            Left s -> do
              Log.payments $ "syncing: " ++ s
            Right invoices -> do
              let is = maybe "collected" inState $ listToMaybe invoices
                  eid = ppID plan
                  ac = ppAccountCode plan
              _ <- cachePlan time Stats.SyncAction ac subscription is eid (ppDunningStep plan) (ppDunningDate plan)
              quantity <- maybe (return 1) (dbQuery . GetCompanyQuantity) $ toMaybe $ ppID plan
              Log.payments $ "Here is the db quantity: " ++ show quantity
              case syncAction (quantity, newplan) subinfo of
                RUpdateNow -> do
                  ms <- liftIO $ changeAccount curl_exe recurlyapikey (subID subscription) (show newplan) quantity True
                  when_ (isRight ms) $
                    cachePlan time Stats.ChangeAction ac (fromRight ms) is eid (ppDunningStep plan) (ppDunningDate plan)
                RUpdateRenewal -> do
                  ms <- liftIO $ changeAccount curl_exe recurlyapikey (subID subscription) (show newplan) quantity False
                  when_ (isRight ms) $
                    cachePlan time Stats.ChangeAction ac (fromRight ms) is eid (ppDunningStep plan) (ppDunningDate plan)
                _ -> do
                  _ <- cachePlan time Stats.SyncAction ac subscription is eid (ppDunningStep plan) (ppDunningDate plan)
                  return ()
        _ -> Log.payments $ "Could not parse subscription from Recurly."
  dunnings <- dbQuery $ PaymentPlansExpiredDunning time
  Log.payments $ "Found " ++ show (length dunnings) ++ " plans requiring dunning email."
  forM_ dunnings $ \plan -> do
    case ppDunningStep plan of
      Just n -> do
        muc <- case ppID plan of
          Left uid -> do
            muser <- dbQuery $ GetUserByID uid
            case muser of
              Nothing -> do
                Log.payments "post back: no user for account"
                return Nothing
              Just user -> return $ Just (user, Nothing)
          Right cid -> do
            mcompany <- dbQuery $ GetCompany cid
            case mcompany of
              Nothing -> do
                Log.payments "post back: no company for account"
                return Nothing
              Just company -> do
                users <- dbQuery $ GetCompanyAccounts cid
                case filter useriscompanyadmin users of
                  (admin:_) -> return $ Just (admin, Just company)
                  _ -> return Nothing
        case muc of
          Nothing -> return ()
          Just (user, mcompany) -> do
            eins <- liftIO $ getInvoicesForAccount curl_exe recurlyapikey (show $ ppAccountCode plan)
            case eins of
              Left msg -> Log.payments $ "post back: " ++ msg
              Right [] -> Log.payments "post back: no invoices"
              Right (invoice:_) -> do
                -- next dunning step: 3 days later
                -- eventually, Recurly will expire the account
                _ <- dbUpdate $ SavePaymentPlan (plan {ppDunningStep = Just (n+1), ppDunningDate = Just $ daysAfter 3 time}) time
                let locale' = locale $ usersettings user
                _ <- sendInvoiceFailedEmail hostpart mailsconfig locale' templates user mcompany invoice
                return ()
      _ -> return ()  
      
postBackCache :: Kontrakcja m => PushRequest -> m ()
postBackCache pr = do
  time <- ctxtime <$> getContext
  recurlyapikey <- recurlyAPIKey . ctxrecurlyconfig <$> getContext
  -- we need to ask recurly for the info again for security
  ac             <- pguard' "post back: Could not parse account code (should be int)." $ maybeRead $ pushAccountCode pr
  plan           <- pguardM' "post back: Could not find plan for account." $ dbQuery $ GetPaymentPlanByAccountCode ac
  esubscriptions <- pguardM "post back:" $ liftIO $ getSubscriptionsForAccount curl_exe recurlyapikey $ show $ ac
  s              <- pguard' "post back: no subscriptions for account" $ listToMaybe esubscriptions
  ins            <- pguardM "post back:" $ liftIO $ getInvoicesForAccount curl_exe recurlyapikey (show ac)
  invoice        <- pguard' "post back: no invoices for account" $ listToMaybe ins
  let is = inState invoice
  (user, mcompany) <- case ppID plan of
    Left uid -> do
      user <- pguardM' "post back: no user for account" $ dbQuery $ GetUserByID uid
      return (user, Nothing)
    Right cid -> do
      company <- pguardM' "post back: no company for account" $ dbQuery $ GetCompany cid
      users <- dbQuery $ GetCompanyAccounts cid
      admin <- pguard' "post back: no admin for company" $ listToMaybe $ filter useriscompanyadmin users
      return (admin, Just company)
  case (pr, fromRecurlyStatus $ subState s, ppDunningStep plan) of
    (FailedPayment _, (ActiveStatus, _), Nothing) -> do
      -- need to add dunning step + date to plan
      -- next email 7 days later
      _ <- cachePlan time Stats.PushAction ac s is (ppID plan) (Just 1) (Just $ daysAfter 7 time) 
      ctx <- getContext
      sendInvoiceFailedEmail (ctxhostpart ctx) (ctxmailsconfig ctx) (ctxlocale ctx) (ctxglobaltemplates ctx) user mcompany invoice
    (SuccessfulPayment _, _, _) -> do
      -- need to remove dunning step
      _ <- cachePlan time Stats.PushAction ac s is (ppID plan) Nothing Nothing
      sendInvoiceEmail user mcompany s
    (ExpiredSubscription _, _, Just _) -> do
      -- need to remove dunning step
      _ <- cachePlan time Stats.PushAction ac s is (ppID plan) Nothing Nothing
      sendExpiredEmail user
    (ExpiredSubscription _, _, _) -> do
      sendExpiredEmail user
    _ -> do
      _ <- cachePlan time Stats.PushAction ac s is (ppID plan) (ppDunningStep plan) (ppDunningDate plan)
      return ()
        
handleRecurlyPostBack :: Kontrakcja m => m ()
handleRecurlyPostBack = do
  Log.payments "Got a Push notification from Recurly"
  vbody <- rqBody <$> askRq
  bdy <- liftIO $ BSL.toString <$> unBody <$> takeMVar vbody      
  case parsePush bdy of
    Just ps -> postBackCache ps
    _       -> return ()
    
handlePricePageJSON :: Kontrakcja m => m JSValue
handlePricePageJSON = do
  Log.payments $ "Handling Price Page JSON"
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  code <- dbUpdate GetAccountCode
  teamsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "team")]
  formsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "form")]
  runJSONGenT $ do
    J.value "subdomain" recurlySubdomain
    J.value "account_code" $ show code
    J.object "plans" $ do
      J.object "team" $ do
        J.value "signature" teamsig
      J.object "form" $ do
        J.value "signature" formsig
    
-- mails    

sendInvoiceEmail :: Kontrakcja m => User -> Maybe Company -> Subscription -> m ()
sendInvoiceEmail user mcompany subscription = do
  ctx <- getContext
  mail <- mailSignup (ctxhostpart ctx) user mcompany subscription
  scheduleEmailSendout (ctxmailsconfig ctx)
                        (mail{to = [MailAddress{
                                     fullname = getFullName user
                                   , email = getEmail user }]})

sendInvoiceFailedEmail :: (MonadDB m, CryptoRNG m) => String -> MailsConfig -> Locale -> KontrakcjaGlobalTemplates -> User -> Maybe Company -> Invoice -> m ()
sendInvoiceFailedEmail hostpart mailsconfig locale templates user mcompany invoice = do
  mail <- runTemplatesT (locale, templates) $ mailFailed hostpart user mcompany invoice
  scheduleEmailSendout mailsconfig
    (mail{to = [MailAddress { fullname = getFullName user
                            , email = getEmail user}]})
    
sendExpiredEmail :: Kontrakcja m => User -> m ()
sendExpiredEmail user = do
  ctx <- getContext
  mail <- mailExpired (ctxhostpart ctx)
  scheduleEmailSendout (ctxmailsconfig ctx)
    (mail{to = [MailAddress { fullname = getFullName user
                            , email = getEmail user }]})
  
fromRecurlyStatus :: String -> (PaymentPlanStatus, PaymentPlanStatus)
fromRecurlyStatus "active"   = (ActiveStatus, ActiveStatus)
fromRecurlyStatus "canceled" = (ActiveStatus, CanceledStatus)
fromRecurlyStatus "expired"  = (CanceledStatus, CanceledStatus)
fromRecurlyStatus "future"   = (DeactivatedStatus, ActiveStatus)
fromRecurlyStatus _          = (ActiveStatus, ActiveStatus)

fromRecurlyPricePlan :: String -> PricePlan
fromRecurlyPricePlan "free"         = FreePricePlan
fromRecurlyPricePlan "team"         = TeamPricePlan
fromRecurlyPricePlan "form"         = FormPricePlan
fromRecurlyPricePlan _              = TeamPricePlan
  
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