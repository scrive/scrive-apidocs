{-# OPTIONS_GHC -fno-warn-orphans #-}
module Payments.Control (
                         handleSyncNewSubscriptionWithRecurly
                        ,handleChangePlan
                        ,handleSyncWithRecurly
                        ,handleRecurlyPostBack
                        ,handlePricePageJSON
                        ,handleUserExists
                        ,handleCreateUser
                        ,handleSyncNewSubscriptionWithRecurlyOutside)
       where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Control.Monad.Base
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Mails.MailsConfig

import Company.Model
import DB hiding (update, query)
import Kontra
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
import qualified Login

import Payments.JSON ()
import Payments.Model
import Payments.Rules
import Payments.View
import Payments.Config (RecurlyConfig(..))
import qualified Payments.Stats as Stats
  
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
  invoices <- pguardM "handleChangePlan" $ 
              liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey $ show ac
  let is = maybe "collected" inState $ listToMaybe invoices
  let eid = maybe (Left $ userid user) Right $ usercompany user
  _ <- cachePlan time Stats.SignupAction ac subscription is eid Nothing Nothing
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
      be      = fromMaybe (daysAfter 30 time) $ parseMinutesTime "%Y-%m-%dT%H:%M:%S%Z" $ subCurrentBillingEnds subscription
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
                                , ppDunningDate         = mdd
                                , ppBillingEndDate      = be}
  r <- dbUpdate $ SavePaymentPlan paymentplan time
  if r 
    then Stats.record time pa RecurlyProvider (subQuantity subscription) (fromRecurlyPricePlan $ subPricePlan subscription) eid ac
    else do
    Log.payments "cachePlan: Could not save payment plan."
    return False
  

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
      sendInvoiceFailedEmail (ctxhostpart ctx) (ctxmailsconfig ctx) (locale $ usersettings user) (ctxglobaltemplates ctx) user mcompany invoice
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
    
handlePricePageUserPlanRecurlyJSON :: Kontrakcja m => User -> PaymentPlan -> m JSValue
handlePricePageUserPlanRecurlyJSON user plan = do
  Log.payments $ "Handling Price Page JSON for existing user with payment plan on recurly"
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  billingsig  <- liftIO $ genSignature recurlyPrivateKey [("account[account_code]", show $ ppAccountCode plan)]
  esub <- liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show $ ppAccountCode plan
  einvoices <- liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey (show $ ppAccountCode plan)
  quantity <- (maybe (return 1) (dbQuery . GetCompanyQuantity) $ usercompany user)
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
  runJSONGenT $ do
    J.value "type"         $ "planrecurly"
    J.value "subscription" $ msub
    J.value "invoices"     $ minvoices
    J.value "subdomain" recurlySubdomain
    J.value "accountCode"  $ show $ ppAccountCode plan
    J.value "paidPlan"     $ show $ ppPricePlan plan    
    J.value "status"       $ show $ ppStatus plan
    J.value "firstName"    $ getFirstName user
    J.value "lastName"     $ getLastName user
    J.value "email"        $ getEmail user
    J.value "billingSig" billingsig
    J.value "quantity" quantity
    J.value "provider" "recurly"

handlePricePageUserPlanNoneJSON :: Kontrakcja m => User -> PaymentPlan -> m JSValue
handlePricePageUserPlanNoneJSON user plan = do
  Log.payments $ "Handling Price Page JSON for existing user with payment plan no provider"
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany user
  quantity <- (maybe (return 1) (dbQuery . GetCompanyQuantity) $ usercompany user)
  runJSONGenT $ do
    J.value "type"        $ "plannone"
    J.value "accountCode" $ show $ ppAccountCode plan
    J.value "firstName"   $ getFirstName user
    J.value "lastName"    $ getLastName user
    J.value "email"       $ getEmail user
    J.value "companyName" $ getCompanyName (user, mcompany)        
    J.value "paidPlan"    $ show $ ppPricePlan plan
    J.value "status"      $ show $ ppStatus plan
    J.value "provider"    $ "none"
    J.value "quantity"    $ quantity

handlePricePageUserJSON :: Kontrakcja m => User -> m JSValue
handlePricePageUserJSON user = do
  Log.payments $ "Handling Price Page JSON for existing user"
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  code <- dbUpdate GetAccountCode
  teamsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "team")]
  formsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "form")]
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany user
  quantity <- (maybe (return 1) (dbQuery . GetCompanyQuantity) $ usercompany user)
  runJSONGenT $ do
    J.value "type"      "user"
    J.value "subdomain" recurlySubdomain
    J.value "accountCode" $ show code
    J.value "firstName" $ getFirstName user
    J.value "lastName" $ getLastName user
    J.value "email" $ getEmail user
    J.value "quantity" quantity
    J.value "companyName" $ getCompanyName (user, mcompany)    
    J.object "plans" $ do
      J.object "team" $ do
        J.value "signature" teamsig
      J.object "form" $ do
        J.value "signature" formsig
    
handlePricePageNoUserJSON :: Kontrakcja m => m JSValue
handlePricePageNoUserJSON = do
  Log.payments $ "Handling Price Page JSON for non-existing user"
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  code <- dbUpdate GetAccountCode
  teamsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "team")]
  formsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "form")]
  runJSONGenT $ do
    J.value "type"      "nouser"
    J.value "subdomain" recurlySubdomain
    J.value "accountCode" $ show code
    J.value "quantity" (1::Int)
    J.object "plans" $ do
      J.object "team" $ do
        J.value "signature" teamsig
      J.object "form" $ do
        J.value "signature" formsig
    
handlePricePageJSON :: Kontrakcja m => m JSValue
handlePricePageJSON = do
  muser <- ctxmaybeuser <$> getContext
  mplan <- maybe (return Nothing) (dbQuery . GetPaymentPlan . toEID) muser
  case (muser, mplan, ppPaymentPlanProvider $ fromJust mplan) of
    (Nothing  , _        , _)               -> handlePricePageNoUserJSON
    (Just user, Nothing  , _)               -> handlePricePageUserJSON user
    (Just user, Just plan, NoProvider)      -> handlePricePageUserPlanNoneJSON user plan
    (Just user, Just plan, RecurlyProvider) -> handlePricePageUserPlanRecurlyJSON user plan
        
toEID :: User -> Either UserID CompanyID
toEID user = maybe (Left (userid user)) Right (usercompany user)

handleUserExists :: Kontrakcja m => m JSValue
handleUserExists = do
  email <- pguardM' "handleUserExists: email must exist." $ 
           getField "email"
  muser <- dbQuery $ GetUserByEmail $ Email email
  let meid = maybe (Left $ userid $ fromJust muser) Right . usercompany <$> muser
  mplan <- maybe (return Nothing) (dbQuery . GetPaymentPlan) meid
  runJSONGenT $ do
    J.value "user_exists" $ isJust muser
    J.value "has_plan"    $ isJust mplan
    
handleCreateUser :: Kontrakcja m => m JSValue
handleCreateUser = do
  Log.payments $ "handleCreateUser"
  mres <- Login.handleSignup
  runJSONGenT $ do
    J.value "success" $ isJust mres

handleSyncNewSubscriptionWithRecurlyOutside :: Kontrakcja m => m ()
handleSyncNewSubscriptionWithRecurlyOutside = do
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  time <- ctxtime <$> getContext
  ac <- pguardM' "handleSyncNewSubscriptionWithRecurlyOutside: account_code must exist and be an integer." $ 
        readField "accountCode"
  email <- pguardM' "handleSyncNewSubscriptionWithRecurlyOutside: email must exist." $ 
        getField "email"
  user <- pguardM' "handleSyncNewSubscriptionWithRecurlyOutside: user does not exist." $
          dbQuery $ GetUserByEmail $ Email email
  subscriptions <- pguardM "handleSyncNewSubscriptionWithRecurly" $ 
                   liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show ac
  subscription <- pguard' "handleSyncNewSubscriptionWithRecurly: No subscription." $ 
                  listToMaybe subscriptions
  invoices <- pguardM "handleChangePlan" $ 
              liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey $ show ac
  let is = maybe "collected" inState $ listToMaybe invoices
  let eid = maybe (Left $ userid user) Right $ usercompany user
  _ <- cachePlan time Stats.SignupAction ac subscription is eid Nothing Nothing
  return ()

-- mails    

sendInvoiceEmail :: Kontrakcja m => User -> Maybe Company -> Subscription -> m ()
sendInvoiceEmail user mcompany subscription = do
  ctx <- getContext
  mail <- runTemplatesT (locale $ usersettings user, ctxglobaltemplates ctx) $ mailSignup (ctxhostpart ctx) user mcompany subscription
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
  mail <- runTemplatesT (locale $ usersettings user, ctxglobaltemplates ctx) $ mailExpired (ctxhostpart ctx)
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