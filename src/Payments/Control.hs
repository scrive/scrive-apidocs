{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Payments.Control (
                         handleSyncNewSubscriptionWithRecurly
                        ,handleChangePlan
                        ,handleSyncWithRecurly
                        ,handleSyncNoProvider
                        ,handleRecurlyPostBack
                        ,handlePricePageJSON
                        ,handleUserExists
                        ,handleSyncNewSubscriptionWithRecurlyOutside)
       where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.State
import Data.Functor
import Happstack.Server hiding (simpleHTTP)
import Recurly
import Recurly.JS
import Recurly.Push
import Text.JSON
import Text.JSON.Gen hiding (value)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Text.JSON.Gen as J

import AppConf
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Company.Model
import Crypto.RNG
import DB hiding (update, query)
import Happstack.Fields
import Kontra
import KontraPrelude
import Mails.MailsConfig
import Mails.SendMail
import MinutesTime
import Payments.Config (RecurlyConfig(..))
import Payments.JSON ()
import Payments.Model
import Payments.Rules
import Payments.Stats
import Payments.View
import Templates
import Theme.Model
import User.Email
import User.Lang
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils
import Utils.Either
import Utils.IO
import Utils.Monad
import Utils.Read
import qualified Log
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
  _ <- cachePlan time Stats.SignupAction ac subscription is (usercompany user) Nothing Nothing
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
          dbQuery $ GetPaymentPlan $ usercompany user
  subscriptions <- pguardM "handleChangePlan" $
                   liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show $ ppAccountCode plan
  subscription <- pguard' "handleChangePlan: No subscriptions for Recurly account." $
                  listToMaybe subscriptions
  quantity <- dbQuery $ GetCompanyQuantity $ usercompany user
  subinfo <- pguard "handleChangePlan" $ subInfo subscription
  invoices <- pguardM "handleChangePlan" $
              liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey $ show $ ppAccountCode plan
  let is = maybe "collected" inState $ listToMaybe invoices
      ac  = ppAccountCode plan
  case syncAction (quantity, newplan) subinfo of
    RNoAction -> do
      return ()
    RUpdateNow -> do
      s <- pguardM "handleChangePlan" $
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity True
      _ <- cachePlan time Stats.ChangeAction ac s is (usercompany user) (ppDunningStep plan) (ppDunningDate plan)
      return ()
    RUpdateRenewal -> do
      s <- pguardM "handleChangePlan" $
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity False
      _ <- cachePlan time Stats.ChangeAction ac s is (usercompany user) (ppDunningStep plan) (ppDunningDate plan)
      return ()
    RReactivateNow -> do
      _ <- liftIO $ reactivateSubscription curl_exe recurlyAPIKey (subID subscription)
      s <- pguardM "handleChangePlan" $
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity True
      _ <- cachePlan time Stats.ReactivateAction ac s is (usercompany user) (ppDunningStep plan) (ppDunningDate plan)
      return ()
    RReactivateRenewal -> do
      _ <- liftIO $ reactivateSubscription curl_exe recurlyAPIKey (subID subscription)
      s <- pguardM "handleChangePlan" $
           liftIO $ changeAccount curl_exe recurlyAPIKey (subID subscription) (show newplan) quantity False
      _ <- cachePlan time Stats.ReactivateAction ac s is (usercompany user) (ppDunningStep plan) (ppDunningDate plan)
      return ()
    RCancel -> do
      _ <- pguardM "handleChangePlan" $
           liftIO $ cancelSubscription curl_exe recurlyAPIKey $ subID subscription
      _ <- cachePlan time Stats.CancelAction ac subscription { subState = "canceled" } is (usercompany user) (ppDunningStep plan) (ppDunningDate plan)
      return ()
    RTerminate -> return ()

cachePlan :: (MonadDB m, MonadThrow m, Log.MonadLog m) => UTCTime -> Stats.PaymentsAction -> AccountCode -> Subscription -> String -> CompanyID -> Maybe Int -> Maybe UTCTime -> m Bool
cachePlan time pa ac subscription invoicestatus cid mds mdd = do
  let p       = fromRecurlyPricePlan $ subPricePlan subscription
      (s, sp) = if invoicestatus == "failed"
                then (OverdueStatus, OverdueStatus)
                else fromRecurlyStatus $ subState subscription
      q       = subQuantity subscription
      pp      = maybe p (fromRecurlyPricePlan . penPricePlan) $ subPending subscription
      qp      = maybe q penQuantity                           $ subPending subscription
      be      = fromMaybe (daysAfter 30 time) $ parseTime' "%Y-%m-%dT%H:%M:%S%Z" $ subCurrentBillingEnds subscription
  let paymentplan = PaymentPlan { ppAccountCode         = ac
                                , ppCompanyID           = cid
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
    then Stats.record time pa RecurlyProvider (subQuantity subscription) (fromRecurlyPricePlan $ subPricePlan subscription) cid ac
    else do
    Log.mixlog_ "cachePlan: Could not save payment plan."
    return False


{- Should be run once per day, preferably at night -}
handleSyncWithRecurly :: (MonadBase IO m, MonadIO m, MonadDB m, MonadThrow m, Log.MonadLog m, CryptoRNG m) => AppConf -> MailsConfig -> KontrakcjaGlobalTemplates -> String -> UTCTime -> m ()
handleSyncWithRecurly appConf mailsconfig templates recurlyapikey time = do
  Log.mixlog_ "Syncing with Recurly."
  plans <- dbQuery $ PaymentPlansRequiringSync RecurlyProvider time
  Log.mixlog_ $ "Found " ++ show (length plans) ++ " plans requiring sync."
  forM_ plans $ \plan -> do
    esubscriptions <- liftIO $ getSubscriptionsForAccount curl_exe recurlyapikey $ show $ ppAccountCode plan
    case esubscriptions of
      Left s -> do
        Log.mixlog_ $ "syncing: " ++ s
      Right [] ->
        Log.mixlog_ $ "syncing: no subscriptions for Recurly account; skipping."
      Right (subscription:_) -> case subInfo subscription of
        Right subinfo@(_,_,_,newplan) -> do
          einvoices <- liftIO $ getInvoicesForAccount curl_exe recurlyapikey $ show $ ppAccountCode plan
          case einvoices of
            Left s -> do
              Log.mixlog_ $ "syncing: " ++ s
            Right invoices -> do
              let is = maybe "collected" inState $ listToMaybe invoices
                  cid = ppCompanyID plan
                  ac = ppAccountCode plan
              _ <- cachePlan time Stats.SyncAction ac subscription is cid (ppDunningStep plan) (ppDunningDate plan)
              quantity <- dbQuery $ GetCompanyQuantity $ ppCompanyID plan
              Log.mixlog_ $ "Here is the db quantity: " ++ show quantity
              case syncAction (quantity, newplan) subinfo of
                RUpdateNow -> do
                  ms <- liftIO $ changeAccount curl_exe recurlyapikey (subID subscription) (show newplan) quantity True
                  when_ (isRight ms) $
                    cachePlan time Stats.ChangeAction ac (fromRight ms) is cid (ppDunningStep plan) (ppDunningDate plan)
                RUpdateRenewal -> do
                  ms <- liftIO $ changeAccount curl_exe recurlyapikey (subID subscription) (show newplan) quantity False
                  when_ (isRight ms) $
                    cachePlan time Stats.ChangeAction ac (fromRight ms) is cid (ppDunningStep plan) (ppDunningDate plan)
                RTerminate ->  do -- If we will find empty company while doing synch, we whole recurly connection
                  ms <- liftIO $ terminateSubscription curl_exe recurlyapikey (subID subscription) NoRefund
                  when_ (isRight ms) $
                     dbUpdate $ DeletePaymentPlan cid
                _ -> return ()
        _ -> Log.mixlog_ $ "Could not parse subscription from Recurly."
  dunnings <- dbQuery $ PaymentPlansExpiredDunning time
  Log.mixlog_ $ "Found " ++ show (length dunnings) ++ " plans requiring dunning email."
  forM_ dunnings $ \plan -> do
    case ppDunningStep plan of
      Just n -> do
        muc <- do
            company <- guardJustM $ dbQuery $ GetCompany $ ppCompanyID plan
            users <- dbQuery $ GetCompanyAccounts $ ppCompanyID plan
            case filter useriscompanyadmin users of
                  (admin:_) -> return $ Just (admin, company)
                  _ -> return Nothing
        case muc of
          Nothing -> return ()
          Just (user, company) -> do
            eins <- liftIO $ getInvoicesForAccount curl_exe recurlyapikey (show $ ppAccountCode plan)
            case eins of
              Left msg -> Log.mixlog_ $ "post back: " ++ msg
              Right [] -> Log.mixlog_ "post back: no invoices"
              Right (invoice:_) -> do
                -- next dunning step: 3 days later
                -- eventually, Recurly will expire the account
                _ <- dbUpdate $ SavePaymentPlan (plan {ppDunningStep = Just (n+1), ppDunningDate = Just $ daysAfter 3 time}) time
                let lang' = lang $ usersettings user
                bd <- dbQuery $ GetBrandedDomainByUserID (userid user)
                _ <- sendInvoiceFailedEmail bd (hostpart appConf) mailsconfig lang' templates user company invoice
                return ()
      _ -> return ()

handleSyncNoProvider :: (MonadDB m, MonadThrow m, Log.MonadLog m, CryptoRNG m) => UTCTime -> m ()
handleSyncNoProvider time = do
  Log.mixlog_ "Syncing accounts with no provider."
  plans <- dbQuery $ PaymentPlansRequiringSync NoProvider time
  Log.mixlog_ $ "Found " ++ show (length plans) ++ " plans requiring sync."
  forM_ plans $ \plan -> do
    quantity <- dbQuery $ GetCompanyQuantity $ ppCompanyID plan
    Log.mixlog_ $ "Here is the db quantity: " ++ show quantity
    r <- dbUpdate $ SavePaymentPlan plan {ppQuantity = quantity, ppPendingQuantity = quantity} time
    if r
      then do
        _ <- Stats.record time SyncAction NoProvider quantity (ppPricePlan plan) (ppCompanyID plan) (ppAccountCode plan)
        return ()
      else do
        Log.mixlog_ "cachePlan: Could not save payment plan."

postBackCache :: Kontrakcja m => PushRequest -> m ()
postBackCache pr = do
  time <- ctxtime <$> getContext
  recurlyapikey <- recurlyAPIKey . ctxrecurlyconfig <$> getContext
  -- we need to ask recurly for the info again for security
  ac             <- pguard' "post back: Could not parse account code (should be int)." $ maybeRead $ pushAccountCode pr
  plan           <- pguardM' ("post back: Could not find plan for account. ac: " ++ show ac) $ dbQuery $ GetPaymentPlanByAccountCode ac
  esubscriptions <- pguardM "post back:" $ liftIO $ getSubscriptionsForAccount curl_exe recurlyapikey $ show $ ac
  s              <- pguard' "post back: no subscriptions for account" $ listToMaybe esubscriptions
  ins            <- pguardM "post back:" $ liftIO $ getInvoicesForAccount curl_exe recurlyapikey (show ac)
  invoice        <- pguard' "post back: no invoices for account" $ listToMaybe ins
  let is = inState invoice
  (user, company) <- do
      company <- pguardM' "post back: no company for account" $ dbQuery $ GetCompany $ ppCompanyID plan
      users <- dbQuery $ GetCompanyAccounts $ ppCompanyID plan
      admin <- pguard' "post back: no admin for company" $ listToMaybe $ filter useriscompanyadmin users
      return (admin, company)
  case (pr, fromRecurlyStatus $ subState s, ppDunningStep plan) of
    (FailedPayment _, (ActiveStatus, _), Nothing) -> do
      -- need to add dunning step + date to plan
      -- next email 7 days later
      _ <- cachePlan time Stats.PushAction ac s is (ppCompanyID plan) (Just 1) (Just $ daysAfter 7 time)
      ctx <- getContext
      bd <- dbQuery $ GetBrandedDomainByUserID (userid user)

      sendInvoiceFailedEmail bd (ctxhostpart ctx) (ctxmailsconfig ctx) (lang $ usersettings user) (ctxglobaltemplates ctx) user company invoice
    (SuccessfulPayment _, _, _) -> do
      -- need to remove dunning step
      _ <- cachePlan time Stats.PushAction ac s is (ppCompanyID plan) Nothing Nothing
      sendInvoiceEmail user company s
    (ExpiredSubscription _, _, Just _) -> do
      -- need to remove dunning step
      _ <- cachePlan time Stats.PushAction ac s is (ppCompanyID plan) Nothing Nothing
      sendExpiredEmail user
    (ExpiredSubscription _, _, _) -> do
      sendExpiredEmail user
    _ -> do
      _ <- cachePlan time Stats.PushAction ac s is (ppCompanyID plan) (ppDunningStep plan) (ppDunningDate plan)
      return ()

handleRecurlyPostBack :: Kontrakcja m => m ()
handleRecurlyPostBack = do
  Log.mixlog_ "Got a Push notification from Recurly"

  -- takeRequestBody uses tryTakeMVar which never blocks
  -- so even if the body mvar was already read (e.g. by decodeBody)
  -- we will never block here
  vbody <- askRq >>= takeRequestBody
  case parsePush =<< BSL.toString <$> unBody <$> vbody of
    Just ps -> postBackCache ps
    _       -> return ()

handlePricePageUserPlanRecurlyJSON :: Kontrakcja m => User -> PaymentPlan -> m JSValue
handlePricePageUserPlanRecurlyJSON user plan = do
  Log.mixlog_ $ "Handling Price Page JSON for existing user with payment plan on recurly"
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  billingsig  <- liftIO $ genSignature recurlyPrivateKey [("account[account_code]", show $ ppAccountCode plan)]
  esub <- liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show $ ppAccountCode plan
  einvoices <- liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey (show $ ppAccountCode plan)
  quantity <- dbQuery $ GetCompanyQuantity $ usercompany user
  msub <- case esub of
    Left e -> do
      Log.mixlog_ $ "handleSubscriptionDashboardInfo: When fetching subscriptions for payment plan: " ++ e
      return Nothing
    Right (sub:_) -> return $ Just sub
    Right _ -> do
      Log.mixlog_ $ "handleSubscriptionDashboardInfo: No subscriptions returned."
      return Nothing
  minvoices <- case einvoices of
    Left e -> do
      Log.mixlog_ $ "handleSubscriptionDashboardInfo: When fetching invoices for payment plan: " ++ e
      return Nothing
    Right i -> return $ Just i
  teamsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "team")]
  formsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "form")]
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
    J.object "plans" $ do
      J.object "team" $ do
        J.value "signature" teamsig
      J.object "form" $ do
        J.value "signature" formsig

handlePricePageUserPlanNoneJSON :: Kontrakcja m => User -> PaymentPlan -> m JSValue
handlePricePageUserPlanNoneJSON user plan = do
  Log.mixlog_ $ "Handling Price Page JSON for existing user with payment plan no provider"
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  company <- dbQuery $ GetCompany $ usercompany user
  quantity <- dbQuery $ GetCompanyQuantity $ usercompany user
  teamsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "team")]
  formsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "form")]
  runJSONGenT $ do
    J.value "type"        $ "plannone"
    J.value "accountCode" $ show $ ppAccountCode plan
    J.value "firstName"   $ getFirstName user
    J.value "lastName"    $ getLastName user
    J.value "email"       $ getEmail user
    J.value "companyName" $ getCompanyName company
    J.value "paidPlan"    $ show $ ppPricePlan plan
    J.value "status"      $ show $ ppStatus plan
    J.value "provider"    $ "none"
    J.value "quantity"    $ quantity
    J.object "plans" $ do
      J.object "team" $ do
        J.value "signature" teamsig
      J.object "form" $ do
        J.value "signature" formsig

handlePricePageUserJSON :: Kontrakcja m => User -> m JSValue
handlePricePageUserJSON user = do
  Log.mixlog_ $ "Handling Price Page JSON for existing user"
  RecurlyConfig{..} <- ctxrecurlyconfig <$> getContext
  code <- dbUpdate GetAccountCode
  teamsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "team")]
  formsig <- liftIO $ genSignature recurlyPrivateKey [("subscription[plan_code]", "form")]
  company <- dbQuery $  GetCompany $ usercompany user
  quantity <- dbQuery $ GetCompanyQuantity $ usercompany user
  runJSONGenT $ do
    J.value "type"      "user"
    J.value "subdomain" recurlySubdomain
    J.value "accountCode" $ show code
    J.value "firstName" $ getFirstName user
    J.value "lastName" $ getLastName user
    J.value "email" $ getEmail user
    J.value "quantity" quantity
    J.value "companyName" $ getCompanyName company
    J.value "is_admin" $ useriscompanyadmin user
    J.object "plans" $ do
      J.object "team" $ do
        J.value "signature" teamsig
      J.object "form" $ do
        J.value "signature" formsig

handlePricePageNoUserJSON :: Kontrakcja m => m JSValue
handlePricePageNoUserJSON = do
  Log.mixlog_ $ "Handling Price Page JSON for non-existing user"
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
  case muser of
     Nothing -> handlePricePageNoUserJSON
     Just user -> do
       mplan <- dbQuery $ GetPaymentPlan $ usercompany $ user
       case (mplan, ppPaymentPlanProvider $ $fromJust mplan) of
          (Nothing  , _)               -> handlePricePageUserJSON user
          (Just plan, NoProvider)      -> handlePricePageUserPlanNoneJSON user plan
          (Just plan, RecurlyProvider) -> handlePricePageUserPlanRecurlyJSON user plan

handleUserExists :: Kontrakcja m => m JSValue
handleUserExists = do
  email <- pguardM' "handleUserExists: email must exist." $
           getField "email"
  muser <- dbQuery $ GetUserByEmail $ Email email
  mplan <- maybe (return Nothing) (dbQuery . GetPaymentPlan) (usercompany <$> muser)
  runJSONGenT $ do
    J.value "user_exists" $ isJust muser
    J.value "has_plan"    $ isJust mplan
    J.value "is_admin"    $ maybe False useriscompanyadmin muser

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
  subscriptions <- pguardM "handleSyncNewSubscriptionWithRecurlyOutside" $
                   liftIO $ getSubscriptionsForAccount curl_exe recurlyAPIKey $ show ac
  subscription <- pguard' "handleSyncNewSubscriptionWithRecurlyOutside: No subscription." $
                  listToMaybe subscriptions
  invoices <- pguardM "handleSyncNewSubscriptionWithRecurlyOutside: no invoices" $
              liftIO $ getInvoicesForAccount curl_exe recurlyAPIKey $ show ac
  let is = maybe "collected" inState $ listToMaybe invoices
  _ <- cachePlan time Stats.SignupAction ac subscription is (usercompany user) Nothing Nothing
  return ()

-- mails

sendInvoiceEmail :: Kontrakcja m => User -> Company -> Subscription -> m ()
sendInvoiceEmail user company subscription = do
  ctx <- getContext
  bd <- dbQuery $ GetBrandedDomainByUserID (userid user)
  theme <- dbQuery $ GetTheme (bdMailTheme bd)
  mail <- runTemplatesT (lang $ usersettings user, ctxglobaltemplates ctx) $ mailSignup bd theme (ctxhostpart ctx) user company subscription
  scheduleEmailSendout (ctxmailsconfig ctx)
                        (mail{to = [MailAddress{
                                     fullname = getFullName user
                                   , email = getEmail user }]})

sendInvoiceFailedEmail :: (MonadDB m, MonadThrow m, Log.MonadLog m, CryptoRNG m) => BrandedDomain -> String -> MailsConfig -> Lang -> KontrakcjaGlobalTemplates -> User -> Company -> Invoice -> m ()
sendInvoiceFailedEmail bd hostpart mailsconfig lang templates user company invoice = do
  theme <- dbQuery $ GetTheme (bdMailTheme bd)
  mail <- runTemplatesT (lang, templates) $ mailFailed bd theme hostpart user company invoice
  scheduleEmailSendout mailsconfig
    (mail{to = [MailAddress { fullname = getFullName user
                            , email = getEmail user}]})

sendExpiredEmail :: Kontrakcja m => User -> m ()
sendExpiredEmail user = do
  ctx <- getContext
  bd <- dbQuery $ GetBrandedDomainByUserID (userid user)
  theme <- dbQuery $ GetTheme (bdMailTheme bd)
  mail <- runTemplatesT (lang $ usersettings user, ctxglobaltemplates ctx) $ mailExpired bd theme (ctxhostpart ctx)
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
pguard :: (MonadBase IO m, Log.MonadLog m) => String -> Either String a -> m a
pguard _   (Right v) = return v
pguard pre (Left msg) = do
  Log.mixlog_ $ pre ++ ": " ++ msg
  internalError

pguardM :: (MonadBase IO m, Log.MonadLog m) => String -> m (Either String a) -> m a
pguardM pre action = pguard pre =<< action

pguard' :: (MonadBase IO m, Log.MonadLog m) => String -> Maybe a -> m a
pguard' _ (Just v) = return v
pguard' msg Nothing = do
  Log.mixlog_ msg
  internalError

pguardM' :: (MonadBase IO m, Log.MonadLog m) => String -> m (Maybe a) -> m a
pguardM' msg action = pguard' msg =<< action
