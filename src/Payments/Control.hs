{-# OPTIONS_GHC -fno-warn-orphans #-}
module Payments.Control where

import Control.Monad.State
--import Data.Convertible
import Data.Functor
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
import Payments.View
import Payments.Config (RecurlyConfig(..))

-- these should go to config
--recurlyApiKey :: String
--recurlyApiKey = "efab03eb45ca4666bc907b81c1dc7efc"

--recurlyPrivateKey :: String
--recurlyPrivateKey = "10e6c073926c402f9e89a8f8c4d14082"

{-
recurlyUsername :: User -> String
recurlyUsername user = maybe ("U" ++ show (userid user)) ((++) "C" . show) $ usercompany user
-}

handleSubscriptionDashboard :: Kontrakcja m => m (Either KontraLink Response)
handleSubscriptionDashboard = checkUserTOSGet $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  showSubscriptionDashboard user >>= renderFromBody kontrakcja
  
  
handleSubscriptionDashboardInfo :: Kontrakcja m => m (Either KontraLink JSValue)
handleSubscriptionDashboardInfo = checkUserTOSGet $ do
  wantedplan <- guardJustM $ getField "plan"
  user <- guardJustM $ ctxmaybeuser <$> getContext
  privateKey <- recurlyPrivateKey . ctxrecurlyconfig <$> getContext
  mcompany <- case usercompany user of
    Nothing -> return Nothing
    Just cid -> dbQuery $ GetCompany cid
  quantity <- case (usercompany user, useriscompanyadmin user) of
    (Just cid, True) -> length <$> (dbQuery $ GetCompanyAccounts cid)
    _ -> return 1
  mplan <- dbQuery $ GetPaymentPlan (maybe (Left (userid user)) Right (usercompany user))
  code <- case mplan of
    Nothing   -> dbUpdate GetAccountCode
    Just plan -> return $ ppAccountCode plan
  let currency = "SEK" -- for now, we only support euros in the test account
  basicsig    <- liftIO $ genSignature privateKey [("subscription[plan_code]",    "basic")]
  brandingsig <- liftIO $ genSignature privateKey [("subscription[plan_code]", "branding")]
  advancedsig <- liftIO $ genSignature privateKey [("subscription[plan_code]", "advanced")]
  billingsig  <- liftIO $ genSignature privateKey [("account[account_code]",    show code)]
  runJSONGenT $ do
    J.object "contact" $ do
      J.value "first_name"   $ getFirstName user
      J.value "last_name"    $ getLastName user
      J.value "email"        $ getEmail user
      J.value "company_name" $ getCompanyName (user, mcompany)
      J.value "country"      $ "SE"    
    J.object "server" $ do
      J.value "subdomain"    $ "scrive-test"
    J.object "account" $ do
      J.value "quantity"     $ quantity
      J.value "currency"     $ currency
      case mplan of
        Nothing   -> do
          J.value "code" $ show code
          J.value "plan" $ wantedplan
          J.value "status" "none"
          J.object "signatures" $ do
            J.value "basic"    basicsig
            J.value "branding" brandingsig
            J.value "advanced" advancedsig
        Just plan -> do
          J.value "code"   $ show $ ppAccountCode plan
          J.value "plan"   $ show $ ppPricePlan plan
          J.value "status" $ show $ ppStatus plan
          J.object "signatures" $ do
            J.value "billing"  billingsig          
          case plan of
            UserPaymentPlan    {} -> J.value "userid"    $ show $ ppUserID plan
            CompanyPaymentPlan {} -> J.value "companyid" $ show $ ppCompanyID plan
    
handleSubscriptionResult :: Kontrakcja m => m (Either KontraLink JSValue)
handleSubscriptionResult = checkUserTOSGet $ do
  let currency = "SEK" -- for now, we only support euros in the test account
  user <- guardJustM $ ctxmaybeuser <$> getContext
  mplan <- dbQuery $ GetPaymentPlan (maybe (Left (userid user)) Right (usercompany user))
  privateKey <- recurlyPrivateKey . ctxrecurlyconfig <$> getContext  
  case mplan of
    Just _ -> runJSONGenT $ J.value "error" "Already have a plan."
    Nothing -> do
      maccountCode <- readField "account_code"
      case maccountCode of
        Nothing -> runJSONGenT $ J.value "error" "Need an account code."
        Just ac -> do
          mplan' <- syncSubscriptionWithRecurly ac user
          case mplan' of
            Left  s    -> runJSONGenT $ J.value "error" $ "After sync, still no payment plan." ++ s
            Right plan -> do
              mcompany <- case usercompany user of
                Nothing -> return Nothing
                Just cid -> dbQuery $ GetCompany cid
              billingsig  <- liftIO $ genSignature privateKey [("account[account_code]", show ac)]
              quantity <- case (usercompany user, useriscompanyadmin user) of
                (Just cid, True) -> length <$> (dbQuery $ GetCompanyAccounts cid)
                _ -> return 1
              runJSONGenT $ do
                  J.object "contact" $ do
                    J.value "first_name"   $ getFirstName user
                    J.value "last_name"    $ getLastName user
                    J.value "email"        $ getEmail user
                    J.value "company_name" $ getCompanyName (user, mcompany)
                    J.value "country"      $ "SE"    
                  J.object "server" $ do
                    J.value "subdomain"    $ "scrive-test"
                  J.object "account" $ do
                    J.value "quantity"     $ quantity
                    J.value "currency"     $ currency
                    J.value "code"   $ show $ ppAccountCode plan
                    J.value "plan"   $ show $ ppPricePlan   plan
                    J.value "status" $ show $ ppStatus      plan
                    J.object "signatures" $ do
                      J.value "billing"  billingsig
                    case plan of
                      UserPaymentPlan    {} -> J.value "userid"    $ show $ ppUserID plan
                      CompanyPaymentPlan {} -> J.value "companyid" $ show $ ppCompanyID plan

handleGetInvoices :: Kontrakcja m => m (Either KontraLink JSValue)
handleGetInvoices = checkUserTOSGet $ do
  params <- getListParamsNew
  user <- guardJustM $ ctxmaybeuser <$> getContext
  recurlyApiKey <- recurlyAPIKey . ctxrecurlyconfig <$> getContext
  mplan <- dbQuery $ GetPaymentPlan (maybe (Left (userid user)) Right (usercompany user))
  case mplan of
    Nothing   -> runJSONGenT $ J.value "error" "No plan for logged in user."
    Just plan -> do
      einvoices <- liftIO $ getInvoicesForAccount curl_exe recurlyApiKey (show $ ppAccountCode plan)
      case einvoices of
        Left s -> runJSONGenT $ J.value "error" $ "Could not fetch invoices: " ++ s
        Right invoices -> runJSONGenT $ do
          J.value "list" $ for invoices $ \i -> runJSONGen $ J.value "fields" i
          J.value "paging" $ pagingParamsJSON $ PagedList { list       = invoices
                                                          , params     = params
                                                          , pageSize   = 12
                                                          }

-- to call this, user must not have an account code yet (no payment plan in table)
syncSubscriptionWithRecurly :: Kontrakcja m => AccountCode -> User -> m (Either String PaymentPlan)
syncSubscriptionWithRecurly ac u = do
  recurlyApiKey <- recurlyAPIKey . ctxrecurlyconfig <$> getContext
  esubscriptions <- liftIO $ getSubscriptionsForAccount curl_exe recurlyApiKey $ show ac
  case esubscriptions of
    Left s -> return $ Left s
    Right subscriptions ->
      case listToMaybe subscriptions of
        Nothing -> return $ Left "No subscriptions"
        Just s -> do
          -- just to be safe, if we get a strange price plan, we'll set them to advanced so we don't block
          let pricePlan = fromRecurlyPricePlan $ subPricePlan s
              status = fromRecurlyStatus $ subState s
          let pp = case usercompany u of
                Nothing -> UserPaymentPlan     ac (userid u) pricePlan status
                Just cid -> CompanyPaymentPlan ac cid        pricePlan status
          r <- dbUpdate $ SavePaymentPlan pp
          if r then return (Right pp) else return $ Left "Could not save subscription"

handleCancelAccount :: Kontrakcja m => m (Either KontraLink JSValue)
handleCancelAccount = checkUserTOSGet $ do
  recurlyApiKey <- recurlyAPIKey . ctxrecurlyconfig <$> getContext
  user <- guardJustM $ ctxmaybeuser <$> getContext
  mplan <- dbQuery $ GetPaymentPlan (maybe (Left (userid user)) Right (usercompany user))
  case mplan of
    Nothing -> runJSONGenT $ J.value "error" "No plan for logged in user."
    Just plan -> do
      res <- liftIO $ deleteAccount curl_exe recurlyApiKey $ show $ ppAccountCode plan
      case res of
        False -> runJSONGenT $ J.value "error" "Could not delete plan at Recurly."
        True  -> do
          _ <- dbUpdate $ DeletePaymentPlan $ maybe (Left $ userid user) Right $ usercompany user
          runJSONGenT $ J.value "success" "Deleted."
          
fromRecurlyStatus :: String -> PaymentPlanStatus
fromRecurlyStatus "active" = ActiveStatus
fromRecurlyStatus "expired" = InactiveStatus
fromRecurlyStatus _ = ActiveStatus

fromRecurlyPricePlan :: String -> PricePlan
fromRecurlyPricePlan "basic" = BasicPricePlan
fromRecurlyPricePlan "branded" = BrandingPricePlan
fromRecurlyPricePlan "advanced" = AdvancedPricePlan
fromRecurlyPricePlan _ = AdvancedPricePlan

instance ToJSValue Invoice where
  toJSValue (Invoice{..}) = runJSONGen $ do
    J.value "invoice_number" inNumber
    J.value "total_in_cents" inTotalInCents
    J.value "currency"       inCurrency
    J.value "state"          inState
    J.value "account_code"   inAccount
    J.value "date"           inDate