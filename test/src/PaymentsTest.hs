module PaymentsTest (paymentsTests
                    ) where

import DB
import User.Model
import TestingUtil
import TestKontra
import Company.Model
import MinutesTime

import Data.Maybe
import Control.Monad
import Test.Framework
import Control.Monad.Trans

import CompanyAccounts.Model
import Util.HasSomeUserInfo

--import Utils.Either

import Payments.Model
import Payments.Control

import Recurly

paymentsTests  :: TestEnvSt -> Test
paymentsTests env = testGroup "Payments" [
    testThat "Company with no users has 0 quantity" env testNewCompanyNoUserZeroQuantity
  , testThat "Company quantity works with free user" env testCompanyQuantity
  , testThat "SavePaymentPlan can be read in" env testSavePaymentPlan
  , testThat "PaymentPlansRequiringSync conditions are correct" env testPaymentPlansRequiringSync
  , testThat "PaymentPlansRequiringSyncNoProvider conditions are correct" env testPaymentPlansNoProviderRequiringSync    
  , testThat "PaymentPlansExpiredDunning conditions are correct" env testPaymentPlansExpiredDunning
  , testThat "GetPaymentPlanInactiveUser works for inactive users" env testPaymentPlanInactiveUser
  , testThat "GetPaymentPlanInactiveUser does not work for active users" env testPaymentPlanInactiveUserActive
  , testGroup "Recurly" [
      testThat "Recurly saves account properly" env testRecurlySavesAccount
     ,testThat "Recurly changes account properly" env testRecurlyChangeAccount     
     ,testThat "Recurly changes account properly on defer" env testRecurlyChangeAccountDefer
     ,testThat "Recurly cancels and reactivates properly" env testRecurlyCancelReactivate
    ]
  ]


testNewCompanyNoUserZeroQuantity :: TestEnv ()
testNewCompanyNoUserZeroQuantity = do
  company <- addNewCompany
  q <- dbQuery $ GetCompanyQuantity (companyid company)
  assert $ q == 0
  
testCompanyQuantity :: TestEnv ()
testCompanyQuantity = do
  company <- addNewCompany
  u1 <- addNewRandomUser
  u2 <- addNewRandomUser
  u3 <- addNewRandomUser
  u4 <- addNewRandomUser
  forM_ [u1,u2,u3,u4] $ \u -> dbUpdate $ SetUserCompany (userid u) (Just (companyid company))
  _ <- dbUpdate $ SetUserIsFree (userid u4) True
  q <- dbQuery $ GetCompanyQuantity (companyid company)
  assert $ q == 3
  
testSavePaymentPlan :: TestEnv ()
testSavePaymentPlan = do
  user <- addNewRandomUser
  ac <- dbUpdate $ GetAccountCode
  time <- getMinutesTime
  let pp = PaymentPlan { ppAccountCode = ac
                       , ppID = Left (userid user)
                       , ppPricePlan = TeamPricePlan
                       , ppPendingPricePlan = TeamPricePlan
                       , ppStatus = ActiveStatus
                       , ppPendingStatus = ActiveStatus
                       , ppQuantity = 1
                       , ppPendingQuantity = 1
                       , ppPaymentPlanProvider = NoProvider
                       , ppDunningStep = Nothing
                       , ppDunningDate = Nothing
                       , ppBillingEndDate = time
                       }
  b <- dbUpdate $ SavePaymentPlan pp time
  assert b
  mpp <- dbQuery $ GetPaymentPlan (Left $ userid user)
  assert $ Just pp == mpp
  mpp' <- dbQuery $ GetPaymentPlanByAccountCode ac
  assert $ Just pp == mpp'
  -- save it again!
  let pp' = pp { ppQuantity = 5 }
  b' <- dbUpdate $ SavePaymentPlan pp' time
  assert b'
  mpp2 <- dbQuery $ GetPaymentPlan (Left $ userid user)
  assert $ Just pp' == mpp2
  mpp2' <- dbQuery $ GetPaymentPlanByAccountCode ac
  assert $ Just pp' == mpp2'

testPaymentPlansRequiringSync :: TestEnv ()
testPaymentPlansRequiringSync = do
  now <- getMinutesTime
  let past = daysBefore 12 now
  forM_ [False, True] $ \c ->
    forM_ [4, 10, 20] $ \q ->
    forM_ [q, 2, 10] $ \pq ->
    forM_ [now, past] $ \sync->
    forM_ [NoProvider, RecurlyProvider] $ \prov ->
      if c 
        then do
        company <- addNewCompany
        forM_ [0..q] $ \_-> do
          user <- addNewRandomUser
          dbUpdate $ SetUserCompany (userid user) (Just $ companyid company)
        ac <- dbUpdate $ GetAccountCode
        let pp = PaymentPlan { ppAccountCode = ac
                             , ppID = Right (companyid company)
                             , ppPricePlan = TeamPricePlan
                             , ppPendingPricePlan = TeamPricePlan
                             , ppStatus = ActiveStatus
                             , ppPendingStatus = ActiveStatus
                             , ppQuantity = 10
                             , ppPendingQuantity = pq
                             , ppPaymentPlanProvider = prov
                             , ppDunningStep = Nothing
                             , ppDunningDate = Nothing
                             , ppBillingEndDate = now
                             }
        _ <- dbUpdate $ SavePaymentPlan pp sync
        return ()
      else do
        user <- addNewRandomUser
        ac <- dbUpdate $ GetAccountCode
        let pp = PaymentPlan { ppAccountCode = ac
                             , ppID = Left (userid user)
                             , ppPricePlan = TeamPricePlan
                             , ppPendingPricePlan = TeamPricePlan
                             , ppStatus = ActiveStatus
                             , ppPendingStatus = ActiveStatus
                             , ppQuantity = 1
                             , ppPendingQuantity = 1
                             , ppPaymentPlanProvider = prov
                             , ppDunningStep = Nothing
                             , ppDunningDate = Nothing
                             , ppBillingEndDate = now
                             }
        _ <- dbUpdate $ SavePaymentPlan pp sync
        return ()
  rs <- dbQuery $ PaymentPlansRequiringSync RecurlyProvider now
  assert $ length rs == 27 -- hand calculated

testPaymentPlansNoProviderRequiringSync :: TestEnv ()
testPaymentPlansNoProviderRequiringSync = do
  now <- getMinutesTime
  company <- addNewCompany
  forM_ [0..9] $ \(i::Int) -> do
    user <- addNewRandomUser
    _ <- dbUpdate $ SetUserCompany (userid user) (Just $ companyid company)
    when (i < 5) $ do
      _ <- dbUpdate $ AddCompanyInvite $ CompanyInvite {invitedemail = Email $ getEmail user
                                                       ,invitedfstname = getFirstName user
                                                       ,invitedsndname = getLastName user
                                                       ,invitingcompany = companyid company}
      return ()
  forM_ [0..4] $ \(i::Int) -> do
    _ <- dbUpdate $ AddCompanyInvite $ CompanyInvite {invitedemail = Email $ "a" ++ show i ++ "@poo.com"
                                                     ,invitedfstname = "F" ++ show i
                                                     ,invitedsndname = "L" ++ show i
                                                     ,invitingcompany = companyid company}
    return ()
  ac <- dbUpdate $ GetAccountCode
  let pp = PaymentPlan { ppAccountCode         = ac
                       , ppID                  = Right (companyid company)
                       , ppPricePlan           = TeamPricePlan
                       , ppPendingPricePlan    = TeamPricePlan
                       , ppStatus              = ActiveStatus
                       , ppPendingStatus       = ActiveStatus
                       , ppQuantity            = 10
                       , ppPendingQuantity     = 10
                       , ppPaymentPlanProvider = NoProvider
                       , ppDunningStep         = Nothing
                       , ppDunningDate         = Nothing
                       , ppBillingEndDate      = now
                       }
  _ <- dbUpdate $ SavePaymentPlan pp now
  rs <- dbQuery $ PaymentPlansRequiringSync NoProvider now
  assert $ length rs == 1
  q <- dbQuery $ GetCompanyQuantity (companyid company)
  assert $ q == 15
  handleSyncNoProvider now
  rs' <- dbQuery $ PaymentPlansRequiringSync NoProvider now
  assert $ length rs' == 0
  
testPaymentPlansExpiredDunning :: TestEnv ()
testPaymentPlansExpiredDunning = do
  now <- getMinutesTime
  let past = daysBefore 12 now
  forM_ [NoProvider, RecurlyProvider] $ \prov ->
    forM_ [Just now, Just past, Nothing] $ \dun -> do
      user <- addNewRandomUser
      ac <- dbUpdate $ GetAccountCode
      let pp = PaymentPlan { ppAccountCode = ac
                           , ppID = Left (userid user)
                           , ppPricePlan = TeamPricePlan
                           , ppPendingPricePlan = TeamPricePlan
                           , ppStatus = ActiveStatus
                           , ppPendingStatus = ActiveStatus
                           , ppQuantity = 1
                           , ppPendingQuantity = 1
                           , ppPaymentPlanProvider = prov
                           , ppDunningStep = if isJust dun then Just 1 else Nothing
                           , ppDunningDate = dun
                           , ppBillingEndDate = now
                           }
      _ <- dbUpdate $ SavePaymentPlan pp now
      return ()
  rs <- dbQuery $ PaymentPlansExpiredDunning now
  assert $ length rs == 1
  
testPaymentPlanInactiveUser :: TestEnv ()
testPaymentPlanInactiveUser = do
  user <- addNewRandomUser
  assertBool "User should not have accepted terms of service." (isNothing $ userhasacceptedtermsofservice user)
  ac <- dbUpdate $ GetAccountCode
  time <- getMinutesTime
  let pp = PaymentPlan { ppAccountCode = ac
                       , ppID = Left (userid user)
                       , ppPricePlan = TeamPricePlan
                       , ppPendingPricePlan = TeamPricePlan
                       , ppStatus = ActiveStatus
                       , ppPendingStatus = ActiveStatus
                       , ppQuantity = 1
                       , ppPendingQuantity = 1
                       , ppPaymentPlanProvider = NoProvider
                       , ppDunningStep = Nothing
                       , ppDunningDate = Nothing
                       , ppBillingEndDate = time
                       }
  b <- dbUpdate $ SavePaymentPlan pp time
  assert b
  mpp <- dbQuery $ GetPaymentPlanInactiveUser (userid user)
  assert $ Just pp == mpp
  
testPaymentPlanInactiveUserActive :: TestEnv ()
testPaymentPlanInactiveUserActive = do
  user <- addNewRandomUser
  time <- getMinutesTime
  _ <- dbUpdate $ AcceptTermsOfService (userid user) time
  ac <- dbUpdate $ GetAccountCode

  let pp = PaymentPlan { ppAccountCode = ac
                       , ppID = Left (userid user)
                       , ppPricePlan = TeamPricePlan
                       , ppPendingPricePlan = TeamPricePlan
                       , ppStatus = ActiveStatus
                       , ppPendingStatus = ActiveStatus
                       , ppQuantity = 1
                       , ppPendingQuantity = 1
                       , ppPaymentPlanProvider = NoProvider
                       , ppDunningStep = Nothing
                       , ppDunningDate = Nothing
                       , ppBillingEndDate = time
                       }
  b <- dbUpdate $ SavePaymentPlan pp time
  assert b
  mpp <- dbQuery $ GetPaymentPlanInactiveUser (userid user)
  assert $ mpp == Nothing

testRecurlySavesAccount :: TestEnv ()
testRecurlySavesAccount = do
  t <- getMinutesTime
  let ac = show $ toSeconds t -- generate a random account code to avoid collisions
      email = "eric+" ++ ac ++ "@scrive.com"

  cs <- liftIO $ createSubscription "curl" "c31afaf14af3457895ee93e7e08e4451" "pay" "SEK" ac email "Eric" "Normand" "4111111111111111" "09" "2020" 5

  assertRight cs

  gs <- liftIO $ getSubscriptionsForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac
  assertRight gs
  case gs of
    Right [sub] -> do
      assertBool "quantity should be equal" $ subQuantity sub == 5
      assertBool "currency should be equal" $ subCurrency sub == "SEK"
      is <- liftIO $ getInvoicesForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac

      assertRight is

      case is of
        Right [inv] -> do
          assertBool "currency should be equal" $ inCurrency inv == "SEK"
          assertBool "total should be right" $ inTotalInCents inv == 5 * 29900
        _ -> assertBool "Should have one invoice" False

    _ -> assertBool "should only have one subscription" False

testRecurlyChangeAccount :: TestEnv ()
testRecurlyChangeAccount = do
  t <- getMinutesTime
  let ac = show $ toSeconds t -- generate a random account code to avoid collisions
      email = "eric+" ++ ac ++ "@scrive.com"

  cs <- liftIO $ createSubscription "curl" "c31afaf14af3457895ee93e7e08e4451" "pay" "SEK" ac email "Eric" "Normand" "4111111111111111" "09" "2020" 5
  assertRight cs

  gs <- liftIO $ getSubscriptionsForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac
  assertRight gs

  let Right [sub] = gs
  cc <- liftIO $ changeAccount "curl" "c31afaf14af3457895ee93e7e08e4451" (subID sub) "pay" 100 True
  assertRight cc

  gs2 <- liftIO $ getSubscriptionsForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac
  assertRight gs2

  case gs2 of
    Right [sub2] -> do
      assertBool "quantity should be equal" $ subQuantity sub2 == 100
      assertBool "currency should be equal" $ subCurrency sub2 == "SEK"
      is <- liftIO $ getInvoicesForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac

      assertRight is
      case is of
        Right invoices@(_:_) -> do
          let total = sum [inTotalInCents x | x <- invoices]
          assertBool "currency should be equal" $ inCurrency (head invoices) == "SEK"
          assertBool "total should be right" $ total == 100 * 29900
        _ -> assertBool "Should have some invoices" False

    _ -> assertBool "should have 1 subscription" False
  
testRecurlyChangeAccountDefer :: TestEnv ()
testRecurlyChangeAccountDefer = do
  t <- getMinutesTime
  let ac = show $ toSeconds t -- generate a random account code to avoid collisions
      email = "eric+" ++ ac ++ "@scrive.com"

  cs <- liftIO $ createSubscription "curl" "c31afaf14af3457895ee93e7e08e4451" "pay" "SEK" ac email "Eric" "Normand" "4111111111111111" "09" "2020" 5

  assertRight cs


  gs <- liftIO $ getSubscriptionsForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac
  assertRight gs

  let Right [sub] = gs
  cc <- liftIO $ changeAccount "curl" "c31afaf14af3457895ee93e7e08e4451" (subID sub) "pay" 100 False
  assertRight cc

  gs2 <- liftIO $ getSubscriptionsForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac
  assertRight gs2

  case gs2 of
    Right [sub2] -> do
      assertBool "quantity should be equal" $ subQuantity sub2 == 5
      assertBool "currency should be equal" $ subCurrency sub2 == "SEK"

      assertBool "Should have pending subscription" $ isJust $ subPending sub2
      let Just p = subPending sub2
      assertBool "pending quantity should be right" $ penQuantity p == 100

      is <- liftIO $ getInvoicesForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac

      assertRight is
      case is of
        Right invoices@(_:_) -> do
          let total = sum [inTotalInCents x | x <- invoices]
          assertBool "currency should be equal" $ inCurrency (head invoices) == "SEK"
          assertBool "total should be right" $ total == 5 * 29900

        _ -> assertBool "Should have some invoices" False

    _ -> assertBool "should have 1 subscription" False
  
testRecurlyCancelReactivate :: TestEnv ()
testRecurlyCancelReactivate = do
  t <- getMinutesTime
  let ac = show $ toSeconds t -- generate a random account code to avoid collisions
      email = "eric+" ++ ac ++ "@scrive.com"
  
  cs <- liftIO $ createSubscription "curl" "c31afaf14af3457895ee93e7e08e4451" "pay" "SEK" ac email "Eric" "Normand" "4111111111111111" "09" "2020" 5
  assertRight cs

  gs <- liftIO $ getSubscriptionsForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac
  assertRight gs

  let Right [sub] = gs

  cancel <- liftIO $ cancelSubscription "curl" "c31afaf14af3457895ee93e7e08e4451" (subID sub)
  assertRight cancel

  gs2 <- liftIO $ getSubscriptionsForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac
  assertRight gs2

  let Right [sub2] = gs2

  assertBool ("Should be 'canceled', was '" ++ subState sub2 ++ "'") $ subState sub2 == "canceled"
  
  react <- liftIO $ reactivateSubscription "curl" "c31afaf14af3457895ee93e7e08e4451" (subID sub)
  assertRight react

  gs3 <- liftIO $ getSubscriptionsForAccount "curl" "c31afaf14af3457895ee93e7e08e4451" ac
  assertRight gs3

  let Right [sub3] = gs3

  assertBool ("Should be 'active', was '" ++ subState sub3 ++ "'") $ subState sub3 == "active"

  