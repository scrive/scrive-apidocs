module PaymentsTest (paymentsTests
                    ) where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Recurly
import Test.Framework

import Company.Model
import CompanyAccounts.Model
import DB
import MinutesTime
import Payments.Control
import Payments.Model
import TestingUtil
import TestKontra

{- About this test

It requires a real recurly account to be there. Access is controled with recurlyToken.
Current test account settings:
  mariusz+recurly@scrive.com / admin123

Test account requirements:
  1) One payment plan named "team" with price set to 299 SEK
  2) Currency set to SEK
  3) No address required. This can be set Site Settings / Address Requirement

Note that failure to set any of the above can result in strange test errors.


-}

recurlyToken :: String
recurlyToken =  "7f780028adce4ca0a0e043f93487e074"

recurlyPlanName :: String
recurlyPlanName ="team"

paymentsTests  :: TestEnvSt -> Test
paymentsTests env = testGroup "Payments" [
    testThat "Company with no users has 0 quantity" env testNewCompanyNoUserZeroQuantity
  , testThat "SavePaymentPlan can be read in" env testSavePaymentPlan
  , testThat "PaymentPlansRequiringSync conditions are correct" env testPaymentPlansRequiringSync
  , testThat "PaymentPlansRequiringSyncNoProvider conditions are correct" env testPaymentPlansNoProviderRequiringSync
  , testThat "PaymentPlansExpiredDunning conditions are correct" env testPaymentPlansExpiredDunning
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


testSavePaymentPlan :: TestEnv ()
testSavePaymentPlan = do
  user <- addNewRandomUser
  ac <- dbUpdate $ GetAccountCode
  time <- currentTime
  let pp = PaymentPlan { ppAccountCode = ac
                       , ppCompanyID = (usercompany user)
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
  mpp <- dbQuery $ GetPaymentPlan (usercompany user)
  assert $ Just pp == mpp
  mpp' <- dbQuery $ GetPaymentPlanByAccountCode ac
  assert $ Just pp == mpp'
  -- save it again!
  let pp' = pp { ppQuantity = 5 }
  b' <- dbUpdate $ SavePaymentPlan pp' time
  assert b'
  mpp2 <- dbQuery $ GetPaymentPlan (usercompany user)
  assert $ Just pp' == mpp2
  mpp2' <- dbQuery $ GetPaymentPlanByAccountCode ac
  assert $ Just pp' == mpp2'

testPaymentPlansRequiringSync :: TestEnv ()
testPaymentPlansRequiringSync = do
  now <- currentTime
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
          dbUpdate $ SetUserCompany (userid user) (companyid company)
        ac <- dbUpdate $ GetAccountCode
        let pp = PaymentPlan { ppAccountCode = ac
                             , ppCompanyID = companyid company
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
                             , ppCompanyID = (usercompany user)
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
  now <- currentTime
  company <- addNewCompany
  forM_ [0..9] $ \(i::Int) -> do
    user <- addNewRandomUser
    _ <- dbUpdate $ SetUserCompany (userid user) (companyid company)
    when (i < 5) $ do
      _ <- dbUpdate $ AddCompanyInvite $ CompanyInvite {inviteduserid =  (userid user)
                                                       ,invitingcompany = companyid company}
      return ()
  forM_ [0..4] $ \_ -> do
    user <- addNewRandomUser
    _ <- dbUpdate $ AddCompanyInvite $ CompanyInvite {inviteduserid =  (userid user)
                                                     ,invitingcompany = companyid company}
    return ()
  ac <- dbUpdate $ GetAccountCode
  let pp = PaymentPlan { ppAccountCode         = ac
                       , ppCompanyID                  = companyid company
                       , ppPricePlan           = TeamPricePlan
                       , ppPendingPricePlan    = TeamPricePlan
                       , ppStatus              = ActiveStatus
                       , ppPendingStatus       = ActiveStatus
                       , ppQuantity            = 6
                       , ppPendingQuantity     = 6
                       , ppPaymentPlanProvider = NoProvider
                       , ppDunningStep         = Nothing
                       , ppDunningDate         = Nothing
                       , ppBillingEndDate      = now
                       }
  _ <- dbUpdate $ SavePaymentPlan pp now
  rs <- dbQuery $ PaymentPlansRequiringSync NoProvider now
  assert $ length rs == 1
  q <- dbQuery $ GetCompanyQuantity (companyid company)
  assert $ q == 10 -- We count only users that actually are in company | Skip invites
  handleSyncNoProvider now
  rs' <- dbQuery $ PaymentPlansRequiringSync NoProvider now
  assert $ length rs' == 0

testPaymentPlansExpiredDunning :: TestEnv ()
testPaymentPlansExpiredDunning = do
  now <- currentTime
  let past = daysBefore 12 now
  forM_ [NoProvider, RecurlyProvider] $ \prov ->
    forM_ [Just now, Just past, Nothing] $ \dun -> do
      user <- addNewRandomUser
      ac <- dbUpdate $ GetAccountCode
      let pp = PaymentPlan { ppAccountCode = ac
                           , ppCompanyID = (usercompany user)
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

testRecurlySavesAccount :: TestEnv ()
testRecurlySavesAccount = do
  t <- currentTime
  let ac = getEmailId t -- generate a random account code to avoid collisions
      email = "eric+" ++ ac ++ "@scrive.com"

  cs <- liftIO $ createSubscription "curl" recurlyToken recurlyPlanName "SEK" ac email "Eric" "Normand" "4111111111111111" "09" "2020" "Adress line 1" "Stockholm" "Sweden" "12-345" 5
  assertRight cs

  gs <- liftIO $ getSubscriptionsForAccount "curl" recurlyToken ac
  assertRight gs

  case gs of
    Right [sub] -> do
      assertBool "quantity should be equal" $ subQuantity sub == 5
      assertBool "currency should be equal" $ subCurrency sub == "SEK"
      is <- liftIO $ getInvoicesForAccount "curl" recurlyToken ac

      assertRight is

      case is of
        Right [inv] -> do
          assertBool "currency should be equal" $ inCurrency inv == "SEK"
          assertBool "total should be right" $ inTotalInCents inv == 5 * 29900
        _ -> assertBool "Should have one invoice" False

    _ -> assertBool "should only have one subscription" False

testRecurlyChangeAccount :: TestEnv ()
testRecurlyChangeAccount = do
  t <- currentTime
  let ac = getEmailId t -- generate a random account code to avoid collisions
      email = "eric+" ++ ac ++ "@scrive.com"

  cs <- liftIO $ createSubscription "curl" recurlyToken recurlyPlanName "SEK" ac email "Eric" "Normand" "4111111111111111" "09" "2020" "Adress line 1" "Stockholm" "Sweden" "12-345" 5
  assertRight cs

  gs <- liftIO $ getSubscriptionsForAccount "curl" recurlyToken ac
  assertRight gs

  let Right [sub] = gs
  cc <- liftIO $ changeAccount "curl" recurlyToken (subID sub) recurlyPlanName 100 True
  assertRight cc

  gs2 <- liftIO $ getSubscriptionsForAccount "curl" recurlyToken ac
  assertRight gs2

  case gs2 of
    Right [sub2] -> do
      assertBool "quantity should be equal" $ subQuantity sub2 == 100
      assertBool "currency should be equal" $ subCurrency sub2 == "SEK"
      is <- liftIO $ getInvoicesForAccount "curl" recurlyToken ac

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
  t <- currentTime
  let ac = getEmailId t -- generate a random account code to avoid collisions
      email = "eric+" ++ ac ++ "@scrive.com"

  cs <- liftIO $ createSubscription "curl" recurlyToken recurlyPlanName "SEK" ac email "Eric" "Normand" "4111111111111111" "09" "2020" "Adress line 1" "Stockholm" "Sweden" "12-345" 5

  assertRight cs


  gs <- liftIO $ getSubscriptionsForAccount "curl" recurlyToken ac
  assertRight gs

  let Right [sub] = gs
  cc <- liftIO $ changeAccount "curl" recurlyToken (subID sub) recurlyPlanName 100 False
  assertRight cc

  gs2 <- liftIO $ getSubscriptionsForAccount "curl" recurlyToken ac
  assertRight gs2

  case gs2 of
    Right [sub2] -> do
      assertBool "quantity should be equal" $ subQuantity sub2 == 5
      assertBool "currency should be equal" $ subCurrency sub2 == "SEK"

      assertBool "Should have pending subscription" $ isJust $ subPending sub2
      let Just p = subPending sub2
      assertBool "pending quantity should be right" $ penQuantity p == 100

      is <- liftIO $ getInvoicesForAccount "curl" recurlyToken ac

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
  t <- currentTime
  let ac = getEmailId t -- generate a random account code to avoid collisions
      email = "eric+" ++ ac ++ "@scrive.com"

  cs <- liftIO $ createSubscription "curl" recurlyToken recurlyPlanName "SEK" ac email "Eric" "Normand" "4111111111111111" "09" "2020" "Adress line 1" "Stockholm" "Sweden" "12-345" 5
  assertRight cs

  gs <- liftIO $ getSubscriptionsForAccount "curl" recurlyToken ac
  assertRight gs

  let Right [sub] = gs

  cancel <- liftIO $ cancelSubscription "curl" recurlyToken (subID sub)
  assertRight cancel

  gs2 <- liftIO $ getSubscriptionsForAccount "curl" recurlyToken ac
  assertRight gs2

  let Right [sub2] = gs2

  assertBool ("Should be 'canceled', was '" ++ subState sub2 ++ "'") $ subState sub2 == "canceled"

  react <- liftIO $ reactivateSubscription "curl" recurlyToken (subID sub)
  assertRight react

  gs3 <- liftIO $ getSubscriptionsForAccount "curl" recurlyToken ac
  assertRight gs3

  let Right [sub3] = gs3

  assertBool ("Should be 'active', was '" ++ subState sub3 ++ "'") $ subState sub3 == "active"

getEmailId :: UTCTime -> String
getEmailId = formatTime' "%H%M%S%q"
