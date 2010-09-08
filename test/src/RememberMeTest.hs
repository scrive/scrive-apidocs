module RememberMeTest(
    rememberMeTests
) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import User
import UserState

rememberMeTests :: [Test]
rememberMeTests = [testCase "valid remember me cookie makes a serialize/deserialize round trip" cookieRoundTrip]

cookieRoundTrip = do
    cookieString <- createRememberMeCookie (UserID {unUserID = 1}) True
    let Just (RememberMe identity longTerm expiry _ _) = readRememberMeCookie cookieString
    assertEqual "user ID is preserved" 1 (unUserID identity)
    
