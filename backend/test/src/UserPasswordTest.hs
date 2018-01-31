module UserPasswordTest(userPasswordTests) where

import Test.Framework

import TestingUtil
import TestKontra as T
import User.Password

userPasswordTests :: TestEnvSt -> Test
userPasswordTests env = testGroup "UserPassword"
  [ testThat "Test that Password verifies correctly"
    env testPasswordCreateVerify
  , testThat "Test that maybeMkPassword works and verifies"
    env testPasswordMaybeVerify
  , testThat "Test that strengthenPassword works and verifies"
    env testStrengthenPassword
  , testThat "Old passwords can be verified with new implementation"
    env testLegacyPasswordCreateVerify
  ]

-- * Password Tests

testPasswordCreateVerify :: TestEnv ()
testPasswordCreateVerify = do
  s1 <- randomPasswordString
  p1 <- createPassword s1
  assert $ verifyPassword p1 s1
  assert $ not $ verifyPassword p1 ""

  s2 <- randomPasswordString
  p2 <- createPassword s2
  assert $ verifyPassword p2 s2
  assert $ not $ verifyPassword p2 ""

  assert $ not $ verifyPassword p1 s2
  assert $ not $ verifyPassword p2 s1

testPasswordMaybeVerify :: TestEnv ()
testPasswordMaybeVerify = do
  s <- randomPasswordString
  assert $ not $ maybeVerifyPassword Nothing s
  p <- createPassword s
  let triplet = (Just $ pwdHash p, Just $ pwdSalt p, Just $ pwdAlgorithm p)
      mpass = maybeMkPassword triplet
  assertJust mpass
  assert $ maybeVerifyPassword mpass s

testStrengthenPassword :: TestEnv ()
testStrengthenPassword = do
  s      <- randomPasswordString
  wrong  <- randomPasswordString
  legacy <- createLegacyPassword s
  assert $ verifyPassword legacy s
  assert $ not $ verifyPassword legacy wrong

  pass <- strengthenPassword legacy
  assert $ verifyPassword pass s
  assert $ not $ verifyPassword pass wrong

testLegacyPasswordCreateVerify :: TestEnv ()
testLegacyPasswordCreateVerify = replicateM_ 10 $ do
  s <- randomPasswordString
  wrong <- randomPasswordString
  p <- createLegacyPassword s
  let Just pass = maybeMkPassword ( Just $ pwdHash p, Just $ pwdSalt p
                                  , Just $ int16ToPwdAlgorithm 0)
  assert $ verifyPassword pass s
  assert $ not $ verifyPassword pass wrong
