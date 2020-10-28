module UserPasswordTest(userPasswordTests) where

import Test.Framework

import TestingUtil
import TestKontra as T
import User.Password

userPasswordTests :: TestEnvSt -> Test
userPasswordTests env = testGroup
  "UserPassword"
  [ testThat "Test that Password verifies correctly"        env testPasswordCreateVerify
  , testThat "Test that maybeMkPassword works and verifies" env testPasswordMaybeVerify
  ]

-- * Password Tests

testPasswordCreateVerify :: TestEnv ()
testPasswordCreateVerify = do
  s1 <- randomPasswordString
  p1 <- createPassword s1
  assert $ verifyPassword p1 s1
  assert . not $ verifyPassword p1 ""

  s2 <- randomPasswordString
  p2 <- createPassword s2
  assert $ verifyPassword p2 s2
  assert . not $ verifyPassword p2 ""

  assert . not $ verifyPassword p1 s2
  assert . not $ verifyPassword p2 s1

testPasswordMaybeVerify :: TestEnv ()
testPasswordMaybeVerify = do
  s <- randomPasswordString
  assert . not $ maybeVerifyPassword Nothing s
  p <- createPassword s
  let mpass =
        maybeMkPassword (Just $ pwdHash p) (Just $ pwdSalt p) (Just $ pwdAlgorithm p)
  assertJust'_ mpass
  assert $ maybeVerifyPassword mpass s
