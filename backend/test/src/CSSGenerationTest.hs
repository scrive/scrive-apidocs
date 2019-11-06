module CSSGenerationTest(
    cssGenerationTests
) where

import Test.Framework
import qualified Data.ByteString.Lazy as BSL

import Branding.CSS
import DB
import TestingUtil
import TestKontra as T
import Theme.Model
import User.Lang (defaultLang)

cssGenerationTests :: TestEnvSt -> Test
cssGenerationTests env = testGroup
  "CSSGeneration"
  [ testThat "Signview branding generation" env testSignviewBrandingGeneration
  , testThat "Service  branding generation" env testServiceBrandingGeneration
  , testThat "Login  branding generation"   env testLoginBrandingGeneration
  , testThat "Domain  branding generation"  env testDomainBrandingGeneration
  ]

testSignviewBrandingGeneration :: TestEnv ()
testSignviewBrandingGeneration = do
  bd               <- view #brandedDomain <$> mkContext defaultLang
  theme            <- dbQuery $ GetTheme $ bd ^. #signviewTheme
  emptyBrandingCSS <- signviewBrandingCSS theme
  assertBool "CSS generated for signview branding is not empty"
             (not $ BSL.null $ emptyBrandingCSS)

testServiceBrandingGeneration :: TestEnv ()
testServiceBrandingGeneration = do
  bd               <- view #brandedDomain <$> mkContext defaultLang
  theme            <- dbQuery $ GetTheme $ bd ^. #serviceTheme
  emptyBrandingCSS <- serviceBrandingCSS theme
  assertBool "CSS generated for service branding is not empty"
             (not $ BSL.null $ emptyBrandingCSS)

testLoginBrandingGeneration :: TestEnv ()
testLoginBrandingGeneration = do
  bd               <- view #brandedDomain <$> mkContext defaultLang
  theme            <- dbQuery $ GetTheme $ bd ^. #loginTheme
  emptyBrandingCSS <- loginBrandingCSS theme
  assertBool "CSS generated for login branding is not empty"
             (not $ BSL.null $ emptyBrandingCSS)

testDomainBrandingGeneration :: TestEnv ()
testDomainBrandingGeneration = do
  bd               <- view #brandedDomain <$> mkContext defaultLang
  emptyBrandingCSS <- domainBrandingCSS bd
  assertBool "CSS generated for domain branding is not empty"
             (not $ BSL.null $ emptyBrandingCSS)

