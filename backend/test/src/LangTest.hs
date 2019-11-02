module LangTest (langTests) where

import Happstack.Server
import Test.Framework

import AppControl
import BrandedDomain.Model
import DB
import Doc.DocStateData
import Doc.Model
import Kontra (KontraG(..))
import Login
import TestingUtil
import TestKontra as T
import User.Lang
import User.Model

langTests :: TestEnvSt -> Test
langTests env = testGroup
  "Lang"
  [ testThat "logged in lang switching" env testLoggedInLangSwitching
  , testThat "doc lang can be switched from swedish to english"
             env
             testDocumentLangSwitchToEnglish
  , testThat "doc lang can be switched from english to swedish"
             env
             testDocumentLangSwitchToSwedish
  ]

{- |
    Creates user in english and logs him from swedish page
-}
testLoggedInLangSwitching :: TestEnv ()
testLoggedInLangSwitching = do
    --create a new swedish user - after login from english page - he should still use swedish
  user <- createTestUser LANG_SV
  ctx0 <- (set #lang LANG_EN) <$> mkContext defaultLang
  req0 <- mkRequest
    POST
    [ ("email"    , inText "andrzej@skrivapa.se")
    , ("password" , inText "password_8866")
    , ("loginType", inText "RegularLogin")
    ]
  (_, ctx1) <- runTestKontra req0 ctx0 $ handleLoginPost

  assertBool "User was logged into context" $ (userid <$> ctx1 ^. #maybeUser) == Just
    (userid user)
  assertUserLang (userid user) LANG_SV
  assertContextLang (userid user) ctx1 LANG_SV

  where
    assertUserLang uid lang = do
      Just user <- dbQuery $ GetUserByID uid
      assertLang user lang
    assertContextLang uid ctx lang = do
      emptyReq      <- mkRequest GET []
      muser         <- dbQuery $ GetUserByID uid
      (userlang, _) <- runTestKontra emptyReq ctx $ Kontra $ getStandardLang muser
      assertLang userlang lang
    assertLang :: HasLang a => a -> Lang -> TestEnv ()
    assertLang langlike lang = assertEqual "Lang" lang (getLang langlike)

testDocumentLangSwitchToEnglish :: TestEnv ()
testDocumentLangSwitchToEnglish = do
  user <- createTestUser LANG_SV
  doc  <- createTestDoc user

  --make sure the doc lang matches the author lang
  assertEqual "Initial lang is Swedish" LANG_SV (getLang doc)


testDocumentLangSwitchToSwedish :: TestEnv ()
testDocumentLangSwitchToSwedish = do
  user <- createTestUser LANG_EN
  doc  <- createTestDoc user

  -- make sure the doc lang matches the author's lang
  assertEqual "Initial lang is English" LANG_EN (getLang doc)


createTestDoc :: User -> TestEnv Document
createTestDoc user = do
  doc <- addRandomDocument (rdaDefault user) { rdaStatuses = OneOf [Preparation] }
  dbQuery $ GetDocumentByDocumentID $ documentid doc

createTestUser :: Lang -> TestEnv User
createTestUser lang = do
  bd        <- dbQuery $ GetMainBrandedDomain
  pwd       <- createPassword "password_8866"
  ug        <- addNewUserGroup
  Just user <- createNewUser ("", "")
                             "andrzej@skrivapa.se"
                             (Just pwd)
                             (ug ^. #ugID, True)
                             lang
                             (bd ^. #id)
                             AccountRequest
  return user
