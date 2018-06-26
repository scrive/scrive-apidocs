module LangTest (langTests) where

import Happstack.Server
import Test.Framework

import AppControl
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Context
import DB
import Doc.DocStateData
import Doc.Model
import Kontra (KontraG(..))
import Login
import MinutesTime
import TestingUtil
import TestKontra as T
import User.Lang
import User.Model
import UserGroup.Data
import UserGroup.Model

langTests :: TestEnvSt -> Test
langTests env = testGroup "Lang" [
      testThat "logged in lang switching" env testLoggedInLangSwitching
    , testThat "doc lang can be switched from swedish to english" env
               testDocumentLangSwitchToEnglish
    , testThat "doc lang can be switched from english to swedish" env
               testDocumentLangSwitchToSwedish
    ]

{- |
    Creates user in english and logs him from swedish page
-}
testLoggedInLangSwitching :: TestEnv ()
testLoggedInLangSwitching = do
    --create a new swedish user - after login from english page - he should still use swedish
    user <- createTestUser LANG_SV
    ctx0 <- (set ctxlang LANG_EN) <$> mkContext def
    req0 <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin"), ("loginType", inText "RegularLogin")]
    (_, ctx1) <- runTestKontra req0 ctx0 $ handleLoginPost

    assertBool "User was logged into context" $ (userid <$> get ctxmaybeuser ctx1) == Just (userid user)
    assertUserLang (userid user) LANG_SV
    assertContextLang (userid user) ctx1 LANG_SV

  where
    assertUserLang uid lang = do
      Just user <- dbQuery $ GetUserByID uid
      assertLang user lang
    assertContextLang uid ctx lang = do
      emptyReq <- mkRequest GET []
      muser <- dbQuery $ GetUserByID uid
      (userlang, _) <- runTestKontra emptyReq ctx $ Kontra $ getStandardLang muser
      assertLang userlang lang
    assertLang :: HasLang a => a -> Lang -> TestEnv ()
    assertLang langlike lang = assertEqual "Lang" lang (getLang langlike)

testDocumentLangSwitchToEnglish :: TestEnv ()
testDocumentLangSwitchToEnglish = do
  user <- createTestUser LANG_SV
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def
  doc <- createTestDoc user (get ctxtime ctx)

  --make sure the doc lang matches the author lang
  assertEqual "Initial lang is Swedish" LANG_SV (getLang doc)


testDocumentLangSwitchToSwedish :: TestEnv ()
testDocumentLangSwitchToSwedish = do
  user <- createTestUser LANG_EN
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def
  doc <- createTestDoc user (get ctxtime ctx)

  -- make sure the doc lang matches the author's lang
  assertEqual "Initial lang is English" LANG_EN (getLang doc)


createTestDoc :: User -> UTCTime -> TestEnv Document
createTestDoc user _ctxtime = do
  doc <- addRandomDocumentWithAuthorAndCondition user
           (\d -> documentstatus d == Preparation)
  dbQuery $ GetDocumentByDocumentID $ documentid doc

createTestUser :: Lang -> TestEnv User
createTestUser lang = do
    bd <- dbQuery $ GetMainBrandedDomain
    pwd <- createPassword "admin"
    ug <- dbUpdate $ UserGroupCreate def
    Just user <- dbUpdate $ AddUser ("", "") "andrzej@skrivapa.se" (Just pwd) (get ugID ug, True) lang (get bdid bd) AccountRequest
    return user
