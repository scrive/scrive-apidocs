module LangTest (langTests) where

import Control.Applicative
import Happstack.Server
import Test.Framework

import AppControl
import DB
import Doc.Model
import Doc.DocStateData
import Context
import Kontra (Kontra(..))
import Login
import MinutesTime
import TestingUtil
import TestKontra as T
import User.Lang
import User.Model
import Utils.Default
import User.API

langTests :: TestEnvSt -> Test
langTests env = testGroup "Lang" [
      testThat "logged in lang switching" env testLoggedInLangSwitching
    , testThat "doc lang can be switched from swedish to english" env
               testDocumentLangSwitchToEnglish
    , testThat "doc lang can be switched from english to swedish" env
               testDocumentLangSwitchToSwedish
    ]

{- |
    Logs in as an english user, and then switches to swedish lang and
    then back to english lang.
    Checks along the way that the user has the correct lang, and also that the
    context has the correct lang.
-}
testLoggedInLangSwitching :: TestEnv ()
testLoggedInLangSwitching = do
    --create a new uk user and login
    user <- createTestUser LANG_EN
    ctx0 <- (\c -> c { ctxlang = LANG_EN })
      <$> mkContext defaultValue
    req0 <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin"), ("loginType", inText "RegularLogin")]
    (_, ctx1) <- runTestKontra req0 ctx0 $ handleLoginPost

    assertBool "User was logged into context" $ (userid <$> ctxmaybeuser ctx1) == Just (userid user)
    assertUserLang (userid user) LANG_EN
    assertContextLang (userid user) ctx1 LANG_EN

    --from the /upload page switch lang to swedish
    req1 <- mkRequest POST [("lang", inText "sv")]
    (_res2, ctx2) <- runTestKontra req1 ctx1 $ apiCallChangeUserLanguage
    assertLoggedIn (userid user) ctx2
    assertUserLang (userid user) LANG_SV
    assertContextLang (userid user) ctx2 LANG_SV

    --now switch back again to uk
    req2 <- mkRequest POST [("lang", inText "en")]
    (_res3, ctx3) <- runTestKontra req2 ctx2 $ apiCallChangeUserLanguage
    assertLoggedIn (userid user) ctx3
    assertUserLang (userid user) LANG_EN
    assertContextLang (userid user) ctx3 LANG_EN
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
    assertLoggedIn uid ctx = do
      assertBool "User was logged into context" $ (userid <$> ctxmaybeuser ctx) == Just uid
      assertBool "No flash messages were added" $ null $ ctxflashmessages ctx

testDocumentLangSwitchToEnglish :: TestEnv ()
testDocumentLangSwitchToEnglish = do
  user <- createTestUser LANG_SV
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue
  doc <- createTestElegDoc user (ctxtime ctx)

  --make sure the doc lang matches the author lang
  assertEqual "Initial lang is Swedish" LANG_SV (getLang doc)

  -- check that eleg is used
  --assertEqual "Eleg is used" ELegAuthentication (documentauthenticationmethod doc)

testDocumentLangSwitchToSwedish :: TestEnv ()
testDocumentLangSwitchToSwedish = do
  user <- createTestUser LANG_EN
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue
  doc <- createTestElegDoc user (ctxtime ctx)

  -- make sure the doc lang matches the author's lang
  assertEqual "Initial lang is English" LANG_EN (getLang doc)


createTestElegDoc :: User -> MinutesTime -> TestEnv Document
createTestElegDoc user _ctxtime = do
  doc <- addRandomDocumentWithAuthorAndCondition user
           (\d -> documentstatus d == Preparation)
  --True <- dbUpdate $ SetDocumentAuthenticationMethod (documentid doc) ELegAuthentication (systemActor ctxtime)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  return ndoc

createTestUser :: Lang -> TestEnv User
createTestUser lang = do
    pwd <- createPassword "admin"
    Just user <- dbUpdate $ AddUser ("", "") "andrzej@skrivapa.se" (Just pwd) Nothing lang Nothing
    return user
