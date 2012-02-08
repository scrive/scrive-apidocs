module LocaleTest (localeTests) where

import Control.Applicative
import DB.Nexus
import Happstack.Server
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import qualified Data.ByteString.Char8 as BS

import AppControl
import DB.Classes
import Doc.Model
import Doc.DocStateData
import Context
import Login
import MinutesTime
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Locale
import User.Model
import User.UserControl
import Misc
import EvidenceLog.Model


localeTests :: Nexus -> Test
localeTests conn = testGroup "Locale" [
      testCase "restricts allowed locales" $ testRestrictsAllowedLocales
    , testCase "logged in locale switching" $ testLoggedInLocaleSwitching conn
    , testCase "doc locale can be switched from sweden to britain" $
               testDocumentLocaleSwitchToBritain conn
    , testCase "doc locale can be switched from britain to sweden" $
               testDocumentLocaleSwitchToSweden conn
    ]

{- |
    This makes sure that the mkLocale and mkLocaleFromRegion functions
    only create Locales that are one of the two valid ones:
      Sweden and Swedish
      British and English
-}
testRestrictsAllowedLocales :: Assertion
testRestrictsAllowedLocales = do
  assertBool "bad locale found" . all isAllowedLocale $ map mkLocaleFromRegion allValues
  assertBool "bad locale found" . all isAllowedLocale $ map (uncurry mkLocale) allRegionsAndLangs
  where
    allRegionsAndLangs = (,) <$> allValues <*> allValues
    isAllowedLocale locale = (getRegion locale == REGION_SE && getLang locale == LANG_SE)
                             || (getRegion locale == REGION_GB && getLang locale == LANG_EN)

{- |
    Logs in as a british user, and then switches to sweden and
    then back to british regions.
    Checks along the way that the user has the correct locale, and also that the
    context has the correct locale.
-}
testLoggedInLocaleSwitching :: Nexus -> Assertion
testLoggedInLocaleSwitching conn = withTestEnvironment conn $ do
    --create a new uk user and login
    user <- createTestUser REGION_GB LANG_EN
    globaltemplates <- readGlobalTemplates
    ctx0 <- (\c -> c { ctxdbconn = conn, ctxlocale = mkLocale REGION_GB LANG_EN })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req0 <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res1, ctx1) <- runTestKontra req0 ctx0 $ handleLoginPost >>= sendRedirect
    assertLoggedInAndOnUploadPage (userid user) res1 ctx1
    assertUserLocale (userid user) REGION_GB LANG_EN
    assertContextLocale (userid user) ctx1 REGION_GB LANG_EN

    --from the /upload page switch region to sweden
    req1 <- mkRequest POST [("region", inText "REGION_SE")]
    (res2, ctx2) <- runTestKontra req1 ctx1 $ handlePostUserLocale >>= sendRedirect
    assertLoggedInAndOnUploadPage (userid user) res2 ctx2
    assertUserLocale (userid user) REGION_SE LANG_SE
    assertContextLocale (userid user) ctx2 REGION_SE LANG_SE

    --now switch back again to uk
    req2 <- mkRequest POST [("region", inText "REGION_GB")]
    (res3, ctx3) <- runTestKontra req2 ctx2 $ handlePostUserLocale >>= sendRedirect
    assertLoggedInAndOnUploadPage (userid user) res3 ctx3
    assertUserLocale (userid user) REGION_GB LANG_EN
    assertContextLocale (userid user) ctx3 REGION_GB LANG_EN
  where
    assertUserLocale uid region lang = do
      Just user <- dbQuery $ GetUserByID uid
      assertLocale user region lang
    assertContextLocale uid ctx region lang = do
      emptyReq <- mkRequest GET []
      muser <- dbQuery $ GetUserByID uid
      (userlocale, _) <- runTestKontra emptyReq ctx $ getStandardLocale muser
      assertLocale userlocale region lang
    assertLocale :: HasLocale a => a -> Region -> Lang -> DB ()
    assertLocale locale region lang = do
      assertEqual "Region" region (getRegion locale)
      assertEqual "Lang" lang (getLang locale)
    assertLoggedInAndOnUploadPage uid res ctx = do
      assertBool "Response code is 303" $ rsCode res == 303
      assertBool "Location is /upload" $ T.getHeader "location" (rsHeaders res) == Just "/upload"
      assertBool "User was logged into context" $ (userid <$> ctxmaybeuser ctx) == Just uid
      assertBool "No flash messages were added" $ null $ ctxflashmessages ctx

testDocumentLocaleSwitchToBritain :: Nexus -> Assertion
testDocumentLocaleSwitchToBritain conn = withTestEnvironment conn $ do
  user <- createTestUser REGION_SE LANG_SE
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
  doc <- createTestElegDoc user (ctxtime ctx)

  --make sure the doc locale matches the author locale
  assertEqual "Initial region is Sweden" REGION_SE (getRegion doc)
  assertEqual "Initial lang is Swedish" LANG_SE (getLang doc)

  -- check that eleg is used
  assertEqual "Eleg is used" [ELegitimationIdentification] (documentallowedidtypes doc)

testDocumentLocaleSwitchToSweden :: Nexus -> Assertion
testDocumentLocaleSwitchToSweden conn = withTestEnvironment conn $ do
  user <- createTestUser REGION_GB LANG_EN
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
  doc <- createTestElegDoc user (ctxtime ctx)

  -- make sure the doc locale matches the author locale
  assertEqual "Initial region is Britain" REGION_GB (getRegion doc)
  assertEqual "Initial lang is English" LANG_EN (getLang doc)


createTestElegDoc :: User -> MinutesTime -> DB Document
createTestElegDoc user ctxtime = do
  doc <- addRandomDocumentWithAuthorAndCondition user
           (\d -> documentstatus d == Preparation
                  && documentfunctionality d == AdvancedFunctionality)
  (Right elegdoc) <- dbUpdate $ SetDocumentIdentification (documentid doc) [ELegitimationIdentification] (SystemActor ctxtime)
  return elegdoc

createTestUser :: Region -> Lang -> DB User
createTestUser region lang = do
    pwd <- createPassword $ BS.pack "admin"
    Just user <- dbUpdate $ AddUser (BS.empty, BS.empty) (BS.pack "andrzej@skrivapa.se") (Just pwd) False Nothing Nothing (mkLocale region lang)
    return user
