module LocaleTest (localeTests) where

import Control.Applicative
import Database.HDBC.PostgreSQL
import Happstack.Server
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import qualified Data.ByteString.Char8 as BS

import AppControl
import DB.Classes
import Context
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import User.UserControl
import Misc


localeTests :: Connection -> Test
localeTests conn = testGroup "Locale" [
      testCase "restricts allowed locales" $ testRestrictsAllowedLocales
    , testCase "logged in locale switching" $ testLoggedInLocaleSwitching conn
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
testLoggedInLocaleSwitching :: Connection -> Assertion
testLoggedInLocaleSwitching conn = withTestEnvironment conn $ do
    --create a new uk user and login
    uid <- createTestUser REGION_GB LANG_EN
    ctx0 <- (\c -> c { ctxdbconn = conn, ctxuserlocale = mkLocale REGION_GB LANG_EN })
      <$> (mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates)
    req0 <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res1, ctx1) <- runTestKontra req0 ctx0 $ handleLoginPost >>= sendRedirect
    assertLoggedInAndOnUploadPage uid res1 ctx1
    assertUserLocale uid REGION_GB LANG_EN
    assertContextLocale uid ctx1 REGION_GB LANG_EN

    --from the /upload page switch region to sweden
    req1 <- mkRequest POST [("region", inText "REGION_SE")]
    (res2, ctx2) <- runTestKontra req1 ctx1 $ handlePostUserLocale >>= sendRedirect
    assertLoggedInAndOnUploadPage uid res2 ctx2
    assertUserLocale uid REGION_SE LANG_SE
    assertContextLocale uid ctx2 REGION_SE LANG_SE

    --now switch back again to uk
    req2 <- mkRequest POST [("region", inText "REGION_GB")]
    (res3, ctx3) <- runTestKontra req2 ctx2 $ handlePostUserLocale >>= sendRedirect
    assertLoggedInAndOnUploadPage uid res3 ctx3
    assertUserLocale uid REGION_GB LANG_EN
    assertContextLocale uid ctx3 REGION_GB LANG_EN
  where
    assertUserLocale uid region lang = do
      Just user <- dbQuery $ GetUserByID uid
      assertLocale user region lang
    assertContextLocale uid ctx region lang = do
      emptyReq <- mkRequest GET []
      (doclocale, _) <- runTestKontra emptyReq ctx $ getDocumentLocale
      assertEqual "No doclocale" Nothing doclocale
      muser <- dbQuery $ GetUserByID uid
      (userlocale, _) <- runTestKontra emptyReq ctx $ getUserLocale conn muser
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

createTestUser :: Region -> Lang -> DB UserID
createTestUser region lang = do
    pwd <- createPassword $ BS.pack "admin"
    Just User{userid} <- dbUpdate $ AddUser (BS.empty, BS.empty) (BS.pack "andrzej@skrivapa.se") (Just pwd) False Nothing Nothing defaultValue (mkLocale region lang)
    return userid

