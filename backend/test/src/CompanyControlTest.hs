module CompanyControlTest (companyControlTests) where

import Happstack.Server hiding (simpleHTTP)
import Test.Framework
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T

import BrandedDomain.BrandedDomain
import Company.CompanyControl
import Context
import DB
import TestingUtil
import TestKontra as T
import Theme.Model
import User.Lang (defaultLang)
import UserGroup.Model
import UserGroup.Types
import Util.MonadUtils

companyControlTests :: TestEnvSt -> Test
companyControlTests env = testGroup "CompanyControl" [
    testThat "handleGetCompanyJSON works" env test_handleGetCompanyJSON
  , testThat "handleChangeCompanyBranding can be used to set the company ui" env test_settingUIWithHandleChangeCompanyBranding
  , testThat "handleChangeCompanyBranding can't connect any company ui with different company or domain themes" env test_settingUIWithHandleChangeCompanyBrandingRespectsThemeOwnership
  ]

test_handleGetCompanyJSON :: TestEnv ()
test_handleGetCompanyJSON = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  let ugui = get ugUI ug

  ctx <- (set ctxmaybeuser (Just user))
    <$> mkContext defaultLang

  req <- mkRequest GET []
  (avalue, _ctx') <- runTestKontra req ctx $ handleGetCompanyBranding Nothing
  (jsv :: JSValue) <- case decode (BSL.toString $ A.encode avalue) of
               Ok js -> return $ js
               _ -> unexpectedError "Response from handleGetCompanyBranding is not a valid JSON"
  (jsonCompanyid :: String) <- guardJustM $ withJSValue jsv $ fromJSValueField "companyid"
  (jsonMailTheme :: Maybe String) <- withJSValue jsv $ fromJSValueField "mailTheme"
  (jsonSignviewTheme :: Maybe String) <- withJSValue jsv $ fromJSValueField "signviewTheme"
  (jsonServiceTheme :: Maybe String) <- withJSValue jsv $ fromJSValueField "serviceTheme"
  (jsonBrowserTitle :: Maybe String) <- withJSValue jsv $ fromJSValueField "browserTitle"
  (jsonSmsOriginator :: Maybe String) <- withJSValue jsv $ fromJSValueField "smsOriginator"
  (jsonFavicon :: Maybe String) <-  withJSValue jsv $ fromJSValueField "favicon"

  assertEqual "JSON companyid matches company id"  (show $ get ugID ug) (jsonCompanyid)
  assertEqual "JSON companyMailTheme matches companyMailTheme"  (show <$> (get uguiMailTheme ugui)) (jsonMailTheme)
  assertEqual "JSON companySignviewTheme matches companySignviewTheme"  (show <$> (get uguiSignviewTheme ugui)) (jsonSignviewTheme)
  assertEqual "JSON companyServiceTheme matches companyServiceTheme"  (show <$> (get uguiServiceTheme ugui)) (jsonServiceTheme)
  assertEqual "JSON browserTitle matches browserTitle"  (T.unpack <$> get uguiBrowserTitle ugui) (jsonBrowserTitle)
  assertEqual "JSON smsOriginator matches SmsOriginator"  (T.unpack <$> get uguiSmsOriginator ugui) (jsonSmsOriginator)
  assertEqual "JSON favicon matches favicon"  (get uguiFavicon ugui) (B64.decodeLenient <$> BS.fromString <$> drop 1 <$> dropWhile ((/=) ',')  <$> jsonFavicon)



test_settingUIWithHandleChangeCompanyBranding :: TestEnv ()
test_settingUIWithHandleChangeCompanyBranding = do

  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx <- (set ctxmaybeuser (Just user))
    <$> mkContext defaultLang

  -- Try setting new themes
  mailThemeFromDomain <- dbQuery $ GetTheme
                         (get (bdMailTheme . ctxbrandeddomain) ctx)
  mailTheme <- dbUpdate $ InsertNewThemeForUserGroup (get ugID ug) mailThemeFromDomain
  signviewThemeFromDomain <- dbQuery $ GetTheme (get (bdSignviewTheme . ctxbrandeddomain) ctx)
  signviewTheme <- dbUpdate $ InsertNewThemeForUserGroup (get ugID ug) signviewThemeFromDomain
  serviceThemeFromDomain <- dbQuery $ GetTheme (get (bdServiceTheme . ctxbrandeddomain) ctx)
  serviceTheme <- dbUpdate $ InsertNewThemeForUserGroup (get ugID ug) serviceThemeFromDomain
  let browserTitle = "Super"
  let smsOriginator = "Super SMS"
  let favicon  = "-almoust-binary-data-aaa-000000000-"
  let faviconBase64 =
        T.pack $ BS.toString $
        BS.append (BS.fromString "data:image/png;base64,") $
        B64.encode $ BS.fromString $ favicon
  req1 <- mkRequest POST [
    ( "companyui"
    , inText $ "{\"companyid\":\"" <> showt (get ugID ug) <>
        "\",\"mailTheme\":\"" <> showt (themeID mailTheme) <>
        "\",\"signviewTheme\":\"" <> showt (themeID signviewTheme) <>
        "\",\"serviceTheme\":\"" <> showt (themeID serviceTheme) <>
        "\",\"browserTitle\":\"" <> browserTitle <>
        "\",\"smsOriginator\":\"" <> smsOriginator <>
        "\",\"favicon\":\"" <> faviconBase64 <> "\"}"
    )]
  (_, _) <- runTestKontra req1 ctx $ handleChangeCompanyBranding Nothing
  req2 <- mkRequest GET []
  (avalue, _) <- runTestKontra req2 ctx $ handleGetCompanyBranding Nothing
  (jsv :: JSValue) <- case decode (BSL.toString $ A.encode avalue) of
               Ok js -> return $ js
               _ -> unexpectedError "Response from handleGetCompanyBranding is not a valid JSON"
  (jsonMailTheme :: Maybe Text) <- withJSValue jsv $ fromJSValueField "mailTheme"
  (jsonSignviewTheme :: Maybe Text) <- withJSValue jsv $ fromJSValueField "signviewTheme"
  (jsonServiceTheme :: Maybe Text) <- withJSValue jsv $ fromJSValueField "serviceTheme"
  (jsonBrowserTitle :: Maybe Text) <- withJSValue jsv $ fromJSValueField "browserTitle"
  (jsonSmsOriginator :: Maybe Text) <- withJSValue jsv $ fromJSValueField "smsOriginator"
  (jsonFavicon :: Maybe Text) <-  withJSValue jsv $ fromJSValueField "favicon"

  assertEqual "JSON companyMailTheme matches companyMailTheme after update"
    (Just $ showt $ themeID mailTheme) (jsonMailTheme)
  assertEqual "JSON companySignviewTheme matches companySignviewTheme after update"
    (Just $ showt $ themeID signviewTheme) (jsonSignviewTheme)
  assertEqual "JSON companyServiceTheme matches companyServiceTheme after update"
    (Just $ showt $ themeID serviceTheme) (jsonServiceTheme)
  assertEqual "JSON browserTitle matches browserTitle after update"
    (Just browserTitle) (jsonBrowserTitle)
  assertEqual "JSON smsOriginator matches SmsOriginator after update"
    (Just smsOriginator) (jsonSmsOriginator)
  assertEqual "JSON favicon matches favicon after update"
    (Just favicon)
    (BS.toString <$> B64.decodeLenient <$> BS.fromString <$>
      drop 1 <$> dropWhile ((/=) ',') <$>
      T.unpack <$> jsonFavicon
    )

  --Test removing all compoany ui settings
  req3 <- mkRequest POST [
    ( "companyui"
    , inText $ "{\"companyid\":\"" <> showt (get ugID ug) <>
        "\",\"mailTheme\":null,\"signviewTheme\":null,\"serviceTheme\":null,\"browserTitle\": null ,\"smsOriginator\": null,\"favicon\":null}"
    )]
  (_, _) <- runTestKontra req3 ctx $ handleChangeCompanyBranding Nothing
  let ugui = get ugUI ug
  assertEqual "CompanyMailTheme  is empty"  (get uguiMailTheme ugui) (Nothing)
  assertEqual "CompanySignviewTheme  is empty" (get uguiSignviewTheme ugui) (Nothing)
  assertEqual "CompanyServiceTheme  is empty" (get uguiServiceTheme ugui) (Nothing)
  assertEqual "BrowserTitle is empty"  (get uguiBrowserTitle ugui) (Nothing)
  assertEqual "SmsOriginator is empty"  (get uguiSmsOriginator ugui) (Nothing)
  assertEqual "Favicon is empty"  (get uguiFavicon ugui) (Nothing)

test_settingUIWithHandleChangeCompanyBrandingRespectsThemeOwnership :: TestEnv ()
test_settingUIWithHandleChangeCompanyBrandingRespectsThemeOwnership = do

  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  ctx <- (set ctxmaybeuser (Just user))
    <$> mkContext defaultLang

  --Test we can't set mailTheme to domain theme
  req1 <- mkRequest POST [
    ( "companyui"
    , inText $ "{\"companyid\":\"" <> showt (get ugID ug) <>
        "\",\"mailTheme\":\""<> showt (get (bdMailTheme . ctxbrandeddomain) ctx) <>
        "\",\"signviewTheme\":null,\"serviceTheme\":null,\"browserTitle\": null ,\"smsOriginator\": null,\"favicon\":null}"
    )]
  (_, _) <- runTestKontra req1 ctx $ handleChangeCompanyBranding Nothing

  ug' <- (dbQuery $ UserGroupGet (get ugID ug))
  assertEqual "Can't set domain theme as company theme" (get (uguiMailTheme . ugUI) <$> ug') (Just $ get (uguiMailTheme . ugUI) ug)

  -- Create theme for other company
  (_, otherUg) <- addNewAdminUserAndUserGroup "Other" "Guy" "other_guy@skrivapa.se"
  someTheme <- dbQuery $ GetTheme (get (bdMailTheme . ctxbrandeddomain) ctx)
  otherUgTheme <- dbUpdate $ InsertNewThemeForUserGroup (get ugID otherUg) someTheme

  --Test we can't set mailTheme to other company theme
  req2 <- mkRequest POST [ ("companyui", inText $ T.pack $ "{\"companyid\":\""<>show (get ugID ug) <>"\",\"mailTheme\":\""<>show (themeID otherUgTheme)<>"\",\"signviewTheme\":null,\"serviceTheme\":null,\"browserTitle\": null ,\"smsOriginator\": null,\"favicon\":null}")]
  (_, _) <- runTestKontra req2 ctx $ handleChangeCompanyBranding Nothing
  ugui2 <- (get ugUI <$>) <$> (dbQuery $ UserGroupGet (get ugID ug))
  assertEqual "Can't set other company theme as company theme"  (get uguiMailTheme <$> ugui2) (Just Nothing)
