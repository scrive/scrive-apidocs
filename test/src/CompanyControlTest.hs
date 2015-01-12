module CompanyControlTest (companyControlTests) where

import Control.Applicative
--import Control.Monad.Trans (liftIO)
--import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Test.Framework

import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS

import BrandedDomain.BrandedDomain
import Company.CompanyControl
import Company.CompanyUI
import Company.Model
import CompanyAccounts.Model
import Context
import DB
--import Redirect
import TestingUtil
import TestKontra as T
import Theme.Model
import Util.MonadUtils
import Utils.Default
import Utils.String

companyControlTests :: TestEnvSt -> Test
companyControlTests env = testGroup "CompanyControl" [
    testThat "handleGetCompanyJSON works" env test_handleGetCompanyJSON
  , testThat "handleChangeCompanyBranding can be used to set the company ui" env test_settingUIWithHandleChangeCompanyBranding
  ]

test_handleGetCompanyJSON :: TestEnv ()
test_handleGetCompanyJSON = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  companyui <- dbQuery $ GetCompanyUI (companyid company)
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest GET []
  (rsp, _ctx') <- runTestKontra req ctx $ handleGetCompanyBranding Nothing
  (jsv :: JSValue) <- case decode (BS.toString $ concatChunks $ rsBody rsp) of
               Ok js -> return $ js
               _ -> assertFailure "Response from handleGetCompanyBranding is not a valid JSON" >> error ""
  (jsonCompanyid :: String) <- guardJustM $ withJSValue jsv $ fromJSValueField "companyid"
  (jsonMailTheme :: Maybe String) <- withJSValue jsv $ fromJSValueField "mailTheme"
  (jsonSignviewTheme :: Maybe String) <- withJSValue jsv $ fromJSValueField "signviewTheme"
  (jsonServiceTheme :: Maybe String) <- withJSValue jsv $ fromJSValueField "serviceTheme"
  (jsonBrowserTitle :: Maybe String) <- withJSValue jsv $ fromJSValueField "browserTitle"
  (jsonSmsOriginator :: Maybe String) <- withJSValue jsv $ fromJSValueField "smsOriginator"
  (jsonFavicon :: Maybe String) <-  withJSValue jsv $ fromJSValueField "favicon"

  assertEqual "JSON companyid matches company id"  (show $ companyid $ company) (jsonCompanyid)
  assertEqual "JSON companyMailTheme matches companyMailTheme"  (show <$> companyMailTheme companyui) (jsonMailTheme)
  assertEqual "JSON companySignviewTheme matches companySignviewTheme"  (show <$> companySignviewTheme companyui) (jsonSignviewTheme)
  assertEqual "JSON companyServiceTheme matches companyServiceTheme"  (show <$> companyServiceTheme companyui) (jsonServiceTheme)
  assertEqual "JSON browserTitle matches browserTitle"  (companyBrowserTitle companyui) (jsonBrowserTitle)
  assertEqual "JSON smsOriginator matches SmsOriginator"  (companySmsOriginator companyui) (jsonSmsOriginator)
  assertEqual "JSON favicon matches favicon"  (companyFavicon companyui) (Binary <$> B64.decodeLenient <$> BS.fromString <$> drop 1 <$> dropWhile ((/=) ',')  <$> jsonFavicon)



test_settingUIWithHandleChangeCompanyBranding :: TestEnv ()
test_settingUIWithHandleChangeCompanyBranding = do

  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  -- Try setting new themes
  mailThemeFromDomain <- dbQuery $ GetTheme (bdMailTheme $ ctxbrandeddomain ctx)
  mailTheme <- dbUpdate $ InsertNewThemeForCompany (companyid company) mailThemeFromDomain
  signviewThemeFromDomain <- dbQuery $ GetTheme (bdSignviewTheme $ ctxbrandeddomain ctx)
  signviewTheme <- dbUpdate $ InsertNewThemeForCompany (companyid company) signviewThemeFromDomain
  serviceThemeFromDomain <- dbQuery $ GetTheme (bdSignviewTheme $ ctxbrandeddomain ctx)
  serviceTheme <- dbUpdate $ InsertNewThemeForCompany (companyid company) serviceThemeFromDomain
  let browserTitle = "Super"
  let smsOriginator = "Super SMS"
  let favicon  = "-almoust-binary-data-aaa-000000000-"
  let faviconBase64 = BS.toString $ BS.append (BS.fromString "data:image/png;base64,") $ B64.encode $ BS.fromString $ favicon
  req1 <- mkRequest POST [ ("companyui", inText $ "{\"companyid\":\""++show (companyid company) ++"\",\"mailTheme\":\""++show (themeID mailTheme) ++"\",\"signviewTheme\":\""++show (themeID signviewTheme)++"\",\"serviceTheme\":\""++show (themeID serviceTheme)++"\",\"browserTitle\":\""++browserTitle++"\",\"smsOriginator\":\""++smsOriginator++"\",\"favicon\":\""++faviconBase64++"\"}")]
  (_, _) <- runTestKontra req1 ctx $ handleChangeCompanyBranding Nothing
  req2 <- mkRequest GET []
  (rsp, _) <- runTestKontra req2 ctx $ handleGetCompanyBranding Nothing
  (jsv :: JSValue) <- case decode (BS.toString $ concatChunks $ rsBody rsp) of
               Ok js -> return $ js
               _ -> assertFailure "Response from handleGetCompanyBranding is not a valid JSON" >> error ""
  (jsonMailTheme :: Maybe String) <- withJSValue jsv $ fromJSValueField "mailTheme"
  (jsonSignviewTheme :: Maybe String) <- withJSValue jsv $ fromJSValueField "signviewTheme"
  (jsonServiceTheme :: Maybe String) <- withJSValue jsv $ fromJSValueField "serviceTheme"
  (jsonBrowserTitle :: Maybe String) <- withJSValue jsv $ fromJSValueField "browserTitle"
  (jsonSmsOriginator :: Maybe String) <- withJSValue jsv $ fromJSValueField "smsOriginator"
  (jsonFavicon :: Maybe String) <-  withJSValue jsv $ fromJSValueField "favicon"

  assertEqual "JSON companyMailTheme matches companyMailTheme after update"  (Just $ show $ themeID mailTheme) (jsonMailTheme)
  assertEqual "JSON companySignviewTheme matches companySignviewTheme after update"  (Just $ show $ themeID signviewTheme) (jsonSignviewTheme)
  assertEqual "JSON companyServiceTheme matches companyServiceTheme after update"  (Just $ show $ themeID serviceTheme) (jsonServiceTheme)
  assertEqual "JSON browserTitle matches browserTitle after update"  (Just browserTitle) (jsonBrowserTitle)
  assertEqual "JSON smsOriginator matches SmsOriginator after update"  (Just smsOriginator) (jsonSmsOriginator)
  assertEqual "JSON favicon matches favicon after update"  (Just favicon) (BS.toString <$> B64.decodeLenient <$> BS.fromString <$> drop 1 <$> dropWhile ((/=) ',')  <$> jsonFavicon)

  --Test removing all compoany ui settings
  req3 <- mkRequest POST [ ("companyui", inText $ "{\"companyid\":\""++show (companyid company) ++"\",\"mailTheme\":null,\"signviewTheme\":null,\"serviceTheme\":null,\"browserTitle\": null ,\"smsOriginator\": null,\"favicon\":null}")]
  (_, _) <- runTestKontra req3 ctx $ handleChangeCompanyBranding Nothing
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  assertEqual "CompanyMailTheme  is empty"  (companyMailTheme companyui) (Nothing)
  assertEqual "CompanySignviewTheme  is empty" (companySignviewTheme companyui) (Nothing)
  assertEqual "CompanyServiceTheme  is empty" (companyServiceTheme companyui) (Nothing)
  assertEqual "BrowserTitle is empty"  (companyBrowserTitle companyui) (Nothing)
  assertEqual "SmsOriginator is empty"  (companySmsOriginator companyui) (Nothing)
  assertEqual "Favicon is empty"  (companyFavicon companyui) (Nothing)

addNewAdminUserAndCompany :: String -> String -> String -> TestEnv (User, Company)
addNewAdminUserAndCompany fstname sndname email = do
  company <- addNewCompany
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  _ <- dbUpdate $ SetCompanyUI (companyid company) companyui --{companyemailbackgroundcolour = Just "#abcdef"}
  Just user <- addNewCompanyUser fstname sndname email (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  Just updatedcompany <- dbQuery $ GetCompany (companyid company)
  return (updateduser, updatedcompany)

