module CompanyControlTest (companyControlTests) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Text.JSON.FromJSValue
import Test.Framework
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS

import Company.CompanyControl
import Company.Model
import Company.CompanyUI
import CompanyAccounts.Model
import Context
import DB
import Utils.Default
import Redirect
import TestingUtil
import TestKontra as T

companyControlTests :: TestEnvSt -> Test
companyControlTests env = testGroup "CompanyControl" [
    testThat "handleGetCompanyJSON works" env test_handleGetCompanyJSON
  , testThat "handlePostCompany can be used to set the company ui" env test_settingUIWithHandlePostCompany
  , testThat "handleCompanyLogo responds when noone is logged in" env test_handleCompanyLogo
  ]

test_handleGetCompanyJSON :: TestEnv ()
test_handleGetCompanyJSON = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  companyui <- dbQuery $ GetCompanyUI (companyid company)
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest GET []
  (jsv, _ctx') <- runTestKontra req ctx $ handleGetCompanyJSON Nothing
  (ejsonemailemailbackgroundcolour :: Maybe String) <- withJSValue jsv $ fromJSValueField "companyemailemailbackgroundcolour"
  assertEqual "JSON companyemailemailbackgroundcolour matches company id"  (fromMaybe "" $ companyemailemailbackgroundcolour $ companyui) (fromMaybe "" ejsonemailemailbackgroundcolour)

test_settingUIWithHandlePostCompany :: TestEnv ()
test_settingUIWithHandlePostCompany = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue
  logo <- liftIO $ BS.readFile "public/img/logo_email.png"
  let logo64 = BS.unpack $ B64.encode logo

  req1 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"companyemailfont\":\"font2\",\"companyemailbordercolour\":\"color1\",\"companyemailbuttoncolour\":\"color2\",\"companyemailemailbackgroundcolour\":\"color3\",\"companyemailbackgroundcolour\":\"color11\",\"companyemailtextcolour\":\"color4\",\"companysignviewtextcolour\":\"color5\",\"companysignviewtextfont\":\"font3\",\"companysignviewbarscolour\":\"color6\",\"companysignviewbarstextcolour\":\"color7\",\"companysignviewbackgroundcolour\":\"color10\",\"companyemaillogo\":\"" ++ logo64 ++ "\",\"companysignviewlogo\":\"" ++ logo64 ++ "\",\"companyemaillogochanged\":\"true\",\"companysignviewlogochanged\":\"true\"}")]
  (res1, _ctx') <- runTestKontra req1 ctx $ handlePostCompany Nothing >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res1)
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  assertEqual "Email font was set" (Just "font2") $ companyemailfont $ companyui
  assertEqual "Email border colour was set" (Just "color1") $ companyemailbordercolour $ companyui
  assertEqual "Email button colour was set" (Just "color2") $ companyemailbuttoncolour $ companyui
  assertEqual "Email email background was set" (Just "color3") $ companyemailemailbackgroundcolour $ companyui
  assertEqual "Email background was set" (Just "color11") $ companyemailbackgroundcolour $ companyui
  assertEqual "Email text colour was set" (Just "color4") $ companyemailtextcolour $ companyui
  assertBool "Email logo file was not set" $ isNothing $ companyemaillogo $ companyui
  assertBool "Signview logo file was not set" $ isNothing $ companysignviewlogo $ companyui
  assertEqual "Signview text colour was set" (Just "color5") $ companysignviewtextcolour $ companyui
  assertEqual "Signview text font was set" (Just "font3") $ companysignviewtextfont $ companyui
  assertEqual "Signview bars colour was set" (Just "color6") $ companysignviewbarscolour $ companyui
  assertEqual "Signview bars text color was set" (Just "color7") $ companysignviewbarstextcolour $ companyui
  assertEqual "Signview background colour was set" (Just "color10") $ companysignviewbackgroundcolour $ companyui

  req2 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"companyemailfont\":\"\",\"companyemailbordercolour\":\"\",\"companyemailbuttoncolour\":\"\",\"companyemailemailbackgroundcolour\":\"\",\"companyemailbackgroundcolour\":\"\",\"companyemailtextcolour\":\"\",\"companysignviewtextcolour\":\"\",\"companysignviewtextfont\":\"\",\"companysignviewbarscolour\":\"\",\"companysignviewbarstextcolour\":\"\",\"companysignviewbackgroundcolour\":\"\"}")]
  (res2, _ctx') <- runTestKontra req2 ctx $ handlePostCompany Nothing >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res2)
  companyui2 <- dbQuery $ GetCompanyUI (companyid company)

  assertEqual "Email font reset" Nothing $ companyemailfont $ companyui2
  assertEqual "Email border colour reset" Nothing $ companyemailbordercolour $ companyui2
  assertEqual "Email button colour reset" Nothing $ companyemailbuttoncolour $ companyui2
  assertEqual "Email email background reset" Nothing $ companyemailemailbackgroundcolour $ companyui2
  assertEqual "Email background reset" Nothing $ companyemailbackgroundcolour $ companyui2
  assertEqual "Email text colour reset" Nothing $ companyemailtextcolour $ companyui2
  assertEqual "Signview text colour reset" Nothing $ companysignviewtextcolour $ companyui2
  assertEqual "Signview text font reset" Nothing $ companysignviewtextfont $ companyui2
  assertEqual "Signview bars colour reset" Nothing $ companysignviewbarscolour $ companyui2
  assertEqual "Signview bars text colour reset" Nothing $ companysignviewbarstextcolour $ companyui2
  assertEqual "Signview background colour reset" Nothing $ companysignviewbackgroundcolour $ companyui2

  req3 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"companyemailfont\":\"font2\",\"companyemailbordercolour\":\"color1\",\"companyemailbuttoncolour\":\"color2\",\"companyemailemailbackgroundcolour\":\"color3\",\"companyemailbackgroundcolour\":\"color11\",\"companyemailtextcolour\":\"color4\",\"companysignviewtextcolour\":\"color5\",\"companysignviewtextfont\":\"font3\",\"companysignviewbarscolour\":\"color6\",\"companysignviewbarstextcolour\":\"color7\",\"companysignviewbackgroundcolour\":\"color10\",\"companyemaillogo\":\"\",\"companysignviewlogo\":\"\",\"companyemaillogochanged\":\"true\",\"companysignviewlogochanged\":\"true\"}")]
  (res3, _ctx') <- runTestKontra req3 ctx $ handlePostCompany Nothing >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res3)
  companyui3 <- dbQuery $ GetCompanyUI (companyid company)
  assertEqual "Email font was set" (Just "font2") $ companyemailfont $ companyui3
  assertEqual "Email border colour was set" (Just "color1") $ companyemailbordercolour $ companyui3
  assertEqual "Email button colour was set" (Just "color2") $ companyemailbuttoncolour $ companyui3
  assertEqual "Email email background was set" (Just "color3") $ companyemailemailbackgroundcolour $ companyui3
  assertEqual "Email background was set" (Just "color11") $ companyemailbackgroundcolour $ companyui3
  assertEqual "Email text colour was set" (Just "color4") $ companyemailtextcolour $ companyui3
  assertEqual "Signview text colour was set" (Just "color5") $ companysignviewtextcolour $ companyui3
  assertEqual "Signview text font was set" (Just "font3") $ companysignviewtextfont $ companyui3
  assertEqual "Signview bars  colour was set" (Just "color6") $ companysignviewbarscolour $ companyui3
  assertEqual "Signview bars text colour was set" (Just "color7") $ companysignviewbarstextcolour $ companyui3
  assertEqual "Signview background colour was set" (Just "color10") $ companysignviewbackgroundcolour $ companyui3
  assertEqual "Email logo file reset" Nothing $ companyemaillogo $ companyui3
  assertEqual "Signview logo file reset" Nothing $ companysignviewlogo $ companyui3


test_handleCompanyLogo :: TestEnv ()
test_handleCompanyLogo = do
  (_user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx <- mkContext defaultValue

  req <- mkRequest GET []
  (res, _ctx') <- runTestKontra req ctx $ handleCompanyEmailLogo (companyid company)

  assertEqual "Response code is 200" 200 (rsCode res)

addNewAdminUserAndCompany :: String -> String -> String -> TestEnv (User, Company)
addNewAdminUserAndCompany fstname sndname email = do
  company <- addNewCompany
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  _ <- dbUpdate $ SetCompanyUI (companyid company) companyui {companyemailbackgroundcolour = Just "#abcdef"}
  Just user <- addNewCompanyUser fstname sndname email (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  Just updatedcompany <- dbQuery $ GetCompany (companyid company)
  return (updateduser, updatedcompany)
