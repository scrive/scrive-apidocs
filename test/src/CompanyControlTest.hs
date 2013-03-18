module CompanyControlTest (companyControlTests) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Text.JSON
import Text.JSON.FromJSValue
import Test.Framework
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS

import Company.CompanyControl
import Company.Model
import CompanyAccounts.Model
import Context
import DB
import Utils.Default
import Utils.Either
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

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest GET []
  (jsv, _ctx') <- runTestKontra req ctx $ handleGetCompanyJSON Nothing

  let ejsonemailemailbackgroundcolour = getemailemailbackgroundcolourFromJSON jsv
  assertBool "Able to get emailemailbackgroundcolour from json" (isRight ejsonemailemailbackgroundcolour)

  let (Right jsonemailemailbackgroundcolour) = ejsonemailemailbackgroundcolour
  assertEqual "JSON emailemailbackgroundcolour matches company id" (fromJust $ companyemailemailbackgroundcolour $ companyui company) jsonemailemailbackgroundcolour
  where
    getemailemailbackgroundcolourFromJSON :: JSValue -> Either String String
    getemailemailbackgroundcolourFromJSON jsv = maybe (Left "Unable to parse JSON!") Right $ do
      (x :: String) <- fromJSValueField "emailemailbackgroundcolour" jsv
      return x

test_settingUIWithHandlePostCompany :: TestEnv ()
test_settingUIWithHandlePostCompany = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue
  logo <- liftIO $ BS.readFile "public/img/email-logo.png"
  let logo64 = BS.unpack $ B64.encode logo

  req1 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"companyemailheaderfont\":\"font1\",\"companyemailfont\":\"font2\",\"companyemailbordercolour\":\"color1\",\"companyemailbuttoncolour\":\"color2\",\"companyemailemailbackgroundcolour\":\"color3\",\"companyemailbackgroundcolour\":\"color11\",\"companyemailtextcolour\":\"color4\",\"companysignviewtextcolour\":\"color5\",\"companysignviewtextfont\":\"font3\",\"companysignviewbarscolour\":\"color6\",\"companysignviewbarstextcolour\":\"color7\",\"companysignviewbackgroundcolour\":\"color10\",\"companyemaillogo\":\"" ++ logo64 ++ "\",\"companysignviewlogo\":\"" ++ logo64 ++ "\",\"companyemaillogochanged\":\"true\",\"companysignviewlogochanged\":\"true\"}")]
  (res1, _ctx') <- runTestKontra req1 ctx $ handlePostCompany Nothing >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res1)
  Just newcompany1 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Email header font was set" (Just "font1") $ companyemailheaderfont $ companyui newcompany1
  assertEqual "Email font was set" (Just "font2") $ companyemailfont $ companyui newcompany1
  assertEqual "Email border colour was set" (Just "color1") $ companyemailbordercolour $ companyui newcompany1
  assertEqual "Email button colour was set" (Just "color2") $ companyemailbuttoncolour $ companyui newcompany1
  assertEqual "Email email background was set" (Just "color3") $ companyemailemailbackgroundcolour $ companyui newcompany1
  assertEqual "Email background was set" (Just "color11") $ companyemailbackgroundcolour $ companyui newcompany1
  assertEqual "Email text colour was set" (Just "color4") $ companyemailtextcolour $ companyui newcompany1
  assertBool "Email logo file was set" $ isJust $ companyemaillogo $ companyui newcompany1
  assertBool "Signview logo file was set" $ isJust $ companysignviewlogo $ companyui newcompany1
  assertEqual "Signview text colour was set" (Just "color5") $ companysignviewtextcolour $ companyui newcompany1
  assertEqual "Signview text font was set" (Just "font3") $ companysignviewtextfont $ companyui newcompany1
  assertEqual "Signview bars colour was set" (Just "color6") $ companysignviewbarscolour $ companyui newcompany1
  assertEqual "Signview bars text color was set" (Just "font7") $ companysignviewbarstextcolour $ companyui newcompany1
  assertEqual "Signview background colour was set" (Just "color10") $ companysignviewbackgroundcolour $ companyui newcompany1

  req2 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"companyemailheaderfont\":\"\",\"companyemailfont\":\"\",\"companyemailbordercolour\":\"\",\"companyemailbuttoncolour\":\"\",\"companyemailemailbackgroundcolour\":\"\",\"companyemailbackgroundcolour\":\"\",\"companyemailtextcolour\":\"\",\"companysignviewtextcolour\":\"\",\"companysignviewtextfont\":\"\",\"companysignviewbarscolour\":\"\",\"companysignviewbarstextcolour\":\"\",\"companysignviewbackgroundcolour\":\"\",\"companyemaillogo\":\"\",\"companysignviewlogo\":\"\",\"companyemaillogochanged\":\"false\",\"companysignviewlogochanged\":\"false\"}")]
  (res2, _ctx') <- runTestKontra req2 ctx $ handlePostCompany Nothing >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res2)
  Just newcompany2 <- dbQuery $ GetCompany (companyid company)

  assertEqual "Email header font reset" Nothing $ companyemailheaderfont $ companyui newcompany2
  assertEqual "Email font reset" Nothing $ companyemailfont $ companyui newcompany2
  assertEqual "Email border colour reset" Nothing $ companyemailbordercolour $ companyui newcompany2
  assertEqual "Email button colour reset" Nothing $ companyemailbuttoncolour $ companyui newcompany2
  assertEqual "Email email background reset" Nothing $ companyemailemailbackgroundcolour $ companyui newcompany2
  assertEqual "Email background reset" Nothing $ companyemailbackgroundcolour $ companyui newcompany2
  assertEqual "Email text colour reset" Nothing $ companyemailtextcolour $ companyui newcompany2
  assertEqual "Signview text colour reset" Nothing $ companysignviewtextcolour $ companyui newcompany2
  assertEqual "Signview text font reset" Nothing $ companysignviewtextfont $ companyui newcompany2
  assertEqual "Signview bars colour reset" Nothing $ companysignviewbarscolour $ companyui newcompany2
  assertEqual "Signview bars text colour reset" Nothing $ companysignviewbarstextcolour $ companyui newcompany2
  assertEqual "Signview background colour reset" Nothing $ companysignviewbackgroundcolour $ companyui newcompany2
  assertEqual "Email logo still intact" (companyemaillogo $ companyui newcompany1) (companyemaillogo $ companyui newcompany2)
  assertEqual "Signview logo still intact" (companysignviewlogo $ companyui newcompany1) (companysignviewlogo $ companyui newcompany2)

  req3 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"companyemailheaderfont\":\"font1\",\"companyemailfont\":\"font2\",\"companyemailbordercolour\":\"color1\",\"companyemailbuttoncolour\":\"color2\",\"companyemailemailbackgroundcolour\":\"color3\",\"companyemailbackgroundcolour\":\"color11\",\"companyemailtextcolour\":\"color4\",\"companysignviewtextcolour\":\"color5\",\"companysignviewtextfont\":\"font3\",\"companysignviewbarscolour\":\"color6\",\"companysignviewbarstextcolour\":\"color7\",\"companysignviewbackgroundcolour\":\"color10\",\"companyemaillogo\":\"\",\"companysignviewlogo\":\"\",\"companyemaillogochanged\":\"true\",\"companysignviewlogochanged\":\"true\"}")]
  (res3, _ctx') <- runTestKontra req3 ctx $ handlePostCompany Nothing >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res3)
  Just newcompany3 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Email header font was set" (Just "font1") $ companyemailheaderfont $ companyui newcompany3
  assertEqual "Email font was set" (Just "font2") $ companyemailfont $ companyui newcompany3
  assertEqual "Email border colour was set" (Just "color1") $ companyemailbordercolour $ companyui newcompany3
  assertEqual "Email button colour was set" (Just "color2") $ companyemailbuttoncolour $ companyui newcompany3
  assertEqual "Email email background was set" (Just "color3") $ companyemailemailbackgroundcolour $ companyui newcompany3
  assertEqual "Email background was set" (Just "color11") $ companyemailbackgroundcolour $ companyui newcompany3
  assertEqual "Email text colour was set" (Just "color4") $ companyemailtextcolour $ companyui newcompany3
  assertEqual "Signview text colour was set" (Just "color5") $ companysignviewtextcolour $ companyui newcompany3
  assertEqual "Signview text font was set" (Just "font3") $ companysignviewtextfont $ companyui newcompany3
  assertEqual "Signview bars  colour was set" (Just "color6") $ companysignviewbarscolour $ companyui newcompany3
  assertEqual "Signview bars text colour was set" (Just "color7") $ companysignviewbarstextcolour $ companyui newcompany3
  assertEqual "Signview background colour was set" (Just "color10") $ companysignviewbackgroundcolour $ companyui newcompany3
  assertEqual "Email logo file reset" Nothing $ companyemaillogo $ companyui newcompany3
  assertEqual "Signview logo file reset" Nothing $ companysignviewlogo $ companyui newcompany3


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
  _ <- dbUpdate $ UpdateCompanyUI (companyid company) (companyui company){companyemailbackgroundcolour = Just "#abcdef"}
  Just user <- addNewCompanyUser fstname sndname email (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  Just updatedcompany <- dbQuery $ GetCompany (companyid company)
  return (updateduser, updatedcompany)
