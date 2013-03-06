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

  let ejsoncompanybarsbackground = getcompanybarsbackgroundFromJSON jsv
  assertBool "Able to get companybarsbackground from json" (isRight ejsoncompanybarsbackground)

  let (Right jsoncompanybarsbackground) = ejsoncompanybarsbackground
  assertEqual "JSON companybarsbackground matches company id" (fromJust $ companybarsbackground $ companyui company) jsoncompanybarsbackground
  where
    getcompanybarsbackgroundFromJSON :: JSValue -> Either String String
    getcompanybarsbackgroundFromJSON jsv = maybe (Left "Unable to parse JSON!") Right $ do
      (x :: String) <- fromJSValueField "barsbackground" jsv
      return x

test_settingUIWithHandlePostCompany :: TestEnv ()
test_settingUIWithHandlePostCompany = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue
  logo <- liftIO $ BS.readFile "public/img/email-logo.png"
  let logo64 = BS.unpack $ B64.encode logo

  req1 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"barsbackground\":\"green\",\"barstextcolour\":\"yellow\",\"headerfont\":\"arial\",\"font\":\"arial\",\"bordercolour\":\"red\",\"buttoncolour\":\"red\",\"emailbackgroundcolour\":\"red\",\"logochanged\":true,\"logo\":\"" ++ logo64 ++ "\"}")]
  (res1, _ctx') <- runTestKontra req1 ctx $ handlePostCompany Nothing >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res1)
  Just newcompany1 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Background colour was set" (Just "green") (companybarsbackground $ companyui newcompany1)
  assertEqual "Text colour was set" (Just "yellow") (companybarstextcolour $ companyui newcompany1)
  assertEqual "Header font was set" (Just "arial") (companyemailheaderfont $ companyui newcompany1)
  assertEqual "Font was set" (Just "arial") (companyemailfont $ companyui newcompany1)
  assertEqual "Border colour was set" (Just "red") (companyemailbordercolour $ companyui newcompany1)
  assertEqual "Button colour was set" (Just "red") (companyemailbuttoncolour $ companyui newcompany1)
  assertEqual "Email background colour was set" (Just "red") (companyemailemailbackgroundcolour $ companyui newcompany1)
  assertBool "File was set" $ isJust (companylogo $ companyui newcompany1)

  req2 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"barsbackground\":\"\",\"barstextcolour\":\"\",\"headerfont\":\"\",\"font\":\"\",\"bordercolour\":\"\",\"buttoncolour\":\"\",\"emailbackgroundcolour\":\"\",\"logochanged\":false,\"logo\":\"\"}")]
  (res2, _ctx') <- runTestKontra req2 ctx $ handlePostCompany Nothing >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res2)
  Just newcompany2 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Background colour reset" Nothing (companybarsbackground $ companyui newcompany2)
  assertEqual "Text colour reset" Nothing (companybarstextcolour $ companyui newcompany2)
  assertEqual "Header font reset" Nothing (companyemailheaderfont $ companyui newcompany2)
  assertEqual "Font reset" Nothing (companyemailfont $ companyui newcompany2)
  assertEqual "Border colour reset" Nothing (companyemailbordercolour $ companyui newcompany2)
  assertEqual "Button colour reset" Nothing (companyemailbuttoncolour $ companyui newcompany2)
  assertEqual "Email background colour reset" Nothing (companyemailemailbackgroundcolour $ companyui newcompany2)
  assertEqual "File still intact" (companylogo $ companyui newcompany1) (companylogo $ companyui newcompany2)

  req3 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"barsbackground\":\"blue\",\"barstextcolour\":\"pink\",\"headerfont\":\"arial\",\"font\":\"arial\",\"bordercolour\":\"red\",\"buttoncolour\":\"red\",\"emailbackgroundcolour\":\"red\",\"logochanged\":true,\"logo\":\"\"}")]
  (res3, _ctx') <- runTestKontra req3 ctx $ handlePostCompany Nothing >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res3)
  Just newcompany3 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Background colour was set" (Just "blue") (companybarsbackground $ companyui newcompany3)
  assertEqual "Text colour was set" (Just "pink") (companybarstextcolour $ companyui newcompany3)
  assertEqual "Header font was set" (Just "arial") (companyemailheaderfont $ companyui newcompany3)
  assertEqual "Font was set" (Just "arial") (companyemailfont $ companyui newcompany3)
  assertEqual "Border colour was set" (Just "red") (companyemailbordercolour $ companyui newcompany3)
  assertEqual "Button colour was set" (Just "red") (companyemailbuttoncolour $ companyui newcompany3)
  assertEqual "Email background colour was set" (Just "red") (companyemailemailbackgroundcolour $ companyui newcompany1)
  assertEqual "File reset" Nothing (companylogo $ companyui newcompany3)

test_handleCompanyLogo :: TestEnv ()
test_handleCompanyLogo = do
  (_user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx <- mkContext defaultValue

  req <- mkRequest GET []
  (res, _ctx') <- runTestKontra req ctx $ handleCompanyLogo (companyid company)

  assertEqual "Response code is 200" 200 (rsCode res)

addNewAdminUserAndCompany :: String -> String -> String -> TestEnv (User, Company)
addNewAdminUserAndCompany fstname sndname email = do
  company <- addNewCompany
  _ <- dbUpdate $ UpdateCompanyUI (companyid company) (companyui company){companybarsbackground = Just "#abcdef"}
  Just user <- addNewCompanyUser fstname sndname email (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  Just updatedcompany <- dbQuery $ GetCompany (companyid company)
  return (updateduser, updatedcompany)
