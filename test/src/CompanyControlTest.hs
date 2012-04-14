module CompanyControlTest (companyControlTests) where

import Control.Applicative
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Text.JSON
import Text.JSON.Types
import Test.Framework

import Company.CompanyControl
import Company.Model
import CompanyAccounts.Model
import Context
import Crypto.RNG
import DB
import Misc
import Redirect
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import Util.JSON

companyControlTests :: (Nexus, CryptoRNGState) -> Test
companyControlTests env = testGroup "CompanyControl" [
    testThat "handleGetCompany works" env test_handleGetCompany
  , testThat "handleGetCompanyJSON works" env test_handleGetCompanyJSON
  , testThat "handlePostCompany can be used to set the company ui" env test_settingUIWithHandlePostCompany
  , testThat "handleCompanyLogo responds when noone is logged in" env test_handleCompanyLogo
  ]

test_handleGetCompany :: TestEnv ()
test_handleGetCompany = do
  (user, _company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest GET []
  (res, _ctx') <- runTestKontra req ctx $ handleGetCompany

  assertBool "Something is returned" (length res > 0)

test_handleGetCompanyJSON :: TestEnv ()
test_handleGetCompanyJSON = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest GET []
  (jsv, _ctx') <- runTestKontra req ctx $ handleGetCompanyJSON

  let ejsonid = getIDFromJSON jsv
  assertBool "Able to get id from json" (isRight ejsonid)

  let (Right jsonid) = ejsonid
  assertEqual "JSON id matches company id" (show $ companyid company) jsonid
  where
    getIDFromJSON :: JSValue -> Either String String
    getIDFromJSON jsv = do
      companyjsv <- jsget "company" jsv
      JSString (JSONString jsonid) <- jsget "id" companyjsv
      return jsonid

test_settingUIWithHandlePostCompany :: TestEnv ()
test_settingUIWithHandlePostCompany = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req1 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"barsbackground\":\"green\",\"barstextcolour\":\"yellow\"}")
                        , ("logo", inFile "public/img/email-logo.png")
                        ]
  (res1, _ctx') <- runTestKontra req1 ctx $ handlePostCompany >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res1)
  Just newcompany1 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Background colour was set" (Just "green") (companybarsbackground $ companyui newcompany1)
  assertEqual "Text colour was set" (Just "yellow") (companybarstextcolour $ companyui newcompany1)
  assertBool "File was set" $ isJust (companylogo $ companyui newcompany1)

  req2 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"barsbackground\":\"\",\"barstextcolour\":\"\"}")
                         ]
  (res2, _ctx') <- runTestKontra req2 ctx $ handlePostCompany >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res2)
  Just newcompany2 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Background colour reset" Nothing (companybarsbackground $ companyui newcompany2)
  assertEqual "Text colour reset" Nothing (companybarstextcolour $ companyui newcompany2)
  assertEqual "File still intact" (companylogo $ companyui newcompany1) (companylogo $ companyui newcompany2)

  req3 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"barsbackground\":\"blue\",\"barstextcolour\":\"pink\"}")
                         , ("islogo", inText "false")
                         ]
  (res3, _ctx') <- runTestKontra req3 ctx $ handlePostCompany >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res3)
  Just newcompany3 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Background colour was set" (Just "blue") (companybarsbackground $ companyui newcompany3)
  assertEqual "Text colour was set" (Just "pink") (companybarstextcolour $ companyui newcompany3)
  assertEqual "File reset" Nothing (companylogo $ companyui newcompany3)

test_handleCompanyLogo :: TestEnv ()
test_handleCompanyLogo = do
  (_user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  globaltemplates <- readGlobalTemplates
  ctx <- mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest GET []
  (res, _ctx') <- runTestKontra req ctx $ handleCompanyLogo (companyid company)

  assertEqual "Response code is 200" 200 (rsCode res)

addNewAdminUserAndCompany :: String -> String -> String -> TestEnv (User, Company)
addNewAdminUserAndCompany fstname sndname email = do
  company <- addNewCompany
  Just user <- addNewCompanyUser fstname sndname email (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  return (updateduser, company)
