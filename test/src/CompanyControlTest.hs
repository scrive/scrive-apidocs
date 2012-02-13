module CompanyControlTest (companyControlTests) where

import Control.Applicative
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Text.JSON
import Text.JSON.Types
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import qualified Data.ByteString.UTF8 as BS

import Company.CompanyControl
import Company.Model
import CompanyAccounts.Model
import Context
import DB.Classes
import DB.Nexus
import Misc
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import Util.JSON

companyControlTests :: Nexus -> Test
companyControlTests conn = testGroup "CompanyControl" [
    testCase "handleGetCompany works" $ test_handleGetCompany conn
  , testCase "handleGetCompanyJSON works" $ test_handleGetCompanyJSON conn
  , testCase "handlePostCompany can be used to set the company ui" $ test_settingUIWithHandlePostCompany conn
  , testCase "handleCompanyLogo responds when noone is logged in" $ test_handleCompanyLogo conn
  ]

test_handleGetCompany :: Nexus -> Assertion
test_handleGetCompany conn = withTestEnvironment conn $ do
  (user, _company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest GET []
  (res, _ctx') <- runTestKontra req ctx $ handleGetCompany

  assertEqual "Response code is 200" 200 (rsCode res)

test_handleGetCompanyJSON :: Nexus -> Assertion
test_handleGetCompanyJSON conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

test_settingUIWithHandlePostCompany :: Nexus -> Assertion
test_settingUIWithHandlePostCompany conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req1 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"barsbackground\":\"green\"}")
                        , ("logo", inFile "public/img/email-logo.png")
                        ]
  (res1, _ctx') <- runTestKontra req1 ctx $ handlePostCompany >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res1)
  Just newcompany1 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Colour was set" (Just $ BS.fromString "green") (companybarsbackground $ companyui newcompany1)
  assertBool "File was set" $ isJust (companylogo $ companyui newcompany1)

  req2 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"barsbackground\":\"\"}")
                         ]
  (res2, _ctx') <- runTestKontra req2 ctx $ handlePostCompany >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res2)
  Just newcompany2 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Colour reset" Nothing (companybarsbackground $ companyui newcompany2)
  assertEqual "File still intact" (companylogo $ companyui newcompany1) (companylogo $ companyui newcompany2)

  req3 <- mkRequest POST [ ("company", inText $ "{\"id\":\"" ++ show (companyid company) ++ "\",\"barsbackground\":\"blue\"}")
                         , ("islogo", inText "false")
                         ]
  (res3, _ctx') <- runTestKontra req3 ctx $ handlePostCompany >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res3)
  Just newcompany3 <- dbQuery $ GetCompany (companyid company)
  assertEqual "Colour was set" (Just $ BS.fromString "blue") (companybarsbackground $ companyui newcompany3)
  assertEqual "File reset" Nothing (companylogo $ companyui newcompany3)

test_handleCompanyLogo :: Nexus -> Assertion
test_handleCompanyLogo conn = withTestEnvironment conn $ do
  (_user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest GET []
  (res, _ctx') <- runTestKontra req ctx $ handleCompanyLogo (companyid company)

  assertEqual "Response code is 200" 200 (rsCode res)

addNewAdminUserAndCompany :: String -> String -> String -> DB (User, Company)
addNewAdminUserAndCompany fstname sndname email = do
  company <- addNewCompany
  Just user <- addNewCompanyUser fstname sndname email (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  return (updateduser, company)
