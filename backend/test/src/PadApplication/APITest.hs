{-# LANGUAGE OverloadedStrings #-}
module PadApplication.APITest (padAplicationAPITests) where

import Data.Aeson
import Data.Scientific
import Happstack.Server
import Log
import Test.Framework
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Company.Model
import CompanyAccountsTest (addNewAdminUserAndCompany)
import Context
import DB
import KontraPrelude
import PadApplication.API
import TestingUtil
import TestKontra

padAplicationAPITests :: TestEnvSt -> Test
padAplicationAPITests env = testGroup "PadApplicationAPI"
  [ testThat "Test PadApplication API Pad Info Get" env testPadApplicationPadInfoGet
  ]

testPadApplicationPadInfoGet :: TestEnv ()
testPadApplicationPadInfoGet = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx1 <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  req1 <- mkRequest GET []
  (res1, _ctx2) <- runTestKontra req1 ctx1 apiCallGetPadInfo
  logInfo_ $ "PAD_APP_TEST:" <> (T.pack . show $ res1)
  logInfo_ $ "PAD_APP_TEST2:" <> (T.pack . show $ (decode (rsBody res1) :: Maybe Value))
  let Just (Object resObject1) = decode (rsBody res1) :: Maybe Value
      Just (Bool res1padearchiveenabled) = H.lookup "e_archive_enabled" resObject1
      Just (String res1padappmode) = H.lookup "app_mode" resObject1
      Just (Number res1companyid0) = H.lookup "company_id" resObject1
      Right res1companyid = floatingOrInteger res1companyid0

  assertEqual ("We should get a 200 response") 200 (rsCode res1)
  assertEqual "We should get the same company id" (show . companyid $ company) (show res1companyid)
  assertEqual "We should get the same e_archive_enabled"
              (companypadearchiveenabled . companyinfo $ company)
              res1padearchiveenabled
  assertEqual "We should get the same app_mode"
              (show . companypadappmode . companyinfo $ company)
              (T.unpack res1padappmode)
  return ()
