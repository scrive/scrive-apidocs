{-# LANGUAGE OverloadedStrings #-}
module PadApplication.APITest (padAplicationAPITests) where

import Data.Aeson
import Happstack.Server
import Test.Framework
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Company.Model
import CompanyAccountsTest (addNewAdminUserAndCompany)
import Context
import DB
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

  ctx1 <- (set ctxmaybeuser (Just user)) <$> mkContext def
  req1 <- mkRequest GET []
  (res1, _ctx2) <- runTestKontra req1 ctx1 apiCallGetPadInfo
  let Just (Object resObject1) = decode (rsBody res1) :: Maybe Value
      Just (Bool res1padearchiveenabled) = H.lookup "e_archive_enabled" resObject1
      Just (String res1padappmode) = H.lookup "app_mode" resObject1

  assertEqual ("We should get a 200 response") 200 (rsCode res1)
  assertEqual "We should get the same e_archive_enabled"
              (companypadearchiveenabled . companyinfo $ company)
              res1padearchiveenabled
  assertEqual "We should get the same app_mode"
              "list_view"
              (T.unpack res1padappmode)
  return ()
