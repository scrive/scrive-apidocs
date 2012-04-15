module ServiceStateTest (serviceStateTests) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Test.Framework

import API.Service.Model
import DB
import User.Model
import TestingUtil
import TestKontra

serviceStateTests :: TestEnvSt -> Test
serviceStateTests env = testGroup "ServiceState" [
    testThat "CreateService works" env test_createService
  , testThat "GetService works" env test_getService
  , testThat "GetServicesForAdmin works" env test_getServicesForAdmin
  , testThat "GetServices works" env test_getServices
  , testThat "UpdateServiceUI works" env test_updateServiceUI
  , testThat "UpdateServiceSettings/GetServiceByLocation works" env test_updateServiceSettings
  ]

test_createService :: TestEnv ()
test_createService = do
  mservice <- addTestService
  assertBool "Service created" $ isJust mservice

test_getService :: TestEnv ()
test_getService = do
  Just Service{serviceid} <- addTestService
  mservice <- dbQuery $ GetService serviceid
  assertBool "Service returned" $ isJust mservice

test_getServicesForAdmin :: TestEnv ()
test_getServicesForAdmin = do
  (uid, _, services) <- addTestServices
  result <- dbQuery $ GetServicesForAdmin uid
  assertBool "GetServicesForAdmin returned correct result" $ services == result

test_getServices :: TestEnv ()
test_getServices = do
  (_, service, services) <- addTestServices
  result <- dbQuery GetServices
  assertBool "GetServicesForAdmin returned correct result" $ service : services == result

test_updateServiceUI :: TestEnv ()
test_updateServiceUI = do
  Just Service{serviceid = sid, serviceui} <- addTestService
  let sui = serviceui {
    servicemailfooter = Just "blabla"
  , servicebuttonstextcolor = Just "yellow"
  , servicebackground = Just "red"
  }
  res <- dbUpdate $ UpdateServiceUI sid sui
  assertBool "ServiceUI updated correctly" res
  Just Service{serviceui = newsui} <- dbQuery $ GetService sid
  assertBool "Correct ServiceUI returned" $ sui == newsui

test_updateServiceSettings :: TestEnv ()
test_updateServiceSettings = do
  Just Service{serviceid = sid, servicesettings} <- addTestService
  let ss = servicesettings {
    servicelocation = Just $ ServiceLocation "test_location"
  , servicemailfromaddress = Just "blabla"
  }
  res <- dbUpdate $ UpdateServiceSettings sid ss
  assertBool "ServiceSettings updated correctly" res
  Just Service{servicesettings = newss} <- dbQuery $ GetService sid
  assertBool "Correct ServiceSettings returned" $ ss == newss
  Just Service{serviceid = newsid} <- dbQuery $ GetServiceByLocation $ fromJust $ servicelocation ss
  assertBool "Correct Service returned" $ sid == newsid

addTestService :: TestEnv (Maybe Service)
addTestService = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  addService "test" userid

addTestServices :: TestEnv (UserID, Service, [Service])
addTestServices = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just User{userid = other_userid} <- addNewUser "Emily" "Green" "emily@green.com"
  services <- forM ["test1", "test2", "test3"] $ \s -> fromJust <$> addService s userid
  Just service <- addService "test0" other_userid
  return (userid, service, services)
