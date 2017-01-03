{-# LANGUAGE OverloadedStrings #-}
module Partner.APITest (partnerAPITests) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Int
import Happstack.Server
import Test.Framework
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Company.CompanyID
import Context
import DB
import Doc.API.V2.Calls.DocumentPostCalls (docApiV2New)
import Doc.SignatoryLinkID ()
import Kontra
import KontraPrelude
import Partner.API
import Partner.Model
import TestingUtil
import TestKontra as T
import User.Model

partnerAPITests :: TestEnvSt -> Test
partnerAPITests env = testGroup "PartnerAPI"
  [ testThat "Test Partners API Create Company" env testPartnerCompanyCreate
  , testThat "Test Partners API Update Company" env testPartnerCompanyUpdate
  , testThat "Test Partners API Get Company" env testPartnerCompanyGet
  , testThat "Test Partners API Get Companies" env testPartnerCompaniesGet
  , testThat "Test Partners API Company New User" env testPartnerCompanyUserNew
  , testThat "Test Partners API User Update" env testPartnerUserUpdate
  , testThat "Test Partners API User Get" env testPartnerUserGet
  , testThat "Test Partners API User Get Personal Tokens" env testPartnersUserGetTokens
  , testThat "Test Partners API Company Users Get" env testPartnerCompanyUsersGet
  ]

testPartnerCompanyCreate :: TestEnv ()
testPartnerCompanyCreate = do
  newCompanyJSON <- liftIO $ B.readFile $ inTestDir "json/partner_api_v1/param-partnerCompanyCreate.json"
  let rq_newCompany_params = [ ("json", inTextBS newCompanyJSON) ]
      rq_newCompany_resp_fp = inTestDir "json/partner_api_v1/resp-partnerCompanyCreate.json"

  -- First partner, create company
  (ctx, pid1) <- testJSONCtx
  _ <- runApiJSONTest ctx POST (partnerApiCallV1CompanyCreate pid1) rq_newCompany_params 201 rq_newCompany_resp_fp

  -- Second partner, create company
  (ctx2, pid2) <- testJSONCtx
  _ <- runApiJSONTest ctx2 POST (partnerApiCallV1CompanyCreate pid2) rq_newCompany_params 201 rq_newCompany_resp_fp

  -- Random  user shouldn't be able to create company
  randomUser <- addNewRandomUser
  randomCtx <- (\c -> c { ctxmaybeuser = Just randomUser }) <$> mkContext def
  randomReq <- mkRequestWithHeaders POST [] []
  (randomRes1,_) <- runTestKontra randomReq randomCtx $ partnerApiCallV1CompanyCreate pid1
  (randomRes2,_) <- runTestKontra randomReq randomCtx $ partnerApiCallV1CompanyCreate pid2
  assertEqual ("We should get a 403 response") 403 (rsCode randomRes1)
  assertEqual ("We should get a 403 response") 403 (rsCode randomRes2)

  -- User should only be able to create company for administrated partner ids
  (crossRes1,_) <- runTestKontra randomReq ctx $ partnerApiCallV1CompanyCreate pid2
  (crossRes2,_) <- runTestKontra randomReq ctx2 $ partnerApiCallV1CompanyCreate pid1
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes1)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes2)

  return ()

testPartnerCompanyUpdate :: TestEnv ()
testPartnerCompanyUpdate = do
  companyUpdateJSON <- liftIO $ B.readFile $ inTestDir "json/partner_api_v1/param-partnerCompanyUpdate.json"
  let rq_companyUpdate_params = [ ("json", inTextBS companyUpdateJSON) ]
      rq_companyUpdate_resp_fp = inTestDir "json/partner_api_v1/resp-partnerCompanyUpdate.json"

  -- Update should work
  (ctx,pid,cid) <- testHelperPartnerCompanyCreate
  _ <- runApiJSONTest ctx POST (partnerApiCallV1CompanyUpdate pid cid) rq_companyUpdate_params 200 rq_companyUpdate_resp_fp

  -- Random user shouldn't be able to update
  randomUser <- addNewRandomUser
  randomCtx <- (\c -> c { ctxmaybeuser = Just randomUser }) <$> mkContext def
  randomReq <- mkRequestWithHeaders POST [ ("json", inTextBS companyUpdateJSON) ] []
  (randomRes,_) <- runTestKontra randomReq randomCtx $ partnerApiCallV1CompanyUpdate pid cid
  assertEqual ("We should get a 403 response") 403 (rsCode randomRes)

  -- User should only be able to update company for administrated partner ids
  (ctx',pid',cid') <- testHelperPartnerCompanyCreate
  (crossRes1,_) <- runTestKontra randomReq ctx' $ partnerApiCallV1CompanyUpdate pid cid
  (crossRes2,_) <- runTestKontra randomReq ctx' $ partnerApiCallV1CompanyUpdate pid cid'
  (crossRes3,_) <- runTestKontra randomReq ctx' $ partnerApiCallV1CompanyUpdate pid' cid
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes1)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes2)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes3)

  return ()

testPartnerCompanyGet :: TestEnv ()
testPartnerCompanyGet = do
  let rq_companyUpdate_resp_fp = inTestDir "json/partner_api_v1/resp-partnerCompanyCreate.json"

  -- User should be able to get company
  (ctx,pid,cid) <- testHelperPartnerCompanyCreate
  _ <- runApiJSONTest ctx POST (partnerApiCallV1CompanyGet pid cid) [] 200 rq_companyUpdate_resp_fp

  -- Random user shouldn't be able to update
  randomUser <- addNewRandomUser
  randomCtx <- (\c -> c { ctxmaybeuser = Just randomUser }) <$> mkContext def
  randomReq <- mkRequestWithHeaders POST [] []
  (randomRes,_) <- runTestKontra randomReq randomCtx $ partnerApiCallV1CompanyGet pid cid
  assertEqual ("We should get a 403 response") 403 (rsCode randomRes)

  -- User should only be able to get company for administrated partner ids
  (ctx',pid',cid') <- testHelperPartnerCompanyCreate
  (crossRes1,_) <- runTestKontra randomReq ctx' $ partnerApiCallV1CompanyGet pid cid
  (crossRes2,_) <- runTestKontra randomReq ctx' $ partnerApiCallV1CompanyGet pid cid'
  (crossRes3,_) <- runTestKontra randomReq ctx' $ partnerApiCallV1CompanyGet pid' cid
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes1)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes2)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes3)

  return ()

testPartnerCompaniesGet :: TestEnv ()
testPartnerCompaniesGet = do
  -- create companyA supervised by partnerA
  (ctxA,pidA,_cidA) <- testHelperPartnerCompanyCreate

  -- partnerA can list companies -> [only companyA]
  let rq_companiesList_resp_fp = inTestDir "json/partner_api_v1/resp-partnerCompaniesList.json"
  _ <- runApiJSONTest ctxA POST (partnerApiCallV1CompaniesGet pidA) [] 200 rq_companiesList_resp_fp

  -- create companyB supervised by partnerB
  (_ctxB,_pidB,_cidB) <- testHelperPartnerCompanyCreate

  -- partnerA can list only his companies -> [still only companyA]
  _ <- runApiJSONTest ctxA POST (partnerApiCallV1CompaniesGet pidA) [] 200 rq_companiesList_resp_fp

  -- create randomUser
  randomUser <- addNewRandomUser

  -- random user is denied listing companies of partnerA
  randomCtx <- (\c -> c { ctxmaybeuser = Just randomUser }) <$> mkContext def
  randomReq <- mkRequestWithHeaders POST [] []
  (randomRes,_) <- runTestKontra randomReq randomCtx $ partnerApiCallV1CompaniesGet pidA
  assertEqual ("We should get a 403 response") 403 (rsCode randomRes)

  return ()

testPartnerCompanyUserNew :: TestEnv ()
testPartnerCompanyUserNew = do

  (ctx,pid,cid) <- testHelperPartnerCompanyCreate

  -- Normal user creation should work
  newUserGoodJSON <- liftIO $ B.readFile $ inTestDir "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  let rq_newUserGood_params = [ ("json", inTextBS newUserGoodJSON) ]
      rq_newUserGood_resp_fp = inTestDir "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  _ <- runApiJSONTest ctx POST (partnerApiCallV1UserCreate pid cid) rq_newUserGood_params 201 rq_newUserGood_resp_fp

  -- If Terms of Service have not been agreed to, then we should not create a user
  newUserBadToSJSON <- liftIO $ B.readFile $ inTestDir "json/partner_api_v1/param-partnerCompanyUserNew-no-tos.json"
  rq_newUserBadToS_params <- mkRequestWithHeaders POST [ ("json", inTextBS newUserBadToSJSON) ] []
  (badToSRes,_) <- runTestKontra rq_newUserBadToS_params ctx $ partnerApiCallV1UserCreate pid cid
  assertEqual ("We should get a 403 response") 400 (rsCode badToSRes)

  -- When user with email already exists, we should not create a user
  rq_newUserAlreadyExists_params <- mkRequestWithHeaders POST [ ("json", inTextBS newUserGoodJSON) ] []
  (alreadyExistsRes,_) <- runTestKontra rq_newUserAlreadyExists_params ctx $ partnerApiCallV1UserCreate pid cid
  assertEqual ("We should get a 403 response") 400 (rsCode alreadyExistsRes)

  return ()

testPartnerUserUpdate :: TestEnv ()
testPartnerUserUpdate = do

  (ctx,pid,uid) <- testHelperPartnerUserCreate

  -- Normal user update should work
  updateUserJSON <- liftIO $ B.readFile $ inTestDir "json/partner_api_v1/param-partnerUserUpdate-good.json"
  let rq_updateUser_params = [ ("json", inTextBS updateUserJSON) ]
      rq_UpdateUser_resp_fp = inTestDir "json/partner_api_v1/resp-partnerUserUpdate-good.json"
  _ <- runApiJSONTest ctx POST (partnerApiCallV1UserUpdate pid uid) rq_updateUser_params 200 rq_UpdateUser_resp_fp

  return ()

testPartnerUserGet :: TestEnv ()
testPartnerUserGet = do

  (ctx,pid,uid) <- testHelperPartnerUserCreate

  -- Normal get user should work
  let rq_GetUser_resp_fp = inTestDir "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  _ <- runApiJSONTest ctx GET (partnerApiCallV1UserGet pid uid) [] 200 rq_GetUser_resp_fp

  return ()

testPartnerCompanyUsersGet :: TestEnv ()
testPartnerCompanyUsersGet = do

  -- create company supervised by partner
  (ctx,pid,cid) <- testHelperPartnerCompanyCreate

  -- create another company user
  _uid <- testHelperPartnerCompanyUserCreate ctx pid cid

  -- get users of the company should work
  let rq_GetUsers_resp_fp = inTestDir "json/partner_api_v1/param-partnerCompanyUsersGet.json"
  _ <- runApiJSONTest ctx GET (partnerApiCallV1CompanyUsersGet pid cid) [] 200 rq_GetUsers_resp_fp

  return ()

testPartnersUserGetTokens :: TestEnv ()
testPartnersUserGetTokens = do

  (ctx,pid,uid) <- testHelperPartnerUserCreate

  -- Should be able to get User personal access tokens
  let rq_user_tokens_resp_fp = inTestDir "json/partner_api_v1/resp-partnerUserGetTokens.json"
  respValue <- runApiJSONTest ctx GET (partnerApiCallV1UserGetPersonalToken pid uid) [] 200 rq_user_tokens_resp_fp

  let Object respObject = respValue
      Just (String apitoken) = H.lookup "apitoken" respObject
      Just (String apisecret) = H.lookup "apisecret" respObject
      Just (String accesstoken) = H.lookup "accesstoken" respObject
      Just (String accesssecret) = H.lookup "accesssecret" respObject

  -- Should be able to create a new document using these tokens
  ctx' <- mkContext def
  let authStr = "oauth_signature_method=\"PLAINTEXT\""
             ++ ",oauth_consumer_key=\"" ++ T.unpack apitoken ++ "\""
             ++ ",oauth_token=\"" ++ T.unpack accesstoken ++"\""
             ++ ",oauth_signature=\"" ++ T.unpack apisecret ++ "&" ++ T.unpack accesssecret ++ "\""
  docNewReq <- mkRequestWithHeaders POST [] [ ( "authorization", [authStr] ) ]
  (newDocResp,_) <- runTestKontra docNewReq ctx' $ docApiV2New
  assertEqual "We should get a 201 response" 201 (rsCode newDocResp)

  return ()

--------
-- Utils
--------
-- * These are copied over from API V2 JSON tests
-- They haven't been changed much and have just been minimally reworked so as to
-- fit the current use case
-- Will need some love and care...

testHelperPartnerCompanyCreate :: TestEnv (Context, PartnerID, CompanyID)
testHelperPartnerCompanyCreate = do
  (ctx, pid) <- testJSONCtx
  newCompanyJSON <- liftIO $ B.readFile $ inTestDir "json/partner_api_v1/param-partnerCompanyCreate.json"
  let rq_newCompany_params = [ ("json", inTextBS newCompanyJSON) ]
      rq_newCompany_resp_fp = inTestDir "json/partner_api_v1/resp-partnerCompanyCreate.json"
  respValue <- runApiJSONTest ctx POST (partnerApiCallV1CompanyCreate pid) rq_newCompany_params 201 rq_newCompany_resp_fp
  let Object respObject = respValue
      Just (String cid) = H.lookup "id" respObject
  return (ctx, pid, unsafeCompanyID $ $read $ T.unpack cid)

testHelperPartnerUserCreate :: TestEnv (Context, PartnerID, UserID)
testHelperPartnerUserCreate = do
  (ctx,pid,cid) <- testHelperPartnerCompanyCreate
  uid <- testHelperPartnerCompanyUserCreate ctx pid cid
  return (ctx, pid, uid)

testHelperPartnerCompanyUserCreate :: Context -> PartnerID -> CompanyID -> TestEnv UserID
testHelperPartnerCompanyUserCreate ctx pid cid = do
  newUserGoodJSON <- liftIO $ B.readFile $ inTestDir "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  let rq_newUserGood_params = [ ("json", inTextBS newUserGoodJSON) ]
      rq_newUserGood_resp_fp = inTestDir "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  respValue <- runApiJSONTest ctx POST (partnerApiCallV1UserCreate pid cid) rq_newUserGood_params 201 rq_newUserGood_resp_fp
  let Object respObject = respValue
      Just (String uid) = H.lookup "id" respObject
  return $ unsafeUserID $ $read $ T.unpack uid

testJSONCtx :: TestEnv (Context, PartnerID)
testJSONCtx = do
  partnerAdminUser <- addNewRandomUser
  partnerId <- dbUpdate $ AddNewPartner "My Favourite Upsales"
  _ <- dbUpdate $ MakeUserIDAdminForPartnerID (userid partnerAdminUser) partnerId
  ctx <- (\c -> c { ctxmaybeuser = Just partnerAdminUser }) <$> mkContext def
  return (ctx, partnerId)

runApiJSONTest :: Context          -- ^ Context to run the test in
               -> Method           -- ^ HTTP Method to use for API Call
               -> Kontra Response  -- ^ The API call to use
               -> [(String,Input)] -- ^ List of API call parameters
               -> Int              -- ^ Expected response code
               -> FilePath         -- ^ FilePath to JSON file to match against
               -> TestEnv Value
runApiJSONTest ctx httpMethod apiCall httpHeaders expectedRsCode jsonFile = do
  req <- mkRequestWithHeaders httpMethod httpHeaders []
  (res,_) <- runTestKontra req ctx $ apiCall
  assertEqual ("We should get a " ++ show expectedRsCode ++ " response") expectedRsCode (rsCode res)
  testJSONWith jsonFile (rsBody res)
  let Just jsonValue = decode (rsBody res) :: Maybe Value
  return jsonValue

testJSONWith :: FilePath -> BS.ByteString -> TestEnv ()
testJSONWith fp jsonBS = do
  jsonFileBS <- liftIO $ B.readFile fp
  let Just value    = decode jsonBS
      Just jsonFile = decode jsonFileBS
  assertEqual ("JSON structure and types (including 'null') should match that in " ++ fp)
              (removeValues jsonFile) (removeValues value)
  assertEqual ("JSON structure and values should match if we will remove dynamic values (like documentid or mtime) " ++ fp)
              (removeDynamicValues jsonFile) (removeDynamicValues value)
  return ()

removeValues :: Value -> Value
removeValues (Object m) = Object (H.map removeValues m)
removeValues (Array v)  = Array  (V.map removeValues v)
removeValues (String _) = String ""
removeValues (Number _) = Number 0
removeValues (Bool _)   = Bool False
removeValues Null       = Null

removeDynamicValues :: Value -> Value
removeDynamicValues (Object m) = Object $ H.map removeDynamicValues $ filterOutDynamicKeys m
  where
    filterOutDynamicKeys hm = H.filterWithKey (\k _ -> not $ k `elem` dynamicKeys) hm
    dynamicKeys = ["id", "partner_id", "apitoken", "apisecret", "accesstoken", "accesssecret"]
removeDynamicValues (Array v)  = Array  (V.map removeDynamicValues v)
removeDynamicValues v = v
