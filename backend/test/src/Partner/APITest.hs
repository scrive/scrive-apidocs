{-# LANGUAGE OverloadedStrings #-}
module Partner.APITest (partnerAPITests) where

import Data.Aeson
import Data.Int (Int64)
import Happstack.Server
import Test.Framework
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import AccessControl.Model
import Context
import DB
import Doc.API.V2.Calls.DocumentPostCalls (docApiV2New)
import Doc.SignatoryLinkID ()
import Partner.API
import TestingUtil
import TestingUtil.JSON
import TestKontra as T
import User.Model
import UserGroup.Types

partnerAPITests :: TestEnvSt -> Test
partnerAPITests env = testGroup
  "PartnerAPI"
  [ testThat "Test Partners API Create Company"    env testPartnerCompanyCreate
  , testThat "Test Partners API Update Company"    env testPartnerCompanyUpdate
  , testThat "Test Partners API Update Company Id" env testPartnerCompanyIdUpdate
  , testThat "Test Partners API Partial Update Company"
             env
             testPartnerCompanyPartialUpdate
  , testThat "Test Partners API Get Company"         env testPartnerCompanyGet
  , testThat "Test Partners API Get Companies"       env testPartnerCompaniesGet
  , testThat "Test Partners API Company New User"    env testPartnerCompanyUserNew
  , testThat "Test Partners API User Update"         env testPartnerUserUpdate
  , testThat "Test Partners API User Id Update"      env testPartnerUserIdUpdate
  , testThat "Test Partners API User Partial Update" env testPartnerUserPartialUpdate
  , testThat "Test Partners API User Existing Email Update"
             env
             testPartnerUserUpdateEmailToExisting
  , testThat "Test Partners API User Get"                 env testPartnerUserGet
  , testThat "Test Partners API User Get Personal Tokens" env testPartnersUserGetTokens
  , testThat "Test Partners API Company Users Get"        env testPartnerCompanyUsersGet
  ]

testPartnerCompanyCreate :: TestEnv ()
testPartnerCompanyCreate = do
  newCompanyJSON <- readTestFile "json/partner_api_v1/param-partnerCompanyCreate.json"
  let rq_newCompany_params = [("json", inTextBS newCompanyJSON)]
      rq_newCompany_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyCreate.json"

  -- First partner, create user group
  (ctx1, pid1) <- testJSONCtxWithPartnerGroupID
  void $ runApiJSONTest ctx1
                        POST
                        (partnerApiCallV1CompanyCreate $ fromUserGroupID pid1)
                        rq_newCompany_params
                        201
                        rq_newCompany_resp_fp

  -- Second partner, create user group
  (ctx2, pid2) <- testJSONCtxWithPartnerGroupID
  void $ runApiJSONTest ctx2
                        POST
                        (partnerApiCallV1CompanyCreate $ fromUserGroupID pid2)
                        rq_newCompany_params
                        201
                        rq_newCompany_resp_fp

  -- Random  user shouldn't be able to create company
  randomUser      <- addNewRandomUser
  randomCtx       <- (set #maybeUser (Just randomUser)) <$> mkContext defaultLang
  randomReq       <- mkRequestWithHeaders POST [] []
  (randomRes1, _) <-
    runTestKontra randomReq randomCtx
      $ (partnerApiCallV1CompanyCreate $ fromUserGroupID pid1)
  (randomRes2, _) <-
    runTestKontra randomReq randomCtx
      $ (partnerApiCallV1CompanyCreate $ fromUserGroupID pid2)
  assertEqual ("We should get a 403 response") 403 (rsCode randomRes1)
  assertEqual ("We should get a 403 response") 403 (rsCode randomRes2)

  -- User should only be able to create company for administrated partner ids
  (crossRes1, _) <-
    runTestKontra randomReq ctx1 $ (partnerApiCallV1CompanyCreate $ fromUserGroupID pid2)
  (crossRes2, _) <-
    runTestKontra randomReq ctx2 $ (partnerApiCallV1CompanyCreate $ fromUserGroupID pid1)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes1)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes2)

  -- Test role combinations; use the first user and partner structure generated.
  let (Just uid) = ctx1 ^? #maybeUser % _Just % #id

  -- 1) only a partner admin; should succeed
  void . dbUpdate $ SetUserCompanyAdmin uid False
  (Just usr') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr') ctx1)
                                POST
                                (partnerApiCallV1CompanyCreate $ fromUserGroupID pid1)
                                rq_newCompany_params
                                201

  -- 2) not a partner admin but a company admin; should fail
  void . dbUpdate $ AccessControlRemoveUserGroupAdminRole uid pid1
  void . dbUpdate $ SetUserCompanyAdmin uid True
  (Just usr'') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr'') ctx1)
                                POST
                                (partnerApiCallV1CompanyCreate $ fromUserGroupID pid1)
                                rq_newCompany_params
                                403

  -- 3) not a partner admin, nor a company admin; should fail
  void . dbUpdate $ SetUserCompanyAdmin uid False
  (Just usr''') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr''') ctx1)
                                POST
                                (partnerApiCallV1CompanyCreate $ fromUserGroupID pid1)
                                rq_newCompany_params
                                403

  return ()

testPartnerCompanyUpdate :: TestEnv ()
testPartnerCompanyUpdate = do
  companyUpdateJSON <- readTestFile "json/partner_api_v1/param-partnerCompanyUpdate.json"
  let rq_companyUpdate_params = [("json", inTextBS companyUpdateJSON)]
      rq_companyUpdate_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyUpdate.json"

  -- Update should work
  (ctx, pid, cid) <- testHelperPartnerCompanyCreate

  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1CompanyUpdate (fromUserGroupID pid) cid)
                        rq_companyUpdate_params
                        200
                        rq_companyUpdate_resp_fp

  -- Random user shouldn't be able to update
  randomUser     <- addNewRandomUser
  randomCtx      <- (set #maybeUser (Just randomUser)) <$> mkContext defaultLang
  randomReq      <- mkRequestWithHeaders POST [("json", inTextBS companyUpdateJSON)] []
  (randomRes, _) <- runTestKontra randomReq randomCtx
    $ partnerApiCallV1CompanyUpdate (fromUserGroupID pid) cid
  assertEqual ("We should get a 403 response") 403 (rsCode randomRes)

  -- User should only be able to update company for administrated partner ids
  (ctx', pid', cid') <- testHelperPartnerCompanyCreate
  (crossRes1, _)     <- runTestKontra randomReq ctx'
    $ partnerApiCallV1CompanyUpdate (fromUserGroupID pid) cid
  (crossRes2, _) <- runTestKontra randomReq ctx'
    $ partnerApiCallV1CompanyUpdate (fromUserGroupID pid) cid'
  (crossRes3, _) <- runTestKontra randomReq ctx'
    $ partnerApiCallV1CompanyUpdate (fromUserGroupID pid') cid
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes1)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes2)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes3)

  -- Test role combinations; use the first user and partner structure
  -- generated.
  let (Just uid) = ctx ^? #maybeUser % _Just % #id

  -- 1) user is a partner admin; should succeed
  void . dbUpdate $ SetUserCompanyAdmin uid False
  (Just usr') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk
    (set #maybeUser (Just usr') ctx)
    POST
    (partnerApiCallV1CompanyUpdate (fromUserGroupID pid) cid)
    rq_companyUpdate_params
    200

  -- 2) not a partner admin but a company admin; should fail
  void . dbUpdate $ AccessControlRemoveUserGroupAdminRole uid pid
  void . dbUpdate $ SetUserCompanyAdmin uid True
  (Just usr'') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk
    (set #maybeUser (Just usr'') ctx)
    POST
    (partnerApiCallV1CompanyUpdate (fromUserGroupID pid) cid)
    rq_companyUpdate_params
    403

  -- 3) not a partner admin, nor a company admin; should fail
  void . dbUpdate $ SetUserCompanyAdmin uid False
  (Just usr''') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk
    (set #maybeUser (Just usr''') ctx)
    POST
    (partnerApiCallV1CompanyUpdate (fromUserGroupID pid) cid)
    []
    403

  return ()

testPartnerCompanyPartialUpdate :: TestEnv ()
testPartnerCompanyPartialUpdate = do
  companyUpdateJSON <- readTestFile
    "json/partner_api_v1/param-partnerCompanyPartialUpdate.json"
  let rq_companyUpdate_params = [("json", inTextBS companyUpdateJSON)]
      rq_companyUpdate_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyPartialUpdate.json"

  -- Update should work
  (ctx, pid, cid) <- testHelperPartnerCompanyCreate
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1CompanyUpdate (fromUserGroupID pid) cid)
                        rq_companyUpdate_params
                        200
                        rq_companyUpdate_resp_fp

  return ()

testPartnerCompanyIdUpdate :: TestEnv ()
testPartnerCompanyIdUpdate = do
  companyUpdateJSON <- readTestFile
    "json/partner_api_v1/param-partnerCompanyIdUpdate.json"
  let rq_companyUpdate_params = [("json", inTextBS companyUpdateJSON)]
      rq_companyUpdate_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyIdUpdate.json"

  -- Update should work
  (ctx, pid, cid) <- testHelperPartnerCompanyCreate
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1CompanyUpdate (fromUserGroupID pid) cid)
                        rq_companyUpdate_params
                        200
                        rq_companyUpdate_resp_fp

  return ()

testPartnerCompanyGet :: TestEnv ()
testPartnerCompanyGet = do
  let rq_companyUpdate_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyCreate.json"

  -- User should be able to get company
  (ctx, pid, cid) <- testHelperPartnerCompanyCreate
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1CompanyGet (fromUserGroupID pid) cid)
                        []
                        200
                        rq_companyUpdate_resp_fp

  -- Random user shouldn't be able to update
  randomUser     <- addNewRandomUser
  randomCtx      <- (set #maybeUser (Just randomUser)) <$> mkContext defaultLang
  randomReq      <- mkRequestWithHeaders POST [] []
  (randomRes, _) <- runTestKontra randomReq randomCtx
    $ partnerApiCallV1CompanyGet (fromUserGroupID pid) cid
  assertEqual ("We should get a 403 response") 403 (rsCode randomRes)

  -- User should only be able to get company for administrated partner ids
  (ctx', pid', cid') <- testHelperPartnerCompanyCreate
  (crossRes1, _)     <- runTestKontra randomReq ctx'
    $ partnerApiCallV1CompanyGet (fromUserGroupID pid) cid
  (crossRes2, _) <- runTestKontra randomReq ctx'
    $ partnerApiCallV1CompanyGet (fromUserGroupID pid) cid'
  (crossRes3, _) <- runTestKontra randomReq ctx'
    $ partnerApiCallV1CompanyGet (fromUserGroupID pid') cid
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes1)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes2)
  assertEqual ("We should get a 403 response") 403 (rsCode crossRes3)

  -- Test role combinations; use the first user and partner structure
  -- generated.
  let (Just uid) = ctx ^? #maybeUser % _Just % #id

  -- 1) user is a partner admin; should succeed
  void . dbUpdate $ SetUserCompanyAdmin uid False
  (Just usr') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr') ctx)
                                POST
                                (partnerApiCallV1CompanyGet (fromUserGroupID pid) cid)
                                []
                                200

  -- 2) not a partner admin but a company admin; should fail
  void . dbUpdate $ AccessControlRemoveUserGroupAdminRole uid pid
  void . dbUpdate $ SetUserCompanyAdmin uid True
  (Just usr'') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr'') ctx)
                                POST
                                (partnerApiCallV1CompanyGet (fromUserGroupID pid) cid)
                                []
                                403

  -- 3) not a partner admin, nor a company admin; should fail
  void . dbUpdate $ SetUserCompanyAdmin uid False
  (Just usr''') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr''') ctx)
                                POST
                                (partnerApiCallV1CompanyGet (fromUserGroupID pid) cid)
                                []
                                403

  return ()

testPartnerCompaniesGet :: TestEnv ()
testPartnerCompaniesGet = do
  -- create companyA supervised by partnerA
  (ctxA, pidA, _cidA) <- testHelperPartnerCompanyCreate

  -- partnerA can list companies -> [only companyA]
  let rq_companiesList_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompaniesList.json"
  void $ runApiJSONTest ctxA
                        POST
                        (partnerApiCallV1CompaniesGet (fromUserGroupID pidA))
                        []
                        200
                        rq_companiesList_resp_fp

  -- create companyB supervised by partnerB
  (_ctxB, _pidB, _cidB) <- testHelperPartnerCompanyCreate

  -- partnerA can list only his companies -> [still only companyA]
  void $ runApiJSONTest ctxA
                        POST
                        (partnerApiCallV1CompaniesGet (fromUserGroupID pidA))
                        []
                        200
                        rq_companiesList_resp_fp

  -- create randomUser
  randomUser     <- addNewRandomUser

  -- random user is denied listing companies of partnerA
  randomCtx      <- (set #maybeUser (Just randomUser)) <$> mkContext defaultLang
  randomReq      <- mkRequestWithHeaders POST [] []
  (randomRes, _) <- runTestKontra randomReq randomCtx
    $ partnerApiCallV1CompaniesGet (fromUserGroupID pidA)
  assertEqual ("We should get a 403 response") 403 (rsCode randomRes)

  -- Test role combinations; use the first user and partner structure generated.
  let (Just uid) = ctxA ^? #maybeUser % _Just % #id

  -- 1) only a partner admin; should succeed
  void . dbUpdate $ SetUserCompanyAdmin uid False
  (Just usr') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr') ctxA)
                                POST
                                (partnerApiCallV1CompaniesGet (fromUserGroupID pidA))
                                []
                                200

  -- 2) not a partner admin but a company admin; should fail
  void . dbUpdate $ AccessControlRemoveUserGroupAdminRole uid pidA
  void . dbUpdate $ SetUserCompanyAdmin uid True
  (Just usr'') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr'') ctxA)
                                POST
                                (partnerApiCallV1CompaniesGet (fromUserGroupID pidA))
                                []
                                403

  -- 3) not a partner admin, nor a company admin; should fail
  void . dbUpdate $ SetUserCompanyAdmin uid False
  (Just usr''') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr''') ctxA)
                                POST
                                (partnerApiCallV1CompaniesGet (fromUserGroupID pidA))
                                []
                                403

  return ()

testPartnerCompanyUserNew :: TestEnv ()
testPartnerCompanyUserNew = do

  (ctx, pid, cid) <- testHelperPartnerCompanyCreate

  -- Normal user creation should work
  newUserGoodJSON <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  let rq_newUserGood_params = [("json", inTextBS newUserGoodJSON)]
      rq_newUserGood_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyUserNew-good.json"
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1UserCreate (fromUserGroupID pid) cid)
                        rq_newUserGood_params
                        201
                        rq_newUserGood_resp_fp

  -- If Terms of Service have not been agreed to, then we should not
  -- create a user.
  newUserBadToSJSON <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-no-tos.json"
  rq_newUserBadToS_params <- mkRequestWithHeaders POST
                                                  [("json", inTextBS newUserBadToSJSON)]
                                                  []
  (badToSRes, _) <- runTestKontra rq_newUserBadToS_params ctx
    $ partnerApiCallV1UserCreate (fromUserGroupID pid) cid
  assertEqual ("We should get a 400 response") 400 (rsCode badToSRes)

  -- When user with email already exists, we should not create a user
  rq_newUserAlreadyExists_params <- mkRequestWithHeaders
    POST
    [("json", inTextBS newUserGoodJSON)]
    []
  (alreadyExistsRes, _) <-
    runTestKontra rq_newUserAlreadyExists_params ctx
      $ partnerApiCallV1UserCreate (fromUserGroupID pid) cid
  assertEqual ("We should get a 400 response") 400 (rsCode alreadyExistsRes)

  -- Test role combinations; use the first user and partner structure
  -- generated.
  let (Just uid) = ctx ^? #maybeUser % _Just % #id
  -- cannot reuse old user email
  newUserGoodJSON' <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-good3.json"
  let rq_newUserGood_params' = [("json", inTextBS newUserGoodJSON')]

  -- 1) user is a partner admin; should succeed
  void . dbUpdate $ SetUserCompanyAdmin uid False
  (Just usr') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr') ctx)
                                POST
                                (partnerApiCallV1UserCreate (fromUserGroupID pid) cid)
                                rq_newUserGood_params'
                                201

  newUserGoodJSON'' <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-good4.json"
  let rq_newUserGood_params'' = [("json", inTextBS newUserGoodJSON'')]

  -- 2) not a partner admin but a company admin; should fail
  void . dbUpdate $ AccessControlRemoveUserGroupAdminRole uid pid
  void . dbUpdate $ SetUserCompanyAdmin uid True
  (Just usr'') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr'') ctx)
                                POST
                                (partnerApiCallV1UserCreate (fromUserGroupID pid) cid)
                                rq_newUserGood_params''
                                403

  newUserGoodJSON''' <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-good5.json"
  let rq_newUserGood_params''' = [("json", inTextBS newUserGoodJSON''')]

  -- 3) not a partner admin, nor a company admin; should fail
  void . dbUpdate $ SetUserCompanyAdmin uid False
  (Just usr''') <- dbQuery . GetUserByID $ uid
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr''') ctx)
                                POST
                                (partnerApiCallV1UserCreate (fromUserGroupID pid) cid)
                                rq_newUserGood_params'''
                                403

  return ()

testPartnerUserUpdate :: TestEnv ()
testPartnerUserUpdate = do

  (ctx, pid, uid) <- testHelperPartnerUserCreate

  -- Normal user update should work
  updateUserJSON  <- readTestFile "json/partner_api_v1/param-partnerUserUpdate-good.json"
  let rq_updateUser_params = [("json", inTextBS updateUserJSON)]
      rq_UpdateUser_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerUserUpdate-good.json"
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1UserUpdate (fromUserGroupID pid) uid)
                        rq_updateUser_params
                        200
                        rq_UpdateUser_resp_fp

  -- Updating has_accepted_tos should not work
  updateUserNoToSJSON <- readTestFile
    "json/partner_api_v1/param-partnerUserUpdate-no-tos.json"
  rq_tos         <- mkRequestWithHeaders POST [("json", inTextBS updateUserNoToSJSON)] []
  (tosResult, _) <- runTestKontra rq_tos ctx
    $ partnerApiCallV1UserUpdate (fromUserGroupID pid) uid
  assertEqual ("We should get a 400 response") 400 (rsCode tosResult)

  -- Test role combinations; use the first user and partner structure
  -- generated.
  let (Just uidAdmin) = ctx ^? #maybeUser % _Just % #id

  -- 1) user is a partner admin; should succeed
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin False
  (Just usr') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr') ctx)
                                POST
                                (partnerApiCallV1UserUpdate (fromUserGroupID pid) uid)
                                rq_updateUser_params
                                200

  -- 2) not a partner admin but a company admin; should fail
  void . dbUpdate $ AccessControlRemoveUserGroupAdminRole uidAdmin pid
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin True
  (Just usr'') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr'') ctx)
                                POST
                                (partnerApiCallV1UserUpdate (fromUserGroupID pid) uid)
                                rq_updateUser_params
                                403

  -- 3) not a partner admin, nor a company admin; should fail
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin False
  (Just usr''') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr''') ctx)
                                POST
                                (partnerApiCallV1UserUpdate (fromUserGroupID pid) uid)
                                rq_updateUser_params
                                403

  return ()

testPartnerUserUpdateEmailToExisting :: TestEnv ()
testPartnerUserUpdateEmailToExisting = do

  (ctx, pid, cid)  <- testHelperPartnerCompanyCreate

  -- Normal user creation should work.
  newUserGood1JSON <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  let rq_newUserGood1_params = [("json", inTextBS newUserGood1JSON)]
      rq_newUserGood1_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyUserNew-good.json"
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1UserCreate (fromUserGroupID pid) cid)
                        rq_newUserGood1_params
                        201
                        rq_newUserGood1_resp_fp

  -- Normal creation of another user should work.
  newUserGood2JSON <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-good2.json"
  let rq_newUserGood2_params = [("json", inTextBS newUserGood2JSON)]
      rq_newUserGood2_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyUserNew-good2.json"
  respValue <- runApiJSONTest ctx
                              POST
                              (partnerApiCallV1UserCreate (fromUserGroupID pid) cid)
                              rq_newUserGood2_params
                              201
                              rq_newUserGood2_resp_fp
  let Object respObject      = respValue
      Just   (String uidstr) = H.lookup "id" respObject
      uid                    = unsafeUserID $ read uidstr

  -- When user with email already exists, we must not modify email
  updateToExistingEmailJSON <- readTestFile
    "json/partner_api_v1/param-partnerUserEmailUpdate.json"
  rq_updateToExistingEmail_params <- mkRequestWithHeaders
    POST
    [("json", inTextBS updateToExistingEmailJSON)]
    []
  (alreadyExistsRes, _) <-
    runTestKontra rq_updateToExistingEmail_params ctx
      $ partnerApiCallV1UserUpdate (fromUserGroupID pid) uid
  assertEqual ("We should get a 400 response") 400 (rsCode alreadyExistsRes)

  return ()

testPartnerUserPartialUpdate :: TestEnv ()
testPartnerUserPartialUpdate = do

  (ctx, pid, uid) <- testHelperPartnerUserCreate

  -- Normal user update should work
  updateUserJSON  <- readTestFile
    "json/partner_api_v1/param-partnerUserPartialUpdate-good.json"
  let rq_updateUser_params = [("json", inTextBS updateUserJSON)]
      rq_UpdateUser_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerUserPartialUpdate-good.json"
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1UserUpdate (fromUserGroupID pid) uid)
                        rq_updateUser_params
                        200
                        rq_UpdateUser_resp_fp

  return ()

testPartnerUserIdUpdate :: TestEnv ()
testPartnerUserIdUpdate = do

  (ctx, pid, uid) <- testHelperPartnerUserCreate

  -- Normal user update should work
  updateUserJSON  <- readTestFile "json/partner_api_v1/param-partnerUserIdUpdate.json"
  let rq_updateUser_params = [("json", inTextBS updateUserJSON)]
      rq_UpdateUser_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerUserIdUpdate.json"
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1UserUpdate (fromUserGroupID pid) uid)
                        rq_updateUser_params
                        200
                        rq_UpdateUser_resp_fp

  return ()

testPartnerUserGet :: TestEnv ()
testPartnerUserGet = do

  (ctx, pid, uid) <- testHelperPartnerUserCreate

  -- Normal get user should work.
  let rq_GetUser_resp_fp =
        inTestDir "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  void $ runApiJSONTest ctx
                        GET
                        (partnerApiCallV1UserGet (fromUserGroupID pid) uid)
                        []
                        200
                        rq_GetUser_resp_fp

  -- Test role combinations; use the first user and partner structure
  -- generated.
  let (Just uidAdmin) = ctx ^? #maybeUser % _Just % #id

  -- 1) only a partner admin; should succeed
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin False
  (Just usr') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr') ctx)
                                POST
                                (partnerApiCallV1UserGet (fromUserGroupID pid) uid)
                                []
                                200

  -- 2) not a partner admin but a company admin; should fail
  void . dbUpdate $ AccessControlRemoveUserGroupAdminRole uidAdmin pid
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin True
  (Just usr'') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr'') ctx)
                                POST
                                (partnerApiCallV1UserGet (fromUserGroupID pid) uid)
                                []
                                403

  -- 3) not a partner admin, nor a company admin; should fail
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin False
  (Just usr''') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk (set #maybeUser (Just usr''') ctx)
                                POST
                                (partnerApiCallV1UserGet (fromUserGroupID pid) uid)
                                []
                                403

  return ()

testPartnerCompanyUsersGet :: TestEnv ()
testPartnerCompanyUsersGet = do

  -- create company supervised by partner
  (ctx, pid, cid) <- testHelperPartnerCompanyCreate

  -- create another company user
  _uid            <- testHelperPartnerCompanyUserCreate ctx (fromUserGroupID pid) cid

  -- get users of the company should work
  let rq_GetUsers_resp_fp =
        inTestDir "json/partner_api_v1/param-partnerCompanyUsersGet.json"
  void $ runApiJSONTest ctx
                        GET
                        (partnerApiCallV1CompanyUsersGet (fromUserGroupID pid) cid)
                        []
                        200
                        rq_GetUsers_resp_fp

  -- Test role combinations; use the first user and partner structure
  -- generated.
  let (Just uidAdmin) = ctx ^? #maybeUser % _Just % #id

  -- 1) only a partner admin; should succeed
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin False
  (Just usr') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk
    (set #maybeUser (Just usr') ctx)
    POST
    (partnerApiCallV1CompanyUsersGet (fromUserGroupID pid) cid)
    []
    200

  -- 2) not a partner admin but a company admin; should fail
  void . dbUpdate $ AccessControlRemoveUserGroupAdminRole uidAdmin pid
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin True
  (Just usr'') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk
    (set #maybeUser (Just usr'') ctx)
    POST
    (partnerApiCallV1CompanyUsersGet (fromUserGroupID pid) cid)
    []
    403

  -- 3) not a partner admin, nor a company admin; should fail
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin False
  (Just usr''') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk
    (set #maybeUser (Just usr''') ctx)
    POST
    (partnerApiCallV1CompanyUsersGet (fromUserGroupID pid) cid)
    []
    403

  return ()

testPartnersUserGetTokens :: TestEnv ()
testPartnersUserGetTokens = do

  (ctx, pid, uid) <- testHelperPartnerUserCreate

  -- Should be able to get User personal access tokens
  let rq_user_tokens_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerUserGetTokens.json"
  respValue <- runApiJSONTest
    ctx
    GET
    (partnerApiCallV1UserGetPersonalToken (fromUserGroupID pid) uid)
    []
    200
    rq_user_tokens_resp_fp

  let Object respObject            = respValue
      Just   (String apitoken    ) = H.lookup "apitoken" respObject
      Just   (String apisecret   ) = H.lookup "apisecret" respObject
      Just   (String accesstoken ) = H.lookup "accesstoken" respObject
      Just   (String accesssecret) = H.lookup "accesssecret" respObject

  -- Should be able to create a new document using these tokens
  ctx' <- mkContext defaultLang
  let authStr =
        "oauth_signature_method=\"PLAINTEXT\""
          ++ ",oauth_consumer_key=\""
          ++ T.unpack apitoken
          ++ "\""
          ++ ",oauth_token=\""
          ++ T.unpack accesstoken
          ++ "\""
          ++ ",oauth_signature=\""
          ++ T.unpack apisecret
          ++ "&"
          ++ T.unpack accesssecret
          ++ "\""
  docNewReq       <- mkRequestWithHeaders POST [] [("authorization", [T.pack authStr])]
  (newDocResp, _) <- runTestKontra docNewReq ctx' $ docApiV2New
  assertEqual "We should get a 201 response" 201 (rsCode newDocResp)

  -- Test role combinations; use the first user and partner structure
  -- generated.
  let (Just uidAdmin) = ctx ^? #maybeUser % _Just % #id

  -- 1) only a partner admin; should succeed
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin False
  (Just usr') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk
    (set #maybeUser (Just usr') ctx)
    GET
    (partnerApiCallV1UserGetPersonalToken (fromUserGroupID pid) uid)
    []
    200

  -- 2) not a partner admin but a company admin; should fail
  void . dbUpdate $ AccessControlRemoveUserGroupAdminRole uidAdmin pid
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin True
  (Just usr'') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk
    (set #maybeUser (Just usr'') ctx)
    POST
    (partnerApiCallV1UserGetPersonalToken (fromUserGroupID pid) uid)
    []
    403

  -- 3) not a partner admin, nor a company admin; should fail
  void . dbUpdate $ SetUserCompanyAdmin uidAdmin False
  (Just usr''') <- dbQuery . GetUserByID $ uidAdmin
  void $ runApiJSONTestNoResChk
    (set #maybeUser (Just usr''') ctx)
    POST
    (partnerApiCallV1UserGetPersonalToken (fromUserGroupID pid) uid)
    []
    403

  return ()

--------
-- Utils
--------

-- These are copied over from API V2 JSON tests
--
-- They haven't been changed much and have just been minimally
-- reworked so as to fit the current use case.
--
-- Will need some love and care...

testHelperPartnerCompanyCreate :: TestEnv (Context, UserGroupID, UserGroupID)
testHelperPartnerCompanyCreate = do
  (ctx, partnerUgID) <- testJSONCtxWithPartnerGroupID
  newCompanyJSON     <- readTestFile "json/partner_api_v1/param-partnerCompanyCreate.json"
  let rq_newCompany_params = [("json", inTextBS newCompanyJSON)]
      rq_newCompany_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyCreate.json"
  respValue <- runApiJSONTest
    ctx
    POST
    (partnerApiCallV1CompanyCreate $ fromUserGroupID partnerUgID)
    rq_newCompany_params
    201
    rq_newCompany_resp_fp
  let Object respObject   = respValue
      Just   (String cid) = H.lookup "id" respObject
  return (ctx, partnerUgID, read cid)

testHelperPartnerUserCreate :: TestEnv (Context, UserGroupID, UserID)
testHelperPartnerUserCreate = do
  (ctx, pid, company_ugid) <- testHelperPartnerCompanyCreate
  uid <- testHelperPartnerCompanyUserCreate ctx (fromUserGroupID pid) company_ugid
  return (ctx, pid, uid)

testHelperPartnerCompanyUserCreate :: Context -> Int64 -> UserGroupID -> TestEnv UserID
testHelperPartnerCompanyUserCreate ctx pid company_ugid = do
  newUserGoodJSON <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  let rq_newUserGood_params = [("json", inTextBS newUserGoodJSON)]
      rq_newUserGood_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyUserNew-good.json"
  respValue <- runApiJSONTest ctx
                              POST
                              (partnerApiCallV1UserCreate pid company_ugid)
                              rq_newUserGood_params
                              201
                              rq_newUserGood_resp_fp
  let Object respObject   = respValue
      Just   (String uid) = H.lookup "id" respObject
  return $ unsafeUserID $ read uid

-- we produce the UserGroupID as an Int64 since that is now what the partners
-- API expect for its handlers.
testJSONCtxWithPartnerGroupID :: TestEnv (Context, UserGroupID)
testJSONCtxWithPartnerGroupID = do
  (partnerAdminUser, partnerAdminUserGroup) <- addNewRandomPartnerUser
  ctx <- (set #maybeUser (Just partnerAdminUser)) <$> mkContext defaultLang
  return (ctx, partnerAdminUserGroup ^. #id)

runApiJSONTest
  :: Context             -- ^ Context to run the test in
  -> Method              -- ^ HTTP Method to use for API Call
  -> KontraTest Response -- ^ The API call to use
  -> [(Text, Input)]    -- ^ List of API call parameters
  -> Int                 -- ^ Expected response code
  -> FilePath            -- ^ FilePath to JSON file to match against
  -> TestEnv Value
runApiJSONTest ctx httpMethod apiCall httpHeaders expectedRsCode jsonFile = do
  req      <- mkRequestWithHeaders httpMethod httpHeaders []
  (res, _) <- runTestKontra req ctx $ apiCall
  assertEqual ("We should get a " ++ show expectedRsCode ++ " response")
              expectedRsCode
              (rsCode res)
  testJSONWith jsonFile (rsBody res)
  let Just jsonValue = decode (rsBody res) :: Maybe Value
  return jsonValue

runApiJSONTestNoResChk
  :: Context             -- ^ Context to run the test in
  -> Method              -- ^ HTTP Method to use for API Call
  -> KontraTest Response -- ^ The API call to use
  -> [(Text, Input)]    -- ^ List of API call parameters
  -> Int                 -- ^ Expected response code
  -> TestEnv ()
runApiJSONTestNoResChk ctx httpMethod apiCall httpHeaders expectedRsCode = do
  req      <- mkRequestWithHeaders httpMethod httpHeaders []
  (res, _) <- runTestKontra req ctx $ apiCall
  assertEqual ("We should get a " ++ show expectedRsCode ++ " response")
              expectedRsCode
              (rsCode res)
  return ()

testJSONWith :: FilePath -> BS.ByteString -> TestEnv ()
testJSONWith = testJSONWithDynamicKeys
  ["id", "accesssecret", "accesstoken", "apisecret", "apitoken", "partner_id"]
