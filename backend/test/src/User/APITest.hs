{-# LANGUAGE OverloadedStrings #-}
module User.APITest (userAPITests) where

import Control.Monad.IO.Class
import Crypto.Hash.Algorithms (SHA1(..))
import Data.Aeson
import Data.Either (Either(..))
import Data.List.Split (splitOneOf)
import Data.OTP (totp)
import Happstack.Server
import Test.Framework
import qualified Codec.Binary.Base32 as B32
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as H
import qualified Data.Text.Encoding as TE

import Company.Model
import CompanyAccountsTest
import Context
import DB
import Doc.Data.Document
import Doc.Data.DocumentStatus
import Doc.DocumentMonad
import Doc.Model.Update
import Doc.SignatoryLinkID ()
import MinutesTime
import TestingUtil
import TestKontra as T
import User.API
import User.Email
import User.Model
import Util.Actor
import Util.QRCode

userAPITests :: TestEnvSt -> Test
userAPITests env = testGroup "UserAPI"
  [ testThat "Test User API Create Login Link" env testUserLoginAndGetSession
  , testThat "Test User API Too Many Attempts To Get Tokens" env testUserTooManyGetTokens
  , testThat "Test User API 2FA setup and disable workflow works" env testUser2FAWorkflow
  , testThat "Test User API Don't delete a user if the email is wrong"
             env testUserNoDeletionIfWrongEmail
  , testThat "Test User API Don't delete a user if she's needed by the company"
             env testUserNoDeletionIfNeededByCompany
  , testThat "Test User API Don't delete a user if she has pending documents"
             env testUserNoDeletionIfPendingDocuments
  , testThat "Test User API Delete a user if there is nothing preventing it"
             env testUserDeletion
  ]

testUserLoginAndGetSession :: TestEnv ()
testUserLoginAndGetSession = do
  -- create a user
  let password = "Secret Password!"
  randomUser <- addNewRandomUserWithPassword password
  ctx <- mkContext def
  req1 <- mkRequest GET
    [ ("email", inText $ unEmail $ useremail $ userinfo randomUser)
    , ("password", inText password)
    ]
  -- get access tokens using an email and password
  (res1, _) <- runTestKontra req1 ctx $ apiCallGetUserPersonalToken
  let Just (Object respObject1) = decode (rsBody res1) :: Maybe Value
      Just (String apitoken) = H.lookup "apitoken" respObject1
      Just (String apisecret) = H.lookup "apisecret" respObject1
      Just (String accesstoken) = H.lookup "accesstoken" respObject1
      Just (String accesssecret) = H.lookup "accesssecret" respObject1

  -- use access tokens to log in and get cookie-like session id
  req2 <- mkRequest GET [ ("personal_token", inTextBS $ rsBody res1) ]
  (res2, _) <- runTestKontra req2 ctx $ apiCallLoginUserAndGetSession
  let Just (Object respObject2) = decode (rsBody res2) :: Maybe Value
      Just (String session_id) = H.lookup "session_id" respObject2
  assertBool ("We should get an ok status in JSON") (session_id /= "")

  -- switch API and Access secrets to get bad input in correct format
  let badtokens = encode . Object . H.fromList $
        [ ("apitoken"    , String apitoken    )
        , ("apisecret"   , String accesssecret)
        , ("accesstoken" , String accesstoken )
        , ("accesssecret", String apisecret   )
        ]
  req3 <- mkRequest GET [ ("personal_token", inTextBS badtokens) ]
  (res3, _) <- runTestKontra req3 ctx $ apiCallLoginUserAndGetSession
  let Just (Object respObject3) = decode (rsBody res3) :: Maybe Value
      Just (String errorType) = H.lookup "error_type" respObject3
  assertEqual ("We should get an error status in JSON") "invalid_authorisation" errorType

testUserTooManyGetTokens :: TestEnv ()
testUserTooManyGetTokens = do
  -- create a user
  let password = "Secret Password!"
  let wrongpassword = "Hello World!"
  randomUser <- addNewRandomUserWithPassword password
  ctx <- mkContext def
  -- getting personap token works with correct password
  req1 <- mkRequest GET
    [ ("email", inText $ unEmail $ useremail $ userinfo randomUser)
    , ("password", inText password)
    ]
  (res1,_) <- runTestKontra req1 ctx $ apiCallGetUserPersonalToken
  assertEqual "We should get a 200 response" 200 (rsCode res1)

  -- now we fail to get access tokens 6 times
  req2 <- mkRequest GET
    [ ("email", inText $ unEmail $ useremail $ userinfo randomUser)
    , ("password", inText wrongpassword)
    ]
  forM_ [1..6] $ \_ -> do
    (res2,_) <- runTestKontra req2 ctx $ apiCallGetUserPersonalToken
    assertEqual "We should get a 403 error response" 403 (rsCode res2)

  -- after 6 failed requests, trying valid password also fails
  req3 <- mkRequest GET
    [ ("email", inText $ unEmail $ useremail $ userinfo randomUser)
    , ("password", inText password)
    ]
  (res3,_) <- runTestKontra req3 ctx $ apiCallGetUserPersonalToken
  assertEqual "We should get a 403 error response" 403 (rsCode res3)

testUser2FAWorkflow :: TestEnv ()
testUser2FAWorkflow = do
  password <- rand 10 $ arbString 3 30
  randomUser <- addNewRandomUserWithPassword password
  ctx' <- set ctxmaybeuser (Just randomUser) <$> mkContext def

  -- Start setting up 2FA
  req_setup2fa <- mkRequest POST []
  (res_setup2fa,_) <- runTestKontra req_setup2fa ctx' setup2FA
  let Just (Object setupRespObj) = decode (rsBody res_setup2fa) :: Maybe Value
      Just (Bool setupActive) = H.lookup "twofactor_active" setupRespObj
      Just (String setupQRCode) = H.lookup "qr_code" setupRespObj
  assertEqual "We should get a 200 response" 200 (rsCode res_setup2fa)
  assertEqual "2FA should not be active yet" False setupActive

  -- Get the secret from the QR code
  now <- currentTime
  qrText <- liftIO $ decodeQR (QRCode . Base64.decodeLenient $ TE.encodeUtf8 setupQRCode)
  let encsecret = head . drop 1 . dropWhile (/= "secret") . splitOneOf "?&=" $ qrText
      Right secret = B32.decode $ BSC.pack encsecret
      totpcode = filter (/='"') . show $ totp SHA1 secret now 30 6

  -- For some reason we need to get updated User and add to Context
  -- otherwise tests fail because TOTP changes are not "seen"
  Just user <- dbQuery $ GetUserByID $ userid randomUser
  ctx <- set ctxmaybeuser (Just user) <$> mkContext def

  -- apiCallGetUserPersonalToken should still work: 2FA not yet confirmed
  do
    req <- mkRequest GET
      [ ("email", inText $ unEmail $ useremail $ userinfo randomUser)
      , ("password", inText password)
      ]
    (res,_) <- runTestKontra req ctx $ apiCallGetUserPersonalToken
    assertEqual "We should get a 200 response" 200 (rsCode res)

  -- "random" confirmation code should not work
  do
    req <- mkRequest POST [ ("totp", inText "123456") ]
    (res,_) <- runTestKontra req ctx confirm2FA
    assertEqual "We should get a 400 response" 400 (rsCode res)

  -- correct TOTP code should work to confirm2FA
  do
    req <- mkRequest POST [ ("totp", inText totpcode) ]
    (res,_) <- runTestKontra req ctx confirm2FA
    assertEqual "TOTP got activated properly" "{\"twofactor_active\":true,\"totp_valid\":true}" (rsBody res)
    assertEqual "We should get a 200 response" 200 (rsCode res)

  -- now apiCallGetUserPersonalToken without totp should fail
  do
    req <- mkRequest GET
      [ ("email", inText $ unEmail $ useremail $ userinfo randomUser)
      , ("password", inText password)
      ]
    (res,_) <- runTestKontra req ctx $ apiCallGetUserPersonalToken
    assertEqual "We should get a 403 response" 403 (rsCode res)

  -- and apiCallGetUserPersonalToken with totp should work
  do
    req <- mkRequest GET
      [ ("email", inText $ unEmail $ useremail $ userinfo randomUser)
      , ("password", inText password)
      , ("totp", inText totpcode)
      ]
    (res,_) <- runTestKontra req ctx $ apiCallGetUserPersonalToken
    assertEqual "We should get a 200 response" 200 (rsCode res)

testUserNoDeletionIfWrongEmail :: TestEnv ()
testUserNoDeletionIfWrongEmail = do
  (anna, _) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"

  ctx <- set ctxmaybeuser (Just anna) <$> mkContext def

  do
    req <- mkRequest POST [("email", inText "wrong@email.com")]
    (res, _) <- runTestKontra req ctx apiCallDeleteUser
    assertEqual "can't delete if the email is wrong"
                400 (rsCode res)

  do
    req <- mkRequest POST [("email", inText "anna@android.com")]
    (res, _) <- runTestKontra req ctx apiCallDeleteUser
    assertEqual "can delete if the email is correct"
                200 (rsCode res)

testUserNoDeletionIfNeededByCompany :: TestEnv ()
testUserNoDeletionIfNeededByCompany = do
  (anna, company) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"
  Just bob   <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)
  Just alice <- addNewCompanyUser "Alice" "Red" "alice@red.com" (companyid company)

  annaCtx  <- set ctxmaybeuser (Just anna)  <$> mkContext def
  bobCtx   <- set ctxmaybeuser (Just bob)   <$> mkContext def
  aliceCtx <- set ctxmaybeuser (Just alice) <$> mkContext def

  do
    req <- mkRequest POST [("email", inText "anna@android.com")]
    (res, _) <- runTestKontra req annaCtx apiCallDeleteUser
    assertEqual "can't delete last company admin with some user left"
                409 (rsCode res)

  do
    _ <- dbUpdate $ SetUserCompanyAdmin (userid bob) True
    req <- mkRequest POST [("email", inText "anna@android.com")]
    (res, _) <- runTestKontra req annaCtx apiCallDeleteUser
    assertEqual "can delete admin if not the last one" 200 (rsCode res)

  do
    req <- mkRequest POST [("email", inText "alice@red.com")]
    (res, _) <- runTestKontra req aliceCtx apiCallDeleteUser
    assertEqual "can delete non-admin user" 200 (rsCode res)

  do
    req <- mkRequest POST [("email", inText "bob@blue.com")]
    (res, _) <- runTestKontra req bobCtx apiCallDeleteUser
    assertEqual "can delete admin if last user" 200 (rsCode res)

testUserNoDeletionIfPendingDocuments :: TestEnv ()
testUserNoDeletionIfPendingDocuments = do
  (_, company) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"
  Just bob <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)

  ctx <- set ctxmaybeuser (Just bob) <$> mkContext def

  doc <- addRandomDocumentWithAuthorAndCondition bob $ \doc ->
    documentstatus doc == Pending && documenttype doc == Signable

  do
    req <- mkRequest POST [("email", inText "bob@blue.com")]
    (res, _) <- runTestKontra req ctx apiCallDeleteUser
    assertEqual "can't delete last user if she has pending documents"
                409 (rsCode res)

  withDocument doc $ randomUpdate $ \t -> CancelDocument $ systemActor t

  do
    req <- mkRequest POST [("email", inText "bob@blue.com")]
    (res, _) <- runTestKontra req ctx apiCallDeleteUser
    assertEqual "can delete once pending documnts are aborted" 200 (rsCode res)

testUserDeletion :: TestEnv ()
testUserDeletion = do
  (anna, _) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"
  ctx <- set ctxmaybeuser (Just anna) <$> mkContext def

  req <- mkRequest POST [("email", inText "anna@android.com")]
  (res, _) <- runTestKontra req ctx apiCallDeleteUser
  assertEqual "user got deleted" 200 (rsCode res)
