module User.APITest (userAPITests) where

import Control.Monad.IO.Class
import Crypto.Hash.Algorithms (SHA1(..))
import Data.Aeson
import Data.List.Split (splitOneOf)
import Data.OTP (totp)
import Data.Unjson
import Happstack.Server
import Test.Framework
import Test.QuickCheck
import qualified Codec.Binary.Base32 as B32
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Attachment.Model
import DataRetentionPolicy
import DB
import Doc.API.V2.AesonTestUtils
import Doc.DocumentMonad
import Doc.Model.Query
import Doc.Model.Update
import Doc.SignatoryLinkID ()
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import MinutesTime
import Tag
import TestingUtil
import TestKontra as T
import User.API
import User.Email
import User.Model
import UserGroup.Model
import Util.Actor
import Util.QRCode

userAPITests :: TestEnvSt -> Test
userAPITests env = testGroup
  "UserAPI"
  [ testThat "Test User API Create Login Link" env testUserLoginAndGetSession
  , testThat "Test User API Too Many Attempts To Get Tokens" env testUserTooManyGetTokens
  , testThat "Test User API 2FA setup and disable workflow works" env testUser2FAWorkflow
  , testThat "Test User API Don't delete a user if the email is wrong"
             env
             testUserNoDeletionIfWrongEmail
  , testThat "Test User API Don't delete a user if she has pending documents"
             env
             testUserNoDeletionIfPendingDocuments
  , testThat "Test User API Delete a user if there is nothing preventing it"
             env
             testUserDeletion
  , testThat
    "Test User API Delete a user and give the shared\
             \ attachments/templates to the oldest admin or user"
    env
    testUserDeletionOwnershipTransfer
  , testThat "Test User API Set user's data retention policy"
             env
             testUserSetDataRetentionPolicy
  , testThat
    "Test User API Don't set data retention policy if it's not\
             \ at least as strict as company's one"
    env
    testUserSetDataRetentionPolicyOnlyIfAsStrict
  , testThat "Test User API get tags"    env testUserGetTags
  , testThat "Test User API update tags" env testUserUpdateTags
  ]

testUserLoginAndGetSession :: TestEnv ()
testUserLoginAndGetSession = do
  -- create a user
  let password = "Secret Password!"
  randomUser <- instantiateUser $ randomUserTemplate { password = Just password }
  ctx        <- mkContext defaultLang
  req1       <- mkRequest
    GET
    [ ("email", inText . unEmail $ randomUser ^. #info % #email)
    , ("password", inText password)
    ]
  -- get access tokens using an email and password
  (res1, _) <- runTestKontra req1 ctx apiCallGetUserPersonalToken
  let Just (Object respObject1 ) = decode (rsBody res1) :: Maybe Value
      Just (String apitoken    ) = H.lookup "apitoken" respObject1
      Just (String apisecret   ) = H.lookup "apisecret" respObject1
      Just (String accesstoken ) = H.lookup "accesstoken" respObject1
      Just (String accesssecret) = H.lookup "accesssecret" respObject1

  -- use access tokens to log in and get cookie-like session id
  req2      <- mkRequest GET [("personal_token", inTextBS $ rsBody res1)]
  (res2, _) <- runTestKontra req2 ctx apiCallLoginUserAndGetSession
  let Just (Object respObject2) = decode (rsBody res2) :: Maybe Value
      Just (String session_id ) = H.lookup "session_id" respObject2
  assertBool "We should get an ok status in JSON" (session_id /= "")

  -- switch API and Access secrets to get bad input in correct format
  let badtokens =
        encode
          . Object
          . H.fromList
          $ [ ("apitoken"    , String apitoken)
            , ("apisecret"   , String accesssecret)
            , ("accesstoken" , String accesstoken)
            , ("accesssecret", String apisecret)
            ]
  req3      <- mkRequest GET [("personal_token", inTextBS badtokens)]
  (res3, _) <- runTestKontra req3 ctx apiCallLoginUserAndGetSession
  let Just (Object respObject3) = decode (rsBody res3) :: Maybe Value
      Just (String errorType  ) = H.lookup "error_type" respObject3
  assertEqual "We should get an error status in JSON" "invalid_authorisation" errorType

testUserTooManyGetTokens :: TestEnv ()
testUserTooManyGetTokens = do
  -- create a user
  let password      = "Secret Password!"
  let wrongpassword = "Hello World!"
  randomUser <- instantiateUser $ randomUserTemplate { password = Just password }
  ctx        <- mkContext defaultLang
  -- getting personap token works with correct password
  req1       <- mkRequest
    GET
    [ ("email", inText . unEmail $ randomUser ^. #info % #email)
    , ("password", inText password)
    ]
  (res1, _) <- runTestKontra req1 ctx apiCallGetUserPersonalToken
  assertEqual "We should get a 200 response" 200 (rsCode res1)

  -- now we fail to get access tokens 6 times
  req2 <- mkRequest
    GET
    [ ("email", inText . unEmail $ randomUser ^. #info % #email)
    , ("password", inText wrongpassword)
    ]
  forM_ [1 .. 6] $ \_ -> do
    (res2, _) <- runTestKontra req2 ctx apiCallGetUserPersonalToken
    assertEqual "We should get a 403 error response" 403 (rsCode res2)

  -- after 6 failed requests, trying valid password also fails
  req3 <- mkRequest
    GET
    [ ("email", inText . unEmail $ randomUser ^. #info % #email)
    , ("password", inText password)
    ]
  (res3, _) <- runTestKontra req3 ctx apiCallGetUserPersonalToken
  assertEqual "We should get a 403 error response" 403 (rsCode res3)

testUser2FAWorkflow :: TestEnv ()
testUser2FAWorkflow = do
  password          <- rand 10 $ arbText 3 30
  randomUser        <- instantiateUser $ randomUserTemplate { password = Just password }

  ctx'              <- mkContextWithUser defaultLang randomUser

  -- Start setting up 2FA
  req_setup2fa      <- mkRequest POST []
  (res_setup2fa, _) <- runTestKontra req_setup2fa ctx' setup2FA
  let Just (Object setupRespObj) = decode (rsBody res_setup2fa) :: Maybe Value
      Just (Bool   setupActive ) = H.lookup "twofactor_active" setupRespObj
      Just (String setupQRCode ) = H.lookup "qr_code" setupRespObj
  assertEqual "We should get a 200 response" 200   (rsCode res_setup2fa)
  assertEqual "2FA should not be active yet" False setupActive

  -- Get the secret from the QR code
  now    <- currentTime
  qrText <- liftIO $ decodeQR (QRCode . Base64.decodeLenient $ TE.encodeUtf8 setupQRCode)
  let encsecret    = (!! 1) . dropWhile (/= "secret") . splitOneOf "?&=" $ qrText
      Right secret = B32.decode $ BSC.pack encsecret
      totpcode     = T.pack . filter (/= '"') . show $ totp SHA1 secret now 30 6

  -- For some reason we need to get updated User and add to Context
  -- otherwise tests fail because TOTP changes are not "seen"
  Just user <- dbQuery . GetUserByID $ randomUser ^. #id
  ctx       <- mkContextWithUser defaultLang user

  -- apiCallGetUserPersonalToken should still work: 2FA not yet confirmed
  do
    req <- mkRequest
      GET
      [ ("email", inText . unEmail $ randomUser ^. #info % #email)
      , ("password", inText password)
      ]
    (res, _) <- runTestKontra req ctx apiCallGetUserPersonalToken
    assertEqual "We should get a 200 response" 200 (rsCode res)

  -- "random" confirmation code should not work
  do
    req      <- mkRequest POST [("totp", inText "123456")]
    (res, _) <- runTestKontra req ctx confirm2FA
    assertEqual "We should get a 400 response" 400 (rsCode res)

  -- correct TOTP code should work to confirm2FA
  do
    req      <- mkRequest POST [("totp", inText totpcode)]
    (res, _) <- runTestKontra req ctx confirm2FA
    assertEqual "TOTP got activated properly"
                "{\"twofactor_active\":true,\"totp_valid\":true}"
                (rsBody res)
    assertEqual "We should get a 200 response" 200 (rsCode res)

  -- now apiCallGetUserPersonalToken without totp should fail
  do
    req <- mkRequest
      GET
      [ ("email", inText . unEmail $ randomUser ^. #info % #email)
      , ("password", inText password)
      ]
    (res, _) <- runTestKontra req ctx apiCallGetUserPersonalToken
    assertEqual "We should get a 403 response" 403 (rsCode res)

  -- and apiCallGetUserPersonalToken with totp should work
  do
    req <- mkRequest
      GET
      [ ("email", inText . unEmail $ randomUser ^. #info % #email)
      , ("password", inText password)
      , ("totp"    , inText totpcode)
      ]
    (res, _) <- runTestKontra req ctx apiCallGetUserPersonalToken
    assertEqual "We should get a 200 response" 200 (rsCode res)

testUserNoDeletionIfWrongEmail :: TestEnv ()
testUserNoDeletionIfWrongEmail = do
  anna <- instantiateUser $ randomUserTemplate { firstName      = return "Anna"
                                               , lastName       = return "Android"
                                               , email = return "anna@android.com"
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }

  ctx <- mkContextWithUser defaultLang anna

  do
    req      <- mkRequest POST [("email", inText "wrong@email.com")]
    (res, _) <- runTestKontra req ctx apiCallDeleteUser
    assertEqual "can't delete if the email is wrong" 400 (rsCode res)

  do
    req      <- mkRequest POST [("email", inText "anna@android.com")]
    (res, _) <- runTestKontra req ctx apiCallDeleteUser
    assertEqual "can delete if the email is correct" 200 (rsCode res)

testUserNoDeletionIfPendingDocuments :: TestEnv ()
testUserNoDeletionIfPendingDocuments = do
  anna <- instantiateUser $ randomUserTemplate { firstName      = return "Anna"
                                               , lastName       = return "Android"
                                               , email = return "anna@android.com"
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  now <- currentTime
  void . dbUpdate $ AcceptTermsOfService (anna ^. #id) now

  bob <- instantiateUser $ randomUserTemplate { email   = return "bob@blue.com"
                                              , groupID = return $ anna ^. #groupID
                                              }

  ctx <- mkContextWithUser defaultLang bob

  doc <- addRandomDocument (rdaDefault bob) { rdaTypes    = OneOf [Signable]
                                            , rdaStatuses = OneOf [Pending]
                                            }

  do
    req      <- mkRequest POST [("email", inText "bob@blue.com")]
    (res, _) <- runTestKontra req ctx apiCallDeleteUser
    assertEqual "can't delete last user if she has pending documents" 409 (rsCode res)

  withDocument doc . randomUpdate $ CancelDocument . systemActor

  do
    req      <- mkRequest POST [("email", inText "bob@blue.com")]
    (res, _) <- runTestKontra req ctx apiCallDeleteUser
    assertEqual "can delete once pending documents are aborted" 200 (rsCode res)

testUserDeletion :: TestEnv ()
testUserDeletion = do
  anna <- instantiateUser $ randomUserTemplate { firstName      = return "Anna"
                                               , lastName       = return "Android"
                                               , email = return "anna@android.com"
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  ctx      <- mkContextWithUser defaultLang anna

  req      <- mkRequest POST [("email", inText "anna@android.com")]
  (res, _) <- runTestKontra req ctx apiCallDeleteUser
  assertEqual "user got deleted" 200 (rsCode res)

testUserDeletionOwnershipTransfer :: TestEnv ()
testUserDeletionOwnershipTransfer = do
  anna <- instantiateUser $ randomUserTemplate { firstName      = return "Anna"
                                               , lastName       = return "Android"
                                               , email = return "anna@android.com"
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  bob <- instantiateUser $ randomUserTemplate { email   = return "bob@blue.com"
                                              , groupID = return $ anna ^. #groupID
                                              }

  now <- currentTime
  void . dbUpdate $ AcceptTermsOfService (anna ^. #id) now
  void . dbUpdate $ AcceptTermsOfService (bob ^. #id) now
  void . dbUpdate $ SetUserCompanyAdmin (bob ^. #id) True

  sharedTemplate <- addRandomDocument (rdaDefault anna) { rdaTypes    = OneOf [Template]
                                                        , rdaSharings = OneOf [Shared]
                                                        }
  unsharedTemplate <- addRandomDocument (rdaDefault anna) { rdaTypes    = OneOf [Template]
                                                          , rdaSharings = OneOf [Private]
                                                          }

  ctx <- mkContextWithUser defaultLang anna

  fid                <- addNewRandomFile
  fid'               <- addNewRandomFile
  sharedAttachment   <- dbUpdate $ NewAttachment (anna ^. #id) "shared" fid
  unsharedAttachment <- dbUpdate $ NewAttachment (anna ^. #id) "shared" fid'
  dbUpdate $ SetAttachmentsSharing (anna ^. #id) [attachmentid sharedAttachment] True

  req      <- mkRequest POST [("email", inText "anna@android.com")]
  (res, _) <- runTestKontra req ctx apiCallDeleteUser
  assertEqual "user got deleted" 200 (rsCode res)

  sharedTemplate'   <- dbQuery . GetDocumentByDocumentID $ documentid sharedTemplate
  unsharedTemplate' <- dbQuery . GetDocumentByDocumentID $ documentid unsharedTemplate

  let domains =
        [ AttachmentsOfAuthorDeleteValue (bob ^. #id)  False
        , AttachmentsOfAuthorDeleteValue (anna ^. #id) False
        , AttachmentsSharedInUsersUserGroup (bob ^. #id)
        ]
  [sharedAttachment'] <- dbQuery
    $ GetAttachments domains [AttachmentFilterByID (attachmentid sharedAttachment)] []
  [unsharedAttachment'] <- dbQuery
    $ GetAttachments domains [AttachmentFilterByID (attachmentid unsharedAttachment)] []

  assertEqual "other admin has been given shared template"
              (maybesignatory . head $ documentsignatorylinks sharedTemplate')
              (Just $ bob ^. #id)
  assertEqual "other admin has not been given unshared template"
              (maybesignatory . head $ documentsignatorylinks unsharedTemplate')
              (Just $ anna ^. #id)

  assertEqual "other admin has been given shared attachment"
              (attachmentuser sharedAttachment')
              (bob ^. #id)
  assertEqual "other admin has not been given unshared attachment"
              (attachmentuser unsharedAttachment')
              (anna ^. #id)

testUserSetDataRetentionPolicy :: TestEnv ()
testUserSetDataRetentionPolicy = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Bob"
                                               , lastName       = return "Blue"
                                               , email          = return "bob@email.tld"
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  ctx <- mkContextWithUser defaultLang user

  replicateM_ 10 $ do
    drp <- rand 10 arbitrary
    let drpBS = unjsonToByteStringLazy unjsonDataRetentionPolicy drp
    req      <- mkRequest POST [("data_retention_policy", inTextBS drpBS)]
    (res, _) <- runTestKontra req ctx apiCallSetDataRetentionPolicy
    assertEqual "should return 200" 200 (rsCode res)

    Just user' <- dbQuery . GetUserByID $ user ^. #id
    assertEqual "policy should have been saved"
                (user' ^. #settings % #dataRetentionPolicy)
                drp

testUserSetDataRetentionPolicyOnlyIfAsStrict :: TestEnv ()
testUserSetDataRetentionPolicyOnlyIfAsStrict = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Bob"
                                               , lastName       = return "Blue"
                                               , email          = return "bob@email.tld"
                                               , groupID        = return $ ug ^. #id
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  ctx <- mkContextWithUser defaultLang user

  replicateM_ 10 $ do
    userDRP    <- rand 10 arbitrary
    companyDRP <- rand 10 arbitrary

    let ug' = set (#settings % _Just % #dataRetentionPolicy) companyDRP ug
    void . dbUpdate $ UserGroupUpdate ug'

    let drpBS = unjsonToByteStringLazy unjsonDataRetentionPolicy userDRP
    req      <- mkRequest POST [("data_retention_policy", inTextBS drpBS)]
    (res, _) <- runTestKontra req ctx apiCallSetDataRetentionPolicy

    let expCode = if userDRP `isAsStrict` companyDRP then 200 else 400
    assertEqual ("should return " ++ show expCode) expCode (rsCode res)

  where
    isAsStrict :: DataRetentionPolicy -> DataRetentionPolicy -> Bool
    isAsStrict drp1 drp2 =
      check #idleDocTimeoutPreparation drp1 drp2
        && check #idleDocTimeoutClosed   drp1 drp2
        && check #idleDocTimeoutCanceled drp1 drp2
        && check #idleDocTimeoutTimedout drp1 drp2
        && check #idleDocTimeoutRejected drp1 drp2
        && check #idleDocTimeoutError    drp1 drp2
        && (not (drp2 ^. #immediateTrash) || drp1 ^. #immediateTrash)

    check
      :: Ord a
      => Lens' DataRetentionPolicy (Maybe a)
      -> DataRetentionPolicy
      -> DataRetentionPolicy
      -> Bool
    check l drp1 drp2 = case (drp1 ^. l, drp2 ^. l) of
      (Just x1, Just x2) -> x1 <= x2
      _                  -> True

testUserGetTags :: TestEnv ()
testUserGetTags = do
  user         <- instantiateRandomUser
  ctx          <- mkContextWithUser defaultLang user
  (Array tags) <- jsonTestRequestHelper ctx GET [] apiCallGetTags 200
  assertEqual "user can view tags" (length $ user ^. #externalTags) (length tags)

testUserUpdateTags :: TestEnv ()
testUserUpdateTags = do
  user     <- instantiateUser $ randomUserTemplate & #externalTags .~ return initialTags
  ctx      <- mkContextWithUser defaultLang user
  req      <- mkRequest POST [("tags", inText tagUpdateJson)]
  (res, _) <- runTestKontra req ctx apiCallUpdateTags
  assertEqual "should return" 200 (rsCode res)

  Just user' <- dbQuery . GetUserByID $ user ^. #id
  assertEqual "user can update tags" expectUpdatedTags (user' ^. #externalTags)
  where
    tagUpdates =
      [ TagUpdate "legs" (SetTo "six")
      , TagUpdate "size" Delete
      , TagUpdate "eyes" (SetTo "big")
      ]
    tagUpdateJson = TE.decodeUtf8 . BSL.toStrict . encode $ toJSON tagUpdates
    initialTags   = S.fromList [Tag "legs" "four", Tag "size" "tiny", Tag "color" "black"]
    expectUpdatedTags =
      S.fromList [Tag "color" "black", Tag "eyes" "big", Tag "legs" "six"]
