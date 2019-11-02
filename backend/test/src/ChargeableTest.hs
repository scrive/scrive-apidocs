module ChargeableTest where

import Control.Monad.Trans
import Data.Int
import Happstack.Server
import Test.Framework
import qualified Data.ByteString as BS
import qualified Data.Text as T

import DB hiding (query, update)
import Doc.API.V1.Calls
import Doc.API.V2.AesonTestUtils (testRequestHelperNoAssert_)
import Doc.API.V2.Calls
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Mock.TestUtils
import Doc.DocStateData
import Doc.DocumentMonad (withDocument)
import Doc.Model
import File.Storage
import SMS.SMS
import SMS.Types (SMSProvider(..))
import TestingUtil
import TestKontra as T
import UserGroupAccounts.Model
import Util.Actor
import Util.SignatoryLinkUtils

chargeableTest :: TestEnvSt -> Test
chargeableTest env = testGroup
  "Chargeable Items"
  [ testThat "SMSes for default provider are counted properly"
             env
             test_smsCounting_default
  , testThat "SMSes for Telia CallGuide are counted properly" env test_smsCounting_telia
  , testThat "Starting a document with V1/V2 adds a chargeable item"
             env
             test_startDocumentCharging
  , testThat
    (  "Closing a document with V1/V2 adds chargeable items"
    <> " (closed signatures and documents)"
    )
    env
    test_closeDocAndSigCharging
  ]

test_smsCounting_default :: TestEnv ()
test_smsCounting_default = do
  ugid      <- view #ugID <$> addNewUserGroup
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  True      <- dbUpdate $ SetUserUserGroup (userid user) ugid
  doc       <- addRandomDocument (rdaDefault user)
  let sms = SMS { smsMSISDN        = "+48666666666"
                , kontraInfoForSMS = Nothing
                , smsBody          = ""
                , smsOriginator    = "Scrive"
                , smsProvider      = SMSDefault
                }

  scheduleSMS doc sms { smsBody = "Test sms" }
  runSQL_ selectQuantity
  fetchOne runIdentity >>= assertEqual "One SMS was counted" (1 :: Int32)

  runSQL_ clearChargeable

  scheduleSMS doc sms { smsBody = T.intercalate ", " $ replicate 25 "Test sms" }
  runSQL_ selectQuantity
  fetchOne runIdentity >>= assertEqual "Two SMSes were counted" (2 :: Int32)
  where
    clearChargeable = "DELETE FROM chargeable_items WHERE type = 1"
    selectQuantity  = "SELECT quantity FROM chargeable_items WHERE type = 1"

test_smsCounting_telia :: TestEnv ()
test_smsCounting_telia = do
  ugid      <- view #ugID <$> addNewUserGroup
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  True      <- dbUpdate $ SetUserUserGroup (userid user) ugid
  doc       <- addRandomDocument (rdaDefault user)
  let sms = SMS { smsMSISDN        = "+48666666666"
                , kontraInfoForSMS = Nothing
                , smsBody          = ""
                , smsOriginator    = "Scrive"
                , smsProvider      = SMSTeliaCallGuide
                }

  scheduleSMS doc sms { smsBody = "Test sms" }
  runSQL_ selectQuantity
  fetchOne runIdentity >>= assertEqual "One SMS was counted" (1 :: Int32)

  runSQL_ clearChargeable

  scheduleSMS doc sms { smsBody = T.intercalate ", " $ replicate 25 "Test sms" }
  runSQL_ selectQuantity
  fetchOne runIdentity >>= assertEqual "Two SMSes were counted" (2 :: Int32)
  where
    clearChargeable = "DELETE FROM chargeable_items WHERE type = 5"
    selectQuantity  = "SELECT quantity FROM chargeable_items WHERE type = 5"

test_startDocumentCharging :: TestEnv ()
test_startDocumentCharging = do
  ugid        <- view #ugID <$> addNewUserGroup
  Just user   <- addNewUser "Bob" "Blue" "bob@blue.com"
  True        <- dbUpdate $ SetUserUserGroup (userid user) ugid
  ctxWithUser <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  did1        <- newDocumentReadyToStart user
  req1        <- mkRequest POST []
  (_, _)      <- runTestKontra req1 ctxWithUser $ apiCallV1Ready $ did1
  runSQL_
    $ "SELECT count(*) FROM chargeable_items WHERE type = 6 AND quantity = 1 AND user_group_id = "
    <?> ugid
  fetchOne runIdentity
    >>= assertEqual "User group was charged for starting one document" (1 :: Int64)

  did2   <- newDocumentReadyToStart user
  req2   <- mkRequest POST []
  (_, _) <- runTestKontra req2 ctxWithUser $ docApiV2Start $ did2
  runSQL_
    $ "SELECT count(*) FROM chargeable_items WHERE type = 6 AND quantity = 1 AND user_group_id = "
    <?> ugid
  fetchOne runIdentity
    >>= assertEqual "User group was charged for starting other document" (2 :: Int64)

  where
    newDocumentReadyToStart user = do
      let filename = inTestDir "pdfs/simple.pdf"
      filecontent <- liftIO $ BS.readFile filename
      file        <- saveNewFile (T.pack filename) filecontent
      doc         <- addRandomDocumentWithFile
        file
        (rdaDefault user)
          { rdaTypes       = OneOf [Signable]
          , rdaStatuses    = OneOf [Preparation]
          , rdaSignatories = let signatory =
                                   OneOf [AllOf [RSC_DeliveryMethodIs EmailDelivery]]
                             in  OneOf $ map (`replicate` signatory) [1 .. 10]
          }

      True <- withDocument doc $ do
        randomUpdate $ ResetSignatoryDetails
          ([ (defaultSignatoryLink
               { signatoryfields   = (signatoryfields $ fromJust $ getAuthorSigLink doc)
               , signatoryisauthor = True
               , signatoryrole     = SignatoryRoleViewer
               , maybesignatory    = Just $ userid user
               }
             )
           , (defaultSignatoryLink
               { signatorysignorder = SignOrder 1
               , signatoryisauthor  = False
               , signatoryrole      = SignatoryRoleSigningParty
               , signatoryfields    = [ fieldForTests (NameFI (NameOrder 1)) "Fred"
                                      , fieldForTests (NameFI (NameOrder 2)) "Frog"
                                      , fieldForTests EmailFI "fred@frog.com"
                                      ]
               }
             )
           ]
          )
          (systemActor $ documentctime doc)
      return $ documentid doc

test_closeDocAndSigCharging :: TestEnv ()
test_closeDocAndSigCharging = do
  user <- addNewRandomUser
  ctx  <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  let queryChargeableSigClose =
        "SELECT count(*) FROM chargeable_items WHERE type = 9 "
          <>  "AND quantity = 1 AND user_group_id ="
          <?> usergroupid user
  let queryChargeableDocClose =
        "SELECT count(*) FROM chargeable_items WHERE type = 8 "
          <>  "AND quantity = 1 AND user_group_id ="
          <?> usergroupid user

  -- Test that closing document in V1 adds a chargeable item
  mockDocV1 <- testDocApiV2StartNew ctx
  let didV1  = getMockDocId mockDocV1
  let slidV1 = getMockDocSigLinkId 1 mockDocV1

  testRequestHelperNoAssert_ ctx POST [("fields", inText "[]")] $ apiCallV1Ready didV1
  testRequestHelperNoAssert_ ctx POST [("fields", inText "[]")]
    $ apiCallV1Sign didV1 slidV1

  runSQL_ queryChargeableSigClose
  fetchOne runIdentity
    >>= assertEqual "Company was charged for closing one signature" (1 :: Int64)

  runSQL_ queryChargeableDocClose
  fetchOne runIdentity
    >>= assertEqual "Company was charged for closing one document" (1 :: Int64)

  -- Test that closing document in V2 adds a chargeable item
  mockDocV2 <- testDocApiV2StartNew ctx
  let didV2  = getMockDocId mockDocV2
  let slidV2 = getMockDocSigLinkId 1 mockDocV2

  _mockDocSigned <- mockDocTestRequestHelper
    ctx
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign didV2 slidV2)
    200

  runSQL_ queryChargeableSigClose
  fetchOne runIdentity
    >>= assertEqual "Company was charged for closing another signature" (2 :: Int64)

  runSQL_ queryChargeableDocClose
  fetchOne runIdentity
    >>= assertEqual "Company was charged for closing another document" (2 :: Int64)
