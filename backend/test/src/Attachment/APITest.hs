module Attachment.APITest
  ( attachmentAPITests
  ) where

import Happstack.Server
import Test.Framework
import qualified Data.ByteString.Lazy as BSL

import Attachment.API
import Attachment.Model
import Context
import DB
import File.Storage
import MinutesTime
import TestingUtil
import TestKontra
import User.Model
import UserGroup.Types
import Util.Actor

attachmentAPITests :: TestEnvSt -> Test
attachmentAPITests env = testGroup "Attachment.API"
  [ testThat "Attachment API List"     env testAttachmentList
  , testThat "Attachment API Create"   env testAttachmentCreate
  , testThat "Attachment API Share"    env testAttachmentSetSharing
  , testThat "Attachment API Delete"   env testAttachmentDelete
  , testThat "Attachment API Download" env testAttachmentDownload
  ]

testAttachmentList :: TestEnv ()
testAttachmentList = do
  (anna, ug) <- addNewAdminUserAndUserGroup "Anna" "Android" "anna@android.com"
  Just bob   <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (get ugID ug)

  now  <- currentTime
  fid  <- addNewRandomFile
  attA <- dbUpdate $ NewAttachment (userid anna) "a" fid $ systemActor now
  attB <- dbUpdate $ NewAttachment (userid bob)  "b" fid $ systemActor now
  _    <- dbUpdate $ NewAttachment (userid bob)  "c" fid $ systemActor now
  _    <- dbUpdate $ SetAttachmentsSharing (userid bob) [attachmentid attB] True

  ctx <- set ctxmaybeuser (Just anna) <$> mkContext def

  do
    req <- mkRequest GET []
    (attachments, _) <- runTestKontra req ctx attachmentsApiV2List_
    assertEqual "only Anna's attachment" attachments [attA]

  do
    req <- mkRequest GET
      [ ("domain", inText "All")
      , ("sorting", inText "[{\"order\":\"descending\",\"sort_by\":\"title\"}]")
      ]
    (attachments, _) <- runTestKontra req ctx attachmentsApiV2List_
    assertEqual "Anna's attachment and Bob's shared attachment"
                attachments [attB { attachmentshared = True }, attA]

testAttachmentCreate :: TestEnv ()
testAttachmentCreate = do
  Just bob <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- set ctxmaybeuser (Just bob) <$> mkContext def

  req <- mkRequest POST
    [ ("file", inFile $ inTestDir "pdfs/50page.pdf")
    , ("title", inText "Terms and conditions")
    ]
  (res, _) <- runTestKontra req ctx attachmentsApiV2Create
  assertEqual "should return 201" (rsCode res) 201

  [att] <- dbQuery $ GetAttachments
    [AttachmentsOfAuthorDeleteValue (userid bob) False] [] []

  assertEqual "should set the title"
              (attachmenttitle att) "Terms and conditions"
  assert $ not $ attachmentshared att

testAttachmentSetSharing :: TestEnv ()
testAttachmentSetSharing = do
  Just bob <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- set ctxmaybeuser (Just bob) <$> mkContext def
  now <- currentTime

  fid  <- addNewRandomFile
  attA <- dbUpdate $ NewAttachment (userid bob) "a" fid $ systemActor now
  attB <- dbUpdate $ NewAttachment (userid bob) "b" fid $ systemActor now

  -- IDs are supposed to be sent as strings, e.g. ["3", "4"]
  let idsStr = show $ map (show . attachmentid) [attA, attB]

  do
    req <- mkRequest POST
      [ ("shared",         inText "true")
      , ("attachment_ids", inText idsStr)
      ]
    (res, _) <- runTestKontra req ctx attachmentsApiV2SetSharing
    assertEqual "should return 200" (rsCode res) 200

    attachments <- dbQuery $ GetAttachments
      [AttachmentsOfAuthorDeleteValue (userid bob) False] [] []
    assert $ not $ null attachments
    assert $ all attachmentshared attachments

  do
    req <- mkRequest POST
      [ ("shared",         inText "false")
      , ("attachment_ids", inText idsStr)
      ]
    (res, _) <- runTestKontra req ctx attachmentsApiV2SetSharing
    assertEqual "should return 200" (rsCode res) 200

    attachments <- dbQuery $ GetAttachments
      [AttachmentsOfAuthorDeleteValue (userid bob) False] [] []
    assert $ not $ null attachments
    assert $ all (not . attachmentshared) attachments

testAttachmentDelete :: TestEnv ()
testAttachmentDelete = do
  Just bob <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- set ctxmaybeuser (Just bob) <$> mkContext def
  now <- currentTime

  fid <- addNewRandomFile
  attA <- dbUpdate $ NewAttachment (userid bob) "a" fid $ systemActor now
  attB <- dbUpdate $ NewAttachment (userid bob) "b" fid $ systemActor now

  -- IDs are supposed to be sent as strings, e.g. ["3", "4"]
  let idsStr = show $ map (show . attachmentid) [attA, attB]

  req <- mkRequest POST [("attachment_ids", inText idsStr)]
  (res, _) <- runTestKontra req ctx attachmentsApiV2Delete
  assertEqual "should return 202" (rsCode res) 202

  attachments <- dbQuery $ GetAttachments
    [AttachmentsOfAuthorDeleteValue (userid bob) True] [] []
  assert $ not $ null attachments
  assert $ all attachmentdeleted attachments

testAttachmentDownload :: TestEnv ()
testAttachmentDownload = do
  Just bob <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- set ctxmaybeuser (Just bob) <$> mkContext def
  now <- currentTime

  fid      <- addNewRandomFile
  att      <- dbUpdate $ NewAttachment (userid bob) "a" fid $ systemActor now
  contents <- getFileIDContents fid

  req <- mkRequest GET []
  (res, _) <- runTestKontra req ctx $
    attachmentsApiV2Download $ attachmentid att
  assertEqual "should return 200" (rsCode res) 200
  assertEqual "should return the contents of the file"
              (rsBody res) (BSL.fromStrict contents)
