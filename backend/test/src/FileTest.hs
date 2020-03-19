module FileTest (fileTests) where

import Data.Int
import Happstack.Server.SimpleHTTP
import Test.Framework
import Test.QuickCheck
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T

import DB
import File.Conditions
import File.File
import File.Model
import File.Storage
import Purging.Files
import TestCron
import TestingUtil
import TestKontra
import User.Lang

fileTests :: TestEnvSt -> Test
fileTests env = testGroup
  "Files"
  [
  -- Primitive properties
    testThat "FileID read - show works" env testFileIDReadShow
  , testThat "FileID from uri getter matches show implementation" env testFileIDUriShow
  , testThat "GetFileByFileID throws exception when fetching non existing file"
             env
             testFileDoesNotExist
  ,

  --Basic DB operations
    testThat "File insert persists content" env testFileNewFile
  , testThat "File purging works"           env testPurgeFiles
  , testThat "File purging consumer works"  env testFilePurgingConsumer
  ]

testFileIDReadShow :: TestEnv ()
testFileIDReadShow = replicateM_ 100 $ do
  (fid :: FileID) <- rand 10 arbitrary
  assertEqual "read . show == id" fid ((read . showt) fid)

testFileIDUriShow :: TestEnv ()
testFileIDUriShow = replicateM_ 100 $ do
  (fid :: FileID) <- rand 10 arbitrary
  assertEqual "fromReqURI . show == id" (Just fid) ((fromReqURI . show) fid)

testFileNewFile :: TestEnv ()
testFileNewFile = replicateM_ 100 $ do
  (name, content) <- fileData
  fileid'         <- saveNewFile (T.pack name) content
  file1@File { fileid, filename = fname1 } <- dbQuery $ GetFileByFileID fileid'
  fcontent1       <- getFileContents file1

  assertEqual "We got the file we were asking for" fileid' fileid
  assertEqual "File content doesn't change"        content fcontent1
  assertEqual "File name doesn't change"           name    (T.unpack fname1)

testFileDoesNotExist :: TestEnv ()
testFileDoesNotExist = replicateM_ 5 $ do
  assertRaisesKontra (\FileDoesNotExist{} -> True) $ randomQuery GetFileByFileID

testPurgeFiles :: TestEnv ()
testPurgeFiles = replicateM_ 100 $ do
  (name, content) <- fileData
  fid             <- saveNewFile (T.pack name) content
  fidsToPurge     <- dbUpdate $ MarkOrphanFilesForPurgeAfter mempty
  assertEqual "File successfully marked for purge" [fid] fidsToPurge
  dbUpdate $ PurgeFile fid

  assertRaisesKontra (\FileWasPurged{} -> True) $ do
    dbQuery $ GetFileByFileID fid

  orphanFidsAfterPurge <- dbUpdate $ MarkOrphanFilesForPurgeAfter mempty
  assertEqual "File not marked for purge after it was purged" [] orphanFidsAfterPurge

testFilePurgingConsumer :: TestEnv ()
testFilePurgingConsumer = do
  (name, content) <- fileData
  -- This file is not referenced anywhere, it should therefore be purged.
  fid             <- saveNewFile (T.pack name) content

  void . dbUpdate $ MarkOrphanFilesForPurgeAfter mempty

  ctx <- mkContext defaultLang
  runTestCronUntilIdle ctx

  runSQL_
    $   "SELECT COUNT(*) FROM files WHERE id ="
    <?> fid
    <+> "AND purged_time IS NOT NULL"
  c <- fetchOne runIdentity

  assertEqual "should be purged" (1 :: Int64) c

viewableS :: TestEnv String
viewableS = rand 10 $ arbString 10 100

fileData :: TestEnv (String, BS.ByteString)
fileData = do
  n <- viewableS
  c <- rand 10 arbitrary
  return (n, c)
