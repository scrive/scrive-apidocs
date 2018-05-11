module FileTest (fileTests) where

import Happstack.Server.SimpleHTTP
import Test.Framework
import Test.QuickCheck
import qualified Data.ByteString.UTF8 as BS

import DB
import File.Conditions
import File.File
import File.Model
import File.Storage
import Purging.Files
import TestingUtil
import TestKontra

fileTests :: TestEnvSt -> Test
fileTests env = testGroup "Files" [
  -- Primitive properties
  testThat "FileID read - show works" env testFileIDReadShow,
  testThat "FileID from uri getter matches show implementation" env testFileIDUriShow,
  testThat "GetFileByFileID throws exception when fetching non existing file" env testFileDoesNotExist,

  --Basic DB operations
  testThat "File insert persists content"  env testFileNewFile,

  testThat "File purging works"  env testPurgeFiles
  ]

testFileIDReadShow :: TestEnv ()
testFileIDReadShow = replicateM_ 100 $  do
   (fid :: FileID) <- rand 10 arbitrary
   assertEqual "read . show == id" fid  ((read . show) fid)

testFileIDUriShow :: TestEnv ()
testFileIDUriShow = replicateM_ 100 $  do
   (fid :: FileID) <- rand 10 arbitrary
   assertEqual "fromReqURI . show == id" (Just fid) ((fromReqURI . show) fid)

testFileNewFile :: TestEnv ()
testFileNewFile  = replicateM_ 100 $ do
  (name, content) <- fileData
  fileid' <- saveNewFile name content
  file1@File{ fileid, filename = fname1 } <- dbQuery $ GetFileByFileID fileid'
  fcontent1 <- getFileContents file1

  assertEqual "We got the file we were asking for" fileid' fileid
  assertEqual "File content doesn't change" content fcontent1
  assertEqual "File name doesn't change" name fname1

testFileDoesNotExist :: TestEnv ()
testFileDoesNotExist = replicateM_ 5 $ do
  assertRaisesKontra (\FileDoesNotExist {} -> True) $
    randomQuery GetFileByFileID

testPurgeFiles :: TestEnv ()
testPurgeFiles  = replicateM_ 100 $ do
  let maxMarked = 1000
  (name,content) <- fileData
  fid <- saveNewFile name content
  runQuery_ $ "DELETE FROM amazon_upload_jobs WHERE id =" <?> fid
  fidsToPurge <- dbUpdate $ MarkOrphanFilesForPurgeAfter maxMarked mempty
  assertEqual "File successfully marked for purge" [fid] fidsToPurge
  dbUpdate $ PurgeFile fid

  assertRaisesKontra (\FileWasPurged {} -> True) $ do
     dbQuery $ GetFileByFileID fid

  orphanFidsAfterPurge <- dbUpdate $ MarkOrphanFilesForPurgeAfter maxMarked mempty
  assertEqual "File not marked for purge after it was purged" [] orphanFidsAfterPurge

viewableS :: TestEnv String
viewableS = rand 10 $ arbString 10 100

fileData :: TestEnv (String, BS.ByteString)
fileData = do
    n <- viewableS
    c <- rand 10 arbitrary
    return (n , c)
