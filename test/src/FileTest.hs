module FileTest (fileTests) where

import Happstack.Server.SimpleHTTP
import Control.Monad (replicateM_)
import Test.Framework
import Test.QuickCheck
import qualified Data.ByteString.UTF8 as BS

import Crypto
import DB
import File.Conditions
import File.File
import File.Model
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
  testThat "File move to AWS works"  env testFileMovedToAWS,

  testThat "File purging works"  env testPurgeFiles,

  -- Advanced tests
  testThat "Newly created files are supposed to be moved to amazon"  env testNewFileThatShouldBeMovedToAWS
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
  fileid' <- dbUpdate $ NewFile name $ Binary content
  File { fileid = fileid, filename = fname1, filestorage = FileStorageMemory fcontent1 } <- dbQuery $ GetFileByFileID fileid'

  assertEqual "We got the file we were asking for" fileid' fileid
  assertEqual "File content doesn't change" content fcontent1
  assertEqual "File name doesn't change" name fname1
  File { filename = fname2 , filestorage = FileStorageMemory fcontent2} <- dbQuery $ GetFileByFileID fileid'
  assertEqual "File name doesn't change after storing" name fname2
  assertEqual "File content doesn't change after storing" content fcontent2

testFileDoesNotExist :: TestEnv ()
testFileDoesNotExist = replicateM_ 5 $ do
  assertRaisesKontra (\FileDoesNotExist {} -> True) $ do
    randomQuery $ GetFileByFileID

testFileMovedToAWS :: TestEnv ()
testFileMovedToAWS  = replicateM_ 100 $ do
  (name,content) <- fileData
  bucket <- viewableS
  url <- viewableS
  fileid' <- dbUpdate $ NewFile name $ Binary content
  let Right aes = mkAESConf (BS.fromString (take 32 $ repeat 'a')) (BS.fromString (take 16 $ repeat 'b'))

  dbUpdate $ FileMovedToAWS fileid' bucket url aes
  File { filename = fname , filestorage = FileStorageAWS fbucket furl aes2 } <- dbQuery $ GetFileByFileID fileid'
  assertEqual "File data name does not change" name fname
  assertEqual "Bucket and url are persistent" (bucket,url) (fbucket,furl)
  assertEqual "AES key is persistent" aes aes2

testPurgeFiles :: TestEnv ()
testPurgeFiles  = replicateM_ 100 $ do
  (name,content) <- fileData
  _fileid' <- dbUpdate $ NewFile name $ Binary content

  -- here we might purge some other file that is a left over in the
  -- database from former test but that is ok
  [(idx,_,_,_)] <- dbQuery $ FindFilesForPurging 1
  dbUpdate $ PurgeFile idx

  assertRaisesKontra (\FileWasPurged {} -> True) $ do
     dbQuery $ GetFileByFileID idx

testNewFileThatShouldBeMovedToAWS :: TestEnv ()
testNewFileThatShouldBeMovedToAWS  = do
  (name,content) <- fileData
  fileid' <- dbUpdate $ NewFile name $ Binary content
  checker fileid'
 where
  checker fileid' = do
   mf <- dbQuery $ GetFileThatShouldBeMovedToAmazon
   case mf of
       Just f -> if (fileid f == fileid')
                    then return ()
                    else do
                        let Right aes = mkAESConf (BS.fromString (take 32 $ repeat 'a')) (BS.fromString (take 16 $ repeat 'b'))
                        dbUpdate $ FileMovedToAWS fileid' "" "" aes
                        checker fileid'
       Nothing ->  assertFailure  "Newly created file will not"

viewableS :: TestEnv String
viewableS = rand 10 $ arbString 10 100

fileData :: TestEnv (String, BS.ByteString)
fileData = do
    n <- viewableS
    c <- rand 10 arbitrary
    return (n , c)
