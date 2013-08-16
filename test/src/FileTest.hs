module FileTest (fileTests) where

import Crypto
import DB
import TestingUtil
import TestKontra

import Test.Framework
import Test.QuickCheck
import File.Model

import qualified Data.ByteString.UTF8 as BS
import Happstack.Server.SimpleHTTP
import File.File

fileTests :: TestEnvSt -> Test
fileTests env = testGroup "Files" [
  -- Primitive properties
  testThat "FileID read - show works" env testFileIDReadShow,
  testThat "FileID from uri getter matches show implementation" env testFileIDUriShow,

  --Basic DB operations
  testThat "File insert persists content"  env testFileNewFile,
  testThat "File move to AWS works"  env testFileMovedToAWS,

  -- Advanced tests
  testThat "Newly created files are supposed to be moved to amazon"  env testNewFileThatShouldBeMovedToAWS
  ]

testFileIDReadShow :: TestEnv ()
testFileIDReadShow = doTimes 100 $  do
   (fid :: FileID) <- rand 10 arbitrary
   assertEqual "read . show == id" fid  ((read . show) fid)

testFileIDUriShow :: TestEnv ()
testFileIDUriShow = doTimes 100 $  do
   (fid :: FileID) <- rand 10 arbitrary
   assertEqual "fromReqURI . show == id" (Just fid) ((fromReqURI . show) fid)

testFileNewFile :: TestEnv ()
testFileNewFile  = doTimes 100 $ do
  (name, content) <- fileData
  File { fileid = fileid , filename = fname1 , filestorage = FileStorageMemory fcontent1 } <- dbUpdate $  NewFile name $ Binary content
  assertEqual "File content doesn't change" content fcontent1
  assertEqual "File name doesn't change" name fname1
  Just (File { filename = fname2 , filestorage = FileStorageMemory fcontent2}) <- dbQuery $ GetFileByFileID fileid
  assertEqual "File name doesn't change after storing" name fname2
  assertEqual "File content doesn't change after storing" content fcontent2

testFileMovedToAWS :: TestEnv ()
testFileMovedToAWS  = doTimes 100 $ do
  (name,content) <- fileData
  bucket <- viewableS
  url <- viewableS
  file <- dbUpdate $ NewFile name $ Binary content
  let Right aes = mkAESConf (BS.fromString (take 32 $ repeat 'a')) (BS.fromString (take 16 $ repeat 'b'))

  dbUpdate $ FileMovedToAWS (fileid file) bucket url aes
  Just (File { filename = fname , filestorage = FileStorageAWS fbucket furl aes2 }) <- dbQuery $ GetFileByFileID (fileid file)
  assertEqual "File data name does not change" name fname
  assertEqual "Bucket and url are persistent" (bucket,url) (fbucket,furl)
  assertEqual "AES key is persistent" aes aes2

testNewFileThatShouldBeMovedToAWS :: TestEnv ()
testNewFileThatShouldBeMovedToAWS  = do
  (name,content) <- fileData
  file <- dbUpdate $ NewFile name $ Binary content
  checker file
 where
  checker file = do
   mf <- dbQuery $ GetFileThatShouldBeMovedToAmazon
   case mf of
       Just f -> if (f == file)
                    then return ()
                    else do
                        let Right aes = mkAESConf (BS.fromString (take 32 $ repeat 'a')) (BS.fromString (take 16 $ repeat 'b'))
                        dbUpdate $ FileMovedToAWS (fileid f) "" "" aes
                        checker file
       Nothing ->  assertFailure  "Newly created file will not"

viewableS :: TestEnv String
viewableS = rand 10 $ arbString 10 100

fileData :: TestEnv (String, BS.ByteString)
fileData = do
    n <- viewableS
    c <- rand 10 arbitrary
    return (n , c)
