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
testFileIDReadShow = doNTimes 100 $  do
   (fid :: FileID) <- rand 10 arbitrary 
   assertBool "read . show == id" ( fid == (read . show)  fid ) 

testFileIDUriShow :: TestEnv ()
testFileIDUriShow = doNTimes 100 $  do
   (fid :: FileID) <- rand 10 arbitrary 
   assertBool "fromReqURI . show == id" ( Just fid == (fromReqURI . show)  fid )  

testFileNewFile :: TestEnv ()
testFileNewFile  = doNTimes 100 $ do
  (name, content) <- fileData 
  File { fileid = fileid , filename = fname1 , filestorage = FileStorageMemory fcontent1 aes1 } <- dbUpdate $  NewFile name $ Binary content
  assertBool ("File content doesn't change " ++ show content ++ " vs "++ show fcontent1) (content == aesDecrypt aes1 fcontent1)
  assertBool ("File name doesn't change " ++ show name ++ " vs "++ show fname1) (name == fname1)
  Just (File { filename = fname2 , filestorage = FileStorageMemory fcontent2 aes2}) <- dbQuery $ GetFileByFileID fileid
  assertBool "File data doesn't change after storing" ( name == fname2 && content == aesDecrypt aes2 fcontent2)

testFileMovedToAWS :: TestEnv ()
testFileMovedToAWS  = doNTimes 100 $ do
  (name,content) <- fileData
  bucket <- viewableS
  url <- viewableS 
  file <- dbUpdate $ NewFile name $ Binary content
  dbUpdate $ FileMovedToAWS (fileid file) bucket url
  Just (File { filename = fname , filestorage = FileStorageAWS fbucket furl _ }) <- dbQuery $ GetFileByFileID (fileid file)
  assertBool "File data name does not change" ( name == fname ) 
  assertBool "Bucket and url are persistent" ( bucket == fbucket && url == furl ) 
   
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
                        dbUpdate $ FileMovedToAWS (fileid f) "" ""
                        checker file
       Nothing ->  assertFailure  "Newly created file will not" 

viewableS :: TestEnv String
viewableS = rand 10 $ arbString 10 100

fileData :: TestEnv (String, BS.ByteString)
fileData = do
    n <- viewableS
    c <- rand 10 arbitrary
    return (n , c)
