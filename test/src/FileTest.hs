{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module FileTest (fileTests) where

import DB.Classes
import TestingUtil

import Test.Framework
import Test.QuickCheck
import File.Model
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)

import qualified Data.ByteString.UTF8 as BS
import Happstack.Server.SimpleHTTP
import Control.Monad.Trans
import File.File

fileTests :: DBEnv -> Test
fileTests env = testGroup "Files" [
  
  -- Primitive properties
  testCase "FileID read - show works" testFileIDReadShow,
  testCase "FileID from uri getter matches show implementation" testFileIDUriShow,
  testCase "File are equal when they have equal ids" testFileEquality,
  
  --Basic DB operations
  testThat "File insert persists content"  env testFileNewFile,
  testThat "File move to disk works"  env testFileMovedToDisc,
  testThat "File move to AWS works"  env testFileMovedToAWS,
  testThat "We can put file unchecked in db"  env testUncheckedStoring,
  
  
  -- Advanced tests
  testThat "Newly created files are supposed to be moved to amazon"  env testNewFileThatShouldBeMovedToAWS
  ]
  
  
testFileIDReadShow :: Assertion
testFileIDReadShow = doNTimes 100 $  do
   (fid :: FileID) <- rand 10 arbitrary 
   assertBool "read . show == id" ( fid == (read . show)  fid ) 

   
testFileIDUriShow :: Assertion
testFileIDUriShow = doNTimes 100 $  do
   (fid :: FileID) <- rand 10 arbitrary 
   assertBool "fromReqURI . show == id" ( Just fid == (fromReqURI . show)  fid )  

testFileEquality:: Assertion
testFileEquality = doNTimes 100 $  do
   (f1 :: File) <- rand 10 arbitrary 
   (f2 :: File) <- rand 10 arbitrary 
   assertBool "file == file <-> fileid == fileid" ((f1 == f2) == (fileid f1 == fileid f2))  
   
   
testFileNewFile ::   DB ()
testFileNewFile  = doNTimes 100 $ do
  (name, content) <- fileData 
  File { fileid = fileid , filename = fname1 , filestorage = FileStorageMemory fcontent1} <- dbUpdate $  NewFile name content
  assertBool ("File content doesn't change " ++ show content ++ " vs "++ show fcontent1) (content == fcontent1)
  assertBool ("File name doesn't change " ++ show name ++ " vs "++ show fname1) (name == fname1)
  Just (File { filename = fname2 , filestorage = FileStorageMemory fcontent2}) <- dbQuery $ GetFileByFileID fileid
  assertBool "File data doesn't change after storing" ( name == fname2 && content == fcontent2) 
  
  
testFileMovedToDisc ::   DB ()
testFileMovedToDisc  = doNTimes 100 $ do
  (name,content) <- fileData
  (pth) <- rand 10 $  arbString 10 100
  file <- dbUpdate $  NewFile name content
  dbUpdate $ FileMovedToDisk (fileid file) pth
  Just (File { filename = fname , filestorage = FileStorageDisk fpath }) <- dbQuery $ GetFileByFileID (fileid file)
  assertBool "File data name does not change" ( name == fname ) 
  assertBool "Path is persistent" ( pth ==  fpath ) 
    
testFileMovedToAWS ::   DB ()
testFileMovedToAWS  = doNTimes 100 $ do
  (name,content) <- fileData
  bucket <- viewableS
  url <- viewableS 
  file <- dbUpdate $  NewFile name content
  dbUpdate $ FileMovedToAWS (fileid file) bucket url
  Just (File { filename = fname , filestorage = FileStorageAWS fbucket furl }) <- dbQuery $ GetFileByFileID (fileid file)
  assertBool "File data name does not change" ( name == fname ) 
  assertBool "Bucket and url are persistent" ( bucket == fbucket && url == furl ) 
        
testNewFileThatShouldBeMovedToAWS ::   DB ()
testNewFileThatShouldBeMovedToAWS  = do
  (name,content) <- fileData
  file <- dbUpdate $  NewFile name content
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
        

testUncheckedStoring :: DB ()
testUncheckedStoring  = sequence_ $ replicate 10 $ do
  (name,content) <- fileData
  fid <- rand 10 arbitrary
  mf <-  dbQuery $ GetFileByFileID fid
  case mf of 
    Just _ -> return ()
    Nothing -> do
        let f1 = File {fileid = fid, filename = name, filestorage = FileStorageMemory content}
        dbUpdate $  PutFileUnchecked File {fileid = fid, filename = name, filestorage = FileStorageMemory content}
        mf1 <-  dbQuery $ GetFileByFileID (fid)
        assertBool "We can put file in db with mem starage" ( Just f1 == mf1 ) 

viewableS :: (MonadIO m) => m String
viewableS = rand 10 $ arbString 10 100

fileData :: (MonadIO m) =>  m (String, BS.ByteString)
fileData = do
    n <- viewableS
    c <- rand 10 arbitrary
    return (n , c)
