{-# LANGUAGE OverloadedStrings #-}
module DocJSONTest (docJSONTests) where

import DocJSONTypes

import Control.Applicative
import Control.Monad.IO.Class
--import Control.Monad
--import Control.Monad.Trans
--import Data.Maybe
import Happstack.Server
--import Network.URI
--import Data.Typeable
import Test.Framework
--import Data.Int
--import Test.QuickCheck
--import Data.Text
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
--import Text.JSON
import qualified Data.Unjson as U
import Data.Unjson
import Data.Aeson
--import Data.Typeable
--import Text.JSON.FromJSValue
--import Text.JSON.Gen
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BS

import Context
--import DB
import Doc.API.V1.Calls
--import Doc.API.V1.DocumentToJSON
--import Doc.DocStateData
--import Doc.Model
--import OAuth.Model
import TestingUtil
import TestKontra as T
--import User.Model
--import Util.HasSomeUserInfo
import Utils.Default
--import qualified Log
--import Utils.Read
--import DB.Derive
import Doc.DocumentID (unsafeDocumentID)

docJSONTests :: TestEnvSt -> Test
docJSONTests env = testGroup "DocJSON"
  [ testThat "Test JSON structure for API V1 'createfromfile' and 'ready'" env testFromFileAndReadySimple
  , testThat "Test JSON structure for API V1 'createfromtemplate' and 'ready'" env testFromTemplateAndReadySimple
  ]

testFromFileAndReadySimple :: TestEnv ()
testFromFileAndReadySimple = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  doc <- testJSONWith "test/json/test1.json" (rsBody resDoc) docUnjsonDef

  reqReady <- mkRequest POST []
  (resReady, _) <- runTestKontra reqReady ctx $ apiCallV1Ready (unsafeDocumentID $ fromInt64AsString $ docid doc)
  assertEqual "We should get a 202 response" 202 (rsCode resReady)
  _ <- testJSONWith "test/json/test2.json" (rsBody resReady) docUnjsonDef
  return ()

testFromTemplateAndReadySimple :: TestEnv ()
testFromTemplateAndReadySimple = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  doc <- testJSONWith "test/json/test1.json" (rsBody resDoc) docUnjsonDef
  let did = unsafeDocumentID $ fromInt64AsString $ docid doc

  -- TODO Use Aeson Lenses to set values, this is clunky and prone to errors
  -- because the Unjson uses derived Show for datatypes which is just wrong and
  -- this just works because our server ignores the bad values in the JSON, but
  -- we will want to set values for these and then it won't work because it
  -- will be ignored :(
  -- We want to behave like a client and check things:
  -- check things = stuff in DocJSONTypes.hs
  -- behave like a client = only mess around with JSON (i.e. Aeson Value w/ lenses)
  let Just docJSON = decode (rsBody resDoc) :: Maybe Value
      Result docUnjson _ = (parse docUnjsonDef docJSON)
      docUnjson' = docUnjson { doctemplate = True }
      value = unjsonToJSON docUnjsonDef docUnjson'
      strDoc = encode value

  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS strDoc)] []
  (resUpdate, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate)

  reqFromTemplate <- mkRequestWithHeaders POST [] []
  (resFromTemplate, _) <- runTestKontra reqFromTemplate ctx $ apiCallV1CreateFromTemplate did
  assertEqual "We should get a 201 response" 201 (rsCode resFromTemplate)

  return ()


-- | Takes a 'FilePath' to a JSON file with the desired structure (with 'null'
-- considered as a type), a 'ByteString' with the JSON to test, and a
-- 'UnjsonDef' which the ByteString should parse with.
--
-- Checks that the ByteString parses, and also that it matches the JSON given
-- in 'FilePath' (using (==) and 'removeValues').
testJSONWith :: FilePath -> BS.ByteString -> UnjsonDef Doc -> TestEnv Doc
testJSONWith fp jsonBS useUnjsonDef = do
  jsonFileBS <- liftIO $ B.readFile fp
  let Just value    = decode jsonBS
      Just jsonFile = decode jsonFileBS
      -- FIXME Can we use the 'a' in U.Result to test stuff?
      U.Result doc problems = parse useUnjsonDef value
  -- Using Unjson we check several things about the JSON in 'jsonBS':
  -- 1) That the correct fields exist
  -- 2) That these are of the correct type
  -- 3) That some JSON String types have the correct "underlying" type or set
  --    of values (e.g. An Int64 inside JSON String, or a datatype inside a
  --    JSON String)
  assertEqual "There should be no problems in Result" [] problems
  -- The second check is similar, but not as powerful or helpful as Unjson, but
  -- what it does that Unjson cannot do is:
  -- 1) Check that no extra fields exist
  -- 2) Check that Null fields match, regardless of what their type "should" be
  assertEqual ("JSON structure and types (including 'null') should match that in " ++ fp)
              (removeValues jsonFile) (removeValues value)
  return doc

removeValues :: Value -> Value
removeValues (Object m) = Object (H.map removeValues m)
removeValues (Array v)  = Array  (V.map removeValues v)
removeValues (String _) = String ""
removeValues (Number _) = Number 0
removeValues (Bool _)   = Bool False
removeValues Null       = Null
