{-# LANGUAGE OverloadedStrings #-}
module DocJSONTest (docJSONTests) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
--import Control.Monad.Trans
--import Data.Maybe
import Happstack.Server
--import Network.URI
import Data.Typeable
import Test.Framework
import Data.Int
--import Test.QuickCheck
import Data.Text
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
--import Text.JSON
import qualified Data.Unjson as U
import Data.Unjson
import Data.Aeson
import Data.Typeable
--import Text.JSON.FromJSValue
--import Text.JSON.Gen
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BS

import Context
--import DB
import Doc.API.V1.Calls
--import Doc.API.V1.DocumentToJSON
import Doc.DocStateData
--import Doc.Model
--import OAuth.Model
import TestingUtil
import TestKontra as T
--import User.Model
--import Util.HasSomeUserInfo
import Utils.Default
import qualified Log
import Utils.Read
import DB.Derive

newtype Int64AsString = Int64AsString Int64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveUnderlyingReadShow ''Int64AsString)

fromInt64AsString :: Int64AsString -> Int64
fromInt64AsString (Int64AsString i) = i

unjsonInt64AsString :: UnjsonDef Int64AsString
unjsonInt64AsString = unjsonInvmapR
                        ((maybe (fail "Can't parse Int64AsString") return) . maybeRead)
                        --(show . fromInt64AsString :: Int64AsString -> String)
                        (show . fromInt64AsString)
                        unjsonDef
instance Unjson Int64AsString where
  unjsonDef = unjsonInt64AsString

docJSONTests :: TestEnvSt -> Test
docJSONTests env = testGroup "DocJSON"
  [ testThat "TODO" env firstTest
  ]

firstTest :: TestEnv ()
firstTest = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  testJSONWith "test/json/test1.json" docUnjsonDef (rsBody resDoc)


data Doc = Doc
  { docid :: Int64AsString
  , title :: String
  }
  deriving (Show)

docUnjsonDef :: UnjsonDef Doc
docUnjsonDef = objectOf $ pure Doc
  <*> field "id"
            docid
            "Doc id"
  <*> field "title"
            title
            "Doc title"

testJSONWith :: FilePath -> UnjsonDef a -> BS.ByteString -> TestEnv ()
testJSONWith fp useUnjsonDef jsonBS = do
  jsonFileBS <- liftIO $ B.readFile fp
  let Just value    = decode jsonBS
      Just jsonFile = decode jsonFileBS
      U.Result unjsoned problems = parse useUnjsonDef value
  assertEqual "There should be no problems in Result" [] problems
  assertEqual ("JSON structure and types (including 'null') should match that in " ++ fp)
              (removeValues jsonFile) (removeValues value)

removeValues :: Value -> Value
removeValues (Object m) = Object (H.map removeValues m)
removeValues (Array v)  = Array  (V.map removeValues v)
removeValues (String _) = String ""
removeValues (Number _) = Number 0
removeValues (Bool _)   = Bool False
removeValues Null       = Null
