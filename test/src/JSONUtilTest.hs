module JSONUtilTest (jsonUtilTests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import TestingUtil
import Test.QuickCheck

import Crypto.RNG
import DB.Nexus
import Util.JSON
import TestKontra
import Text.JSON
import Text.JSON.String
import qualified Data.ByteString.UTF8 as BS

jsonUtilTests :: (Nexus, CryptoRNGState) -> Test
jsonUtilTests env = testGroup "JSONUtil" [
    testGroup "JSON Container Test" [
      testThat "Check basic corelation between getJSON and putJSON" env testJSONContainer
    ]
  , testGroup "Checking diggers" [
      testThat "Check primitive JSON digger" env testAskJSON
    , testCase "Check basic JSON diggers"  testBasicDiggers
    , testCase "Check advanced JSON diggers" testAdvancedDiggers
    ]
  ]

testJSONContainer :: TestEnv ()
testJSONContainer = doNTimes 100 $ do
   (initJSON :: JSValue) <- rand 10 arbitrary 
   (newJSON :: JSValue) <- rand 10 arbitrary
   let changedJSON = getJSON (setJSON newJSON initJSON)
   assertBool "New value gets set" (changedJSON == newJSON)  

testAskJSON :: TestEnv ()
testAskJSON = doNTimes 100 $ do
    json <- rand 10 arbitrary
    json' <- withJSON json $ askJSON
    assertBool "Test askJSON" (json == json')

testBasicDiggers :: Assertion
testBasicDiggers= do
    let Right json = runGetJSON readJSObject "{\"a\":\"1\" , \"b\" : 1 , \"c\": [], \"d\": {} }"
    withJSON json $ do
        a <- fromJSONField "a"
        assertBool "Test fromJSONField String" (a == Just "1")  
        a' <- fromJSONField "a"
        assertBool "Test fromJSONField BS" (a' == Just (BS.fromString "1"))  
        b <-  fromJSONField "b"
        assertBool "Test fromJSONField Integer" (b == Just (1::Integer))  
        c <-  fromJSONField  "c" 
        assertBool "Test fromJSONField List" (c == Just ([]::[String]))  
        d <-  fromJSONField "d"
        assertBool "Test fromJSONField Object" (d == (Just $ JSObject $ toJSObject []))  

testAdvancedDiggers :: Assertion
testAdvancedDiggers= do
    let Right json1 = runGetJSON readJSObject "{\"a\": {\"b\" : [{\"c\":1},{\"c\":2},{\"c\":3}] }}"
    withJSON json1 $ do
        l <- fromJSONLocal "a" $ do 
                fromJSONLocal "b" $ do
                    fromJSONLocalMap (fromJSONField "c")
        assertBool "Test positive digger" (l == Just ([1,2,3]::[Integer]))  
    let Right json2 = runGetJSON readJSArray "[{\"a\":1},{\"a\":2},{}]"
    withJSON json2 $ do
        l <-  fromJSONLocalMap (fromJSONField "a")
        assertBool "Test negative digger" (l == (Nothing::Maybe [String]))  
