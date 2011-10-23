module JSONUtilTest (jsonUtilTests) where



import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import TestingUtil
import Test.QuickCheck

import Util.JSON
import Text.JSON
import Text.JSON.String
import qualified Data.ByteString.UTF8 as BS

jsonUtilTests :: Test
jsonUtilTests = testGroup "JSONUtil"
    [   testGroup "JSON Container Test"
       [  testCase "Check basic corelation between getJSON and putJSON" $ testJSONContainer ]
      , testGroup "Checking diggers"
       [  testCase "Check primitive JSON digger"  testAskJSON
        , testCase "Check basic JSON diggers"  testBasicDiggers
        , testCase "Check advanced JSON diggers" testAdvancedDiggers 
       ]
    ]


  
testJSONContainer ::  Assertion
testJSONContainer = doNTimes 100 $ do
   (initJSON :: JSValue) <- rand 10 arbitrary 
   (newJSON :: JSValue) <- rand 10 arbitrary
   let changedJSON = getJSON (setJSON newJSON initJSON)
   assertBool "New value gets set" (changedJSON == newJSON)  

testAskJSON::  Assertion
testAskJSON = doNTimes 100 $ do
    json <- rand 10 arbitrary
    json' <- withJSON json $ askJSON
    assertBool "Test askJSON" (json == json')

testBasicDiggers ::  Assertion
testBasicDiggers= do
    let Right json = runGetJSON readJSObject "{\"a\":\"1\" , \"b\" : 1 , \"c\": [], \"d\": {} }"
    withJSON json $ do
        a <- askJSONString "a"
        assertBool "Test askJSONString" (a == Just "1")  
        a' <- askJSONBS "a"
        assertBool "Test askJSONBS" (a' == Just (BS.fromString "1"))  
        b <-  askJSONInteger "b"
        assertBool "Test askJSONInteger" (b == Just 1)  
        c <-  askJSONList  "c" 
        assertBool "Test askJSONList" (c == Just [])  
        d <-  askJSONField "d"
        assertBool "Test askJSONField" (d == (Just $ JSObject $ toJSObject []))  
        
testAdvancedDiggers ::  Assertion
testAdvancedDiggers= do
    let Right json1 = runGetJSON readJSObject "{\"a\": {\"b\" : [{\"c\":1},{\"c\":2},{\"c\":3}] }}"
    withJSON json1 $ do
        l <- askJSONLocal "a" $ do 
                askJSONLocal "b" $
                    askJSONLocalMap (askJSONInteger "c")
        assertBool "Test positive digger" (l == Just [1,2,3])  
    let Right json2 = runGetJSON readJSArray "[{\"a\":1},{\"a\":2},{}]"
    withJSON json2 $ do
        l <-  askJSONLocalMap (askJSONInteger "a")
        assertBool "Test negative digger" (l == Nothing)  
        
        
                