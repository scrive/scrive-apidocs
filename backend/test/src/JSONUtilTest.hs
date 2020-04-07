{- This should be removed to its fileds-JSON library -}
module JSONUtilTest (jsonUtilTests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.String
import qualified Data.ByteString.UTF8 as BS

import TestingUtil
import TestKontra

jsonUtilTests :: TestEnvSt -> Test
jsonUtilTests _env = testGroup
  "JSONUtil"
  [ testGroup
      "Checking diggers"
      [ testCase "Check basic JSON diggers"    testBasicDiggers
      , testCase "Check advanced JSON diggers" testAdvancedDiggers
      ]
  ]


testBasicDiggers :: Assertion
testBasicDiggers = do
  let Right json =
        runGetJSON readJSObject "{\"a\":\"1\" , \"b\" : 1 , \"c\": [], \"d\": {} }"
  withJSValue json $ do
    a <- fromJSValueField "a"
    assertBool "Test fromJSValueField String" (a == Just ("1" :: String))
    a' <- fromJSValueField "a"
    assertBool "Test fromJSValueField BS" (a' == Just (BS.fromString "1"))
    b <- fromJSValueField "b"
    assertBool "Test fromJSValueField Integer" (b == Just (1 :: Integer))
    c <- fromJSValueField "c"
    assertBool "Test fromJSValueField List" (c == Just ([] :: [String]))
    d <- fromJSValueField "d"
    assertBool "Test fromJSValueField Object" (d == Just (JSObject $ toJSObject []))

testAdvancedDiggers :: Assertion
testAdvancedDiggers = do
  let Right json1 =
        runGetJSON readJSObject "{\"a\": {\"b\" : [{\"c\":1},{\"c\":2},{\"c\":3}] }}"
  withJSValue json1 $ do
    l <- fromJSValueFieldCustom "a" $ do
      fromJSValueFieldCustom "b" $ do
        fromJSValueCustomMany (fromJSValueField "c")
    assertBool "Test positive digger" (l == Just ([1, 2, 3] :: [Integer]))
  let Right json2 = runGetJSON readJSArray "[{\"a\":1},{\"a\":2},{}]"
  withJSValue json2 $ do
    l <- fromJSValueCustomMany (fromJSValueField "a")
    assertBool "Test negative digger" (l == (Nothing :: Maybe [String]))
