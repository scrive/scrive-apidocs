module Doc.API.V2.AesonTestUtils (
  testRequestHelper
, testRequestHelperNoAssert_
, jsonTestRequestHelper
-- * Aeson Value deconstructors
, lookupObjectArray
, lookupObjectString
, lookupObjectInt
) where

import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Scientific (floatingOrInteger)
import Happstack.Server
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Context (Context)
import TestingUtil (assertEqual, assertFailure)
import TestKontra

-- | Used to succinctly construct a API requests and test response code
testRequestHelper
  :: Context
  -> Method
  -> [(Text, Input)]
  -> KontraTest Response
  -> Int
  -> TestEnv BSL.ByteString
testRequestHelper ctx httpMethod params func expectedRsCode = do
  req      <- mkRequest httpMethod params
  (rsp, _) <- runTestKontra req ctx func
  let showTestDetails :: Text =
        "Method: "
          <> (showt httpMethod)
          <> "\n"
          <> "Params: "
          <> (T.concat $ fmap (\(s, i) -> "\n\t" <> s <> ": " <> (showt i)) params)
          <> "\n"
          <> "Response:\n"
          <> (showt (rsBody rsp))
  assertEqual ("Response code should match for:\n" <> T.unpack showTestDetails)
              expectedRsCode
              (rsCode rsp)
  return (rsBody rsp)

-- | Used to succinctly construct an API request without assertions
testRequestHelperNoAssert_
  :: Context -> Method -> [(String, Input)] -> KontraTest Response -> TestEnv ()
testRequestHelperNoAssert_ ctx httpMethod params func = do
  req      <- mkRequest httpMethod $ fmap (\(t1, t2) -> (T.pack t1, t2)) params
  (rsp, _) <- runTestKontra req ctx func
  let showRequestDetails =
        "Request details:\n"
          <> "Method: "
          <> show httpMethod
          <> "\n"
          <> "Params: "
          <> showParams params
          <> "\n"
          <> "Response:\n"
          <> show (rsBody rsp)
      showParams = concatMap (\(s, i) -> "\n\t" <> s <> ": " <> show i)
  liftIO $ putStrLn showRequestDetails
  return ()

-- | Used to succinctly construct a API requests, test response code, and parse
-- the response body into a JSON Value.
--
-- Example usage:
--
-- @
--   docJSON <- jsonTestRequestHelper ctx GET [] (docApiV2Get did) 200
--   testSomething docJSON
-- @
jsonTestRequestHelper
  :: Context -> Method -> [(Text, Input)] -> KontraTest Response -> Int -> TestEnv Value
jsonTestRequestHelper ctx httpMethod params func expectedRsCode = do
  rsp <- testRequestHelper ctx httpMethod params func expectedRsCode
  let showTestDetails =
        "Method: "
          <> showt httpMethod
          <> "\n"
          <> "Params: "
          <> (T.concat $ fmap (\(s, i) -> "\n\t" <> s <> ": " <> showt i) params)
          <> "\n"
          <> "Response:\n"
          <> showt rsp
  let mRspJS = decode rsp
  when (isNothing mRspJS)
    $  assertFailure
    $  "Could not parse JSON from ByteString response:\n"
    <> T.unpack showTestDetails
  return $ fromJust mRspJS

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `Array`.
lookupObjectArray :: String -> Value -> TestEnv [Value]
lookupObjectArray k v = do
  val <- lookupObject k v
  case val of
    Array a -> return $ V.toList a
    _       -> unexpectedError "Lookup did not give Array"

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `String`.
lookupObjectString :: String -> Value -> TestEnv String
lookupObjectString k v = do
  val <- lookupObject k v
  case val of
    String s -> return $ T.unpack s
    _        -> unexpectedError "Lookup did not give String"

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `Number`.
lookupObjectInt :: String -> Value -> TestEnv Int
lookupObjectInt k v = do
  val <- lookupObject k v
  case val of
    Number n -> case floatingOrInteger n of
      Right i -> return i
      _       -> unexpectedError "Lookup did not give Int"
    _ -> unexpectedError "Lookup did not give Int"



-- * Internal Functions

-- | Internal Utility Function
lookupObject :: String -> Value -> TestEnv Value
lookupObject k (Object o) = case (H.lookup (T.pack k) o) of
  Just v  -> return v
  Nothing -> unexpectedError $ T.pack $ "Could not look up key: '" <> k <> "'"
lookupObject _ _ = unexpectedError "Value was not an Object"
