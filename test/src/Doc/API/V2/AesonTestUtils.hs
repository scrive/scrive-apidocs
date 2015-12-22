module Doc.API.V2.AesonTestUtils (
  testRequestHelper
, jsonTestRequestHelper
-- * Aeson Value deconstructors
, lookupObjectArray
, lookupObjectString
) where

import Data.Aeson
import Happstack.Server
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Context (Context)
import Kontra (Kontra)
import KontraPrelude
import TestingUtil (assertEqual, assertFailure)
import TestKontra

-- | Used to succinctly construct a API requests and test response code
testRequestHelper :: Context -> Method -> [(String, Input)] -> Kontra Response -> Int -> TestEnv BSL.ByteString
testRequestHelper ctx httpMethod params func expectedRsCode = do
  req <- mkRequest httpMethod params
  (rsp,_) <- runTestKontra req ctx func
  let showTestDetails = "Method: " ++ show httpMethod ++ "\n"
                     ++ "Params: " ++ concatMap (\(s,i) -> "\n\t" ++ s ++ ": " ++ show i) params ++ "\n"
                     ++ "Response:\n" ++ show (rsBody rsp)
  assertEqual ("Response code should match for:\n" ++ showTestDetails) expectedRsCode (rsCode rsp)
  return (rsBody rsp)

-- | Used to succinctly construct a API requests, test response code, and parse
-- the response body into a JSON Value.
--
-- Example usage:
--
-- @
--   docJSON <- jsonTestRequestHelper ctx GET [] (docApiV2Get did) 200
--   testSomething docJSON
-- @
jsonTestRequestHelper :: Context -> Method -> [(String, Input)] -> Kontra Response -> Int
       -> TestEnv Value
jsonTestRequestHelper ctx httpMethod params func expectedRsCode = do
  rsp <- testRequestHelper ctx httpMethod params func expectedRsCode
  let showTestDetails = "Method: " ++ show httpMethod ++ "\n"
                     ++ "Params: " ++ concatMap (\(s,i) -> "\n\t" ++ s ++ ": " ++ show i) params ++ "\n"
                     ++ "Response:\n" ++ show rsp
  let mRspJS = decode rsp
  when (isNothing mRspJS) $ assertFailure $ "Could not parse JSON from ByteString response:\n" ++ showTestDetails
  return $ $fromJust mRspJS

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `Array`.
lookupObjectArray :: String -> Value -> TestEnv [Value]
lookupObjectArray k v = do
  val <- lookupObject k v
  case val of
    Array a -> return $ V.toList a
    _ -> $unexpectedErrorM "Lookup did not give Array"

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `String`.
lookupObjectString :: String -> Value -> TestEnv String
lookupObjectString k v = do
  val <- lookupObject k v
  case val of
    String s -> return $ T.unpack s
    _ -> $unexpectedErrorM "Lookup did not give String"

-- * Internal Functions

-- | Internal Utility Function
lookupObject :: String -> Value -> TestEnv Value
lookupObject k (Object o) = case (H.lookup (T.pack k) o) of
                              Just v -> return v
                              Nothing -> $unexpectedErrorM $ "Could not look up key: '" ++ k ++ "'"
lookupObject _ _ = $unexpectedErrorM "Value was not an Object"
