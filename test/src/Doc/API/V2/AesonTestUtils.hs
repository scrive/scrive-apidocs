module Doc.API.V2.AesonTestUtils (
  jsonTestRequestHelper
-- * Aeson Value deconstructors
, lookupObjectArray
, lookupObjectString
-- * Not currently used, but could be useful
, _lookupObjectObjectValue
, _lookupObjectNumber
, _lookupObjectBool
, _lookupObjectNull
) where

import Data.Aeson
import Data.Scientific
import Happstack.Server
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Context (Context)
import Kontra (Kontra)
import KontraPrelude
import TestingUtil (assertEqual)
import TestKontra

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
  req <- mkRequest httpMethod params
  (rsp,_) <- runTestKontra req ctx func
  let showTestDetails = "Method: " ++ show httpMethod ++ "\n"
                     ++ "Params: " ++ concatMap (\(s,i) -> "\n\t" ++ s ++ ": " ++ show i) params ++ "\n"
                     ++ "Response:\n" ++ show (rsBody rsp)
  assertEqual ("Response code should match for:\n" ++ showTestDetails) expectedRsCode (rsCode rsp)
  let rspJSON = case decode . rsBody $ rsp of
                Just j -> j
                Nothing -> $unexpectedError $
                    "Could not parse JSON from ByteString response:\n" ++ showTestDetails
  return rspJSON

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `Object`.
_lookupObjectObjectValue :: String -> Value -> TestEnv Value
_lookupObjectObjectValue k v = do
  val <- lookupObject k v
  case val of
    ov@(Object _) -> return ov
    _ -> $unexpectedErrorM "Lookup did not give Object"

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `Array`.
lookupObjectArray :: String -> Value -> TestEnv (V.Vector Value)
lookupObjectArray k v = do
  val <- lookupObject k v
  case val of
    Array a -> return a
    _ -> $unexpectedErrorM "Lookup did not give Array"

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `String`.
lookupObjectString :: String -> Value -> TestEnv String
lookupObjectString k v = do
  val <- lookupObject k v
  case val of
    String s -> return $ T.unpack s
    _ -> $unexpectedErrorM "Lookup did not give String"

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `Number`.
_lookupObjectNumber :: String -> Value -> TestEnv Scientific
_lookupObjectNumber k v = do
  val <- lookupObject k v
  case val of
    Number n -> return n
    _ -> $unexpectedErrorM "Lookup did not give Number"

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `Bool`.
_lookupObjectBool :: String -> Value -> TestEnv Bool
_lookupObjectBool k v = do
  val <- lookupObject k v
  case val of
    Bool b -> return b
    _ -> $unexpectedErrorM "Lookup did not give Bool"

-- | Given a `Value` that should be an `Object`, lookup a key which should be
-- an JSON `Null`.
_lookupObjectNull :: String -> Value -> TestEnv ()
_lookupObjectNull k v = do
  val <- lookupObject k v
  case val of
    Null -> return ()
    _ -> $unexpectedErrorM "Lookup did not give Null"

-- * Internal Functions

-- | Internal Utility Function
lookupObject :: String -> Value -> TestEnv Value
lookupObject k (Object o) = case (H.lookup (T.pack k) o) of
                              Just v -> return v
                              Nothing -> $unexpectedErrorM $ "Could not look up key: '" ++ k ++ "'"
lookupObject _ _ = $unexpectedErrorM "Value was not an Object"
