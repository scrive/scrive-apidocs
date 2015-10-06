module Doc.API.AesonTestUtils where

import Data.Aeson
import Data.Scientific
import Data.Text
import Data.Unjson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.Document
import Doc.Data.DocumentStatus (DocumentStatus(..))
import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import File.FileID
import KontraPrelude
import TestKontra

valueFromBS :: BS.ByteString -> TestEnv Value
valueFromBS bs = case decode bs of
                      Just j -> return j
                      Nothing -> $unexpectedErrorM "Could not parse JSON from ByteString"

-- | TODO comment
documentIDFromValue :: Value -> TestEnv DocumentID
documentIDFromValue v = do
  idStr <- lookupObjectString "id" v
  case (maybeRead $ unpack idStr) of
    Just did -> return did
    Nothing -> $unexpectedErrorM "Could not read DocumentID using `maybeRead`"

-- TODO comment
signatoryLinkIDsFromValue :: Value -> TestEnv [SignatoryLinkID]
signatoryLinkIDsFromValue v = do
  slArray <- lookupObjectArray "signatories" v
  forM (V.toList slArray) (\sigValue -> do
      slidStr <- lookupObjectString "id" sigValue
      case (maybeRead $ unpack slidStr) of
        Just slid -> return slid
        Nothing -> $unexpectedErrorM "Could not read SignatoryLinkID using `maybeRead`"
      )

-- | TODO comment
fileIDFromFileValue :: Value -> TestEnv FileID
fileIDFromFileValue v = do
  fidStr <- lookupObjectString "id" v
  case (maybeRead $ unpack fidStr) of
    Just fid -> return fid
    Nothing -> $unexpectedErrorM "Could not read FileID using `maybeRead`"

-- | TODO comment (internal function)
lookupObject :: Text -> Value -> TestEnv Value
lookupObject k (Object o) = case (H.lookup k o) of
                              Just v -> return v
                              Nothing -> $unexpectedErrorM $ "Could not look up key: '" ++ unpack k ++ "'"
lookupObject _ _ = $unexpectedErrorM "Value was not an Object"

-- | TODO comment
lookupObjectObjectValue :: Text -> Value -> TestEnv Value
lookupObjectObjectValue k v = do
  val <- lookupObject k v
  case val of
    ov@(Object _) -> return ov
    _ -> $unexpectedErrorM "Lookup did not give Object"

-- | TODO comment
lookupObjectArray :: Text -> Value -> TestEnv (V.Vector Value)
lookupObjectArray k v = do
  val <- lookupObject k v
  case val of
    Array a -> return a
    _ -> $unexpectedErrorM "Lookup did not give Array"

-- | TODO comment
lookupObjectString :: Text -> Value -> TestEnv Text
lookupObjectString k v = do
  val <- lookupObject k v
  case val of
    String s -> return s
    _ -> $unexpectedErrorM "Lookup did not give String"

-- | TODO comment
lookupObjectNumber :: Text -> Value -> TestEnv Scientific
lookupObjectNumber k v = do
  val <- lookupObject k v
  case val of
    Number n -> return n
    _ -> $unexpectedErrorM "Lookup did not give Number"

-- | TODO comment
lookupObjectBool :: Text -> Value -> TestEnv Bool
lookupObjectBool k v = do
  val <- lookupObject k v
  case val of
    Bool b -> return b
    _ -> $unexpectedErrorM "Lookup did not give Bool"

-- | TODO comment
lookupObjectNull :: Text -> Value -> TestEnv ()
lookupObjectNull k v = do
  val <- lookupObject k v
  case val of
    Null -> return ()
    _ -> $unexpectedErrorM "Lookup did not give Null"

-- | TODO comment
parseMockDocumentFromBS :: DocumentID -> BS.ByteString -> TestEnv Value
parseMockDocumentFromBS did bs = do
  docJSON <- valueFromBS bs
  let da = DocumentAccess { daDocumentID = did, daAccessMode = AuthorDocumentAccess , daStatus = Pending}
  case parse (unjsonDocument da) docJSON of
    (Result _ []) -> return docJSON
    (Result _ _) -> $unexpectedErrorM "Could not parse Document JSON"
