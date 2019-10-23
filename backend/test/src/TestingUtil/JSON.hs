module TestingUtil.JSON
  ( testJSONWithDynamicKeys
  , setDocKey
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import TestingUtil
import TestKontra

-- Compare JSON sesults from API calls. We check that the structure of the JSON
-- is the same as expected value as well as its subset that excludes supplied
-- dynamic keys.
testJSONWithDynamicKeys :: [Text] -> FilePath -> BS.ByteString -> TestEnv ()
testJSONWithDynamicKeys dynamicKeys fp jsonBS = do
  jsonFileBS <- liftIO $ B.readFile fp
  let Just value    = decode jsonBS
      Just jsonFile = decode jsonFileBS
  assertEqualJson
    ("JSON structure and types (including 'null') should match that in " ++ fp)
    (removeValues jsonFile)
    (removeValues value)
  assertEqualJson
    ("JSON structure and values should match if we will remove dynamic values (like documentid or mtime) "
    ++ fp
    )
    (removeDynamicValues jsonFile)
    (removeDynamicValues value)
  where
    removeValues :: Value -> Value
    removeValues (Object m) = Object (H.map removeValues m)
    removeValues (Array  v) = Array (V.map removeValues v)
    removeValues (String _) = String ""
    removeValues (Number _) = Number 0
    removeValues (Bool   _) = Bool False
    removeValues Null       = Null

    removeDynamicValues :: Value -> Value
    removeDynamicValues (Object m) = Object . H.map removeDynamicValues $ H.filterWithKey
      (\k _ -> not $ k `elem` dynamicKeys)
      m
    removeDynamicValues (Array v) = Array (V.map removeDynamicValues v)
    removeDynamicValues v         = v

-- | Replace specific key in the document Object with supplied Value.
setDocKey :: Text -> Value -> Value -> Value
setDocKey k n (Object doc) = Object $ H.adjust (const n) k doc
setDocKey _ _ v            = v
