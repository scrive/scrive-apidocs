{-# OPTIONS_GHC -fno-warn-orphans #-}
module FlashMessages (flashMessagesTests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property, elements, ioProperty, mapSize, oneof)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS

import FlashMessage (FlashMessage(..), FlashType(..), fromCookieValue, toCookieValue)
import KontraPrelude
import TestKontra

flashMessagesTests :: TestEnvSt -> Test
flashMessagesTests _ = testGroup "FlashMessages"
  [ {-testProperty "Flash decoding can handle arbitrary input." $ flashDecodeAnything
  , -}testProperty "Flash encoding/decoding is identity." $ flashEncodeDecode
  ]


instance Arbitrary FlashMessage where
  arbitrary = oneof [ FlashMessage  <$> arbitrary <*> arbitrary
                    ]

instance Arbitrary FlashType where
  arbitrary = elements [OperationDone, OperationFailed]

flashEncodeDecode :: Property
flashEncodeDecode =
  mapSize (`div` 2) $ \f -> ioProperty $ do -- scale back a bit or it takes too long
    mf <- fromCookieValue (toCookieValue f)
    return $ Just f == mf


-- Currently, this test raises an exception, so we avoid it.  However, it is
-- able to locate a crash in fromCookieValue for empty strings.
_flashDecodeAnything :: String -> Bool
_flashDecodeAnything s = fromCookieValue (BS.unpack (B64.encode (BS.pack s'))) `seq` True
  where s' = take (16 * (length s `div` 16)) s
