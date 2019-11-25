{-# OPTIONS_GHC -fno-warn-orphans #-}
module FlashMessages (flashMessagesTests) where

import Control.Monad.Identity
import Network.HTTP.Base (urlDecode)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
  ( Arbitrary(..), Property, elements, ioProperty, mapSize, oneof
  )
import Text.JSON
import Text.JSON.FromJSValue

import FlashMessage (FlashMessage(..), FlashType(..), toCookieValue)
import TestKontra

flashMessagesTests :: TestEnvSt -> Test
flashMessagesTests _ = testGroup
  "FlashMessages"
  [testProperty "Flash cookie value is json in expected format" $ flashCookieParse]


instance Arbitrary FlashMessage where
  arbitrary = oneof [FlashMessage <$> arbitrary <*> arbitrary]

instance Arbitrary FlashType where
  arbitrary = elements [OperationDone, OperationFailed]

flashCookieParse :: Property
flashCookieParse = mapSize (`div` 2) $ \f -> ioProperty $ do -- scale back a bit or it takes too long
  case (decode $ toCookieValue f) of
    (Ok jresp) -> do
      r <- return $ runIdentity $ withJSValue jresp $ do
        (t :: Maybe String) <- fromJSValueField "type"
        (c :: Maybe String) <- fromJSValueField "content"
        return (t, c)
      case r of
        (Just t, Just c) ->
          return $ (t `elem` ["success", "error"]) && (urlDecode c == flashMessage f)
        _ -> return False
    _ -> return False
