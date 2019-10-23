{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.JSON.Orphans where

import Data.Binary as B
import Test.QuickCheck (Arbitrary(..), Gen, frequency, oneof)
import qualified Text.JSON as J

import MinutesTime ()

instance Binary J.JSString where
  get = J.toJSString <$> B.get
  put = put . J.fromJSString

instance Binary a => Binary (J.JSObject a) where
  get = J.toJSObject <$> B.get
  put = put . J.fromJSObject

instance Binary J.JSValue where
  put (J.JSNull        ) = putWord8 0
  put (J.JSBool b      ) = putWord8 1 >> put b
  put (J.JSRational b r) = putWord8 2 >> put b >> put r
  put (J.JSString s    ) = putWord8 3 >> put s
  put (J.JSArray  arr  ) = putWord8 4 >> put arr
  put (J.JSObject obj  ) = putWord8 5 >> put obj

  get = do
    tag <- getWord8
    case tag of
      0 -> return J.JSNull
      1 -> J.JSBool <$> B.get
      2 -> J.JSRational <$> B.get <*> B.get
      3 -> J.JSString <$> B.get
      4 -> J.JSArray <$> B.get
      5 -> J.JSObject <$> B.get
      _ -> fail $ "Unable to parse JSValue because of bad tag: " ++ show tag

-- Arbitrary instances for testing
jsobj :: Int -> Gen (J.JSObject J.JSValue)
jsobj sz = do
  vals  <- jslist sz
  names <- map unStr <$> arbitrary
  return $ J.toJSObject $ zip (zipWith (:) ['a' ..] names) vals

jslist :: Int -> Gen [J.JSValue]
jslist sz = do
  n <- oneof $ map return [0 .. 10]
  sequence $ replicate n (jsval sz)

jsval :: Int -> Gen J.JSValue
jsval sz = frequency
  [ (1 , return J.JSNull)
  , (1 , J.JSBool <$> arbitrary)
  , (1, J.JSRational <$> arbitrary <*> arbitrary)
  , (1, J.JSString . J.toJSString . unStr <$> arbitrary)
  , (sz, J.JSArray <$> jslist (sz `div` 2))
  , (sz, J.JSObject <$> jsobj (sz `div` 2))
  ]

instance Arbitrary (J.JSObject J.JSValue) where
  arbitrary = jsobj 10

instance Arbitrary J.JSValue where
  arbitrary = jsval 10

newtype JSStr = JSStr {unStr :: String}

-- This is a _really_ crappy instance, but I'm sick of writing generators!
instance Arbitrary JSStr where
  arbitrary = do
    n <- oneof $ map return [1 .. 100]
    JSStr <$> sequence (replicate n (oneof (map return ['a' .. 'z'])))
