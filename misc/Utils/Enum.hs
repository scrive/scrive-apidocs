{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils.Enum where

-- | Extra classes for one way enums
class SafeEnum a where
  fromSafeEnum :: Integral b =>  a -> b
  toSafeEnum   :: Integral b =>  b -> Maybe a

fromSafeEnumInt :: (SafeEnum a) => a -> Int
fromSafeEnumInt = fromSafeEnum

toSafeEnumInt :: (SafeEnum a) => Int -> Maybe a
toSafeEnumInt = toSafeEnum

-- Enum stuff

instance (Enum a, Bounded a, Enum b, Bounded b) => Enum (a,b) where
  toEnum  l = let block = length (allValues::[a])
              in (toEnum $ l `div` block,toEnum $ l `mod` block)
  fromEnum (a,b) = let block = length (allValues::[a])
                   in (fromEnum a * block) + (fromEnum b)

-- | Enumerate all values of a bounded type.
allValues::(Bounded a, Enum a) => [a]
allValues = enumFromTo minBound maxBound
