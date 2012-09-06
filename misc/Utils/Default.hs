module Utils.Default where

class HasDefaultValue a where
  defaultValue :: a

instance Bounded a => HasDefaultValue a where
  defaultValue = minBound
