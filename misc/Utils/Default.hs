module Utils.Default where

class HasDefaultValue a where
  defaultValue :: a

{-

Do not do stuff like this, because it triggers OverlappingInstances
requirement at the point of usage in a very strange way. Haskell treat
all datatypes as having HasDefaultValue and then just later complains
that they do not have Bounded.

Just don't.

instance Bounded a => HasDefaultValue a where
  defaultValue = minBound

-}
