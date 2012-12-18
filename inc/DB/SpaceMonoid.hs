

module DB.SpaceMonoid
where

import Data.ByteString
import Data.ByteString.Lazy
import Data.Monoid

infixr 6 <+>


{-
 SpaceMonoid is a Monoid with one element distinguished as a separator.
-}
class Monoid m => SpaceMonoid m where
  mspace :: m

instance SpaceMonoid String where
  mspace = " "

instance SpaceMonoid Data.ByteString.ByteString where
  mspace = Data.ByteString.singleton 32

instance SpaceMonoid Data.ByteString.Lazy.ByteString where
  mspace = Data.ByteString.Lazy.singleton 32

-- | Concatenate fragments with a space in between
(<+>) :: (SpaceMonoid sm) => sm -> sm -> sm
s1 <+> s2 = s1 <> mspace <> s2
