module Doc.Screenshot
  ( T(..)
  ) where

import DB.Binary (Binary)

data T = T
  { mimetype :: String
  , image :: Binary
  }
  deriving Show
