module Doc.AddImageSpec where

import Data.Int
import qualified Data.ByteString.Char8 as BS

-- | Image coordinates are in screen coordinate space. That means:
--
-- * upper left corner is (0,0)
-- * units are pixels
-- * (x,y) are coordinates of upper left corner of an image.

data AddImageSpec = AddImageSpec
  { addImageInput              :: String
  , addImageDocumentNumberText :: String
  , addImageImageBinary        :: BS.ByteString -- ^ binary content of image to put into a field
  , addImageX                  :: Double -- ^ left coordinate of field in (0,0)-(1,1)
  , addImageY                  :: Double -- ^ upper coordinate of field in  (0,0)-(1,1)
  , addImagePage               :: Int32  -- ^ on which page should the field be placed
  }
  deriving (Eq, Ord, Show, Read)
