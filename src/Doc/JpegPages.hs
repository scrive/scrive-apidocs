module Doc.JpegPages where

import qualified Data.ByteString as BS

data JpegPages = JpegPagesPending
               | JpegPages [(BS.ByteString, Int, Int)]  -- Data + width + height (scaled with some resolution)
               | JpegPagesError BS.ByteString
    deriving (Eq, Ord)
