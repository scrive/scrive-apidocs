module Doc.JpegPages (JpegPages(..), pagesCount) where

import qualified Data.ByteString as BS

data JpegPages = JpegPagesPending
               | JpegPages [(BS.ByteString, Int, Int)]  -- Data + width + height (scaled with some resolution)
               | JpegPagesError BS.ByteString
    deriving (Eq, Ord, Show)

pagesCount :: JpegPages -> Int
pagesCount (JpegPages ps)   = (length ps)
pagesCount _              =  0
 