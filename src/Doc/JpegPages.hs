module Doc.JpegPages (JpegPages(..), pagesCount) where

import qualified Data.ByteString as BS

data JpegPages = JpegPagesPending                         -- Pages are bing rendered in the background
               | JpegPages [BS.ByteString]                -- Image binary data (currently png)
               | JpegPagesError BS.ByteString             -- There was an error rendering pages
    deriving (Eq, Ord, Show)

pagesCount :: JpegPages -> Int
pagesCount (JpegPages ps) = length ps
pagesCount _              = 0
