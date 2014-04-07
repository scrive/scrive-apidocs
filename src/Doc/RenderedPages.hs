module Doc.RenderedPages
  ( RenderedPages(..)
  , RenderedPagesCache
  , pagesCount
  , legacyWidthInPixels
  ) where

import qualified MemCache
import qualified Data.ByteString as BS
import File.FileID

data RenderedPages
  = RenderedPagesPending [BS.ByteString] -- ^ Pages are bing rendered in
                                     -- the background, pages rendered
                                     -- so far available in the array
  | RenderedPages [BS.ByteString]        -- ^ Image binary data (currently png)
  | RenderedPagesError BS.ByteString     -- ^ There was an error rendering pages
    deriving (Eq, Ord, Show)

pagesCount :: RenderedPages -> Int
pagesCount (RenderedPages ps) = length ps
pagesCount _              = 0

-- | This is a memcache indexed by tripples: FileID of file that was rendered,
-- page width in pixels that was requested and indication if whole document
-- was requested to be rendered (True) or just the first page (False)
type RenderedPagesCache = MemCache.MemCache (FileID,Int,Bool) RenderedPages

legacyWidthInPixels :: Int
legacyWidthInPixels = 943
