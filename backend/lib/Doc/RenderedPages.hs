module Doc.RenderedPages
  ( RenderedPages
  , RenderedPagesCache
  , renderedPages
  , putPage
  , getPage
  , pagesCount
  , legacyWidthInPixels
  , RenderingMode(..)
  ) where

import Control.Monad.Base
import Control.Concurrent.MVar.Lifted
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Log.Class
import File.FileID
import KontraPrelude
import qualified MemCache

newtype RenderedPages = RenderedPages {
    unRenderedPages :: V.Vector (MVar BS.ByteString)
  }

data RenderingMode
  = RenderingModeWholeDocument
  | RenderingModeFirstPageOnly
  deriving (Eq, Ord, Show)

renderedPages :: MonadBase IO m => Int -> m RenderedPages
renderedPages n = RenderedPages <$> V.replicateM n newEmptyMVar

putPage :: MonadBase IO m => RenderedPages -> Int -> BS.ByteString -> m Bool
putPage rp n = tryPutMVar $ unRenderedPages rp V.! (n-1)

getPage :: (MonadBase IO m, MonadLog m) => RenderedPages -> Int -> m BS.ByteString
getPage rp n = readMVar . (V.! (n-1)) $ unRenderedPages rp

pagesCount :: RenderedPages -> Int
pagesCount (RenderedPages ps) = V.length ps

-- | This is a memcache indexed by tripples: FileID of file that was rendered,
-- page width in pixels that was requested and indication if whole document
-- was requested to be rendered (True) or just the first page (False)
type RenderedPagesCache = MemCache.MemCache (FileID, Int, RenderingMode) RenderedPages

legacyWidthInPixels :: Int
legacyWidthInPixels = 943
