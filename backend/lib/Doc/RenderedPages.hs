module Doc.RenderedPages
  ( RenderedPages
  , RenderedPagesCache
  , renderedPages
  , putPage
  , getPage
  , hasPage
  , pagesCount
  , legacyWidthInPixels
  , RenderingMode(..)
  ) where

import Control.Concurrent.MVar.Lifted
import Control.DeepSeq
import Control.Monad.Base
import Control.Monad.Catch
import Data.Hashable
import GHC.Generics
import Log
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V

import File.FileID
import KontraPrelude
import qualified MemCache

newtype RenderedPages = RenderedPages {
    unRenderedPages :: V.Vector (MVar BS.ByteString)
  }

instance NFData RenderedPages where
  rnf (RenderedPages v) = go 0 $ V.length v
    where
      go i len
        | i == len  = ()
        | otherwise = V.unsafeIndex v i `seq` go (i + 1) len

data RenderingMode
  = RenderingModeWholeDocument
  | RenderingModeFirstPageOnly
  deriving (Eq, Ord, Show, Generic)

instance Hashable RenderingMode

renderedPages :: MonadBase IO m => Int -> m RenderedPages
renderedPages n = RenderedPages <$> V.replicateM n newEmptyMVar

putPage
  :: (MonadBase IO m, MonadLog m, MonadThrow m)
  => RenderedPages -> Int -> BS.ByteString -> m Bool
putPage rp pageNo content = throwIfOutsideRange rp pageNo $ do
  tryPutMVar (unRenderedPages rp V.! (pageNo - 1)) content

getPage
  :: (MonadBase IO m, MonadLog m, MonadThrow m)
  => RenderedPages -> Int -> m BS.ByteString
getPage rp pageNo = throwIfOutsideRange rp pageNo $ do
  readMVar . (V.! (pageNo - 1)) $ unRenderedPages rp

hasPage
 :: (MonadBase IO m, MonadLog m, MonadThrow m)
 => RenderedPages -> Int -> m Bool
hasPage rp pageNo = throwIfOutsideRange rp pageNo $ do
  isJust <$> tryReadMVar (unRenderedPages rp V.! (pageNo - 1))

pagesCount :: RenderedPages -> Int
pagesCount (RenderedPages ps) = V.length ps

-- | This is a memcache indexed by tripples: FileID of file that was rendered,
-- page width in pixels that was requested and indication if whole document
-- was requested to be rendered (True) or just the first page (False)
type RenderedPagesCache = MemCache.MemCache (FileID, Int, RenderingMode) RenderedPages

legacyWidthInPixels :: Int
legacyWidthInPixels = 943

----------------------------------------

throwIfOutsideRange :: (MonadLog m, MonadThrow m) => RenderedPages -> Int -> m a -> m a
throwIfOutsideRange rp pageNo action = case pageNo > pagesCount rp of
  False -> action
  True  -> do
    let noRequestedPage = "Requested page doesn't exist"
    logAttention noRequestedPage $ object [
        "pages" .= pagesCount rp
      , "page"  .= pageNo
      ]
    $unexpectedErrorM $ T.unpack noRequestedPage
