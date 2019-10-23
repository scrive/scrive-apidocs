module Doc.Types.HighlightedPage (
    HighlightedPage(..)
  , highlightedPagesSelectors
  ) where

import Data.Int

import DB
import Doc.Tables
import File.FileID

data HighlightedPage = HighlightedPage
  { highlightedPagePage :: Int32
  , highlightedPageFileID :: FileID
  } deriving (Eq, Ord, Show)

---------------------------------

highlightedPagesSelectors :: [SQL]
highlightedPagesSelectors = ["highlighted_pages.page", "highlighted_pages.file_id"]

type instance CompositeRow HighlightedPage = (Int32, FileID)

instance PQFormat HighlightedPage where
  pqFormat = compositeTypePqFormat ctHighlightedPage

instance CompositeFromSQL HighlightedPage where
  toComposite (hppage, hpfid) =
    HighlightedPage { highlightedPagePage = hppage, highlightedPageFileID = hpfid }
