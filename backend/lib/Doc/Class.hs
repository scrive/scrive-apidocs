module Doc.Class where

import Control.Monad.Trans
import Control.Monad.Trans.Control

import Control.Monad.Trans.Control.Util
import DB
import Doc.DocStateData
import Doc.DocumentID

-- This monad is currently implemented as a thin layer over
-- DB.RowCache.  More (document-specific) operations may be coming.

-- | Monad for modifying a document in the database.  The document is
-- kept in a cache so that it can be accessed without having to query
-- the database repeatedly.  Operations in this monad that modify the
-- document in the database (think sqlUpdate) must be wrapped inside
-- 'updateDocument' or 'updateDocumentWithID' so that the cache gets
-- invalidated.

class MonadDB m => DocumentMonad m where
  -- | Retrieve the document from the cache, or from the database if
  -- necessary.
  theDocument :: m Document
  -- | Return the document's ID.
  theDocumentID :: m DocumentID
  -- | Perform an operation that modifies the document in the
  -- database, given the document.
  updateDocument :: (Document -> m a) -> m a
  -- | Perform an operation that modifies the document in the
  -- database, given the document's ID.
  updateDocumentWithID :: (DocumentID -> m a) -> m a

-- | Generic, overlapping instance.
instance (
    DocumentMonad m
  , MonadDB (t m)
  , MonadTrans t
  , MonadTransControl t
  ) => DocumentMonad (t m) where
  theDocument   = lift theDocument
  theDocumentID = lift theDocumentID
  updateDocument m = controlT $ \run -> updateDocument (run . m)
  updateDocumentWithID m = controlT $ \run -> updateDocumentWithID (run . m)
