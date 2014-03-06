{-# LANGUAGE OverlappingInstances #-}
-- | Monad for manipulating documents
module Doc.DocumentMonad
  ( DocumentMonad (..)
  , DocumentT(..)
  , withDocument
  , withDocumentID
  , withDocumentM
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (lift, MonadIO, MonadTrans)
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..), ComposeSt, defaultLiftBaseWith, defaultRestoreM, defaultLiftWith, defaultRestoreT)
import Control.Monad.Trans.Control.Util
import Data.Monoid.Space
import DB
import DB.RowCache (RowCacheT, GetRow, runRowCacheT, runRowCacheTID, runRowCacheTM, rowCache, rowCacheID, updateRow, updateRowWithID)
import Doc.DocStateData (Document)
import Doc.DocumentID (DocumentID)
import Log (MonadLog(..))
import Text.JSON
import Text.JSON.Gen

-- This monad is currently implemented as a thin layer over
-- DB.RowCache.  More (document-specific) operations may be coming.

-- | Monad for modifying a document in the database.  The document is
-- kept in a cache so that it can be accessed without having to query
-- the database repeatedly.  Operations in this monad that modify the
-- document in the database (think sqlUpdate) must be wrapped inside
-- 'updateDocument' or 'updateDocumentWithID' so that the cache gets
-- invalidated.

class MonadDB m => DocumentMonad m where
  -- | Retrieve the document from the cache, or from the database if necessary.
  theDocument :: m Document
  -- | Return the document's ID.
  theDocumentID :: m DocumentID
  -- | Perform an operation that modifies the document in the database, given the document.
  updateDocument :: (Document -> m a) -> m a
  -- | Perform an operation that modifies the document in the database, given the document's ID
  updateDocumentWithID :: (DocumentID -> m a) -> m a

-- | A monad transformer that has a 'DocumentMonad' instance
newtype DocumentT m a = DocumentT { unDocumentT :: RowCacheT Document m a }
  deriving (Applicative, Monad, MonadDB, Functor, MonadIO, MonadTrans, MonadBase b)

instance MonadLog m => MonadLog (DocumentT m) where
  mixlogjs title js = do
    case toJSValue js of
      JSObject jso -> DocumentT $ do
        did <- rowCacheID
        mixlogjs title (JSObject (toJSObject (("documentid",toJSValue (show did)) : fromJSObject jso)))
      jsx -> DocumentT $ mixlogjs title jsx

-- | Lock a document and perform an operation that modifies the
-- document in the database, given the document
withDocument :: (MonadDB m, GetRow Document m) => Document -> DocumentT m a -> m a
withDocument d = runRowCacheT d . unDocumentT . (lockDocument >>)

-- | Lock a document and perform an operation that modifies the
-- document in the database, given the document ID
withDocumentID :: (MonadDB m, GetRow Document m) => DocumentID -> DocumentT m a -> m a
withDocumentID d = runRowCacheTID d . unDocumentT . (lockDocument >>)

-- | Lock a document and perform an operation that modifies the
-- document in the database, given an operation that obtains the
-- document
withDocumentM :: (MonadDB m, GetRow Document m) => m Document -> DocumentT m a -> m a
withDocumentM dm = runRowCacheTM dm . unDocumentT . (lockDocument >>)

-- | Lock a document so that other transactions that attempt to lock or update the document will wait until the current transaction is done.
lockDocument :: DocumentMonad m => m ()
lockDocument = updateDocumentWithID $ \did ->
  runQuery_ $ "SELECT TRUE FROM documents WHERE id =" <?> did <+> "FOR UPDATE"

--  The interesting instance

instance (GetRow Document m, MonadDB m) => DocumentMonad (DocumentT m) where
  theDocument = DocumentT rowCache
  theDocumentID = DocumentT rowCacheID
  updateDocument m = DocumentT $ updateRow $ unDocumentT . m
  updateDocumentWithID m = DocumentT $ updateRowWithID $ unDocumentT . m

-- Generic instance

instance (
    DocumentMonad m
  , MonadDB (t m)
  , MonadTrans t
  , MonadTransControl t
  ) => DocumentMonad (t m) where
    theDocument = lift theDocument
    theDocumentID = lift theDocumentID
    updateDocument m = controlT $ \run -> updateDocument (run . m)
    updateDocumentWithID m = controlT $ \run -> updateDocumentWithID (run . m)

instance MonadBaseControl b m => MonadBaseControl b (DocumentT m) where
  newtype StM (DocumentT m) a = StM { unStM :: ComposeSt DocumentT m a }
  liftBaseWith = defaultLiftBaseWith StM
  restoreM     = defaultRestoreM unStM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl DocumentT where
  newtype StT DocumentT a = StT { unStT :: StT (RowCacheT Document) a }
  liftWith = defaultLiftWith DocumentT unDocumentT StT
  restoreT = defaultRestoreT DocumentT unStT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}
