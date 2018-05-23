-- | Monad for manipulating documents
module Doc.DocumentMonad
  ( module Doc.Class
  , DocumentT(..)
  , withDocument
  , withDocumentID
  , withDocumentM
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Reader (MonadIO, MonadTrans)
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl(..), MonadTransControl(..), defaultLiftBaseWith, defaultLiftWith, defaultRestoreM, defaultRestoreT)
import Log

import DB
import DB.RowCache (GetRow, RowCacheT, rowCache, rowCacheID, runRowCacheT, runRowCacheTID, updateRow, updateRowWithID)
import Doc.Class
import Doc.Data.Document
import Doc.DocumentID (DocumentID)
import Doc.Logging
import FileStorage.Class

-- | A monad transformer that has a 'DocumentMonad' instance
newtype DocumentT m a = DocumentT { unDocumentT :: RowCacheT Document m a }
  deriving (Applicative, Monad, MonadDB, Functor, MonadIO, MonadTrans, MonadBase b, MonadThrow, MonadCatch, MonadMask, MonadFileStorage)

instance MonadBaseControl b m => MonadBaseControl b (DocumentT m) where
  type StM (DocumentT m) a = ComposeSt DocumentT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl DocumentT where
  type StT DocumentT a = StT (RowCacheT Document) a
  liftWith = defaultLiftWith DocumentT unDocumentT
  restoreT = defaultRestoreT DocumentT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance {-# OVERLAPPING #-} (GetRow Document m, MonadDB m) => DocumentMonad (DocumentT m) where
  theDocument = DocumentT rowCache
  theDocumentID = DocumentT rowCacheID
  updateDocument m = DocumentT $ updateRow $ unDocumentT . m
  updateDocumentWithID m = DocumentT $ updateRowWithID $ unDocumentT . m

-- | Lock a document and perform an operation that modifies the
-- document in the database, given the document
withDocument :: (MonadDB m, MonadLog m, GetRow Document m) => Document -> DocumentT m a -> m a
withDocument d = runRowCacheT d . logDocument (documentid d) . unDocumentT . (lockDocument >>)

-- | Lock a document and perform an operation that modifies the
-- document in the database, given the document ID
withDocumentID :: (MonadDB m, MonadLog m, GetRow Document m) => DocumentID -> DocumentT m a -> m a
withDocumentID d = runRowCacheTID d . logDocument d . unDocumentT . (lockDocument >>)

-- | Lock a document and perform an operation that modifies the
-- document in the database, given an operation that obtains the
-- document
withDocumentM :: (MonadDB m, MonadLog m, GetRow Document m) => m Document -> DocumentT m a -> m a
withDocumentM dm action = do
  d <- dm
  runRowCacheT d . logDocument (documentid d) . unDocumentT $ do
    lockDocument
    action

-- | Lock a document so that other transactions that attempt to lock or update the document will wait until the current transaction is done.
lockDocument :: DocumentMonad m => m ()
lockDocument = do
  did <- theDocumentID
  runQuery_ $ "SELECT TRUE FROM documents WHERE id =" <?> did <+> "FOR UPDATE"
