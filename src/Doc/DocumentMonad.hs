-- | Monad for manipulating documents
module Doc.DocumentMonad
  ( module Doc.Class
  , DocumentT(..)
  , withDocument
  , withDocumentID
  , withDocumentM
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (MonadIO, MonadTrans)
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..), ComposeSt, defaultLiftBaseWith, defaultRestoreM, defaultLiftWith, defaultRestoreT)
import Text.JSON
import Text.JSON.Gen

import DB
import DB.RowCache (RowCacheT, GetRow, runRowCacheT, runRowCacheTID, runRowCacheTM, rowCache, rowCacheID, updateRow, updateRowWithID)
import Doc.Class
import Doc.DocStateData (Document)
import Doc.DocumentID (DocumentID)
import KontraPrelude
import Log (MonadLog(..))

-- | A monad transformer that has a 'DocumentMonad' instance
newtype DocumentT m a = DocumentT { unDocumentT :: RowCacheT Document m a }
  deriving (Applicative, Monad, MonadDB, Functor, MonadIO, MonadTrans, MonadBase b)

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

instance (GetRow Document m, MonadDB m) => DocumentMonad (DocumentT m) where
  theDocument = DocumentT rowCache
  theDocumentID = DocumentT rowCacheID
  updateDocument m = DocumentT $ updateRow $ unDocumentT . m
  updateDocumentWithID m = DocumentT $ updateRowWithID $ unDocumentT . m

instance MonadLog m => MonadLog (DocumentT m) where
  mixlogjs time title js = do
    case toJSValue js of
      JSObject jso -> DocumentT $ do
        did <- rowCacheID
        mixlogjs time title (JSObject (toJSObject (("documentid",toJSValue (show did)) : fromJSObject jso)))
      jsx -> DocumentT $ mixlogjs time title jsx

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
lockDocument = do
  did <- theDocumentID
  runQuery_ $ "SELECT TRUE FROM documents WHERE id =" <?> did <+> "FOR UPDATE"
