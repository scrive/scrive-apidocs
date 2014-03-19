{-# LANGUAGE NoImplicitPrelude #-}
module PadQueue.Model (
      AddToPadQueue(..)
    , GetPadQueue(..)
    , ClearPadQueue(..)
    , PadQueue
    ) where

import Control.Monad
import DB
import OurPrelude
import Utils.Monad
import User.Model
import Doc.SignatoryLinkID
import Doc.DocumentID
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID, withDocumentID)
import Doc.Model ()
import Util.Actor
import EvidenceLog.Model
import Text.StringTemplates.Templates
import Util.SignatoryLinkUtils

type PadQueue = Maybe (DocumentID, SignatoryLinkID)

data AddToPadQueue = AddToPadQueue UserID SignatoryLinkID Actor
instance (DocumentMonad m, MonadDB m, TemplatesMonad m) => DBUpdate m AddToPadQueue () where
  update (AddToPadQueue uid slid a) = do
    did <- theDocumentID
    update $ ClearPadQueue uid a
    r <- runQuery . sqlInsert "padqueue" $ do
      sqlSet "user_id" uid
      sqlSet "document_id" did
      sqlSet "signatorylink_id" slid
    when_ (r == 1) $ theDocument >>= \doc -> update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
      SendToPadDevice
      (return ())
      (getSigLinkFor slid doc)
      Nothing
      a

data ClearPadQueue = ClearPadQueue UserID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ClearPadQueue () where
  update (ClearPadQueue uid a) = do
    runQuery_ . sqlSelect "padqueue" $ do
      sqlResult "document_id"
      sqlResult "signatorylink_id"
      sqlWhereEq "user_id" uid
    pq :: PadQueue <- fetchMaybe id
    case pq of
       Nothing -> return ()
       Just (did, slid) -> withDocumentID did $ do
         r <- runQuery $ "DELETE FROM padqueue WHERE user_id =" <?> uid
         theDocument >>= \doc -> when_ (r == 1 && not (hasSigned (doc, slid))) $ do
           update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
                RemovedFromPadDevice
                (return ())
                (getSigLinkFor slid doc)
                Nothing
                a

data GetPadQueue = GetPadQueue UserID
instance MonadDB m => DBQuery m GetPadQueue PadQueue where
  query (GetPadQueue uid) = do
    runQuery_ . sqlSelect "padqueue" $ do
      sqlResult "document_id"
      sqlResult "signatorylink_id"
      sqlWhereEq "user_id" uid
      sqlWhereExists $ sqlSelect "signatory_links" $ do
        sqlResult "1"
        sqlWhereEqSql "signatory_links.document_id" "padqueue.document_id"
        sqlWhereEq "signatory_links.user_id" uid
        sqlWhereIsNULL "signatory_links.deleted"
    fetchMaybe id
