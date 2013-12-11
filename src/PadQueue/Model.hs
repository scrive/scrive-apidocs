{-# LANGUAGE NoImplicitPrelude #-}
module PadQueue.Model (
      AddToPadQueue(..)
    , GetPadQueue(..)
    , ClearPadQueue(..)
    , PadQueue
    ) where

import Control.Monad
import Database.HDBC
import DB
import OurPrelude
import Utils.Monad
import User.Model
import Doc.SignatoryLinkID
import Doc.DocumentID
import Doc.Model
import Data.Maybe (listToMaybe)
import Util.Actor
import EvidenceLog.Model
import Text.StringTemplates.Templates
import Util.SignatoryLinkUtils

type PadQueue = Maybe (DocumentID,SignatoryLinkID)

data AddToPadQueue = AddToPadQueue UserID DocumentID SignatoryLinkID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AddToPadQueue () where
  update (AddToPadQueue uid did slid a) = do
    doc <- query $ GetDocumentByDocumentID did
    update $ ClearPadQueue uid a
    r <- kRun $ SQL "INSERT INTO padqueue( user_id, document_id, signatorylink_id) VALUES(?,?,?)"
                [toSql uid, toSql did, toSql slid]
    when_ (r == 1) $
                update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
                SendToPadDevice
                (return ())
                (getSigLinkFor slid doc)
                Nothing
                a
                did
    return ()

data ClearPadQueue = ClearPadQueue UserID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ClearPadQueue () where
  update (ClearPadQueue uid a) = do
    kRun_ $ SQL "SELECT document_id, signatorylink_id FROM padqueue WHERE user_id = ? "
           [toSql uid]
    pq <- fetchPadQueue
    case pq of
       Nothing -> return ()
       Just (did, slid) -> do
         doc <- query $ GetDocumentByDocumentID did
         r <- kRun $ SQL "DELETE FROM padqueue WHERE user_id = ?"
                   [toSql uid]
         when_ (r == 1) $ do
           _ <- update $ InsertEvidenceEventWithAffectedSignatoryAndMsg
                RemovedFromPadDevice
                (return ())
                (getSigLinkFor slid doc)
                Nothing
                a
                did
           return ()

data GetPadQueue = GetPadQueue UserID
instance MonadDB m => DBQuery m GetPadQueue PadQueue where
  query (GetPadQueue uid) = do
    kRun_ $ SQL ("SELECT document_id, signatorylink_id FROM padqueue WHERE user_id = ? " 
                <+> "AND EXISTS (SELECT * FROM signatory_links WHERE signatory_links.document_id = padqueue.document_id " 
                <+>                                               " AND signatory_links.user_id = ? AND signatory_links.deleted IS NULL)")
            [toSql uid,toSql uid]
    fetchPadQueue

fetchPadQueue :: MonadDB m => m PadQueue
fetchPadQueue = listToMaybe `liftM` kFold decoder []
  where
    decoder acc did slid  = (did, slid) : acc
