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
import Doc.DocStateData
import Doc.Model
import Data.Maybe (listToMaybe, isJust)
import Data.Functor
import Util.Actor
import EvidenceLog.Model
import Templates.Templates
import Templates.Fields
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo

type PadQueue = Maybe (DocumentID,SignatoryLinkID)

data AddToPadQueue = AddToPadQueue UserID DocumentID SignatoryLinkID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m AddToPadQueue () where
  update (AddToPadQueue uid did slid a) = do
    mdoc <- query $ GetDocumentByDocumentID did
    let memail = getEmail <$> (mdoc >>= \d->getSigLinkFor d slid)
    update $ ClearPadQueue uid a
    kPrepare $ "INSERT INTO padqueue( user_id, document_id, signatorylink_id) VALUES(?,?,?)"
    r <- kExecute [toSql uid, toSql did, toSql slid]
    when_ (r == 1) $
                update $ InsertEvidenceEvent
                SendToPadDevice
                (value "email" memail >> value "actor" (actorWho a))
                (Just did)
                a
    return ()

data ClearPadQueue = ClearPadQueue UserID Actor
instance (MonadDB m, TemplatesMonad m) => DBUpdate m ClearPadQueue () where
  update (ClearPadQueue uid a) = do
    pq <- query $ GetPadQueue uid
    case pq of
       Nothing -> return ()
       Just (did, slid) -> do
         mdoc <- query $ GetDocumentByDocumentID did
         let memail = getEmail <$> (mdoc >>= \d->getSigLinkFor d slid)
         kPrepare "DELETE FROM padqueue WHERE user_id = ?"
         r <- kExecute [toSql uid]
         when_ ((r == 1) && isJust pq ) $ do
         _ <- update $ InsertEvidenceEvent
              RemovedFromPadDevice
              (value "email" memail >> value "actor"  (actorWho a))
              (fst <$> pq)
              a
         return ()

data GetPadQueue = GetPadQueue UserID
instance MonadDB m => DBQuery m GetPadQueue PadQueue where
  query (GetPadQueue uid) = do
    kPrepare $ "SELECT document_id, signatorylink_id FROM padqueue WHERE user_id = ?"
    _ <- kExecute [toSql uid]
    fetchPadQueue

fetchPadQueue :: MonadDB m => DBEnv m PadQueue
fetchPadQueue = listToMaybe `liftM` foldDB decoder []
  where
    decoder acc did slid  = (did, slid) : acc
