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
import User.Model
import Doc.DocStateData
import Data.Maybe (listToMaybe, isJust)
import Data.Functor
import Util.Actor
import EvidenceLog.Model
import Util.MonadUtils

type PadQueue = Maybe (DocumentID,SignatoryLinkID)

data AddToPadQueue = AddToPadQueue UserID DocumentID SignatoryLinkID Actor
instance MonadDB m => DBUpdate m AddToPadQueue () where
  update (AddToPadQueue uid did slid a) = do
    update $ ClearPadQueue uid a
    kPrepare $ "INSERT INTO padqueue( user_id, document_id, signatorylink_id) VALUES(?,?,?)"
    r <- kExecute [toSql uid, toSql did, toSql slid]
    when_ (r == 1) $
                update $ InsertEvidenceEvent
                SendToPadDevice
                ("Document send to pad device for signatory \"" ++ show slid ++ "\" by " ++ actorWho a ++ ".")
                (Just did)
                a
    return ()

data ClearPadQueue = ClearPadQueue UserID Actor
instance MonadDB m => DBUpdate m ClearPadQueue () where
  update (ClearPadQueue uid a) = do
    pq <- query $ GetPadQueue uid
    when_ (isJust pq) $ do 
        kPrepare "DELETE FROM padqueue WHERE user_id = ?"
        r <- kExecute [toSql uid]
        when_ ((r == 1) && isJust pq ) $ do
         _ <- update $ InsertEvidenceEvent
                RemovedFromPadDevice
                ("Document removed from pad device for signatory \"" ++ show (snd $ $(fromJust) pq) ++ "\" by " ++ actorWho a ++ ".")
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
