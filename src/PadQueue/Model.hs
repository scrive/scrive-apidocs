{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PadQueue.Model (
      AddToPadQueue(..)
    , GetPadQueue(..)
    , ClearPadQueue(..)
    , PadQueue
    ) where

import Database.HDBC
import DB.Classes
import DB.Fetcher2
import OurPrelude
import User.Model
import Doc.DocStateData
import Data.Maybe (listToMaybe, isJust)
import Data.Functor
import EvidenceLog.Model
import Util.MonadUtils

type PadQueue = Maybe (DocumentID,SignatoryLinkID)

data Actor a => AddToPadQueue a = AddToPadQueue UserID DocumentID SignatoryLinkID a
instance Actor a => DBUpdate (AddToPadQueue a) () where
  dbUpdate (AddToPadQueue uid did slid a) = do
    dbUpdate $ ClearPadQueue uid a
    kPrepare $ "INSERT INTO padqueue( user_id, document_id, signatorylink_id) VALUES(?,?,?)"
    r <- kExecute [toSql uid, toSql did, toSql slid]
    when_ (r == 1) $
                dbUpdate $ InsertEvidenceEvent
                SendToPadDevice
                ("Document send to pad device for signatory \"" ++ show slid ++ "\" by " ++ actorWho a ++ ".")
                (Just did)
                a
    return ()

data Actor a => ClearPadQueue a = ClearPadQueue UserID a
instance Actor a => DBUpdate (ClearPadQueue a) () where
  dbUpdate (ClearPadQueue uid a) = do
    pq <- dbQuery $ GetPadQueue uid
    when_ (isJust pq) $ do 
        kPrepare "DELETE FROM padqueue WHERE user_id = ?"
        r <- kExecute [toSql uid]
        when_ ((r == 1) && isJust pq ) $ do
         _<- dbUpdate $ InsertEvidenceEvent
                RemovedFromPadDevice
                ("Document removed from pad device for signatory \"" ++ show (snd $ $(fromJust) pq) ++ "\" by " ++ actorWho a ++ ".")
                (fst <$> pq)
                a
         return ()
         
data GetPadQueue = GetPadQueue UserID
instance DBQuery GetPadQueue PadQueue where
  dbQuery ( GetPadQueue uid) = do
    kPrepare $ "SELECT document_id, signatorylink_id FROM padqueue WHERE user_id = ?"
    _ <- kExecute [toSql uid]
    fetchPadQueue

fetchPadQueue :: DB PadQueue
fetchPadQueue = listToMaybe <$> foldDB decoder []
  where
    decoder acc did slid  = (did,slid) : acc
