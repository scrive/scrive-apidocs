{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PadQueue.Model (
      AddToPadQueue(..)
    , GetPadQueue(..)
    , PadQueue
    ) where

import Database.HDBC
import DB.Classes
import DB.Fetcher2
import OurPrelude
import User.Model
import Doc.DocStateData
import Data.Maybe
import Data.Functor

type PadQueue = Maybe (DocumentID,SignatoryLinkID)

data AddToPadQueue = AddToPadQueue UserID DocumentID SignatoryLinkID
instance DBUpdate AddToPadQueue () where
  dbUpdate (AddToPadQueue uid did slid) = do
    kPrepare "DELETE FROM padqueue WHERE user_id = ?"
    _ <- kExecute01 [toSql uid]
    kPrepare $ "INSERT INTO padqueue( user_id, document_id, signatorylink_id) VALUES(?,?,?)"
    _ <- kExecute [toSql uid, toSql did, toSql slid]
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
