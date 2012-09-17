module Doc.Tickets.Model (
    GetDocumentTicket(..)
  , AddDocumentTicket(..)
  ) where

import Control.Monad
import Control.Monad.Trans

import Context
import Crypto.RNG
import DB
import Doc.SignatoryLinkID
import KontraMonad
import MagicHash
import Session.Model

data GetDocumentTicket = GetDocumentTicket SignatoryLinkID
instance (KontraMonad m, MonadDB m) => DBQuery m GetDocumentTicket (Maybe MagicHash) where
  query (GetDocumentTicket slid) = do
    sid <- ctxsessionid `liftM` lift getContext
    getOne $ SQL ("SELECT token FROM document_tickets"
      ++ " WHERE session_id = ? AND signatory_link_id = ?") [
        toSql sid
      , toSql slid
      ]

data AddDocumentTicket = AddDocumentTicket SignatoryLinkID MagicHash
instance (CryptoRNG m, KontraMonad m, MonadDB m) => DBUpdate m AddDocumentTicket () where
  update (AddDocumentTicket slid token) = do
    sid <- getNonTempSessionID
    kRun_ $ SQL "SELECT insert_document_ticket(?, ?, ?)" [
        toSql sid
      , toSql slid
      , toSql token
      ]
