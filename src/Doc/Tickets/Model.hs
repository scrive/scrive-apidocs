module Doc.Tickets.Model (
    getDocumentTicket
  , addDocumentTicket
  ) where

import Control.Monad

import Context
import Crypto.RNG
import DB
import Doc.SignatoryLinkID
import KontraMonad
import MagicHash
import Session.Data
import Session.Model

getDocumentTicket :: (KontraMonad m, MonadDB m)
                  => SignatoryLinkID -> m (Maybe MagicHash)
getDocumentTicket slid = do
  sid <- ctxsessionid `liftM` getContext
  runDBEnv . getOne $ SQL ("SELECT token FROM document_tickets"
    ++ " WHERE session_id = ? AND signatory_link_id = ?") [
      toSql sid
    , toSql slid
    ]

addDocumentTicket :: (CryptoRNG m, KontraMonad m, MonadDB m)
                  => SignatoryLinkID -> MagicHash -> m ()
addDocumentTicket slid token = do
  sid <- do
    sid <- ctxsessionid `liftM` getContext
    if sid == tempSessionID
      then do
        new_sid <- insertEmptySession
        modifyContext $ \ctx -> ctx { ctxsessionid = new_sid }
        return new_sid
      else return sid
  runDBEnv . kRun_ $ SQL "SELECT insert_document_ticket(?, ?, ?)" [
      toSql sid
    , toSql slid
    , toSql token
    ]
