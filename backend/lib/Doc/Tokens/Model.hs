module Doc.Tokens.Model
  ( CheckDocumentSession(..)
  , AddDocumentSession(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Crypto.RNG
import Happstack.Server (ServerMonad)

import DB
import Doc.SignatoryLinkID
import KontraMonad
import Session.SessionID

data CheckDocumentSession = CheckDocumentSession SessionID SignatoryLinkID
instance (KontraMonad m, MonadDB m, MonadThrow m) => DBQuery m CheckDocumentSession Bool where
  query (CheckDocumentSession sid slid) = do
    runQuery_ . sqlSelect "document_session_tokens" $ do
      sqlWhereEq "session_id" sid
      sqlWhereEq "signatory_link_id" slid
      sqlResult "TRUE"
    result <- fetchMaybe runIdentity
    return $ result == Just True

data AddDocumentSession = AddDocumentSession SessionID SignatoryLinkID
instance (ServerMonad m, CryptoRNG m, KontraMonad m, MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m AddDocumentSession () where
  update (AddDocumentSession sid slid) = do
    runQuery_ $ rawSQL
      "INSERT INTO document_session_tokens\
      \ (session_id, signatory_link_id) VALUES ($1, $2)\
      \ ON CONFLICT (session_id, signatory_link_id) DO NOTHING"
      (sid, slid)
