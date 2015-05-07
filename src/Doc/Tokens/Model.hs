module Doc.Tokens.Model (
    GetDocumentSessionToken(..)
  , AddDocumentSessionToken(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Happstack.Server (ServerMonad)

import Context
import Crypto.RNG
import DB
import Doc.SignatoryLinkID
import KontraMonad
import KontraPrelude
import MagicHash
import Session.Model

data GetDocumentSessionToken = GetDocumentSessionToken SignatoryLinkID
instance (KontraMonad m, MonadDB m, MonadThrow m) => DBQuery m GetDocumentSessionToken (Maybe MagicHash) where
  query (GetDocumentSessionToken slid) = do
    sid <- ctxsessionid `liftM` getContext
    runQuery_ . sqlSelect "document_session_tokens" $ do
      sqlResult "token"
      sqlWhereEq "session_id" sid
      sqlWhereEq "signatory_link_id" slid
    fetchMaybe runIdentity

data AddDocumentSessionToken = AddDocumentSessionToken SignatoryLinkID MagicHash
instance (ServerMonad m, CryptoRNG m, KontraMonad m, MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m AddDocumentSessionToken () where
  update (AddDocumentSessionToken slid token) = do
    sid <- getNonTempSessionID
    runQuery_ $ rawSQL "SELECT insert_document_session_token($1, $2, $3)" (sid, slid, token)
