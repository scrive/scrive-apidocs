module Flow.EID.EIDService.Provider.Common
  ( getSignatoriesAndDocumentsForUser
  , getTransactionForUser
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Data.Aeson
import Log

import DB
import Doc.DocInfo
import Doc.DocumentID
import Doc.Model.Query
import Doc.SignatoryLinkID
import Doc.Types.Document
import Doc.Types.SignatoryLink
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Types hiding
  ( EIDServiceTransactionFromDB(..), UnifiedRedirectUrl(..)
  )
import Flow.Aggregator
import Flow.EID.EIDService.Model
import Flow.EID.EIDService.Types
import Flow.Id
import Flow.Model.Types
import Flow.Names
import KontraMonad
import Session.Model
import qualified Flow.Model as Model

getTransactionForUser
  :: (FromJSON a, Show a, Kontrakcja m)
  => EIDServiceConf
  -> EIDServiceTransactionProvider
  -> InstanceId
  -> UserName
  -> m
       ( Maybe
           ( AuthenticationKind
           , EIDServiceTransactionFromDB
           , EIDServiceTransactionResponse a
           )
       )
getTransactionForUser conf eidProvider instanceId userName = runMaybeT $ do
  sessionID         <- getNonTempSessionID
  Just fullInstance <- Model.selectFullInstance instanceId
  let authKind = if isComplete $ instanceToAggregator fullInstance
        then AuthenticationToViewArchived
        else AuthenticationToView
  Just estDB <-
    dbQuery
    . GetEIDServiceTransactionGuardSessionID sessionID instanceId userName
    $ EIDServiceAuthToView authKind
  Just trans <- getTransactionFromEIDService conf eidProvider (estID estDB)
  logInfo_ $ "EID transaction: " <> showt trans
  pure (authKind, estDB, trans)

resolveSignatoryAndDocument
  :: (MonadDB m, MonadThrow m)
  => (SignatoryLinkID, DocumentID)
  -> m (SignatoryLink, Document)
resolveSignatoryAndDocument (slid, docid) = do
  doc <- dbQuery $ GetDocumentByDocumentID docid
  sl  <- dbQuery $ GetSignatoryLinkByID docid slid
  pure (sl, doc)

getSignatoriesAndDocumentsForUser
  :: (MonadDB m, MonadThrow m) => InstanceId -> UserName -> m [(SignatoryLink, Document)]
getSignatoriesAndDocumentsForUser instanceId userName = do
  fmap filterPendingDocuments
    .   mapM resolveSignatoryAndDocument
    .   filterUser
    =<< Model.selectSignatoryInfo instanceId
  where
    filterPendingDocuments = filter (\(_, doc) -> isPending doc)
    filterUser xs =
      [ (slid, docid) | (userName', slid, docid) <- xs, userName == userName' ]


