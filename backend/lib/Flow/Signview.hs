{-# LANGUAGE StrictData #-}
module Flow.Signview where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Maybe
import Database.PostgreSQL.PQTypes

import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import Flow.Server.Utils (mkInstanceOverviewUrl)
import qualified Flow.Model as Model

linkToInstanceOverview
  :: (MonadDB m, MonadThrow m) => DocumentID -> SignatoryLinkID -> m (Maybe Text)
linkToInstanceOverview did slid = runMaybeT $ do
  instanceId <- MaybeT $ Model.selectInstanceIdByDocumentId did
  userName   <- MaybeT $ Model.selectUserNameFromKV instanceId slid
  pure $ mkInstanceOverviewUrl instanceId userName

