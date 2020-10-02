module Flow.Utils
  ( findFirstSignatoryLink
  ) where

import Control.Monad.Catch

import DB
import Doc.DocumentID
import Doc.SignatoryLinkID
import Flow.Id
import Flow.Model as Model
import Flow.Names

findFirstSignatoryLink
  :: (MonadDB m, MonadThrow m)
  => InstanceId
  -> UserName
  -> m (DocumentID, SignatoryLinkID)
findFirstSignatoryLink instanceId userName = do
  signatoryInfo <- find (\(userName', _, _) -> userName' == userName)
    <$> Model.selectSignatoryInfo instanceId
  case signatoryInfo of
    Just (_, slid, did) -> pure (did, slid)
    Nothing             -> unexpectedError "signatory info not found!"
