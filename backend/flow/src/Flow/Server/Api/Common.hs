module Flow.Server.Api.Common
  ( selectTemplate
  ) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Extra
import Database.PostgreSQL.PQTypes
import Servant.Server

import Flow.Error
import Flow.Id
import Flow.Model.Types
import qualified Flow.Model as Model

selectTemplate
  :: (MonadDB m, MonadError ServerError m, MonadThrow m) => TemplateId -> m Template
selectTemplate = fromMaybeM throwTemplateNotFoundError . Model.selectMaybeTemplate
