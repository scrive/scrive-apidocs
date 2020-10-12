module ThirdPartyStats.Planhat
    ( processPlanhatEvent
    , simplePlanhatAction
    ) where

import Control.Monad.IO.Class
import Data.Time.Clock (UTCTime)
import Network.HTTP.Conduit (Manager)

import Planhat
import ThirdPartyStats.Core
import User.Email (Email(..))
import User.Model
import Util.HasSomeUserInfo (getEmail)

-- Stop sending data to Planhat:
processPlanhatEvent
  :: (MonadIO m) => Manager -> PlanhatConf -> EventName -> [EventProperty] -> m ProcRes
processPlanhatEvent _reqManager _phConf _ _props = return . Ignored $ "Planhat is dead"

-- | Send in a user action to Planhat.
simplePlanhatAction :: Text -> User -> UTCTime -> [EventProperty]
simplePlanhatAction actionTag author time =
  [ MailProp . Email $ getEmail author
  , stringProp "action" actionTag
  , UserIDProp $ author ^. #id
  , TimeProp time
  ]
