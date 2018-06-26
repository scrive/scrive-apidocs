module ThirdPartyStats.Planhat
    ( processPlanhatEvent
    , simplePlanhatAction
    ) where

import Control.Monad.IO.Class
import Data.Aeson ((.=))
import Data.Char (toLower)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client (httpLbs)
import Network.HTTP.Conduit (Manager)
import Network.HTTP.Simple (getResponseBody)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as BSL

import Planhat
import ThirdPartyStats.Core
import User.Email (Email(..))
import User.Model
import Util.HasSomeUserInfo (getEmail)

-- | Send data to Planhat:
-- * metrics for companies
-- * actions for users; at least email and action are needed
--
-- Caveat: Uses @httpLbs@ which actually reads the response into memory
-- strictly, but is chunked for memory efficiency.
processPlanhatEvent :: (MonadIO m)
                    => Manager
                    -> PlanhatConf
                    -> EventName
                    -> [EventProperty]
                    -> m ProcRes
processPlanhatEvent reqManager phConf SetUserProps props
    | not . hasMailProp $ props = do
        return . Failed $ "no mail property present"
    | otherwise = do
        let reqJSON = JSON.object . catMaybes $ map toPlanhatProperty props
            req = mkPlanhatRequest phConf phActionURL reqJSON
        res <- liftIO $ httpLbs req reqManager
        handlePlanhatResponse . getResponseBody $ res
  where
    hasMailProp :: [EventProperty] -> Bool
    hasMailProp [] = False
    hasMailProp ((MailProp email):_)
            | (unEmail email == "") = False
            | otherwise = True
    hasMailProp (_:props') = hasMailProp props'

processPlanhatEvent reqManager phConf SetCompanyProps props = do
  let reqJSON = JSON.object . catMaybes $ map toPlanhatProperty props
      req = mkPlanhatRequest phConf phMetricsURL reqJSON
  res <- liftIO $ httpLbs req reqManager
  handlePlanhatResponse . getResponseBody $ res

processPlanhatEvent _reqManager _phConf (NamedEvent _) _props = do
  return . Ignored $ "no handler registered"

-- | Send in a user action to Planhat.
simplePlanhatAction :: String -> User -> UTCTime -> [EventProperty]
simplePlanhatAction actionTag author time =
    [ MailProp . Email $ getEmail author
    , stringProp "action" actionTag
    , UserIDProp $ userid author
    , TimeProp time ]

-- Helpers

toPlanhatProperty :: EventProperty -> Maybe JSON.Pair
toPlanhatProperty (MailProp (Email mail)) =
    Just $ "email" .= mail
toPlanhatProperty (SomeProp "action" (PVString s)) =
    Just $ "action" .= s
toPlanhatProperty (SomeProp "value" (PVNumber k)) =
    Just $ "value" .= k
toPlanhatProperty (SomeProp "companyExternalId" (PVString compId)) =
    Just $ "companyExternalId" .= compId
toPlanhatProperty (SomeProp "dimensionId" (PVString dimId)) =
    Just $ "dimensionId" .= (intercalate "_" . words $ map toLower dimId)
toPlanhatProperty (UserIDProp userId) =
    Just $ "externalId" .= (show . unUserID $ userId)
toPlanhatProperty _ =
    Nothing

handlePlanhatResponse :: (MonadIO m) => BSL.ByteString -> m ProcRes
handlePlanhatResponse resBody =
  case maybeErrors resBody of
    Nothing -> return OK
    Just errObject -> return . Failed . show $ errObject
