-- | Mixpanel utilities and event processor for the third party stats system.
--   TODO: implement setting properties on Mixpanel users!
module ThirdPartyStats.Mixpanel (
  MixpanelToken,
  processMixpanelEvent) where

import Data.Maybe
import User.Model

import ThirdPartyStats.Core
import Mixpanel.Event as Mixpanel
import Mixpanel.Engage as Mixpanel
import MinutesTime (toUTCTime)

-- | Token identifying us to Mixpanel.
type MixpanelToken = String

-- | Ship an event off to Mixpanel.
processMixpanelEvent :: MixpanelToken
                     -> EventName
                     -> [EventProperty]
                     -> IO ProcRes
processMixpanelEvent token SetUserProps props = do
  case getUserID props of
    Nothing -> return $ Failed $ "UserID property must be set to use Mixpanel"
               ++ " to engage users."
    Just uid -> do
      res <- Mixpanel.set token (show uid) (catMaybes $ map mixpanelProperty props)
      case res of
        HTTPError reason     -> return (Failed reason)
        MixpanelError reason -> return (Failed reason)
        Success              -> return OK
processMixpanelEvent token (NamedEvent name) props =
  case getUserID props of
    Nothing -> return $ Failed $ "UserID property must be set to use Mixpanel"
               ++ " to track events."
    Just uid -> do
      res <- Mixpanel.track token (show uid) name (catMaybes $ map mixpanelProperty props)
      case res of
        HTTPError reason     -> return (Failed reason)
        MixpanelError reason -> return (Failed reason)
        Success              -> return OK

-- | Convert a generic async event property to a Mixpanel property.
mixpanelProperty :: EventProperty -> Maybe Mixpanel.Property
mixpanelProperty (MailProp mail)     = Just $ CustomString "$email" mail
mixpanelProperty (IPProp ip)         = Just $ IP ip
mixpanelProperty (NameProp name)     = Just $ FullName name
mixpanelProperty (TimeProp t)        = Just $ Time (toUTCTime t)
mixpanelProperty (SomeProp name val) = Just $ mkMixpanelProperty val
    where
      mkMixpanelProperty (PVNumber n) =
        CustomNumber name n
      mkMixpanelProperty (PVString str) =
        CustomString name str
      mkMixpanelProperty (PVMinutesTime t) =
        CustomTime name (toUTCTime t)
      mkMixpanelProperty (PVBool b) =
        CustomBool name b
mixpanelProperty _ = Nothing -- skip UserIDProp

getUserID :: [EventProperty] -> Maybe UserID
getUserID []                 = Nothing
getUserID (UserIDProp uid:_) = Just uid
getUserID (_:xs)             = getUserID xs