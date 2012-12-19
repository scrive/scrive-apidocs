-- | Mixpanel utilities and event processor for the third party stats system.
--   TODO: implement setting properties on Mixpanel users!
module ThirdPartyStats.Mixpanel (
  MixpanelToken,
  processMixpanelEvent) where
import Control.Monad.IO.Class
import ThirdPartyStats.Core
import Mixpanel.Event as Mixpanel
import Mixpanel.Result
import MinutesTime (toUTCTime)

-- | Token identifying us to Mixpanel.
type MixpanelToken = String

-- | Ship an event off to Mixpanel.
processMixpanelEvent :: MonadIO m
                     => MixpanelToken
                     -> EventName
                     -> [EventProperty]
                     -> m ProcRes
processMixpanelEvent _ SetUserProps _ = do
    return $ Failed $  "Attempted to set Mixpanel user property using async "
                    ++ "event, but that's not done yet!"
processMixpanelEvent token (NamedEvent name) props = do
    res <- liftIO $ Mixpanel.track token name (map mixpanelProperty props)
    case res of
      HTTPError reason     -> return (Failed reason)
      MixpanelError reason -> return (Failed reason)
      Success              -> return OK

-- | Convert a generic async event property to a Mixpanel property.
mixpanelProperty :: EventProperty -> Mixpanel.Property
mixpanelProperty (MailProp mail)     = CustomString "$email" mail
mixpanelProperty (IPProp ip)         = IP ip
mixpanelProperty (NameProp name)     = Name name
mixpanelProperty (UserIDProp uid)    = DistinctID (show uid)
mixpanelProperty (TimeProp t)        = Time (toUTCTime t)
mixpanelProperty (SomeProp name val) = mkMixpanelProperty val
    where
      mkMixpanelProperty (PVNumber n) =
        CustomNumber name n
      mkMixpanelProperty (PVString str) =
        CustomString name str
      mkMixpanelProperty (PVMinutesTime t) =
        CustomTime name (toUTCTime t)
      mkMixpanelProperty (PVBool b) =
        CustomBool name b
