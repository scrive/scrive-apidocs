-- | Mixpanel utilities and event processor for the third party stats system.
module ThirdPartyStats.Mixpanel where
import ThirdPartyStats.Core
import Mixpanel.Event as Mixpanel
import MinutesTime (toUTCTime)

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
