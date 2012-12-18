-- | Mixpanel utilities and event processor for the third party stats system.
module ThirdPartyStats.Mixpanel where
import ThirdPartyStats.Core
import Mixpanel.Event as Mixpanel
import Data.Time

-- | Convert a generic async event property to a Mixpanel property.
mixpanelProperty :: EventProperty -> Mixpanel.Property
mixpanelProperty (MailProp mail)     = CustomString "$email" mail
mixpanelProperty (IPProp ip)         = IP ip
mixpanelProperty (NameProp name)     = Name name
mixpanelProperty (UserIDProp uid)    = DistinctID (show uid)
mixpanelProperty (TimeProp t)        = Time (minutesTimeToUTCTime t)
mixpanelProperty (SomeProp name val) = mkMixpanelProperty val
    where
      mkMixpanelProperty (PVNumber n) =
        CustomNumber name n
      mkMixpanelProperty (PVString str) =
        CustomString name str
      mkMixpanelProperty (PVMinutesTime t) =
        CustomTime name (minutesTimeToUTCTime t)
      mkMixpanelProperty (PVBool b) =
        CustomBool name b

minutesTimeToUTCTime :: a -> UTCTime
minutesTimeToUTCTime = error "Need to write minutesTimeToUTCTime!"
