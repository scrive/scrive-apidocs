-- | Mixpanel utilities and event processor for the third party stats system.
module ThirdPartyStats.Mixpanel (
  MixpanelToken,
  processMixpanelEvent) where
import Control.Monad.IO.Class
import Data.List (partition)
import ThirdPartyStats.Core
import Mixpanel.Event as Mixpanel
import Mixpanel.Engage as Mixpanel (set)
import MinutesTime (toUTCTime)
import User.UserID (UserID)
import User.Model (unEmail)

-- | Token identifying us to Mixpanel.
type MixpanelToken = String

-- | Ship an event off to Mixpanel.
processMixpanelEvent :: MonadIO m
                     => MixpanelToken
                     -> EventName
                     -> [EventProperty]
                     -> m ProcRes
processMixpanelEvent token SetUserProps props
  | Just (uid, props') <- extractUID props = do
    res <- liftIO $ Mixpanel.set token (show uid) (map mixpanelProperty props')
    case res of
      HTTPError reason     -> return (Failed reason)
      MixpanelError reason -> return (Failed reason)
      Success              -> return OK
  | otherwise = do
    return (Failed "Tried to set prop without user ID!")
processMixpanelEvent token (NamedEvent name) props
  | Just (uid, props') <- extractUID props = do
    res <- liftIO $ Mixpanel.track token (show uid) name (map mixpanelProperty props')
    case res of
      HTTPError reason     -> return (Failed reason)
      MixpanelError reason -> return (Failed reason)
      Success              -> return OK
  | otherwise = do
    return (Failed "Tried to set user prop without user ID!")


-- | Separate the user ID property from the rest, if present.
--   More than one UID is not OK either.
extractUID :: [EventProperty] -> Maybe (UserID, [EventProperty])
extractUID props =
    case partition isUIDProp props of
      ([UserIDProp uid], props') -> Just (uid, props')
      _                          -> Nothing
  where
    isUIDProp (UserIDProp _) = True
    isUIDProp _              = False

-- | Convert a generic async event property to a Mixpanel property.
mixpanelProperty :: EventProperty -> Mixpanel.Property
mixpanelProperty (MailProp mail)     = CustomString "$email" (unEmail mail)
mixpanelProperty (IPProp ip)         = IP (show ip)
mixpanelProperty (NameProp name)     = FullName name
mixpanelProperty (TimeProp t)        = Time (toUTCTime t)
mixpanelProperty (UserIDProp _)      = error "User ID prop in the wrong place!"
mixpanelProperty (DocIDProp did)     = CustomString "Document ID" (show did)
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
