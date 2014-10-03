-- | Mixpanel utilities and event processor for the third party stats system.
module ThirdPartyStats.Mixpanel (
  MixpanelToken,
  processMixpanelEvent) where
import Control.Monad.IO.Class
import ThirdPartyStats.Core
import ThirdPartyStats.Utils
import Mixpanel.Event as Mixpanel
import Mixpanel.Engage as Mixpanel (set)
import User.Email

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
    return (Failed "Tried to set Mixpanel prop without user ID!")
processMixpanelEvent token (NamedEvent name) props = do
    let (distinctid, props') = case extractUID props of
                                Just (uid, props'') -> (Just (show uid), props'')
                                Nothing -> (Nothing, props)
    res <- liftIO $ Mixpanel.track token distinctid name (map mixpanelProperty props')
    case res of
      HTTPError reason     -> return (Failed reason)
      MixpanelError reason -> return (Failed reason)
      Success              -> return OK
processMixpanelEvent _ (UploadDocInfo _) _ =
  -- We only do this for Precog
  return OK


-- | Convert a generic async event property to a Mixpanel property.
mixpanelProperty :: EventProperty -> Mixpanel.Property
mixpanelProperty (MailProp mail)     = CustomString "$email" (unEmail mail)
mixpanelProperty (IPProp ip)         = IP (show ip)
mixpanelProperty (NameProp name)     = FullName name
mixpanelProperty (TimeProp t)        = Time t
mixpanelProperty (UserIDProp _)      = error "User ID prop in the wrong place!"
mixpanelProperty (DocIDProp did)     = CustomString "Document ID" (show did)
mixpanelProperty (CompanyIDProp cid) = CustomString "Company ID" (show cid)
mixpanelProperty (FirstNameProp name) = FirstName name
mixpanelProperty (LastNameProp name) = LastName name
mixpanelProperty (SomeProp name val) = mkMixpanelProperty val
    where
      mkMixpanelProperty (PVNumber n) =
        CustomNumber name n
      mkMixpanelProperty (PVString str) =
        CustomString name str
      mkMixpanelProperty (PVUTCTime t) =
        CustomTime name t
      mkMixpanelProperty (PVBool b) =
        CustomBool name b
