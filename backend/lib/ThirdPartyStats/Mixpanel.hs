-- | Mixpanel utilities and event processor for the third party stats system.
module ThirdPartyStats.Mixpanel (
  MixpanelToken,
  processMixpanelEvent) where

import Control.Monad.IO.Class
import Mixpanel.Engage as Mixpanel (set)
import Mixpanel.Event as Mixpanel
import qualified Data.Text as T

import ThirdPartyStats.Core
import ThirdPartyStats.Utils
import User.Email

-- | Token identifying us to Mixpanel.
type MixpanelToken = Text

-- | Ship an event off to Mixpanel.
processMixpanelEvent
  :: MonadIO m => MixpanelToken -> EventName -> [EventProperty] -> m ProcRes
processMixpanelEvent token SetUserProps props
  | Just (uid, props') <- extractUID props = do
    res <- liftIO $ Mixpanel.set (T.unpack token) (show uid) (map mixpanelProperty props')
    case res of
      HTTPError     reason -> return (Failed $ T.pack reason)
      MixpanelError reason -> return (Failed $ T.pack reason)
      Success              -> return OK
  | otherwise = do
    return (Failed "Tried to set Mixpanel prop without user ID!")
processMixpanelEvent token (NamedEvent name) props = do
  let (distinctid, props') = case extractUID props of
        Just (uid, props'') -> (Just (show uid), props'')
        Nothing             -> (Nothing, props)
  res <- liftIO $ Mixpanel.track (T.unpack token)
                                 distinctid
                                 (T.unpack name)
                                 (map mixpanelProperty props')
  case res of
    HTTPError     reason -> return (Failed $ T.pack reason)
    MixpanelError reason -> return (Failed $ T.pack reason)
    Success              -> return OK
processMixpanelEvent _token SetCompanyProps _props = do
  return . Ignored $ "no handler registered"

-- | Convert a generic async event property to a Mixpanel property.
mixpanelProperty :: EventProperty -> Mixpanel.Property
mixpanelProperty (MailProp        mail) = CustomString "$email" (T.unpack $ unEmail mail)
mixpanelProperty (IPProp          ip  ) = IP (show ip)
mixpanelProperty (NameProp        name) = FullName $ T.unpack name
mixpanelProperty (TimeProp        t   ) = Time t
mixpanelProperty (UserIDProp _) = unexpectedError "user ID prop in the wrong place!"
mixpanelProperty (DocIDProp       did ) = CustomString "Document ID" (show did)
mixpanelProperty (UserGroupIDProp cid ) = CustomString "Company ID" (show cid)
mixpanelProperty (FirstNameProp   name) = FirstName $ T.unpack name
mixpanelProperty (LastNameProp    name) = LastName $ T.unpack name
mixpanelProperty (SomeProp name val   ) = mkMixpanelProperty val
  where
    mkMixpanelProperty (PVNumber  n  ) = CustomNumber (T.unpack name) n
    mkMixpanelProperty (PVString  str) = CustomString (T.unpack name) (T.unpack str)
    mkMixpanelProperty (PVUTCTime t  ) = CustomTime (T.unpack name) t
    mkMixpanelProperty (PVBool    b  ) = CustomBool (T.unpack name) b
