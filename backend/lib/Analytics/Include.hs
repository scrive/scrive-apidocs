module Analytics.Include
  ( AnalyticsData
  , getAnalyticsData
  , analyticsTemplates
  ) where

import Optics (to)
import Text.JSON
import Text.JSON.Gen
import Text.StringTemplates.Templates
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F

import DB
import HubSpot.Conf
import Kontra
import MinutesTime
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.HasSomeUserInfo
import Utils.Monoid
import Utils.String

data AnalyticsData = AnalyticsData { aUser           :: Maybe User
                                   , aUserGroup      :: Maybe UserGroup
                                   , aToken          :: Maybe Text
                                   , aHubSpotConf    :: Maybe HubSpotConf
                                   , aGACode         :: Maybe Text
                                   , aLanguage       :: Lang
                                   }

getAnalyticsData :: Kontrakcja m => m AnalyticsData
getAnalyticsData = do
  ctx <- getContext

  let muser       = ctx ^. #maybeUser
      token       = ctx ^. #mixpanelToken
      hubspotConf = ctx ^. #hubspotConf
      gaToken     = ctx ^. #gaToken
      lang        = ctx ^. #lang

  musergroup <- case muser of
    Just user -> dbQuery . UserGroupGet $ user ^. #groupID
    Nothing   -> return Nothing

  return $ AnalyticsData { aUser        = muser
                         , aUserGroup   = musergroup
                         , aToken       = token
                         , aHubSpotConf = hubspotConf
                         , aGACode      = gaToken
                         , aLanguage    = lang
                         }

mnop :: Monad m => (a -> m ()) -> Maybe a -> m ()
mnop f m = maybe (return ()) f m

analyticsTemplates :: Monad m => AnalyticsData -> Fields m ()
analyticsTemplates ad = do
  mnop (F.value "userid" . show . view #id) $ aUser ad
  F.value "token" $ aToken ad
  F.value "gacode" $ aGACode ad
  F.value "hubspotConf" $ encode $ toJSValue $ aHubSpotConf ad
  F.value "properties" $ encode $ toJSValue ad

instance ToJSValue AnalyticsData where
  toJSValue AnalyticsData {..} = runJSONGen $ do
    mnop (J.value "$email") $ escapeString <$> getEmail <$> aUser
    mnop (J.value "userid") $ show <$> (aUser ^? _Just % #id)

    mnop (J.value "TOS Date" . formatTimeISO) (aUser ^? _Just % #hasAcceptedTOS % _Just)
    mnop (J.value "Full Name") $ emptyToNothing $ escapeString <$> getFullName <$> aUser
    mnop (J.value "Smart Name") $ emptyToNothing $ escapeString <$> getSmartName <$> aUser
    mnop (J.value "$first_name")
      $   emptyToNothing
      $   escapeString
      <$> (aUser ^? _Just % #info % #firstName)
    mnop (J.value "$last_name") $ emptyToNothing $ escapeString <$> getLastName <$> aUser
    mnop (J.value "$username") $ escapeString <$> getEmail <$> aUser
    mnop (J.value "Phone")
      $   emptyToNothing
      $   escapeString
      <$> (aUser ^? _Just % #info % #phone)
    mnop (J.value "Position")
      $   emptyToNothing
      $   escapeString
      <$> (aUser ^? _Just % #info % #companyPosition)

    mnop (J.value "Company Status")
      $   escapeString
      <$> (aUser ^? _Just % to (\u -> if u ^. #isCompanyAdmin then "admin" else "sub"))
    mnop (J.value "Company Name")
      $   emptyToNothing
      $   escapeString
      <$> (aUserGroup ^? _Just % #name)

    mnop (J.value "Signup Method")
      $   emptyToNothing
      $   escapeString
      <$> (aUser ^? _Just % #signupMethod % to showt)

    J.value "Language" $ codeFromLang aLanguage

    -- Set these values so we can A/B/C test within mixpanel,
    -- for example with emails.
    case unUserID <$> aUser ^? _Just % #id of
      Nothing  -> return ()
      Just uid -> do
        J.value "MOD 2" $ uid `mod` 2
        J.value "MOD 3" $ uid `mod` 3
        J.value "MOD 4" $ uid `mod` 4
        J.value "MOD 5" $ uid `mod` 5
        J.value "MOD 6" $ uid `mod` 6
        J.value "MOD 7" $ uid `mod` 7
