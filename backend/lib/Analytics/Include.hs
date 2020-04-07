module Analytics.Include
  ( AnalyticsData
  , getAnalyticsData
  , analyticsTemplates
  ) where

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

analyticsTemplates :: Monad m => AnalyticsData -> Fields m ()
analyticsTemplates ad = do
  forM_ (aUser ad) $ F.value "userid" . show . view #id
  F.value "token" $ aToken ad
  F.value "gacode" $ aGACode ad
  F.value "hubspotConf" . encode $ toJSValue (aHubSpotConf ad)
  F.value "properties" . encode $ toJSValue ad

instance ToJSValue AnalyticsData where
  toJSValue AnalyticsData {..} = runJSONGen $ do
    forM_ aUser $ \user -> do
      J.value "$email" . escapeString $ getEmail user
      J.value "userid" . show $ user ^. #id

      forM_ (user ^. #hasAcceptedTOS) $ J.value "TOS Date" . formatTimeISO
      J.value "Full Name" . emptyToNothing . escapeString $ getFullName user
      J.value "Smart Name" . emptyToNothing . escapeString $ getSmartName user
      J.value "$first_name" . emptyToNothing . escapeString $ user ^. #info % #firstName
      J.value "$last_name" . emptyToNothing . escapeString $ getLastName user
      J.value "$username" . escapeString $ getEmail user
      J.value "Phone" . emptyToNothing . escapeString $ user ^. #info % #phone
      J.value "Position" . emptyToNothing $ escapeString
        (user ^. #info % #companyPosition)

      J.value "Company Status" . escapeString $ if user ^. #isCompanyAdmin
        then "admin"
        else "sub"
      forM_ aUserGroup $ J.value "Company Name" . emptyToNothing . escapeString . view
        #name

      J.value "Signup Method" . emptyToNothing . escapeString $ showt
        (user ^. #signupMethod)

    J.value "Language" $ codeFromLang aLanguage

    -- Set these values so we can A/B/C test within mixpanel,
    -- for example with emails.
    case unUserID . view #id <$> aUser of
      Nothing  -> return ()
      Just uid -> do
        J.value "MOD 2" $ uid `mod` 2
        J.value "MOD 3" $ uid `mod` 3
        J.value "MOD 4" $ uid `mod` 4
        J.value "MOD 5" $ uid `mod` 5
        J.value "MOD 6" $ uid `mod` 6
        J.value "MOD 7" $ uid `mod` 7
