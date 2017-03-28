module Analytics.Include(
    AnalyticsData
  , getAnalyticsData
  , analyticsTemplates
)

where

import Text.JSON
import Text.JSON.Gen
import Text.StringTemplates.Templates
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F

import Company.Model
import DB
import HubSpot.Conf
import Kontra
import KontraPrelude
import MinutesTime
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Utils.Monoid
import Utils.String

data AnalyticsData = AnalyticsData { aUser           :: Maybe User
                                   , aCompany        :: Maybe Company
                                   , aToken          :: Maybe String
                                   , aHubSpotConf    :: Maybe HubSpotConf
                                   , aLanguage       :: Lang
                                   }

getAnalyticsData :: Kontrakcja m => m AnalyticsData
getAnalyticsData = do
  muser <- ctxmaybeuser <$> getContext
  mcompany <- case muser of
    Just user -> dbQuery $ GetCompany $ usercompany user
    _ -> return Nothing
  token <- ctxmixpaneltoken <$> getContext
  hubspotConf <- ctxhubspotconf <$> getContext
  lang <- ctxlang <$> getContext


  return $ AnalyticsData { aUser         = muser
                         , aCompany      = mcompany
                         , aToken        = token
                         , aHubSpotConf  = hubspotConf
                         , aLanguage     = lang
                         }

mnop :: Monad m => (a -> m ()) -> Maybe a -> m ()
mnop f m = maybe (return ()) f m

analyticsTemplates :: Monad m => AnalyticsData -> Fields m ()
analyticsTemplates ad = do
  mnop (F.value "userid" . show . userid) $ aUser ad
  F.value "token" $ aToken ad
  F.value "hubspotConf" $ encode $ toJSValue $ aHubSpotConf ad
  F.value "properties" $ encode $ toJSValue ad

instance ToJSValue AnalyticsData where
  toJSValue AnalyticsData{..} = runJSONGen $ do
    mnop (J.value "$email") $ escapeString <$> getEmail <$> aUser
    mnop (J.value "userid") $ show <$> userid <$> aUser

    mnop (J.value "TOS Date" . formatTimeISO) $ join $ userhasacceptedtermsofservice <$> aUser
    mnop (J.value "Full Name") $ emptyToNothing $ escapeString <$> getFullName <$> aUser
    mnop (J.value "Smart Name") $ emptyToNothing $ escapeString <$> getSmartName <$> aUser
    mnop (J.value "$first_name") $ emptyToNothing $ escapeString <$> getFirstName <$> aUser
    mnop (J.value "$last_name") $ emptyToNothing $ escapeString <$> getLastName <$> aUser
    mnop (J.value "$username") $ escapeString <$> getEmail <$> aUser
    mnop (J.value "Phone") $ emptyToNothing $ escapeString <$> (userphone . userinfo) <$> aUser
    mnop (J.value "Position") $ emptyToNothing $ escapeString <$> usercompanyposition <$> userinfo  <$> aUser

    mnop (J.value "Company Status") $ escapeString <$> (\u -> if (useriscompanyadmin u) then "admin" else "sub") <$> aUser
    mnop (J.value "Company Name") $ emptyToNothing $ escapeString <$> getCompanyName  <$> aCompany

    mnop (J.value "Signup Method") $ emptyToNothing $ escapeString <$> show <$> usersignupmethod <$> aUser

    J.value "Language" $ codeFromLang aLanguage

    -- Set these values so we can A/B/C test within mixpanel,
    -- for example with emails.
    case unUserID <$> userid <$> aUser of
      Nothing -> return ()
      Just uid -> do
        J.value "MOD 2" $ uid `mod` 2
        J.value "MOD 3" $ uid `mod` 3
        J.value "MOD 4" $ uid `mod` 4
        J.value "MOD 5" $ uid `mod` 5
        J.value "MOD 6" $ uid `mod` 6
        J.value "MOD 7" $ uid `mod` 7
