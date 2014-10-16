module Analytics.Include(
    AnalyticsData
  , getAnalyticsData
  , analyticsTemplates
)

where

import Control.Applicative
import Control.Monad
import Text.JSON
import Text.JSON.Gen
import Text.StringTemplates.Templates
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F

import Company.Model
import DB
import Kontra
import MinutesTime
import Payments.Model
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Utils.String

data AnalyticsData = AnalyticsData { aUser           :: Maybe User
                                   , aCompany        :: Maybe Company
                                   , aToken          :: String
                                   , aTokenGa          :: String
                                   , aPaymentPlan    :: Maybe PaymentPlan
                                   , aLanguage       :: Lang
                                   }

getAnalyticsData :: Kontrakcja m => m AnalyticsData
getAnalyticsData = do
  muser <- ctxmaybeuser <$> getContext
  mcompany <- case muser of
    Just user -> dbQuery $ GetCompany $ usercompany user
    _ -> return Nothing
  token <- ctxmixpaneltoken <$> getContext
  tokenGa <- ctxgoogleanalyticstoken <$> getContext
  mplan <- case muser of
    Just user -> dbQuery $ GetPaymentPlan (usercompany user)
    Nothing -> return Nothing
  lang <- ctxlang <$> getContext
 
  return $ AnalyticsData { aUser         = muser
                         , aCompany      = mcompany
                         , aToken        = token
                         , aTokenGa       = tokenGa
                         , aPaymentPlan  = mplan
                         , aLanguage     = lang
                         }

mnop :: Monad m => (a -> m ()) -> Maybe a -> m ()
mnop f m = maybe (return ()) f m

analyticsTemplates :: Monad m => AnalyticsData -> Fields m ()
analyticsTemplates ad = do
  mnop (F.value "userid" . show . userid) $ aUser ad
  F.value "token" $ aToken ad
  F.value "tokenGa" $ aTokenGa ad
  F.value "properties" $ encode $ toJSValue ad

instance ToJSValue AnalyticsData where
  toJSValue AnalyticsData{..} = runJSONGen $ do
    mnop (J.value "$email") $ escapeString <$> getEmail <$> aUser
    mnop (J.value "userid") $ show <$> userid <$> aUser

    mnop (J.value "Signup Method") $ show <$> usersignupmethod <$> aUser
    mnop (J.value "TOS Date" . formatTimeISO) $ join $ userhasacceptedtermsofservice <$> aUser
    mnop (J.value "Full Name") $ maybeS $ escapeString <$> getFullName <$> aUser
    mnop (J.value "Smart Name") $ maybeS $ escapeString <$> getSmartName <$> aUser
    mnop (J.value "$first_name") $ maybeS $ escapeString <$> getFirstName <$> aUser
    mnop (J.value "$last_name") $ maybeS $ escapeString <$> getLastName <$> aUser
    mnop (J.value "$username") $ escapeString <$> getEmail <$> aUser
    mnop (J.value "Phone") $ maybeS $ escapeString <$> (userphone . userinfo) <$> aUser
    mnop (J.value "Position") $ maybeS $ escapeString <$> usercompanyposition <$> userinfo  <$> aUser

    mnop (J.value "Company Status") $ escapeString <$> (\u -> if (useriscompanyadmin u) then "admin" else "sub") <$> aUser
    mnop (J.value "Company Name") $ maybeS $ escapeString <$> getCompanyName  <$> aCompany

    mnop (J.value "Signup Method") $ maybeS $ escapeString <$> show <$> usersignupmethod <$> aUser

    J.value "Payment Plan" $ maybe "free" show $ ppPricePlan <$> aPaymentPlan
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
