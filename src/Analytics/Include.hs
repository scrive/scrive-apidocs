module Analytics.Include(
    AnalyticsData
  , getAnalyticsData
  , analyticsTemplates
)

where

import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import Text.JSON
import Text.JSON.Gen
import qualified Text.JSON.Gen as J

import Data.Maybe

import Kontra

import User.Model

import Company.Model

import Control.Monad
import Control.Applicative

import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import Utils.String

import MinutesTime

import DB

import Payments.Model
import Doc.Model

data AnalyticsData = AnalyticsData { aUser           :: Maybe User
                                   , aCompany        :: Maybe Company
                                   , aToken          :: String
                                   , aPaymentPlan    :: Maybe PaymentPlan
                                   , aLanguage       :: Lang
                                   , aDocsSent       :: Int
                                   }

getAnalyticsData :: Kontrakcja m => m AnalyticsData
getAnalyticsData = do
  muser <- ctxmaybeuser <$> getContext
  mcompany <- case muser of
    Just User{usercompany = Just cid} -> dbQuery $ GetCompany cid
    _ -> return Nothing
  token <- ctxmixpaneltoken <$> getContext
  mplan <- case muser of
    Just User{usercompany = Just cid } -> dbQuery $ GetPaymentPlan (Right cid)
    Just User{userid} -> dbQuery $ GetPaymentPlan (Left userid)
    Nothing -> return Nothing
  lang <- ctxlang <$> getContext
  docssent <- case muser of
    Just User{userid} -> dbQuery $ GetDocsSent userid
    Nothing -> return 0

  return $ AnalyticsData { aUser         = muser
                         , aCompany      = mcompany
                         , aToken        = token
                         , aPaymentPlan  = mplan
                         , aLanguage     = lang
                         , aDocsSent     = docssent
                         }

mnop :: Monad m => (a -> m ()) -> Maybe a -> m ()
mnop f m = maybe (return ()) f m

analyticsTemplates :: Monad m => AnalyticsData -> Fields m ()
analyticsTemplates ad = do
  mnop (F.value "userid" . show . userid) $ aUser ad
  F.value "token" $ aToken ad
  F.value "properties" $ encode $ toJSValue ad

companyStatus :: User -> String
companyStatus u = case (useriscompanyadmin u, usercompany u) of
  (True, _)        -> "admin"
  (False, Nothing) -> "solo"
  (False, Just _ ) -> "sub"

instance ToJSValue AnalyticsData where
  toJSValue AnalyticsData{..} = runJSONGen $ do
    mnop (J.value "$email") $ escapeString <$> getEmail <$> aUser
    mnop (J.value "userid") $ show <$> userid <$> aUser

    mnop (J.value "Signup Method") $ show <$> usersignupmethod <$> aUser
    mnop (J.value "TOS Date" . formatMinutesTimeRealISO) $ join $ userhasacceptedtermsofservice <$> aUser
    mnop (J.value "Full Name") $ maybeS $ escapeString <$> getFullName <$> aUser
    mnop (J.value "Smart Name") $ maybeS $ escapeString <$> getSmartName <$> aUser
    mnop (J.value "$first_name") $ maybeS $ escapeString <$> getFirstName <$> aUser
    mnop (J.value "$last_name") $ maybeS $ escapeString <$> getLastName <$> aUser
    mnop (J.value "$username") $ escapeString <$> getEmail <$> aUser
    mnop (J.value "Phone") $ maybeS $ escapeString <$> (userphone . userinfo) <$> aUser
    mnop (J.value "Position") $ maybeS $ escapeString <$> usercompanyposition <$> userinfo  <$> aUser

    mnop (J.value "Company Status") $ escapeString <$> companyStatus <$> aUser
    mnop (J.value "Company Name") $ maybeS $ escapeString <$> (\u -> getCompanyName (u, aCompany)) <$> aUser
    mnop (J.value "Company Branding") $ isJust <$> companysignviewlogo <$> companyui <$> aCompany

    mnop (J.value "Signup Method") $ maybeS $ escapeString <$> show <$> usersignupmethod <$> aUser

    J.value "Payment Plan" $ maybe "free" show $ ppPricePlan <$> aPaymentPlan
    J.value "Language" $ codeFromLang aLanguage
    J.value "Docs sent" aDocsSent

    case unUserID <$> userid <$> aUser of
      Nothing -> return ()
      Just uid -> do
        J.value "MOD 2" $ uid `mod` 2
        J.value "MOD 3" $ uid `mod` 3
        J.value "MOD 4" $ uid `mod` 4
        J.value "MOD 5" $ uid `mod` 5
        J.value "MOD 6" $ uid `mod` 6
        J.value "MOD 7" $ uid `mod` 7
