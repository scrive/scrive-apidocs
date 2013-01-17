module Analytics.Include 

where

import Templates.Templates
import qualified Templates.Fields as F

import Text.JSON
import Text.JSON.Gen
import qualified Text.JSON.Gen as J

import Kontra

import User.Model

import Company.Model

import Control.Monad
import Control.Applicative

import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
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
  
-- | A safe version of !!
(??) :: [a] -> Int -> Maybe a
(??) l i = if i < length l then Just (l !! i) else Nothing

-- | Empty strings become Nothing
maybeS :: String -> Maybe String
maybeS "" = Nothing
maybeS s  = Just s

instance ToJSValue AnalyticsData where
  toJSValue AnalyticsData{..} = runJSONGen $ do
    mnop (J.value "$email") $ getEmail <$> aUser
    mnop (J.value "userid") $ show <$> userid <$> aUser
    
    mnop (J.value "Signup Method") $ show <$> usersignupmethod <$> aUser
    mnop (J.value "TOS Date" . formatMinutesTimeRealISO) $ join $ userhasacceptedtermsofservice <$> aUser
    mnop (J.value "Full Name") $ join $ maybeS <$> getFullName <$> aUser
    mnop (J.value "Smart Name") $ join $ maybeS <$> getSmartName <$> aUser
    mnop (J.value "$first_name") $ join $ maybeS <$> getFirstName <$> aUser
    mnop (J.value "$last_name") $ join $ maybeS <$> getLastName <$> aUser
    mnop (J.value "$username") $ getEmail <$> aUser
    mnop (J.value "Phone") $ join $ maybeS <$> (userphone . userinfo) <$> aUser    
    mnop (J.value "Position") $ join $ maybeS <$> usercompanyposition <$> userinfo  <$> aUser
    
    mnop (J.value "Company Status") $ companyStatus <$> aUser
    mnop (J.value "Company Name") $ join $ maybeS <$> (\u -> getCompanyName (u, aCompany)) <$> aUser

    J.value "Payment Plan" $ maybe "free" show $ ppPricePlan <$> aPaymentPlan
    J.value "Language" $ codeFromLang aLanguage
    J.value "Docs sent" aDocsSent
