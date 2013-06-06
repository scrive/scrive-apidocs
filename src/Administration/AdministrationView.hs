-----------------------------------------------------------------------------
-- |
-- Module      :  Administration.AdministrationView
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  develpment
-- Portability :  portable
--
-- Almoust all the stuff that is visible under /adminsonly path
--
-----------------------------------------------------------------------------
module Administration.AdministrationView(
              adminMainPage
            , adminUserPage
            , adminCompanyPage
            , adminCompanyBrandingPage
            , adminCompanyUsersPage
            , adminCompanyPaymentPage
            , adminUserUsageStatsPage
            , adminCompanyUsageStatsPage
            , adminUserPaymentPage
            , adminUserDocumentsPage
            , statisticsCompanyFields
            , statisticsFields
          ) where

import KontraLink
import Text.StringTemplates.Templates
import Data.Maybe
import User.Model
import Company.Model
import Data.List
import MinutesTime
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import Kontra
import ScriveByMail.Model
import ScriveByMail.View
import qualified Text.StringTemplates.Fields as F
import Payments.Model
import Data.Functor
import Control.Monad

{-| Main admin page - can go from here to other pages -}
adminMainPage :: TemplatesMonad m => Context -> m String
adminMainPage ctx = renderTemplate "adminsmain" $ do
    F.value "admin" $ isAdmin ctx

{- | Manage company users page - can find a company user here -}
adminCompanyUsersPage :: TemplatesMonad m => CompanyID -> m String
adminCompanyUsersPage cid =
    renderTemplate "admincompanyusers" $ do
        F.value "adminlink" $ show $ LinkAdminOnly
        F.value "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
        F.value "adminuserslink" $ show $ LinkUserAdmin Nothing
        F.value "companyid" $ show cid

{- | Manage company branding page -}
adminCompanyBrandingPage :: TemplatesMonad m => CompanyID -> m String
adminCompanyBrandingPage cid =
    renderTemplate "admincompanybranding" $ do
        F.value "adminlink" $ show $ LinkAdminOnly
        F.value "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
        F.value "companyid" $ show cid

{-| Manage user page - can change user info and settings here -}
adminUserPage :: TemplatesMonad m => User -> Maybe Company -> m String
adminUserPage user mcompany =
    renderTemplate "adminuser" $ do
        F.value "adminuserslink" $ show $ LinkUserAdmin Nothing
        F.object "user" $ userFields user
        F.object "company" $ companyFields mcompany
        F.value "adminlink" $ show $ LinkAdminOnly

adminUserPaymentPage :: TemplatesMonad m => UserID -> Maybe PaymentPlan -> Maybe CompanyID -> String -> m String
adminUserPaymentPage userid mpaymentplan mcompanyid recurlysubdomain =
  renderTemplate "adminuserpayments" $ do
    F.value "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
    F.value "adminuserslink" $ show $ LinkUserAdmin Nothing
    F.value "companyid" $ show <$> mcompanyid
    F.value "adminlink" $ show $ LinkAdminOnly
    F.value "recurlysubdomain" recurlysubdomain
    F.value "userid" $ show userid
    case mpaymentplan of
      Nothing -> do
        F.value "haspaymentplan" False
        F.value "freeplan" True
      Just paymentplan -> do
        F.value "recurlyplan" $ ppPaymentPlanProvider paymentplan == RecurlyProvider
        F.value "accountcode" $ show $ ppAccountCode paymentplan
        F.value "priceplan" $ show $ ppPricePlan paymentplan
        F.value "haspaymentplan" True
        F.value "status" $ show $ ppStatus paymentplan
        case ppPricePlan paymentplan of
          FreePricePlan       -> F.value "freeplan"       True
          TeamPricePlan       -> F.value "teamplan"       True
          FormPricePlan       -> F.value "formplan"       True
          EnterprisePricePlan -> F.value "enterpriseplan" True
          TrialTeamPricePlan  -> F.value "trialplan"      True
        case ppStatus paymentplan of
          ActiveStatus      -> F.value "activestatus"      True
          OverdueStatus     -> F.value "overduestatus"     True
          CanceledStatus    -> F.value "canceledstatus"    True
          DeactivatedStatus -> F.value "deactivatedstatus" True


adminUserDocumentsPage:: TemplatesMonad m => UserID -> Context -> m String
adminUserDocumentsPage uid ctx =
    renderTemplate "userdocuments" $ do
        F.value "userid" $ show $ uid
        F.value "admin" $ isAdmin ctx


{- | Manager company page - can change company info and settings here -}
adminCompanyPage :: TemplatesMonad m => Company -> Maybe MailAPIInfo -> m String
adminCompanyPage company mmailapiinfo =
  renderTemplate "admincompany" $ do
    F.value "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
    companyFields (Just company)
    when (isJust mmailapiinfo) $ mailAPIInfoFields (fromJust mmailapiinfo)
    F.value "hasmailapi" $ isJust mmailapiinfo
    F.value "adminlink" $ show $ LinkAdminOnly

adminCompanyPaymentPage :: TemplatesMonad m => Maybe PaymentPlan -> Int -> CompanyID -> String -> m String
adminCompanyPaymentPage mpaymentplan quantity companyid recurlysubdomain =
  renderTemplate "admincompanypayments" $ do
    F.value "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
    F.value "companyid" $ show companyid
    F.value "adminlink" $ show $ LinkAdminOnly
    F.value "recurlysubdomain" recurlysubdomain
    F.value "quantity" quantity
    case mpaymentplan of
      Nothing -> do
        F.value "haspaymentplan" False
        F.value "freeplan" True
      Just paymentplan -> do
        F.value "recurlyplan" $ ppPaymentPlanProvider paymentplan == RecurlyProvider
        F.value "accountcode" $ show $ ppAccountCode paymentplan
        F.value "priceplan" $ show $ ppPricePlan paymentplan
        F.value "haspaymentplan" True
        F.value "status" $ show $ ppStatus paymentplan
        case ppPricePlan paymentplan of
          FreePricePlan       -> F.value "freeplan"       True
          TeamPricePlan       -> F.value "teamplan"       True
          FormPricePlan       -> F.value "formplan"       True
          EnterprisePricePlan -> F.value "enterpriseplan" True
          TrialTeamPricePlan  -> F.value "trialplan"      True
        case ppStatus paymentplan of
          ActiveStatus      -> F.value "activestatus"      True
          OverdueStatus     -> F.value "overduestatus"     True
          CanceledStatus    -> F.value "canceledstatus"    True
          DeactivatedStatus -> F.value "deactivatedstatus" True

{-| Manage user page - can change user info and settings here -}
-- adminUserUsageStatsPage :: KontrakcjaTemplates -> User -> DocStatsL -> IO String
adminUserUsageStatsPage :: TemplatesMonad m => User -> Maybe Company -> Fields m () -> m String
adminUserUsageStatsPage user mcompany morefields =
    renderTemplate "userusagestats" $ do
        F.value "adminuserslink" $ show $ LinkUserAdmin Nothing
        F.object "user" $ userFields user
        F.object "company" $ companyFields mcompany
        F.value "adminlink" $ show $ LinkAdminOnly
        morefields

{-| The company stats page -}
adminCompanyUsageStatsPage :: TemplatesMonad m => CompanyID -> Fields m () -> m String
adminCompanyUsageStatsPage companyid morefields =
    renderTemplate "companyusagestats" $ do
        F.value "admincompanieslink" $ show $ LinkCompanyAdmin Nothing
        F.value "adminlink" $ show $ LinkAdminOnly
        F.value "companyid" $ show companyid
        morefields

companyFields :: Monad m => Maybe Company -> Fields m ()
companyFields mc = do
        F.value "companyid" $ maybe "" (show . companyid) mc
        F.value "companyname" $  getCompanyName mc
        F.value "companynumber" $ getCompanyNumber mc
        F.value "companyaddress" $ maybe "" (companyaddress . companyinfo) mc
        F.value "companyzip" $  maybe "" (companyzip . companyinfo)  mc
        F.value "companycity" $  maybe "" (companycity . companyinfo) mc
        F.value "companycountry" $ maybe "" (companycountry . companyinfo) mc
        F.value "companyemaildomain" $ maybe "" (fromMaybe "" . (companyemaildomain . companyinfo)) mc
        F.value "companyipaddressmasklist" $ maybe "" (intercalate "," . fmap show . companyipaddressmasklist . companyinfo) mc

{-| Full fields set about user -}
userFields :: Monad m => User -> Fields m ()
userFields u =  do
        F.value "fstname"          $ getFirstName u
        F.value "sndname"          $ getLastName u
        F.value "personalnumber"   $ getPersonalNumber u
        F.value "companyposition"  $ usercompanyposition $ userinfo u
        F.value "phone"            $ userphone $ userinfo u
        F.value "email"            $ getEmail u
        F.value "langsv"           $ LANG_SV == getLang u
        F.value "langen"           $ LANG_EN == getLang u
        F.value "iscompanyaccount" $ isJust $ usercompany u
        F.value "iscompanyadmin"   $ useriscompanyadmin u
        F.value "id"               $ show (userid u)
        F.value "companynumber"    $ getCompanyNumber u
        F.value "companyname"      $ getCompanyName   u
        F.value "isfree"           $ userisfree u

statisticsFields :: Monad m => (MinutesTime -> String) -> [UserUsageStats] -> [F.Fields m ()]
statisticsFields formatTime = map f
  where f uus = do
                F.value "date" $ formatTime (fst $ uusTimeSpan uus)
                F.value "closed" (uusDocumentsClosed uus)
                F.value "signatures" (uusSignaturesClosed uus)
                F.value "sent" (uusDocumentsSent uus)

statisticsCompanyFields :: Monad m => (MinutesTime -> String) -> [UserUsageStats] -> [F.Fields m ()]
statisticsCompanyFields formatTime = map f
  where f uus = do
                F.value "date" $ formatTime (fst $ uusTimeSpan uus)
                F.value "user" $ ((\(_,_,n) -> n) <$> uusUser uus)
                F.value "istotal" False -- FIMXE: need totals...
                F.value "closed" $ uusDocumentsClosed uus
                F.value "signatures" $ uusSignaturesClosed uus
                F.value "sent" $ uusDocumentsSent uus
