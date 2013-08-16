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
            , statisticsCompanyFields
            , statisticsFields
          ) where

import Text.StringTemplates.Templates
import User.Model
import Company.Model
import Data.List
import MinutesTime
import Kontra
import qualified Text.StringTemplates.Fields as F


adminMainPage :: TemplatesMonad m => Context -> m String
adminMainPage ctx = renderTemplate "adminsmain" $ F.value "admin" $ isAdmin ctx

adminCompanyPage :: TemplatesMonad m => CompanyID ->  m String
adminCompanyPage cid = renderTemplate "admincompany" $ (F.value "companyid" $ show cid)

adminUserPage :: TemplatesMonad m => UserID -> m String
adminUserPage uid = renderTemplate "adminuser" $ (F.value "userid" $ show uid)

statisticsFields :: Monad m => (MinutesTime -> String) -> [UserUsageStats] -> [F.Fields m ()]
statisticsFields formatTime = map f
  where f uus = do
                F.value "date" $ formatTime (fst $ uusTimeSpan uus)
                F.value "closed" (uusDocumentsClosed uus)
                F.value "signatures" (uusSignaturesClosed uus)
                F.value "sent" (uusDocumentsSent uus)

statisticsCompanyFields :: Monad m => (MinutesTime -> String) -> [UserUsageStats] -> [F.Fields m ()]
statisticsCompanyFields formatTime = map f . appendTotalsPerTimespan . filter nonZero
  where f uus = do
                F.value "date" $ formatTime (fst $ uusTimeSpan uus)
                F.value "user" $ maybe "=> Company total" (\(_,_,n) -> n) (uusUser uus)
                F.value "closed" $ uusDocumentsClosed uus
                F.value "signatures" $ uusSignaturesClosed uus
                F.value "sent" $ uusDocumentsSent uus
        nonZero uus = uusDocumentsClosed uus > 0 ||
                      uusDocumentsSent uus > 0 ||
                      uusSignaturesClosed uus > 0
        appendTotalsPerTimespan = concat . map appendTotal . groupBySameTimeSpan
        groupBySameTimeSpan x = groupBy (\a b -> uusTimeSpan a == uusTimeSpan b) x
        appendTotal uuss = uuss ++ [(summarize uuss) { uusUser = Nothing}]
        summarize :: [UserUsageStats] -> UserUsageStats
        summarize uuss' = foldl1' addTwo uuss'
        addTwo u1 u2 = u1 { uusDocumentsSent    = uusDocumentsSent u1    + uusDocumentsSent u2
                          , uusDocumentsClosed  = uusDocumentsClosed u1  + uusDocumentsClosed u2
                          , uusSignaturesClosed = uusSignaturesClosed u1 + uusSignaturesClosed u2
                          }
