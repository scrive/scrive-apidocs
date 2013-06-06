module Stats.Control
       (
         showAdminCompanyUsageStats,
         showAdminUserUsageStats
       )

       where

import Administration.AdministrationView
import AppView
import Company.Model
import DB
import Kontra
import MinutesTime
import Stats.View
import User.Model
import qualified Text.StringTemplates.Fields as F

import Happstack.Server
import User.Utils

showAdminUserUsageStats :: Kontrakcja m => UserID -> m Response
showAdminUserUsageStats userid = onlySalesOrAdmin $ do

  Context{ctxtime} <- getContext
  let timespans = [ (formatMinutesTime "%Y-%m-%d" t, formatMinutesTime "%Y-%m-%d" (daysAfter 1 t))
                     | daysBack <- [0 .. 30]
                     , t <- [daysBefore daysBack ctxtime]
                    ]
  stats <- dbQuery $ GetUserUsageStats (Just userid) Nothing timespans
  Just user <- dbQuery $ GetUserByID userid
  mcompany <- getCompanyForUser user

  content <- adminUserUsageStatsPage user mcompany $ do
    F.objects "statistics" $ statisticsFields (formatMinutesTime "%Y-%m-%d") stats
  renderFromBody kontrakcja content



showAdminCompanyUsageStats :: Kontrakcja m => CompanyID -> m Response
showAdminCompanyUsageStats companyid = onlySalesOrAdmin $ do

  Context{ctxtime} <- getContext
  let timespans = [ (formatMinutesTime "%Y-%m-%d" t, formatMinutesTime "%Y-%m-%d" (daysAfter 1 t))
                     | daysBack <- [0 .. 30]
                     , t <- [daysBefore daysBack ctxtime]
                    ]
  stats <- dbQuery $ GetUserUsageStats Nothing (Just companyid) timespans

  content <- adminCompanyUsageStatsPage companyid $ do
    F.objects "statistics" $ statisticsCompanyFields (formatMinutesTime "%Y-%m-%d") stats
  renderFromBody kontrakcja content
