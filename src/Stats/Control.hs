module Stats.Control
       (
         showAdminCompanyUsageStats,
         showAdminUserUsageStats,
         addDocumentCloseStatEvents,
         addDocumentCreateStatEvents,
         addDocumentSendStatEvents,
         addDocumentCancelStatEvents,
         addDocumentRejectStatEvents,
         addDocumentTimeoutStatEvents,
         addUserIDSignTOSStatEvent,
         addUserSignTOSStatEvent,
         addUserSaveAfterSignStatEvent,
         addUserPhoneAfterTOS,
         addUserCreateCompanyStatEvent,
         addUserLoginStatEvent,
         addUserStatAPIGrantAccess,
         addUserStatAPINewUser
       )

       where

import Administration.AdministrationView
import AppView
import Company.Model
import DB
import Data.Maybe
import Doc.DocStateData
import Kontra
import MinutesTime
import Stats.Model
import Stats.View
import Doc.SignatoryLinkID
import Doc.DocumentID
import User.Model
import Util.MonadUtils
import qualified Log
import qualified Text.StringTemplates.Fields as F

import qualified Control.Exception.Lifted as E
import Happstack.Server
import Control.Monad
import Control.Monad.Trans.Control
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


-- this creates an infinite list, so be careful!


-- some utility functions

falseOnError :: MonadBaseControl IO m => m Bool -> m Bool
falseOnError m = m `E.catch` (\(_::KontraError) -> return False)

-- | Two stats get created for a document close:
--   1. Count 1 for StatDocumentClose
--   2. Count the number of signatures in StatDocumentSignatures
-- Note that this will roll over (using mplus) in case there is
-- an error.
addDocumentCloseStatEvents :: (MonadDB m, MonadBaseControl IO m) => DocumentID -> String -> m Bool
addDocumentCloseStatEvents did apistring = falseOnError $ do
    aOK <- dbUpdate (statUpdate docStatClose did apistring)
    bOK <- dbUpdate (statUpdate (docStatSignMethod DocClosed) did apistring)
    return (aOK && bOK)

addDocumentSendStatEvents :: (MonadDB m, MonadBaseControl IO m) => DocumentID -> String -> m Bool
addDocumentSendStatEvents did apistring = falseOnError $ do
    aOK <- dbUpdate (statUpdate docStatSend did apistring)
    bOK <- dbUpdate (statUpdate (docStatSignMethod DocPending) did apistring)
    return (aOK && bOK)

addDocumentCancelStatEvents :: (MonadDB m, MonadBaseControl IO m) => DocumentID -> String -> m Bool
addDocumentCancelStatEvents did apistring = falseOnError $ do
    aOK <- dbUpdate (statUpdate docStatCancel did apistring)
    bOK <- dbUpdate (statUpdate (docStatSignMethod DocCancel) did apistring)
    return (aOK && bOK)

addDocumentRejectStatEvents :: (MonadDB m, MonadBaseControl IO m) => DocumentID -> String -> m Bool
addDocumentRejectStatEvents did apistring = falseOnError $ do
    aOK <- dbUpdate (statUpdate docStatReject did apistring)
    bOK <- dbUpdate (statUpdate (docStatSignMethod DocReject) did apistring)
    return (aOK && bOK)

addDocumentCreateStatEvents :: (MonadDB m, MonadBaseControl IO m) => DocumentID -> String -> m Bool
addDocumentCreateStatEvents did apistring = falseOnError $ do
      dbUpdate (statUpdate docStatCreate did apistring)

addDocumentTimeoutStatEvents :: (MonadDB m, MonadBaseControl IO m) => DocumentID -> String -> m Bool
addDocumentTimeoutStatEvents did apistring = falseOnError $ do
    aOK <- dbUpdate (statUpdate docStatTimeout did apistring)
    bOK <- dbUpdate (statUpdate (docStatSignMethod DocTimeout) did apistring)
    return (aOK && bOK)

addUserLoginStatEvent :: Kontrakcja m => MinutesTime -> User -> m Bool
addUserLoginStatEvent time user = falseOnError $ do
    addUserIDStatEvent UserLogin (userid user) time (usercompany user)

addUserCreateCompanyStatEvent :: Kontrakcja m => MinutesTime -> User -> m Bool
addUserCreateCompanyStatEvent time user = falseOnError $ do
  case usercompany user of
    Nothing  -> return False
    Just cid -> addUserIDStatEvent UserCreateCompany (userid user) time (Just cid)

addUserSignTOSStatEvent :: Kontrakcja m => User -> m Bool
addUserSignTOSStatEvent = addUserStatEventWithTOSTime UserSignTOS

addUserIDSignTOSStatEvent :: (MonadDB m) => UserID -> MinutesTime -> Maybe CompanyID -> m Bool
addUserIDSignTOSStatEvent = addUserIDStatEvent UserSignTOS

addUserSaveAfterSignStatEvent :: Kontrakcja m => User -> m Bool
addUserSaveAfterSignStatEvent = addUserStatEventWithTOSTime UserSaveAfterSign

addUserPhoneAfterTOS :: Kontrakcja m => User -> m Bool
addUserPhoneAfterTOS = addUserStatEventWithTOSTime UserPhoneAfterTOS

addUserStatEventWithTOSTime :: Kontrakcja m => UserStatQuantity -> User -> m Bool
addUserStatEventWithTOSTime qty user = falseOnError $ do
    if isNothing $ userhasacceptedtermsofservice user then return False
      else do
      Context{ctxtime} <- getContext
      mt  <- guardJust $ userhasacceptedtermsofservice user
      let mt' = if mt == fromSeconds 0
                then 1 `daysBefore` ctxtime -- before running stats
                else mt
      addUserIDStatEvent qty (userid user) mt' (usercompany user)

addUserIDStatEvent :: (MonadDB m) => UserStatQuantity -> UserID -> MinutesTime -> Maybe CompanyID -> m Bool
addUserIDStatEvent qty uid mt mcid =  do
    a <- dbUpdate $ AddUserStatEvent $ UserStatEvent { usUserID     = uid
                                                        , usTime       = mt
                                                        , usQuantity   = qty
                                                        , usAmount     = 1
                                                        , usCompanyID  = mcid
                                                        }
    unless a $ Log.stats $ "Skipping existing user stat for userid: " ++ show uid ++ " and quantity: " ++ show qty
    return a

addUserStatAPIGrantAccess :: (MonadDB m) => UserID -> MinutesTime -> Maybe CompanyID -> m Bool
addUserStatAPIGrantAccess uid mt cid = do
  dbUpdate $ AddUserStatEvent $ UserStatEvent { usUserID = uid
                                              , usTime = mt
                                              , usQuantity = UserAPIGrantAccess
                                              , usAmount = 1
                                              , usCompanyID = cid
                                              }
  
addUserStatAPINewUser :: (MonadDB m) => UserID -> MinutesTime -> Maybe CompanyID -> m Bool
addUserStatAPINewUser uid mt cid = do
  dbUpdate $ AddUserStatEvent $ UserStatEvent { usUserID = uid
                                              , usTime = mt
                                              , usQuantity = UserAPINewUser
                                              , usAmount = 1
                                              , usCompanyID = cid
                                              }
