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
         addUserStatAPINewUser,
         getUsersAndStatsInv,
         addSignStatInviteEvent,
         addSignStatReceiveEvent,
         addSignStatOpenEvent,
         addSignStatLinkEvent,
         addSignStatSignEvent,
         addSignStatRejectEvent,
         addSignStatDeleteEvent,
         addSignStatPurgeEvent
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

-- For User Admin tab in adminonly
getUsersAndStatsInv :: Kontrakcja m => [UserFilter] -> [AscDesc UserOrderBy] -> (Int,Int) -> m [(User, Maybe Company, InviteType)]
getUsersAndStatsInv filters sorting pagination = do
  list <- dbQuery $ GetUsersAndStatsAndInviteInfo filters sorting pagination
  return $ convert' list
  where
    convert' list = map (\(u,mc,iv) ->
                                   ( u
                                   , mc
                                   , case iv of
                                       Nothing                     -> Admin
                                       Just (InviteInfo _ _ mtype) -> fromMaybe Admin mtype
                                   )) list


{------ Sign Stats ------}

addSignStatInviteEvent :: MonadDB m => Document -> SignatoryLink -> MinutesTime -> m Bool
addSignStatInviteEvent doc sl time =
  let dp = toDocumentProcess $ documenttype doc
  in  do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = time
                                                          , ssQuantity        = SignStatInvite
                                                          , ssDocumentProcess = dp
                                                          , ssCompanyID       = Nothing
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatInvite
      return a

addSignStatReceiveEvent :: MonadDB m => Document -> SignatoryLink -> MinutesTime -> m Bool
addSignStatReceiveEvent doc sl time =
  let dp = toDocumentProcess $ documenttype doc
  in case invitationdeliverystatus sl of
    Delivered -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = time
                                                          , ssQuantity        = SignStatReceive
                                                          , ssDocumentProcess = dp
                                                          , ssCompanyID       = Nothing
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatReceive
      return a
    _ -> do
      Log.stats $ "Cannot add receive stat because document is not delivered: " ++ show (signatorylinkid sl)
      return False


addSignStatOpenEvent :: MonadDB m => Document -> SignatoryLink -> m Bool
addSignStatOpenEvent doc sl =
  let dp = toDocumentProcess $ documenttype doc
  in case maybereadinvite sl of
    Just time -> do
        a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                            , ssSignatoryLinkID = signatorylinkid sl
                                                            , ssTime            = time
                                                            , ssQuantity        = SignStatOpen
                                                            , ssDocumentProcess = dp
                                                            , ssCompanyID       = Nothing
                                                            }
        unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
          show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatOpen
        return a
    Nothing -> do
      Log.stats $ "Cannot add open stat because document is not opened: " ++ show (signatorylinkid sl)
      return False

addSignStatLinkEvent :: MonadDB m => Document -> SignatoryLink -> m Bool
addSignStatLinkEvent doc sl =
  let dp = toDocumentProcess $ documenttype doc
  in case maybeseeninfo sl of
    Just (SignInfo {signtime}) -> do
        a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                            , ssSignatoryLinkID = signatorylinkid sl
                                                            , ssTime            = signtime
                                                            , ssQuantity        = SignStatLink
                                                            , ssDocumentProcess = dp
                                                            , ssCompanyID       = Nothing
                                                            }
        unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
          show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatLink
        return a
    Nothing -> do
      Log.stats $ "Cannot add link stat because document is not seen: " ++ show (signatorylinkid sl)
      return False

addSignStatSignEvent :: MonadDB m => Document -> SignatoryLink -> m Bool
addSignStatSignEvent doc sl =
  let dp = toDocumentProcess $ documenttype doc
  in case maybesigninfo sl of
    Just (SignInfo {signtime}) -> do
        a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                            , ssSignatoryLinkID = signatorylinkid sl
                                                            , ssTime            = signtime
                                                            , ssQuantity        = SignStatSign
                                                            , ssDocumentProcess = dp
                                                            , ssCompanyID       = Nothing
                                                            }
        unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
          show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatSign
        return a
    Nothing -> do
      Log.stats $ "Cannot add sign stat because document is not signed: " ++ show (signatorylinkid sl)
      return False

addSignStatRejectEvent :: MonadDB m => Document -> SignatoryLink -> m Bool
addSignStatRejectEvent doc sl =
  let dp = toDocumentProcess $ documenttype doc
  in case signatorylinkrejectiontime sl of
    Just rejecttime -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                       , ssSignatoryLinkID = signatorylinkid sl
                                                       , ssTime            = rejecttime
                                                       , ssQuantity        = SignStatReject
                                                       , ssDocumentProcess = dp
                                                       , ssCompanyID       = Nothing
                                                       }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatReject
      return a
    Nothing -> do
      Log.stats $ "Cannot add reject stat because document is not rejected: " ++ show (signatorylinkid sl)
      return False

addSignStatDeleteEvent :: MonadDB m => SignatoryLinkID -> MinutesTime -> m Bool
addSignStatDeleteEvent slid time = do
  a <- dbUpdate $ AddSignStatEvent2 slid SignStatDelete time
  unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
    show slid ++ " and quantity: " ++ show SignStatDelete
  return a

addSignStatPurgeEvent :: MonadDB m => SignatoryLinkID -> MinutesTime -> m Bool
addSignStatPurgeEvent slid time = do
  a <- dbUpdate $ AddSignStatEvent2 slid SignStatPurge time
  unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
    show slid ++ " and quantity: " ++ show SignStatPurge
  return a
