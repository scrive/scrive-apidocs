module Stats.Control
       (
         showAdminCompanyUsageStats,
         showAdminUserUsageStats,
         showAdminSystemUsageStats,
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
         handleDocStatsCSV,
         handleUserStatsCSV,
         getUsageStatsForUser,
         getUsageStatsForCompany,
         getDocStatsForUser,
         getUsersAndStatsInv,
         addSignStatInviteEvent,
         addSignStatReceiveEvent,
         addSignStatOpenEvent,
         addSignStatLinkEvent,
         addSignStatSignEvent,
         addSignStatRejectEvent,
         addSignStatDeleteEvent,
         addSignStatPurgeEvent,
         handleSignStatsCSV,
         handleDocHistoryCSV,
         handleSignHistoryCSV
       )

       where

import Administration.AdministrationView
import AppView
import Company.Model
import DB
import Data.List
import Data.Maybe
import Doc.DocInfo
import Doc.DocStateData
import Kontra
import MinutesTime
import Utils.List
import Stats.Model
import Stats.View
import User.Model
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import qualified Log
import qualified Templates.Fields as F

import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.UTF8 as BS
import Happstack.Server
import Control.Monad
import Control.Monad.Trans.Control
import Control.Applicative
import User.Utils
import Util.CSVUtil

showAdminUserUsageStats :: Kontrakcja m => UserID -> m Response
showAdminUserUsageStats userid = onlySalesOrAdmin $ do
  now <- ctxtime <$> getContext
  let sixmonthsago = 6 `monthsBefore` now
  statEvents <- dbQuery $ GetDocStatEventsByUserID userid sixmonthsago
  Just user <- dbQuery $ GetUserByID userid
  mcompany <- getCompanyForUser user
  let rawevents = catMaybes $ map statEventToDocStatTuple statEvents
  let stats = calculateStatsByDay rawevents
  content <- adminUserUsageStatsPage user mcompany $ do
    F.objects "statistics" $ statisticsFieldsByDay stats
  renderFromBody kontrakcja content

showAdminCompanyUsageStats :: Kontrakcja m => CompanyID -> m Response
showAdminCompanyUsageStats companyid = onlySalesOrAdmin $ do
  now <- ctxtime <$> getContext
  let sixmonthsago = 6 `monthsBefore` now
  statCompanyEvents <- dbQuery $ GetDocStatEventsByCompanyID companyid sixmonthsago
  let rawevents = catMaybes $ map statCompanyEventToDocStatTuple statCompanyEvents
  let stats = calculateCompanyDocStats rawevents
  fullnames <- convertUserIDToFullName [] stats
  content <- adminCompanyUsageStatsPage companyid $ do
    F.objects "statistics" $ statisticsCompanyFieldsByDay fullnames
  renderFromBody kontrakcja content

showAdminSystemUsageStats :: Kontrakcja m => m Response
showAdminSystemUsageStats = onlySalesOrAdmin $ do
  Context{ctxtime} <- getContext
  let sixmonthsago = 6 `monthsBefore` ctxtime
  let today = asInt ctxtime
      som   = 100 * (today `div` 100) -- start of month
  statEvents <- dbQuery $ GetDocStatEvents sixmonthsago
  userEvents <- dbQuery $ GetUserStatEvents
  let rawstats = (catMaybes $ map statEventToDocStatTuple statEvents)
                 ++ (catMaybes $ map userEventToDocStatTuple userEvents)
  let statsByDay = calculateStatsByDay $ filter (\s -> (fst s) >= som) rawstats
      statsByMonth = calculateStatsByMonth rawstats
  content <- adminUserStatisticsPage $ do
    F.objects "statisticsbyday" $ statisticsFieldsByDay statsByDay
    F.objects "statisticsbymonth" $ statisticsFieldsByMonth statsByMonth
  renderFromBody kontrakcja content

handleDocStatsCSV :: Kontrakcja m => m CSV
handleDocStatsCSV = onlySalesOrAdmin $ do
  let start = fromSeconds 0
  end <- ctxtime <$> getContext
  stats <- dbQuery $ GetDocStatCSV start end
  let docstatsheader = ["userid", "user", "date", "event", "count", "docid", "serviceid", "company", "companyid", "doctype", "api"]
  return $ CSV { csvFilename = "docstats.csv"
               , csvHeader = docstatsheader
               , csvContent = map (map BS.toString) stats
               }



{- |
   What a beast! This must be stopped! Oh, the humanity!
 -}
convertUserIDToFullName :: Kontrakcja m => [User] -> [(Int, UserID, [Int])] -> m [(Int, String, [Int])]
convertUserIDToFullName _ [] = return []
convertUserIDToFullName acc ((a,uid,s):ss)
  | uid == unsafeUserID 0 = do
    rst <- convertUserIDToFullName acc ss
    return $ (a, "Total", s) : rst
  | otherwise = case find (\u->userid u == uid) acc of
    Just u -> do
      rst <- convertUserIDToFullName acc ss
      return $ (a, getSmartName u, s) : rst
    Nothing -> do
      mu <- dbQuery $ GetUserByID uid
      case mu of
        Nothing -> do
          rst <- convertUserIDToFullName acc ss
          return $ (a, "Unknown user", s) : rst
        Just u -> do
          rst <- convertUserIDToFullName (u:acc) ss
          return $ (a, getSmartName u, s) : rst

addStats :: (Int, [Int]) -> (Int, [Int]) -> (Int, [Int])
addStats (_, t1s) (t, t2s) = (t, zipWithPadZeroes (+) t1s t2s)

sumStats :: [(Int, [Int])] -> (Int, [Int])
sumStats = foldl1 addStats

addCStats :: (Int, UserID, [Int]) -> (Int, UserID, [Int]) -> (Int, UserID, [Int])
addCStats (_, _, t1s) (t, uid, t2s) = (t, uid, zipWithPadZeroes (+) t1s t2s)

-- this creates an infinite list, so be careful!
zipWithPadZeroes :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
zipWithPadZeroes f a b = zipWith f (a ++ repeat 0) (b ++ repeat 0)

sumCStats :: [(Int, UserID, [Int])] -> (Int, UserID, [Int])
sumCStats = foldl1 addCStats

statEventToDocStatTuple :: DocStatEvent -> Maybe (Int, [Int])
statEventToDocStatTuple (DocStatEvent {seTime, seQuantity, seAmount}) = case seQuantity of
  DocStatClose           -> Just (asInt seTime, [seAmount, 0, 0, 0])
  DocStatEmailSignatures -> Just (asInt seTime, [0, seAmount, 0, 0])
  DocStatElegSignatures  -> Just (asInt seTime, [0, seAmount, 0, 0])
  DocStatPadSignatures   -> Just (asInt seTime, [0, seAmount, 0, 0])
  DocStatSend            -> Just (asInt seTime, [0, 0, seAmount, 0])
  _                      -> Nothing

statCompanyEventToDocStatTuple :: DocStatEvent -> Maybe (Int, UserID, [Int])
statCompanyEventToDocStatTuple (DocStatEvent {seTime, seUserID, seQuantity, seAmount}) = case seQuantity of
  DocStatClose           -> Just (asInt seTime, seUserID, [seAmount, 0, 0, 0])
  DocStatEmailSignatures -> Just (asInt seTime, seUserID, [0, seAmount, 0, 0])
  DocStatElegSignatures  -> Just (asInt seTime, seUserID, [0, seAmount, 0, 0])
  DocStatPadSignatures   -> Just (asInt seTime, seUserID, [0, seAmount, 0, 0])
  DocStatSend            -> Just (asInt seTime, seUserID, [0, 0, seAmount, 0])
  _                      -> Nothing

userEventToDocStatTuple :: UserStatEvent -> Maybe (Int, [Int])
userEventToDocStatTuple (UserStatEvent {usTime, usQuantity, usAmount}) = case usQuantity of
  UserSignTOS -> Just (asInt usTime, [0, 0, 0, usAmount])
  _           -> Nothing

calculateDocStats :: MinutesTime -> [(Int, [Int])] -> DocStats
calculateDocStats ctxtime events =
  let m1  = asInt $ monthsBefore 1 ctxtime
      m2  = asInt $ monthsBefore 3 ctxtime
      m3  = asInt $ monthsBefore 3 ctxtime
      m6  = asInt $ monthsBefore 6 ctxtime
      m12 = asInt $ monthsBefore 12 ctxtime
      sortedEvents = reverse $ sortWith fst events
      fstSumStats = (\prev (a1,a2) -> let a3 = prev++a1
                                      in (sumStats $ if null a3 then [(0,[0,0])] else a3, a2))
      (s1, r1) = fstSumStats [] $ span (\(m, _) -> m >= m1) sortedEvents
      (s2, r2) = fstSumStats [s1] $ span (\(m, _) -> m >= m2) r1
      (s3, r3) = fstSumStats [s2] $ span (\(m, _) -> m >= m3) r2
      (s6, r6) = fstSumStats [s3] $ span (\(m, _) -> m >= m6) r3
      (s12, r12) = fstSumStats [s6] $ span (\(m, _) -> m >= m12) r6
      sAll = sumStats $ s12:r12
  in DocStats ((!!1) . snd $ sAll) -- doccount
              ((!!0) . snd $ sAll) -- signaturecount
              ((!!0) . snd $ s1)   -- signaturecount1m
              ((!!0) . snd $ s2)   -- signaturecount2m
              ((!!0) . snd $ s3)   -- signaturecount3m
              ((!!0) . snd $ s6)   -- signaturecount6m
              ((!!0) . snd $ s12)  -- signaturecount12m

calculateStatsByDay :: [(Int, [Int])] -> [(Int, [Int])]
calculateStatsByDay events =
  let byDay = groupWith fst $ reverse $ sortWith fst events
  in map sumStats byDay

calculateStatsByMonth :: [(Int, [Int])] -> [(Int, [Int])]
calculateStatsByMonth events =
  let monthOnly = [(100 * (a `div` 100), b) | (a, b) <- events]
      byMonth = groupWith fst $ reverse $ sortWith fst monthOnly
  in map sumStats byMonth

calculateCompanyDocStats :: [(Int, UserID, [Int])] -> [(Int, UserID, [Int])]
calculateCompanyDocStats events =
  let byDay = groupWith (\(a,_,_)->a) $ reverse $ sortWith (\(a,_,_)->a) events
      byUser = map (groupWith (\(_,a,_)->a) . sortWith (\(_,a,_)->a)) byDay
      userTotalsByDay = map (map sumCStats) byUser
      setUID0 (a,_,s) = (a,unsafeUserID 0,s)
      totalByDay = map (setUID0 . sumCStats) byDay
  in concat $ zipWith (\a b->a++[b]) userTotalsByDay totalByDay

calculateCompanyDocStatsByMonth :: [(Int, UserID, [Int])] -> [(Int, UserID, [Int])]
calculateCompanyDocStatsByMonth events =
  let monthOnly = [((100 * (a `div` 100)), b, c) | (a, b, c) <- events]
      byMonth = groupWith (\(a,_,_) -> a) $ reverse $ sortWith  (\(a,_,_) -> a) monthOnly
      byUser = map (groupWith (\(_,a,_) ->a) . sortWith (\(_,a,_)->a)) byMonth
      userTotalsByMonth = map (map sumCStats) byUser
      setUID0 (a,_,s) = (a,unsafeUserID 0, s)
      totalByMonth = map (setUID0 . sumCStats) byMonth
  in concat $ zipWith (\a b->a++[b]) userTotalsByMonth totalByMonth

-- some utility functions

falseOnError :: MonadBaseControl IO m => m Bool -> m Bool
falseOnError m = m `E.catch` (\(_::KontraError) -> return False)

-- | Two stats get created for a document close:
--   1. Count 1 for StatDocumentClose
--   2. Count the number of signatures in StatDocumentSignatures
-- Note that this will roll over (using mplus) in case there is
-- an error.
addDocumentCloseStatEvents :: (MonadDB m, MonadBaseControl IO m) => Document -> String -> m Bool
addDocumentCloseStatEvents doc apistring = falseOnError $ do
    if not (isClosed doc)
      then do
      Log.stats $ "Cannot log CloseStat because document is not closed: " ++ show (documentid doc)
      return False
      else do
      sl  <- guardJust $ getAuthorSigLink doc
      uid <- guardJust $ maybesignatory sl
      let did = documentid doc
          sigs = countSignatures doc
          signtime = getLastSignedTime doc

      when (signtime == fromSeconds 0) $ Log.stats ("weird document: "++show (documentid doc))
      a <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = signtime
                                                        , seQuantity   = DocStatClose
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seDocumentType = documenttype doc
                                                        , seAPIString  = apistring
                                                        }
      unless a $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatClose
      let q = case (documentauthenticationmethod doc, documentdeliverymethod doc) of
            (EmailAuthentication, PadDelivery) -> DocStatPadSignatures
            (EmailAuthentication, _) -> DocStatPadSignatures
            (ELegAuthentication,  _) -> DocStatElegSignatures
      b <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                     , seTime       = signtime
                                                     , seQuantity   = q
                                                     , seAmount     = sigs
                                                     , seCompanyID  = maybecompany sl
                                                     , seDocumentType = documenttype doc
                                                     , seDocumentID = did
                                                     , seAPIString  = apistring
                                                     }
      unless b $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)

addDocumentSendStatEvents :: (MonadDB m, MonadBaseControl IO m) => Document -> String -> m Bool
addDocumentSendStatEvents doc apistring = falseOnError $ do
    if isNothing $ documentinvitetime doc
      then do
      Log.stats $ "Cannot add send stat because there is not invite time: " ++ show (documentid doc)
      return False
      else do
      sl  <- guardJust $ getAuthorSigLink doc
      uid <- guardJust $ maybesignatory sl
      sendtime <- guardJust $ getInviteTime doc
      let did = documentid doc
          sigs = countSignatories doc
      a <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = sendtime
                                                        , seQuantity   = DocStatSend
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seDocumentType = documenttype doc
                                                        , seAPIString  = apistring
                                                        }
      unless a $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatSend
      let q = case (documentauthenticationmethod doc, documentdeliverymethod doc) of
            (EmailAuthentication, PadDelivery) -> DocStatPadSignaturePending
            (EmailAuthentication, _) -> DocStatEmailSignaturePending
            (ELegAuthentication,  _) -> DocStatElegSignaturePending
      b <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = sendtime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        , seAPIString  = apistring
                                                        }
      unless b $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)

addDocumentCancelStatEvents :: (MonadDB m, MonadBaseControl IO m) => Document -> String -> m Bool
addDocumentCancelStatEvents doc apistring = falseOnError $ do
    if not $ isCanceled doc
      then do
      Log.stats $ "Cannot add Cancel event because doc is not canceled: " ++ show (documentid doc)
      return False
      else do
      sl  <- guardJust $ getAuthorSigLink doc
      uid <- guardJust $ maybesignatory sl
      let canceltime = documentmtime doc
      let did = documentid doc
          sigs = countSignatories doc
      a <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = canceltime
                                                        , seQuantity   = DocStatCancel
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seDocumentType = documenttype doc
                                                        , seAPIString  = apistring
                                                        }
      unless a $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatCancel
      let q = case (documentauthenticationmethod doc, documentdeliverymethod doc) of
            (EmailAuthentication, PadDelivery) -> DocStatPadSignatureCancel
            (EmailAuthentication, _)           -> DocStatEmailSignatureCancel
            (ELegAuthentication, _) -> DocStatElegSignatureCancel
      b <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = canceltime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        , seAPIString  = apistring
                                                        }
      unless b $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)

addDocumentRejectStatEvents :: (MonadDB m, MonadBaseControl IO m) => Document -> String -> m Bool
addDocumentRejectStatEvents doc apistring = falseOnError $ do
    if not $ isRejected doc
      then do
      Log.stats $ "Cannot add Reject stat because document is not rejected: " ++ show (documentid doc)
      return False
      else do
      sl  <- guardJust $ getAuthorSigLink doc
      uid <- guardJust $ maybesignatory sl
      (rejecttime,_,_) <- guardJust $ documentrejectioninfo doc
      let did = documentid doc
          sigs = countSignatories doc
      a <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = rejecttime
                                                        , seQuantity   = DocStatReject
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seDocumentType = documenttype doc
                                                        , seAPIString  = apistring
                                                        }
      unless a $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatReject
      let q = case (documentauthenticationmethod doc, documentdeliverymethod doc) of
            (EmailAuthentication, PadDelivery) -> DocStatPadSignatureReject
            (EmailAuthentication, _ )          -> DocStatEmailSignatureReject
            (ELegAuthentication, _) -> DocStatElegSignatureReject
      b <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = rejecttime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        , seAPIString  = apistring
                                                        }
      unless b $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)

addDocumentCreateStatEvents :: (MonadDB m, MonadBaseControl IO m) => Document -> String -> m Bool
addDocumentCreateStatEvents doc apistring = falseOnError $ do
      sl  <- guardJust $ getAuthorSigLink doc
      uid <- guardJust $ maybesignatory sl
      let createtime = documentctime doc
      let did = documentid doc
      a <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = createtime
                                                        , seQuantity   = DocStatCreate
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seDocumentType = documenttype doc
                                                        , seAPIString  = apistring
                                                        }
      unless a $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatCreate
      return a

addDocumentTimeoutStatEvents :: (MonadDB m) => Document -> String -> m Bool
addDocumentTimeoutStatEvents doc apistring = do
  case (isTimedout doc, getAuthorSigLink doc, maybesignatory =<< getAuthorSigLink doc, documenttimeouttime doc) of
    (False,_,_,_) -> do
      Log.stats $ "Cannot add Timeout stat because document is not timed out: " ++ show (documentid doc)
      return False
    (_,Nothing,_,_) -> do
      Log.stats $ "Cannot add Timeout stat because document has no author: " ++ show (documentid doc)
      return False
    (_,_,Nothing,_) -> do
      Log.stats $ "Cannot add Timeout stat because document author has no user: " ++ show (documentid doc)
      return False
    (_,_,_,Nothing) -> do
      Log.stats $ "Cannot add Timeout stat because document has no timeout time: " ++ show (documentid doc)
      return False
    (True, Just sl, Just uid, Just (TimeoutTime ttime)) -> do
      let did = documentid doc
          sigs = countSignatories doc
      a <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = ttime
                                                        , seQuantity   = DocStatTimeout
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seDocumentType = documenttype doc
                                                        , seAPIString  = apistring
                                                        }
      unless a $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatTimeout
      let q = case (documentauthenticationmethod doc, documentdeliverymethod doc) of
            (EmailAuthentication, PadDelivery) -> DocStatPadSignatureTimeout
            (EmailAuthentication, _) -> DocStatEmailSignatureTimeout
            (ELegAuthentication, _) -> DocStatElegSignatureTimeout
      b <- dbUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = ttime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        , seAPIString  = apistring
                                                        }
      unless b $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)


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
                then (60 * 24) `minutesBefore` ctxtime -- one day before running stats
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

handleUserStatsCSV :: Kontrakcja m => m CSV
handleUserStatsCSV = onlySalesOrAdmin $ do
  stats <- dbQuery GetUserStatEvents
  return $ CSV { csvFilename = "userstats.csv"
               , csvHeader = ["userid", "date", "event", "count", "serviceid", "companyid"]
               , csvContent = map csvline stats
               }
  where csvline event = [ show                                 $ usUserID    event
                        , showDateYMD                          $ usTime      event
                        , show                                 $ usQuantity  event
                        , show                                 $ usAmount    event
                        , maybe "" show                        $ usCompanyID event
                        ]

-- For User Admin tab in adminonly
getUsersAndStatsInv :: Kontrakcja m => [UserFilter] -> [AscDesc UserOrderBy] -> UserPagination -> m [(User, Maybe Company, DocStats, InviteType)]
getUsersAndStatsInv filters sorting pagination = do
  Context{ctxtime} <- getContext
  list <- dbQuery $ GetUsersAndStatsAndInviteInfo filters sorting pagination
  return $ convert' ctxtime list
  where
    convert' ctxtime list = map (\(u,mc,l,iv) ->
                                   ( u
                                   , mc
                                   , calculateDocStats ctxtime $ tuples' l
                                   , case iv of
                                       Nothing                     -> Admin
                                       Just (InviteInfo _ _ mtype) -> fromMaybe Admin mtype
                                   )) list
    tuples' :: [(MinutesTime, DocStatQuantity, Int)]
            -> [(Int, [Int])]
    tuples' l = map toTuple' l
    toTuple' (time, quantity, amount) =
        case quantity of
            DocStatClose  -> (asInt time, [amount, 0])
            DocStatCreate -> (asInt time, [0, amount])
            _             -> (asInt time, [0,      0])


getDocStatsForUser :: Kontrakcja m => UserID -> m DocStats
getDocStatsForUser uid = do
  Context{ctxtime} <- getContext
  let sixmonthsago = 6 `monthsBefore` ctxtime
  statEvents <- tuplesFromDocStatsForUser <$> dbQuery (GetDocStatEventsByUserID uid sixmonthsago)
  return $ calculateDocStats ctxtime statEvents

tuplesFromDocStatsForUser :: [DocStatEvent] -> [(Int, [Int])]
tuplesFromDocStatsForUser = catMaybes . map toTuple
  where toTuple (DocStatEvent {seTime, seQuantity, seAmount}) = case seQuantity of
          DocStatClose   -> Just (asInt seTime, [seAmount, 0])
          DocStatCreate  -> Just (asInt seTime, [0, seAmount])
          _              -> Nothing

-- For Usage Stats tab in Account
getUsageStatsForUser :: Kontrakcja m => UserID -> Int -> Int -> m ([(Int, [Int])], [(Int, [Int])])
getUsageStatsForUser uid som sixm = do
  now <- ctxtime <$> getContext
  let sixmonthsago = 6 `monthsBefore` now
  statEvents <- tuplesFromUsageStatsForUser <$> dbQuery (GetDocStatEventsByUserID uid sixmonthsago)
  Log.stats $ "sixm: " ++ show sixm
  Log.stats $ "stat events: " ++ show (length statEvents)
  let statsByDay = calculateStatsByDay $ filter (\s -> (fst s) >= som) statEvents
      statsByMonth = calculateStatsByMonth $ filter (\s -> (fst s) >= sixm) statEvents
  Log.stats $ "stats by month: " ++ show (length statsByMonth)
  return (statsByDay, statsByMonth)

getUsageStatsForCompany :: Kontrakcja m => CompanyID -> Int -> Int -> m ([(Int, String, [Int])], [(Int, String, [Int])])
getUsageStatsForCompany cid som sixm = do
  now <- ctxtime <$> getContext
  let sixmonthsago = 6 `monthsBefore` now
  statEvents <- tuplesFromUsageStatsForCompany <$> dbQuery (GetDocStatEventsByCompanyID cid sixmonthsago)
  let statsByDay = calculateCompanyDocStats $ filter (\(t,_,_)-> t >= som) statEvents
      statsByMonth = calculateCompanyDocStatsByMonth $ filter (\(t,_,_) -> t >= sixm) statEvents
  fullnamesDay <- convertUserIDToFullName [] statsByDay
  fullnamesMonth <- convertUserIDToFullName [] statsByMonth
  return (fullnamesDay, fullnamesMonth)

tuplesFromUsageStatsForUser :: [DocStatEvent] -> [(Int, [Int])]
tuplesFromUsageStatsForUser = catMaybes . map toTuple
  where toTuple (DocStatEvent {seTime, seQuantity, seAmount}) = case seQuantity of
          DocStatEmailSignatures -> Just (asInt seTime, [seAmount, 0, 0])
          DocStatElegSignatures  -> Just (asInt seTime, [seAmount, 0, 0])
          DocStatPadSignatures   -> Just (asInt seTime, [seAmount, 0, 0])
          DocStatClose           -> Just (asInt seTime, [0, seAmount, 0])
          DocStatSend            -> Just (asInt seTime, [0, 0, seAmount])
          _                      -> Nothing

tuplesFromUsageStatsForCompany :: [DocStatEvent] -> [(Int, UserID, [Int])]
tuplesFromUsageStatsForCompany = catMaybes . map toTuple
  where toTuple (DocStatEvent {seTime, seUserID, seQuantity, seAmount}) = case seQuantity of
          DocStatEmailSignatures -> Just (asInt seTime, seUserID, [seAmount, 0, 0])
          DocStatElegSignatures  -> Just (asInt seTime, seUserID, [seAmount, 0, 0])
          DocStatPadSignatures   -> Just (asInt seTime, seUserID, [seAmount, 0, 0])
          DocStatClose           -> Just (asInt seTime, seUserID, [0, seAmount, 0])
          DocStatSend            -> Just (asInt seTime, seUserID, [0, 0, seAmount])
          _                      -> Nothing


{------ Sign Stats ------}

addSignStatInviteEvent :: MonadDB m => Document -> SignatoryLink -> MinutesTime -> m Bool
addSignStatInviteEvent doc sl time =
  let dp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      cid = maybe Nothing maybecompany mal
  in  do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = time
                                                          , ssQuantity        = SignStatInvite
                                                          , ssDocumentProcess = dp
                                                          , ssCompanyID       = cid
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatInvite
      return a

addSignStatReceiveEvent :: MonadDB m => Document -> SignatoryLink -> MinutesTime -> m Bool
addSignStatReceiveEvent doc sl time =
  let dp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      cid = maybe Nothing maybecompany mal
  in case invitationdeliverystatus sl of
    Delivered -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = time
                                                          , ssQuantity        = SignStatReceive
                                                          , ssDocumentProcess = dp
                                                          , ssCompanyID       = cid
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
      mal = getAuthorSigLink doc
      cid = maybe Nothing maybecompany mal
  in case maybereadinvite sl of
    Just time -> do
        a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                            , ssSignatoryLinkID = signatorylinkid sl
                                                            , ssTime            = time
                                                            , ssQuantity        = SignStatOpen
                                                            , ssDocumentProcess = dp
                                                            , ssCompanyID       = cid
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
      mal = getAuthorSigLink doc
      cid = maybe Nothing maybecompany mal
  in case maybeseeninfo sl of
    Just (SignInfo {signtime}) -> do
        a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                            , ssSignatoryLinkID = signatorylinkid sl
                                                            , ssTime            = signtime
                                                            , ssQuantity        = SignStatLink
                                                            , ssDocumentProcess = dp
                                                            , ssCompanyID       = cid
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
      mal = getAuthorSigLink doc
      cid = maybe Nothing maybecompany mal
  in case maybesigninfo sl of
    Just (SignInfo {signtime}) -> do
        a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                            , ssSignatoryLinkID = signatorylinkid sl
                                                            , ssTime            = signtime
                                                            , ssQuantity        = SignStatSign
                                                            , ssDocumentProcess = dp
                                                            , ssCompanyID       = cid
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
      mal = getAuthorSigLink doc
      cid = maybe Nothing maybecompany mal
  in case documentrejectioninfo doc of
    Just (signtime, slid, _) | slid == signatorylinkid sl -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = signtime
                                                          , ssQuantity        = SignStatReject
                                                          , ssDocumentProcess = dp
                                                          , ssCompanyID       = cid
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatReject
      return a
    Just _ -> do
      Log.stats $ "Cannot add reject stat because document is not rejected by this sl: " ++ show (signatorylinkid sl)
      return False
    Nothing -> do
      Log.stats $ "Cannot add reject stat because document is not rejected: " ++ show (signatorylinkid sl)
      return False

addSignStatDeleteEvent :: MonadDB m => Document -> SignatoryLink -> MinutesTime -> m Bool
addSignStatDeleteEvent doc sl time =
  let dp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      cid = maybe Nothing maybecompany mal
  in case signatorylinkdeleted sl of
    True -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = time
                                                          , ssQuantity        = SignStatDelete
                                                          , ssDocumentProcess = dp
                                                          , ssCompanyID       = cid
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatDelete
      return a
    False -> do
      Log.stats $ "Cannot add delete stat because document is not deleted: " ++ show (signatorylinkid sl)
      return False

addSignStatPurgeEvent :: MonadDB m => Document -> SignatoryLink -> MinutesTime -> m Bool
addSignStatPurgeEvent doc sl time =
  let dp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      cid = maybe Nothing maybecompany mal
  in case signatorylinkreallydeleted sl of
    True -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = time
                                                          , ssQuantity        = SignStatPurge
                                                          , ssDocumentProcess = dp
                                                          , ssCompanyID       = cid
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatPurge
      return a
    False -> do
      Log.stats $ "Cannot add purge stat because document is not really deleted: " ++ show (signatorylinkid sl)
      return False


--CSV for sign stats
handleSignStatsCSV :: Kontrakcja m => m CSV
handleSignStatsCSV = do
  stats <- dbQuery GetSignStatEvents
  return $ CSV { csvFilename = "signstats.csv"
               , csvHeader = ["documentid", "signatorylinkid", "date", "event", "doctype", "service (author)", "company (author)"]
               , csvContent = map csvline stats
               }
  where csvline event = [ show        $ ssDocumentID      event
                        , show        $ ssSignatoryLinkID event
                        , showDateYMD $ ssTime            event
                        , show        $ ssQuantity        event
                        , show        $ ssDocumentProcess event
                        , show        $ ssCompanyID       event
                        ]


csvRowFromDocHist :: [DocStatEvent] -> [String] -> [String]
csvRowFromDocHist [] csv = csv
csvRowFromDocHist (s:ss) csv' =
  let csvtail = take 6 $ drop 4 $ csv' ++ repeat ""
      csv2 = case seQuantity s of
        DocStatCreate  -> chng csvtail 0 $ formatMinutesTimeISO (seTime s)
        DocStatSend    -> chng csvtail 1 $ formatMinutesTimeISO (seTime s)
        DocStatClose   -> chng csvtail 2 $ formatMinutesTimeISO (seTime s)
        DocStatReject  -> chng csvtail 3 $ formatMinutesTimeISO (seTime s)
        DocStatCancel  -> chng csvtail 4 $ formatMinutesTimeISO (seTime s)
        DocStatTimeout -> chng csvtail 5 $ formatMinutesTimeISO (seTime s)
        _ -> csvtail
  in csvRowFromDocHist ss
     $ [show $ seDocumentID s,
        maybe "" show $ seCompanyID s,
        show $ seDocumentType s
       ] ++ csv2

-- CSV for document history
handleDocHistoryCSV :: Kontrakcja m => m CSV
handleDocHistoryCSV = do
  now <- ctxtime <$> getContext
  let sixmonthsago = 6 `monthsBefore` now
  stats <- dbQuery $ GetDocStatEvents sixmonthsago
  let byDoc = groupWith seDocumentID $ reverse $ sortWith seDocumentID stats
      rows = map (\es -> csvRowFromDocHist es []) byDoc
  return $ CSV { csvFilename = "dochist.csv"
               , csvHeader = ["documentid", "serviceid", "companyid", "doctype", "create", "send", "close", "reject", "cancel", "timeout"]
               , csvContent = rows
               }


-- CSV for sig history
csvRowFromSignHist :: [SignStatEvent] -> [String] -> [String]
csvRowFromSignHist [] csv = csv
csvRowFromSignHist (s:ss) csv' =
  let csvtail = take 8 $ drop 5 $ csv' ++ repeat ""
      csv2 = case ssQuantity s of
        SignStatInvite  -> chng csvtail 0 $ formatMinutesTimeISO (ssTime s)
        SignStatReceive -> chng csvtail 1 $ formatMinutesTimeISO (ssTime s)
        SignStatOpen    -> chng csvtail 2 $ formatMinutesTimeISO (ssTime s)
        SignStatLink    -> chng csvtail 3 $ formatMinutesTimeISO (ssTime s)
        SignStatSign    -> chng csvtail 4 $ formatMinutesTimeISO (ssTime s)
        SignStatReject  -> chng csvtail 5 $ formatMinutesTimeISO (ssTime s)
        SignStatDelete  -> chng csvtail 6 $ formatMinutesTimeISO (ssTime s)
        SignStatPurge   -> chng csvtail 7 $ formatMinutesTimeISO (ssTime s)
  in csvRowFromSignHist ss $
     [show                $ ssDocumentID s,
      show                $ ssSignatoryLinkID s,
      maybe ""       show $ ssCompanyID s,
      show                $ ssDocumentProcess s
     ] ++ csv2

-- CSV for document history
handleSignHistoryCSV :: Kontrakcja m => m CSV
handleSignHistoryCSV = do
  stats <- dbQuery GetSignStatEvents
  let bySig = groupWith (\s-> (ssDocumentID s, ssSignatoryLinkID s)) $ reverse $ sortWith (\s-> (ssDocumentID s, ssSignatoryLinkID s)) stats
      rows = map (\es -> csvRowFromSignHist es []) bySig
  return $ CSV { csvFilename = "signhist.csv"
               , csvHeader = ["documentid", "signatoryid", "serviceid", "companyid", "doctype", "invite", "receive", "open", "link", "sign", "reject", "delete", "purge"]
               , csvContent = rows
               }
