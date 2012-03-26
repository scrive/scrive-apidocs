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
         addAllDocsToStats,
         addAllUsersToStats,
         handleDocStatsCSV,
         handleMigrate1To2,
         handleUserStatsCSV,
         getUsageStatsForUser,
         getUsageStatsForCompany,
         getDocStatsForUser,
         getUsersAndStats,
         addAllSigsToStats,
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

import API.Service.Model
import Administration.AdministrationView
import AppView
import Company.Model
import DB.Classes
import Data.List
import Data.Maybe
import Doc.DocInfo
import Doc.DocStateData
import Doc.Model
import Kontra
import KontraError (KontraError)
import KontraLink
import MinutesTime
import Misc
import Stats.Model
import Stats.View
import Templates.Templates
import User.Model
import Util.FlashUtil
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import qualified Log

import qualified Data.ByteString.UTF8 as BS
import Happstack.Server
import Control.Monad
import Control.Monad.Error (MonadError, catchError)
import Control.Applicative
import qualified Data.Map as Map
import User.Utils

showAdminUserUsageStats :: Kontrakcja m => UserID -> m Response
showAdminUserUsageStats userid = onlySalesOrAdmin $ do
  statEvents <- runDBQuery $ GetDocStatEventsByUserID userid
  Just user <- runDBQuery $ GetUserByID userid
  mcompany <- getCompanyForUser user
  let rawevents = catMaybes $ map statEventToDocStatTuple statEvents
  let stats = calculateStatsByDay rawevents
  content <- adminUserUsageStatsPage user mcompany $ do
    fieldFL "statistics" $ statisticsFieldsByDay stats
  renderFromBody kontrakcja content

showAdminCompanyUsageStats :: Kontrakcja m => CompanyID -> m Response
showAdminCompanyUsageStats companyid = onlySalesOrAdmin $ do
  statCompanyEvents <- runDBQuery $ GetDocStatEventsByCompanyID companyid
  let rawevents = catMaybes $ map statCompanyEventToDocStatTuple statCompanyEvents
  let stats = calculateCompanyDocStats rawevents
  fullnames <- convertUserIDToFullName [] stats
  content <- adminCompanyUsageStatsPage companyid $ do
    fieldFL "statistics" $ statisticsCompanyFieldsByDay fullnames
  renderFromBody kontrakcja content

showAdminSystemUsageStats :: Kontrakcja m => m Response
showAdminSystemUsageStats = onlySalesOrAdmin $ do
  Context{ctxtime} <- getContext
  let today = asInt ctxtime
      som   = 100 * (today `div` 100) -- start of month
  statEvents <- runDBQuery $ GetDocStatEvents
  userEvents <- runDBQuery $ GetUserStatEvents
  let rawstats = (catMaybes $ map statEventToDocStatTuple statEvents)
                 ++ (catMaybes $ map userEventToDocStatTuple userEvents)
  let statsByDay = calculateStatsByDay $ filter (\s -> (fst s) >= som) rawstats
      statsByMonth = calculateStatsByMonth rawstats
  content <- adminUserStatisticsPage $ do
    fieldFL "statisticsbyday" $ statisticsFieldsByDay statsByDay
    fieldFL "statisticsbymonth" $ statisticsFieldsByMonth statsByMonth
  renderFromBody kontrakcja content

handleDocStatsCSV :: Kontrakcja m => m Response
handleDocStatsCSV = onlySalesOrAdmin $ do
  stats <- runDBQuery GetDocStatEvents
  let docstatsheader = ["userid", "user", "date", "event", "count", "docid", "serviceid", "company", "companyid", "doctype"]
  csvstrings <- docStatsToString stats [] []
  let res = Response 200 Map.empty nullRsFlags (toCSV docstatsheader csvstrings) Nothing
  Log.debug $ "All doc stats length with bytestring" ++ (show $ length stats) ++ " " ++ (show $ length $ show $ rsBody res)
  ok $ setHeader "Content-Disposition" "attachment;filename=docstats.csv"
     $ setHeader "Content-Type" "text/csv"
     $ res

docStatsToString :: Kontrakcja m => [DocStatEvent] -> [(UserID, String)] -> [(CompanyID, String)] -> m [[String]]
docStatsToString [] _ _ = return []
docStatsToString (e:es) usernames companynames = do
  (username, usernames') <- dbUserIDLookup (seUserID e) usernames
  (companyname, companynames') <- maybe (return ("none", companynames))
                                  (\i -> dbCompanyIDLookup i companynames)
                                  (seCompanyID e)
  let servicename = maybe "scrive" (BS.toString . unServiceID) (seServiceID e)
  rest <- docStatsToString es usernames' companynames'
  return $ [ show $ seUserID e
           , username
           , showDateYMD $ seTime e
           , show $ seQuantity e
           , show $ seAmount e
           , show $ seDocumentID e
           , servicename
           , companyname
           , maybe "" show $ seCompanyID e
           , show $ seDocumentType e
           ] : rest

dbUserIDLookup :: (Kontrakcja m) => UserID -> [(UserID, String)] -> m (String, [(UserID, String)])
dbUserIDLookup uid tbl =
  case lookup uid tbl of
    Nothing -> do
      name <- maybe "Unknown user" getSmartName <$> (runDBQuery $ GetUserByID uid)
      return (name, (uid, name):tbl)
    Just name -> return (name, tbl)

dbCompanyIDLookup :: (Kontrakcja m) => CompanyID -> [(CompanyID, String)] -> m (String, [(CompanyID, String)])
dbCompanyIDLookup cid tbl =
  case lookup cid tbl of
    Nothing -> do
      mcompany <- runDBQuery $ GetCompany cid
      let name = case mcompany of
            Nothing -> "Unknown Company"
            Just company -> case companyexternalid company of
              Just eid -> unExternalCompanyID eid
              Nothing -> case companyname $ companyinfo company of
                "" -> "no company name"
                a -> a
      return (name, (cid, name):tbl)
    Just name -> return (name, tbl)



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
      mu <- runDBQuery $ GetUserByID uid
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
  DocStatSend            -> Just (asInt seTime, [0, 0, seAmount, 0])
  _                      -> Nothing

statCompanyEventToDocStatTuple :: DocStatEvent -> Maybe (Int, UserID, [Int])
statCompanyEventToDocStatTuple (DocStatEvent {seTime, seUserID, seQuantity, seAmount}) = case seQuantity of
  DocStatClose           -> Just (asInt seTime, seUserID, [seAmount, 0, 0, 0])
  DocStatEmailSignatures -> Just (asInt seTime, seUserID, [0, seAmount, 0, 0])
  DocStatElegSignatures  -> Just (asInt seTime, seUserID, [0, seAmount, 0, 0])
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

falseOnError :: MonadError e m => m Bool -> m Bool
falseOnError m = m `catchError` const (return False)

-- | Two stats get created for a document close:
--   1. Count 1 for StatDocumentClose
--   2. Count the number of signatures in StatDocumentSignatures
-- Note that this will roll over (using mplus) in case there is
-- an error.
addDocumentCloseStatEvents :: (DBMonad m, MonadError KontraError m) => Document -> m Bool
addDocumentCloseStatEvents doc = falseOnError $ do
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
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = signtime
                                                        , seQuantity   = DocStatClose
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        }
      unless a $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatClose
      let q = case documentallowedidtypes doc of
            [EmailIdentification] -> DocStatEmailSignatures
            [ELegitimationIdentification] -> DocStatElegSignatures
            _ -> DocStatEmailSignatures
      b <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = signtime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        }
      unless b $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)

addDocumentSendStatEvents :: (DBMonad m, MonadError KontraError m) => Document -> m Bool
addDocumentSendStatEvents doc = falseOnError $ do
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
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = sendtime
                                                        , seQuantity   = DocStatSend
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        }
      unless a $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatSend
      let q = case documentallowedidtypes doc of
            [EmailIdentification] -> DocStatEmailSignaturePending
            [ELegitimationIdentification] -> DocStatElegSignaturePending
            _ -> DocStatEmailSignatures
      b <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = sendtime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        }
      unless b $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)

addDocumentCancelStatEvents :: (DBMonad m, MonadError KontraError m) => Document -> m Bool
addDocumentCancelStatEvents doc = falseOnError $ do
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
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = canceltime
                                                        , seQuantity   = DocStatCancel
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        }
      unless a $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatCancel
      let q = case documentallowedidtypes doc of
            [ELegitimationIdentification] -> DocStatElegSignatureCancel
            _ -> DocStatEmailSignatureCancel
      b <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = canceltime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        }
      unless b $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)

addDocumentRejectStatEvents :: (DBMonad m, MonadError KontraError m) => Document -> m Bool
addDocumentRejectStatEvents doc = falseOnError $ do
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
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = rejecttime
                                                        , seQuantity   = DocStatReject
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        }
      unless a $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatReject
      let q = case documentallowedidtypes doc of
            [ELegitimationIdentification] -> DocStatElegSignatureReject
            _ -> DocStatEmailSignatureReject
      b <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = rejecttime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        }
      unless b $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)

addDocumentCreateStatEvents :: (DBMonad m, MonadError KontraError m) => Document -> m Bool
addDocumentCreateStatEvents doc = falseOnError $ do
      sl  <- guardJust $ getAuthorSigLink doc
      uid <- guardJust $ maybesignatory sl
      let createtime = documentctime doc
      let did = documentid doc
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = createtime
                                                        , seQuantity   = DocStatCreate
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        }
      unless a $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatCreate
      return a

addDocumentTimeoutStatEvents :: (DBMonad m) => Document -> m Bool
addDocumentTimeoutStatEvents doc = do
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
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = ttime
                                                        , seQuantity   = DocStatTimeout
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        }
      unless a $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatTimeout
      let q = case documentallowedidtypes doc of
            [ELegitimationIdentification] -> DocStatElegSignatureTimeout
            _ -> DocStatEmailSignatureTimeout
      b <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = ttime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        }
      unless b $ Log.stats $ "Skipping existing document stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)


allDocStats ::  (DBMonad m, MonadError KontraError m) => Document -> m ()
allDocStats doc = do
    _ <- addDocumentSendStatEvents doc
    _ <- addDocumentCloseStatEvents doc
    _ <- addDocumentCreateStatEvents doc
    _ <- addDocumentRejectStatEvents doc
    _ <- addDocumentCancelStatEvents doc
    _ <- addDocumentTimeoutStatEvents doc
    return ()

-- | Must be sorted (both!)
filterMissing :: [DocumentID] -> [Document] -> [Document]
filterMissing _ [] = []
filterMissing [] docs = docs
filterMissing (did:dids) (doc:docs) | did <  documentid doc = filterMissing dids (doc:docs)
filterMissing (did:dids) (doc:docs) | did == documentid doc = filterMissing (did:dids) docs
filterMissing dids (doc:docs) = doc : filterMissing dids docs



addAllDocsToStats :: Kontrakcja m => m KontraLink
addAllDocsToStats = onlyAdmin $ do
  services <- runDBQuery GetServices
  let allservices = Nothing : map (Just . serviceid) services
  stats <- runDBQuery GetDocStatEvents
  let stats' = sort $ map seDocumentID stats
  _ <- forM allservices $ \s -> do
    docs <- runDBQuery $ GetDocumentsByService s
    let docs' = sortBy (\d1 d2 -> compare (documentid d1) (documentid d2)) docs
        docs'' = filterMissing stats' docs'
    mapM allDocStats docs''
  addFlash (OperationDone, "Added all docs to stats")
  return LinkUpload

handleMigrate1To2 :: Kontrakcja m => m KontraLink
handleMigrate1To2 = onlyAdmin $ do
  _ <- runDBUpdate FlushDocStats
  _ <- addAllDocsToStats
  addFlash (OperationDone, "Table migrated")
  return LinkUpload

addAllUsersToStats :: Kontrakcja m => m KontraLink
addAllUsersToStats = onlyAdmin $ do
  docs <- runDBQuery $ GetUsers
  _ <- mapM addUserSignTOSStatEvent docs
  return ()
  addFlash (OperationDone, "Added all users to stats")
  return LinkUpload

addUserLoginStatEvent :: Kontrakcja m => MinutesTime -> User -> m Bool
addUserLoginStatEvent time user = falseOnError $ do
    addUserIDStatEvent UserLogin (userid user) time (usercompany user) (userservice user)

addUserCreateCompanyStatEvent :: Kontrakcja m => MinutesTime -> User -> m Bool
addUserCreateCompanyStatEvent time user = falseOnError $ do
      case usercompany user of
        Nothing -> return False
        Just cid ->
          addUserIDStatEvent UserCreateCompany (userid user) time (Just cid) (userservice user)

addUserSignTOSStatEvent :: Kontrakcja m => User -> m Bool
addUserSignTOSStatEvent = addUserStatEventWithTOSTime UserSignTOS

addUserIDSignTOSStatEvent :: (DBMonad m) => UserID -> MinutesTime -> Maybe CompanyID -> Maybe ServiceID -> m Bool
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
      addUserIDStatEvent qty (userid user) mt' (usercompany user) (userservice user)

addUserIDStatEvent :: (DBMonad m) => UserStatQuantity -> UserID -> MinutesTime -> Maybe CompanyID -> Maybe ServiceID -> m Bool
addUserIDStatEvent qty uid mt mcid msid =  do
    a <- runDBUpdate $ AddUserStatEvent $ UserStatEvent { usUserID     = uid
                                                        , usTime       = mt
                                                        , usQuantity   = qty
                                                        , usAmount     = 1
                                                        , usCompanyID  = mcid
                                                        , usServiceID  = msid
                                                        }
    unless a $ Log.stats $ "Skipping existing user stat for userid: " ++ show uid ++ " and quantity: " ++ show qty
    return a


handleUserStatsCSV :: Kontrakcja m => m Response
handleUserStatsCSV = onlySalesOrAdmin $ do
  stats <- runDBQuery GetUserStatEvents
  Log.stats $ "All user stats length: " ++ (show $ length stats)
  ok $ setHeader "Content-Disposition" "attachment;filename=userstats.csv"
     $ setHeader "Content-Type" "text/csv"
     $ toResponse (userStatisticsCSV stats)

-- For User Admin tab in adminonly
getUsersAndStats :: Kontrakcja m => m [(User, Maybe Company, DocStats)]
getUsersAndStats = do
  Context{ctxtime} <- getContext
  list <- runDBQuery GetUsersAndStats
  return $ convert' ctxtime list
  where
    convert' _ []   = []
    convert' ctxtime list = map (innerGroup' ctxtime)
             $ groupBy (\(u1,_,_) (u2,_,_) -> u1 == u2) list
    innerGroup' _ [] = error "empty list grouped"
    innerGroup' ctxtime l@((u,m,_):_) =
        transform' ctxtime $ foldr (\(_,_,r) (au,ac,al) ->
                           (au,ac,r:al)) (u,m,[]) l
    transform' :: MinutesTime
               -> (User, Maybe Company, [( Maybe MinutesTime
                                         , Maybe DocStatQuantity
                                         , Maybe Int)])
               -> (User, Maybe Company, DocStats)
    transform' ctxtime (u,mc,l) = (u,mc,calculateDocStats ctxtime $ tuples' l)
    tuples' :: [(Maybe MinutesTime, Maybe DocStatQuantity, Maybe Int)]
            -> [(Int, [Int])]
    tuples' l = map toTuple' l
    toTuple' (Just time, Just quantity, Just amount) =
        case quantity of
            DocStatClose  -> (asInt time, [amount, 0])
            DocStatCreate -> (asInt time, [0, amount])
            _             -> (asInt time, [0,      0])
    toTuple' (_, _, _) = (0, [0, 0])

getDocStatsForUser :: Kontrakcja m => UserID -> m DocStats
getDocStatsForUser uid = do
  Context{ctxtime} <- getContext
  statEvents <- tuplesFromDocStatsForUser <$> runDBQuery (GetDocStatEventsByUserID uid)
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
  statEvents <- tuplesFromUsageStatsForUser <$> runDBQuery (GetDocStatEventsByUserID uid)
  Log.stats $ "sixm: " ++ show sixm
  Log.stats $ "stat events: " ++ show (length statEvents)
  let statsByDay = calculateStatsByDay $ filter (\s -> (fst s) >= som) statEvents
      statsByMonth = calculateStatsByMonth $ filter (\s -> (fst s) >= sixm) statEvents
  Log.stats $ "stats by month: " ++ show (length statsByMonth)
  return (statsByDay, statsByMonth)

getUsageStatsForCompany :: Kontrakcja m => CompanyID -> Int -> Int -> m ([(Int, String, [Int])], [(Int, String, [Int])])
getUsageStatsForCompany cid som sixm = do
  statEvents <- tuplesFromUsageStatsForCompany <$> runDBQuery (GetDocStatEventsByCompanyID cid)
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
          DocStatClose           -> Just (asInt seTime, [0, seAmount, 0])
          DocStatSend            -> Just (asInt seTime, [0, 0, seAmount])
          _                      -> Nothing

tuplesFromUsageStatsForCompany :: [DocStatEvent] -> [(Int, UserID, [Int])]
tuplesFromUsageStatsForCompany = catMaybes . map toTuple
  where toTuple (DocStatEvent {seTime, seUserID, seQuantity, seAmount}) = case seQuantity of
          DocStatEmailSignatures -> Just (asInt seTime, seUserID, [seAmount, 0, 0])
          DocStatElegSignatures  -> Just (asInt seTime, seUserID, [seAmount, 0, 0])
          DocStatClose           -> Just (asInt seTime, seUserID, [0, seAmount, 0])
          DocStatSend            -> Just (asInt seTime, seUserID, [0, 0, seAmount])
          _                      -> Nothing


{------ Sign Stats ------}

addSignStatInviteEvent :: Document -> SignatoryLink -> MinutesTime -> DB Bool
addSignStatInviteEvent doc sl time =
  let mdp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      sid = documentservice doc
      cid = maybe Nothing maybecompany mal
  in case mdp of
    Just dp -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = time
                                                          , ssQuantity        = SignStatInvite
                                                          , ssDocumentProcess = dp
                                                          , ssServiceID       = sid
                                                          , ssCompanyID       = cid
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatInvite
      return a
    _ -> do
      Log.stats $ "Cannot save stat if not doc process. docid: " ++ show (documentid doc)
      return False


addSignStatReceiveEvent :: Document -> SignatoryLink -> MinutesTime -> DB Bool
addSignStatReceiveEvent doc sl time =
  let mdp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      sid = documentservice doc
      cid = maybe Nothing maybecompany mal
  in case (invitationdeliverystatus sl, mdp) of
    (Delivered, Just dp) -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = time
                                                          , ssQuantity        = SignStatReceive
                                                          , ssDocumentProcess = dp
                                                          , ssServiceID       = sid
                                                          , ssCompanyID       = cid
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatReceive
      return a
    (_, Just _) -> do
      Log.stats $ "Cannot add receive stat because document is not delivered: " ++ show (signatorylinkid sl)
      return False
    (_, Nothing) -> do
      Log.stats $ "Cannot save stat if not doc process. docid: " ++ show (documentid doc)
      return False


addSignStatOpenEvent :: Document -> SignatoryLink -> DB Bool
addSignStatOpenEvent doc sl =
  let mdp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      sid = documentservice doc
      cid = maybe Nothing maybecompany mal
  in case (maybereadinvite sl, mdp) of
    (Just time, Just dp) -> do
        a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                            , ssSignatoryLinkID = signatorylinkid sl
                                                            , ssTime            = time
                                                            , ssQuantity        = SignStatOpen
                                                            , ssDocumentProcess = dp
                                                            , ssServiceID       = sid
                                                            , ssCompanyID       = cid
                                                            }
        unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
          show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatOpen
        return a
    (Nothing, _) -> do
      Log.stats $ "Cannot add open stat because document is not opened: " ++ show (signatorylinkid sl)
      return False
    (_, Nothing) -> do
      Log.stats $ "Cannot save stat if not doc process. docid: " ++ show (documentid doc)
      return False

addSignStatLinkEvent :: Document -> SignatoryLink -> DB Bool
addSignStatLinkEvent doc sl =
  let mdp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      sid = documentservice doc
      cid = maybe Nothing maybecompany mal
  in case (maybeseeninfo sl, mdp) of
    (Just (SignInfo {signtime}), Just dp) -> do
        a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                            , ssSignatoryLinkID = signatorylinkid sl
                                                            , ssTime            = signtime
                                                            , ssQuantity        = SignStatLink
                                                            , ssDocumentProcess = dp
                                                            , ssServiceID       = sid
                                                            , ssCompanyID       = cid
                                                            }
        unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
          show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatLink
        return a
    (Nothing, _) -> do
      Log.stats $ "Cannot add link stat because document is not seen: " ++ show (signatorylinkid sl)
      return False
    (_, Nothing) -> do
      Log.stats $ "Cannot save stat if not doc process. docid: " ++ show (documentid doc)
      return False

addSignStatSignEvent :: Document -> SignatoryLink -> DB Bool
addSignStatSignEvent doc sl =
  let mdp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      sid = documentservice doc
      cid = maybe Nothing maybecompany mal
  in case (maybesigninfo sl, mdp) of
    (Just (SignInfo {signtime}), Just dp) -> do
        a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                            , ssSignatoryLinkID = signatorylinkid sl
                                                            , ssTime            = signtime
                                                            , ssQuantity        = SignStatSign
                                                            , ssDocumentProcess = dp
                                                            , ssServiceID       = sid
                                                            , ssCompanyID       = cid
                                                            }
        unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
          show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatSign
        return a
    (Nothing, _) -> do
      Log.stats $ "Cannot add sign stat because document is not signed: " ++ show (signatorylinkid sl)
      return False
    (_, Nothing) -> do
      Log.stats $ "Cannot save stat if not doc process. docid: " ++ show (documentid doc)
      return False

addSignStatRejectEvent :: Document -> SignatoryLink -> DB Bool
addSignStatRejectEvent doc sl =
  let mdp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      sid = documentservice doc
      cid = maybe Nothing maybecompany mal
  in case (documentrejectioninfo doc, mdp) of
    (Just (signtime, slid, _), Just dp) | slid == signatorylinkid sl -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = signtime
                                                          , ssQuantity        = SignStatReject
                                                          , ssDocumentProcess = dp
                                                          , ssServiceID       = sid
                                                          , ssCompanyID       = cid
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatReject
      return a
    (Just _, Just _) -> do
      Log.stats $ "Cannot add reject stat because document is not rejected by this sl: " ++ show (signatorylinkid sl)
      return False
    (Nothing, _) -> do
      Log.stats $ "Cannot add reject stat because document is not rejected: " ++ show (signatorylinkid sl)
      return False
    (_, Nothing) -> do
      Log.stats $ "Cannot save stat if not doc process. docid: " ++ show (documentid doc)
      return False

addSignStatDeleteEvent :: Document -> SignatoryLink -> MinutesTime -> DB Bool
addSignStatDeleteEvent doc sl time =
  let mdp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      sid = documentservice doc
      cid = maybe Nothing maybecompany mal
  in case (signatorylinkdeleted sl, mdp) of
    (True, Just dp) -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = time
                                                          , ssQuantity        = SignStatDelete
                                                          , ssDocumentProcess = dp
                                                          , ssServiceID       = sid
                                                          , ssCompanyID       = cid
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatDelete
      return a
    (False, _) -> do
      Log.stats $ "Cannot add delete stat because document is not deleted: " ++ show (signatorylinkid sl)
      return False
    (_, Nothing) -> do
      Log.stats $ "Cannot save stat if not doc process. docid: " ++ show (documentid doc)
      return False

addSignStatPurgeEvent :: Document -> SignatoryLink -> MinutesTime -> DB Bool
addSignStatPurgeEvent doc sl time =
  let mdp = toDocumentProcess $ documenttype doc
      mal = getAuthorSigLink doc
      sid = documentservice doc
      cid = maybe Nothing maybecompany mal
  in case (signatorylinkreallydeleted sl, mdp) of
    (True, Just dp) -> do
      a <- dbUpdate $ AddSignStatEvent $ SignStatEvent { ssDocumentID      = documentid doc
                                                          , ssSignatoryLinkID = signatorylinkid sl
                                                          , ssTime            = time
                                                          , ssQuantity        = SignStatPurge
                                                          , ssDocumentProcess = dp
                                                          , ssServiceID       = sid
                                                          , ssCompanyID       = cid
                                                          }
      unless a $ Log.stats $ "Skipping existing sign stat for signatorylinkid: " ++
        show (signatorylinkid sl) ++ " and quantity: " ++ show SignStatPurge
      return a
    (False, _) -> do
      Log.stats $ "Cannot add purge stat because document is not really deleted: " ++ show (signatorylinkid sl)
      return False
    (_, Nothing) -> do
      Log.stats $ "Cannot save stat if not doc process. docid: " ++ show (documentid doc)
      return False

allSignStats :: Document -> SignatoryLink -> MinutesTime -> DB ()
allSignStats doc sl _time = do
  -- commented out lines are those that don't record time
  --_ <- addSignStatInviteEvent  doc sl time
  --_ <- addSignStatReceiveEvent doc sl time
  _ <- addSignStatOpenEvent    doc sl
  _ <- addSignStatLinkEvent    doc sl
  _ <- addSignStatSignEvent    doc sl
  _ <- addSignStatRejectEvent  doc sl
  --_ <- addSignStatDeleteEvent  doc sl time
  --_ <- addSignStatPurgeEvent   doc sl time
  return ()

addAllSigsToStats :: Kontrakcja m => m KontraLink
addAllSigsToStats = onlyAdmin $ do
  services <- runDBQuery GetServices
  let allservices = Nothing : map (Just . serviceid) services
  stats <- runDBQuery GetSignStatEvents
  let stats' = sort $ map ssDocumentID stats
  Context{ctxtime} <- getContext
  _ <- forM allservices $ \s -> do
    docs <- runDBQuery $ GetDocumentsByService s
    let docs' = sortBy (\d1 d2 -> compare (documentid d1) (documentid d2)) docs
        docs'' = filterMissing stats' docs'
    sequence $ [runDB $ allSignStats d sl ctxtime
               | d  <- docs''
               , sl <- documentsignatorylinks d]
  addFlash (OperationDone, "Added all docs to stats")
  return LinkUpload

--CSV for sign stats
handleSignStatsCSV :: Kontrakcja m => m Response
handleSignStatsCSV = do
  stats <- runDBQuery GetSignStatEvents
  Log.debug $ "All sign stats length: " ++ (show $ length stats)
  ok $ setHeader "Content-Disposition" "attachment;filename=signstats.csv"
     $ setHeader "Content-Type" "text/csv"
     $ toResponse (signStatsCSV stats)

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
        maybe "scrive" show $ seServiceID s,
        maybe "" show $ seCompanyID s,
        show $ seDocumentType s
       ] ++ csv2

-- CSV for document history
handleDocHistoryCSV :: Kontrakcja m => m Response
handleDocHistoryCSV = do
  stats <- runDBQuery GetDocStatEvents
  let byDoc = groupWith seDocumentID $ reverse $ sortWith seDocumentID stats
      rows = map (\es -> csvRowFromDocHist es []) byDoc
  ok $ setHeader "Content-Disposition" "attachment;filename=dochist.csv"
     $ setHeader "Content-Type" "text/csv"
     $ toResponse (docHistCSV rows)

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
      maybe "scrive" show $ ssServiceID s,
      maybe ""       show $ ssCompanyID s,
      show                $ ssDocumentProcess s
     ] ++ csv2

-- CSV for document history
handleSignHistoryCSV :: Kontrakcja m => m Response
handleSignHistoryCSV = do
  stats <- runDBQuery GetSignStatEvents
  let bySig = groupWith (\s-> (ssDocumentID s, ssSignatoryLinkID s)) $ reverse $ sortWith (\s-> (ssDocumentID s, ssSignatoryLinkID s)) stats
      rows = map (\es -> csvRowFromSignHist es []) bySig
  ok $ setHeader "Content-Disposition" "attachment;filename=signhist.csv"
     $ setHeader "Content-Type" "text/csv"
     $ toResponse (signHistCSV rows)

