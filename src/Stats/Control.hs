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
         addUserIDSignTOSStatEvent,
         addUserSignTOSStatEvent,
         addAllDocsToStats,
         addAllUsersToStats,
         handleDocStatsCSV,
         handleMigrate1To2,
         handleUserStatsCSV
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
import Doc.DocState
import Kontra
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
import qualified AppLogger as Log

import qualified Data.ByteString.UTF8 as BS
import Happstack.Server
import Happstack.State
import Control.Monad
import Control.Applicative

import User.Utils

showAdminUserUsageStats :: Kontrakcja m => UserID -> m Response
showAdminUserUsageStats userid = onlySuperUser $ do
  statEvents <- runDBQuery $ GetDocStatEventsByUserID userid
  Just user <- runDBQuery $ GetUserByID userid
  mcompany <- getCompanyForUser user
  let rawevents = catMaybes $ map statEventToDocStatTuple statEvents
  let stats = calculateStatsByDay rawevents
  content <- adminUserUsageStatsPage user mcompany $ do
    fieldFL "statistics" $ statisticsFieldsByDay stats
  renderFromBody TopEmpty kontrakcja content

showAdminCompanyUsageStats :: Kontrakcja m => CompanyID -> m Response
showAdminCompanyUsageStats companyid = onlySuperUser $ do
  statCompanyEvents <- runDBQuery $ GetDocStatEventsByCompanyID companyid
  let rawevents = catMaybes $ map statCompanyEventToDocStatTuple statCompanyEvents
  let stats = calculateCompanyDocStats rawevents
  fullnames <- convertUserIDToFullName [] stats
  content <- adminCompanyUsageStatsPage companyid $ do
    fieldFL "statistics" $ statisticsCompanyFieldsByDay fullnames
  renderFromBody TopEmpty kontrakcja content

showAdminSystemUsageStats :: Kontrakcja m => m Response
showAdminSystemUsageStats = onlySuperUser $ do
  Context{ctxtime} <- getContext
  let today = asInt ctxtime
      som = 100 * (today `div` 100) -- start of month
  statEvents <- runDBQuery $ GetDocStatEvents
  userEvents <- runDBQuery $ GetUserStatEvents
  let rawstats = (catMaybes $ map statEventToDocStatTuple statEvents)
                 ++ (catMaybes $ map userEventToDocStatTuple userEvents)
  let statsByDay = calculateStatsByDay $ filter (\s -> (fst s) >= som) rawstats
      statsByMonth = calculateStatsByMonth rawstats
  content <- adminUserStatisticsPage $ do
    fieldFL "statisticsbyday" $ statisticsFieldsByDay statsByDay
    fieldFL "statisticsbymonth" $ statisticsFieldsByMonth statsByMonth
  renderFromBody TopEmpty kontrakcja content

handleDocStatsCSV :: Kontrakcja m => m Response
handleDocStatsCSV = onlySuperUser $ do
  stats <- runDBQuery GetDocStatEvents
  let docstatsheader = ["userid", "user", "date", "event", "count", "docid", "serviceid", "company", "companyid", "doctype"]
  csvstrings <- docStatsToString stats [] []
  let csv = toCSV docstatsheader csvstrings
  Log.debug $ "All doc stats length: " ++ (show $ length stats)
  ok $ setHeader "Content-Disposition" "attachment;filename=docstats.csv"
     $ setHeader "Content-Type" "text/csv"
     $ toResponse csv
     
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
      name <- maybe "Unknown user" (BS.toString . getSmartName) <$> (runDBQuery $ GetUserByID uid)
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
              Just eid -> BS.toString $ unExternalCompanyID eid
              Nothing -> case BS.toString $ companyname $ companyinfo company of
                "" -> "no company name"
                a -> a
      return (name, (cid, name):tbl)
    Just name -> return (name, tbl)


toCSV :: [String] -> [[String]] -> String
toCSV header ls =
  concatMap csvline (header:ls)
    where csvline line = "\"" ++ intercalate "\",\"" line ++ "\"\n"

{- |
   What a beast! This must be stopped! Oh, the humanity!
 -}
convertUserIDToFullName :: Kontrakcja m => [User] -> [(Int, UserID, [Int])] -> m [(Int, String, [Int])]
convertUserIDToFullName _ [] = return []
convertUserIDToFullName acc ((a,UserID 0,s):ss) = do
  rst <- convertUserIDToFullName acc ss
  return $ (a, "Total", s) : rst
convertUserIDToFullName acc ((a,uid,s):ss) =  
  case find (\u->userid u == uid) acc of
    Just u -> do
      rst <- convertUserIDToFullName acc ss
      return $ (a, BS.toString $ getSmartName u, s) : rst
    Nothing -> do
      mu <- runDBQuery $ GetUserByID uid
      case mu of
        Nothing -> do
          rst <- convertUserIDToFullName acc ss
          return $ (a, "Unknown user", s) : rst
        Just u -> do
          rst <- convertUserIDToFullName (u:acc) ss
          return $ (a, BS.toString $ getSmartName u, s) : rst

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
  -- we'll need this eventually, when there are more constructors
  --  _           -> Nothing

-- | Stats are very simple:
-- Date
-- # of documents Closed that date
-- # of signatures on documents closed that date
-- # of documents sent that date
-- # of users signing tos that day
calculateStatsByDay :: [(Int, [Int])] -> [(Int, [Int])]
calculateStatsByDay events =
  let byDay = groupWith fst $ reverse $ sortWith fst events
  in map sumStats byDay

-- | User Stats are very simple:
-- Date
-- # of documents Closed that date
-- # of signatures on documents closed that date
-- # of documents sent that date
calculateStatsByMonth :: [(Int, [Int])] -> [(Int, [Int])]
calculateStatsByMonth events =
  let monthOnly = [(100 * (a `div` 100), b) | (a, b) <- events]
      byMonth = groupWith fst $ reverse $ sortWith fst monthOnly
  in map sumStats byMonth


-- | Company Stats are very simple:
-- Date
-- UserID
-- # of documents Closed that date
-- # of signatures on documents closed that date
-- # of documents sent that date
calculateCompanyDocStats :: [(Int, UserID, [Int])] -> [(Int, UserID, [Int])]
calculateCompanyDocStats events =
  let byDay = groupWith (\(a,_,_)->a) $ reverse $ sortWith (\(a,_,_)->a) events
      byUser = map (groupWith (\(_,a,_)->a) . sortWith (\(_,a,_)->a)) byDay
      userTotalsByDay = map (map sumCStats) byUser
      setUID0 (a,_,s) = (a,UserID 0,s)
      totalByDay = map (setUID0 . sumCStats) byDay
  in concat $ zipWith (\a b->a++[b]) userTotalsByDay totalByDay


-- some utility functions

-- | Two stats get created for a document close:
--   1. Count 1 for StatDocumentClose
--   2. Count the number of signatures in StatDocumentSignatures
-- Note that this will roll over (using mplus) in case there is
-- an error.
addDocumentCloseStatEvents :: Kontrakcja m => Document -> m Bool
addDocumentCloseStatEvents doc = msum [
  do
    if not (isClosed doc) || none hasSigned (documentsignatorylinks doc) 
      then return False
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
  , return False]
         
addDocumentSendStatEvents :: Kontrakcja m => Document -> m Bool
addDocumentSendStatEvents doc = msum [
  do
    if isNothing $ documentinvitetime doc then return False
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
  , return False]
                                
addDocumentCancelStatEvents :: Kontrakcja m => Document -> m Bool
addDocumentCancelStatEvents doc = msum [
  do
    if not $ isCanceled doc then return False
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
      unless a $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatCancel           
      let q = case documentallowedidtypes doc of
            [EmailIdentification] -> DocStatEmailSignatureCancel
            [ELegitimationIdentification] -> DocStatElegSignatureCancel
            _ -> DocStatEmailSignatures
      b <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = canceltime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        }
      unless b $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)
  , return False]

addDocumentRejectStatEvents :: Kontrakcja m => Document -> m Bool
addDocumentRejectStatEvents doc = msum [
  do
    if not $ isRejected doc then return False
      else do
      sl  <- guardJust $ getAuthorSigLink doc
      uid <- guardJust $ maybesignatory sl
      (rejecttime,_,_) <- guardJust $ documentrejectioninfo doc      
      let did = documentid doc
          sigs = countSignatories doc          
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = rejecttime
                                                        , seQuantity   = DocStatCancel
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        }
      unless a $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatCancel           
      let q = case documentallowedidtypes doc of
            [EmailIdentification] -> DocStatEmailSignatureCancel
            [ELegitimationIdentification] -> DocStatElegSignatureCancel
            _ -> DocStatEmailSignatures
      b <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = rejecttime
                                                        , seQuantity   = q
                                                        , seAmount     = sigs
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        , seDocumentID = did
                                                        }
      unless b $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show q
      return (a && b)
  , return False]

addDocumentCreateStatEvents :: Kontrakcja m => Document -> m Bool
addDocumentCreateStatEvents doc = msum [
  do
    if isNothing $ documentinvitetime doc then return False
      else do
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
  , return False]

allDocStats :: Kontrakcja m => Document -> m ()
allDocStats doc = do
    _ <- addDocumentSendStatEvents doc
    _ <- addDocumentCloseStatEvents doc
    _ <- addDocumentCreateStatEvents doc
    _ <- addDocumentRejectStatEvents doc
    _ <- addDocumentCancelStatEvents doc
    return ()
  
-- | Must be sorted (both!)
filterMissing :: [DocumentID] -> [Document] -> [Document]
filterMissing _ [] = []
filterMissing [] docs = docs
filterMissing (did:dids) (doc:docs) | did <  documentid doc = filterMissing dids (doc:docs)
filterMissing (did:dids) (doc:docs) | did == documentid doc = filterMissing (did:dids) docs
filterMissing dids (doc:docs) = doc : filterMissing dids docs
  


addAllDocsToStats :: Kontrakcja m => m KontraLink
addAllDocsToStats = onlySuperUser $ do
  services <- runDBQuery GetServices
  let allservices = Nothing : map (Just . serviceid) services
  stats <- runDBQuery GetDocStatEvents  
  let stats' = sort $ map seDocumentID stats
  _ <- forM allservices $ \s -> do
    docs <- query $ GetDocuments s
    let docs' = sortBy (\d1 d2 -> compare (documentid d1) (documentid d2)) docs
        docs'' = filterMissing stats' docs'
    mapM allDocStats docs''
  addFlash (OperationDone, "Added all docs to stats")
  return LinkUpload

handleMigrate1To2 :: Kontrakcja m => m KontraLink
handleMigrate1To2 = onlySuperUser $ do
  _ <- runDBUpdate FlushDocStats
  _ <- addAllDocsToStats
  addFlash (OperationDone, "Table migrated")
  return LinkUpload

addAllUsersToStats :: Kontrakcja m => m KontraLink
addAllUsersToStats = onlySuperUser $ do
  docs <- runDBQuery $ GetUsers
  _ <- mapM addUserSignTOSStatEvent docs
  return ()
  addFlash (OperationDone, "Added all users to stats")
  return LinkUpload

addUserSignTOSStatEvent :: Kontrakcja m => User -> m Bool
addUserSignTOSStatEvent user = msum [
  do
    if isNothing $ userhasacceptedtermsofservice user then return False
      else do
      Context{ctxtime} <- getContext
      mt  <- guardJust $ userhasacceptedtermsofservice user
      let mt' = if mt == fromSeconds 0
                then (60 * 24) `minutesBefore` ctxtime -- one day before running stats
                else mt
      addUserIDSignTOSStatEvent (userid user) mt' (usercompany user) (userservice user)
  , return False]

addUserIDSignTOSStatEvent :: (DBMonad m) => UserID -> MinutesTime -> Maybe CompanyID -> Maybe ServiceID -> m Bool
addUserIDSignTOSStatEvent uid mt mcid msid =  do
    a <- runDBUpdate $ AddUserStatEvent $ UserStatEvent { usUserID     = uid
                                                        , usTime       = mt
                                                        , usQuantity   = UserSignTOS
                                                        , usAmount     = 1
                                                        , usCompanyID  = mcid
                                                        , usServiceID  = msid
                                                        }
    unless a $ Log.stats $ "Skipping existing user stat for userid: " ++ show uid ++ " and quantity: " ++ show UserSignTOS
    return a


handleUserStatsCSV :: Kontrakcja m => m Response
handleUserStatsCSV = onlySuperUser $ do
  stats <- runDBQuery GetUserStatEvents
  Log.debug $ "All user stats length: " ++ (show $ length stats)
  ok $ setHeader "Content-Disposition" "attachment;filename=userstats.csv"
     $ setHeader "Content-Type" "text/csv"
     $ toResponse (userStatisticsCSV stats)
