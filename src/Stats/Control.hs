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
         addAllDocsToStats,
         handleDocStatsCSV,
         handleMigrate1To2
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
import User.UserControl
import Util.FlashUtil
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import qualified AppLogger as Log

import qualified Data.ByteString.UTF8 as BS
import Happstack.Server
import Happstack.State
import Control.Monad

showAdminUserUsageStats :: Kontrakcja m => UserID -> m Response
showAdminUserUsageStats userid = onlySuperUser $ do
  statEvents <- runDBQuery $ GetDocStatEventsByUserID userid
  Just user <- runDBQuery $ GetUserByID userid
  mcompany <- getCompanyForUser user
  let stats = calculateDocStatsFromEventsByDay statEvents
  content <- adminUserUsageStatsPage user mcompany $ do
    fieldFL "statistics" $ statisticsFieldsByDay stats
  renderFromBody TopEmpty kontrakcja content

showAdminCompanyUsageStats :: Kontrakcja m => CompanyID -> m Response
showAdminCompanyUsageStats companyid = onlySuperUser $ do
  statCompanyEvents <- runDBQuery $ GetDocStatEventsByCompanyID companyid
  let stats = calculateDocStatsFromCompanyEvents statCompanyEvents
  fullnames <- convertUserIDToFullName [] stats
  content <- adminCompanyUsageStatsPage companyid $ do
    fieldFL "statistics" $ statisticsCompanyFieldsByDay fullnames
  renderFromBody TopEmpty kontrakcja content

showAdminSystemUsageStats :: Kontrakcja m => m Response
showAdminSystemUsageStats = onlySuperUser $ do
  Context{ctxtime} <- getContext
  let today = asInt ctxtime
      som = 100 * (today `div` 100)
  statEvents <- runDBQuery $ GetDocStatEvents
  let statsByDay = calculateDocStatsFromEventsByDay $ filter (\s -> asInt (seTime s) >= som) statEvents
      statsByMonth = calculateDocStatsFromEventsByMonth statEvents
  content <- adminUserStatisticsPage $ do
    fieldFL "statisticsbyday" $ statisticsFieldsByDay statsByDay
    fieldFL "statisticsbymonth" $ statisticsFieldsByMonth statsByMonth
  renderFromBody TopEmpty kontrakcja content

handleDocStatsCSV :: Kontrakcja m => m Response
handleDocStatsCSV = onlySuperUser $ do
  stats <- runDBQuery GetDocStatEvents
  Log.debug $ "All doc stats length: " ++ (show $ length stats)
  ok $ setHeader "Content-Disposition" "attachment;filename=stats.csv"
     $ setHeader "Content-Type" "text/csv"
     $ toResponse (statisticsCSV stats)

{- |
   What a beast! This must be stopped! Oh, the humanity!
 -}
convertUserIDToFullName :: Kontrakcja m => [User] -> [(Int, UserID, Int, Int, Int)] -> m [(Int, String, Int, Int, Int)]
convertUserIDToFullName _ [] = return []
convertUserIDToFullName acc ((a,UserID 0,b,c,d):ss) = do
  rst <- convertUserIDToFullName acc ss
  return $ (a, "Total", b, c, d) : rst
convertUserIDToFullName acc ((a,uid,b,c,d):ss) =  
  case find (\u->userid u == uid) acc of
    Just u -> do
      rst <- convertUserIDToFullName acc ss
      return $ (a, BS.toString $ getSmartName u, b, c, d) : rst
    Nothing -> do
      mu <- runDBQuery $ GetUserByID uid
      case mu of
        Nothing -> do
          rst <- convertUserIDToFullName acc ss
          return $ (a, "Unknown user", b, c, d) : rst
        Just u -> do
          rst <- convertUserIDToFullName (u:acc) ss
          return $ (a, BS.toString $ getSmartName u, b, c, d) : rst

addStats :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addStats (_, t1, s1, i1) (t, t2, s2, i2) = (t, t1+t2, s1+s2, i1+i2)

sumStats :: [(Int, Int, Int, Int)] -> (Int, Int, Int, Int)
sumStats = foldl1 addStats

addCStats :: (Int, UserID, Int, Int, Int) -> (Int, UserID, Int, Int, Int) -> (Int, UserID, Int, Int, Int)
addCStats (_, _, t1, s1, i1) (t, uid, t2, s2, i2) = (t, uid, t1+t2, s1+s2, i1+i2)

sumCStats :: [(Int, UserID, Int, Int, Int)] -> (Int, UserID, Int, Int, Int)
sumCStats = foldl1 addCStats


statEventToDocStatTuple :: DocStatEvent -> Maybe (Int, Int, Int, Int)
statEventToDocStatTuple (DocStatEvent {seTime, seQuantity, seAmount}) = case seQuantity of
  DocStatClose       -> Just (asInt seTime, seAmount, 0, 0)
  DocStatEmailSignatures  -> Just (asInt seTime, 0, seAmount, 0)
  DocStatElegSignatures   -> Just (asInt seTime, 0, seAmount, 0)
  DocStatSend        -> Just (asInt seTime, 0, 0, seAmount)
  _                       -> Nothing

statCompanyEventToDocStatTuple :: DocStatEvent -> Maybe (Int, UserID, Int, Int, Int)
statCompanyEventToDocStatTuple (DocStatEvent {seTime, seUserID, seQuantity, seAmount}) = case seQuantity of
  DocStatClose       -> Just (asInt seTime, seUserID, seAmount, 0, 0)
  DocStatEmailSignatures  -> Just (asInt seTime, seUserID, 0, seAmount, 0)
  DocStatElegSignatures   -> Just (asInt seTime, seUserID, 0, seAmount, 0)
  DocStatSend        -> Just (asInt seTime, seUserID, 0, 0, seAmount)
  _                       -> Nothing


-- | User Stats are very simple:
-- Date
-- # of documents Closed that date
-- # of signatures on documents closed that date
-- # of documents sent that date
calculateDocStatsFromEventsByDay :: [DocStatEvent] -> [(Int, Int, Int, Int)]
calculateDocStatsFromEventsByDay events =
  let byDay = groupWith (\(a,_,_,_)->a) $ reverse $ sortWith (\(a,_,_,_)->a) (catMaybes $ map statEventToDocStatTuple events)
  in map sumStats byDay

-- | User Stats are very simple:
-- Date
-- # of documents Closed that date
-- # of signatures on documents closed that date
-- # of documents sent that date
calculateDocStatsFromEventsByMonth :: [DocStatEvent] -> [(Int, Int, Int, Int)]
calculateDocStatsFromEventsByMonth events =
  let stats  = catMaybes $ map statEventToDocStatTuple events
      monthOnly = [(100 * (a `div` 100), b, c, d) | (a, b, c, d) <- stats]
      byMonth = groupWith (\(a,_,_,_)->a) $ reverse $ sortWith (\(a,_,_,_)->a) monthOnly
  in map sumStats byMonth


-- | Company Stats are very simple:
-- Date
-- UserID
-- # of documents Closed that date
-- # of signatures on documents closed that date
-- # of documents sent that date
calculateDocStatsFromCompanyEvents :: [DocStatEvent] -> [(Int, UserID, Int, Int, Int)]
calculateDocStatsFromCompanyEvents events =
  let byDay = groupWith (\(a,_,_,_,_)->a) $ reverse $ sortWith (\(a,_,_,_,_)->a) (catMaybes $ map statCompanyEventToDocStatTuple events)
      byUser = map (groupWith (\(_,a,_,_,_)->a) . sortWith (\(_,a,_,_,_)->a)) byDay
      userTotalsByDay = map (map sumCStats) byUser
      setUID0 (a,_,c,d,e) = (a,UserID 0,c,d,e)
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
      let did = documentid doc
          sendtime = getInviteTime doc
          sigs = countSignatures doc          
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
      let did = documentid doc
          sendtime = getInviteTime doc
          sigs = countSignatures doc          
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = sendtime
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

addDocumentRejectStatEvents :: Kontrakcja m => Document -> m Bool
addDocumentRejectStatEvents doc = msum [
  do
    if not $ isRejected doc then return False
      else do
      sl  <- guardJust $ getAuthorSigLink doc
      uid <- guardJust $ maybesignatory sl
      let did = documentid doc
          sendtime = getInviteTime doc
          sigs = countSignatures doc          
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = sendtime
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

addDocumentCreateStatEvents :: Kontrakcja m => Document -> m Bool
addDocumentCreateStatEvents doc = msum [
  do
    if isNothing $ documentinvitetime doc then return False
      else do
      sl  <- guardJust $ getAuthorSigLink doc
      uid <- guardJust $ maybesignatory sl
      let did = documentid doc
          sendtime = getInviteTime doc
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = sendtime
                                                        , seQuantity   = DocStatCreate
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        , seCompanyID  = maybecompany sl
                                                        , seServiceID  = documentservice doc
                                                        , seDocumentType = documenttype doc
                                                        }
      unless a $ Log.stats $ "Skipping existing doccument stat for docid: " ++ show did ++ " and quantity: " ++ show DocStatCreate          
      return a
  , return False]

addAllDocsToStats :: Kontrakcja m => m KontraLink
addAllDocsToStats = onlySuperUser $ do
  services <- runDBQuery GetServices
  let allservices = Nothing : map (Just . serviceid) services
  _ <- forM allservices $ \s -> do
    docs <- query $ GetDocuments s
    _ <- mapM addDocumentSendStatEvents docs
    _ <- mapM addDocumentCloseStatEvents docs
    _ <- mapM addDocumentCreateStatEvents docs
    _ <- mapM addDocumentRejectStatEvents docs
    _ <- mapM addDocumentCancelStatEvents docs
    return ()
  addFlash (OperationDone, "Added all docs to stats")
  return LinkUpload

handleMigrate1To2 :: Kontrakcja m => m KontraLink
handleMigrate1To2 = onlySuperUser $ do
  _ <- runDBUpdate FlushStats
  _ <- addAllDocsToStats
  addFlash (OperationDone, "Table migrated")
  return LinkUpload

