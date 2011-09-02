module Stats.Control 
       (
         showAdminUserUsageStats,
         addDocumentCloseStatEvents,
         addDocumentSendStatEvents,
         addAllDocsToStats
       )
       
       where

import Stats.Model
import Stats.View
import AppView
import DB.Classes
import User.Model
import Kontra
import MinutesTime
import Misc
import User.UserControl
import Administration.AdministrationView
import Data.Maybe
import Doc.DocStateData
import Doc.DocInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import qualified AppLogger as Log
import KontraLink
import Doc.DocState
import Util.FlashUtil

import Happstack.Server
import Happstack.State
import Control.Monad

showAdminUserUsageStats :: Kontrakcja m => UserID -> m Response
showAdminUserUsageStats userid = onlySuperUser $ do
  statEvents <- runDBQuery $ GetDocStatEventsByUserID userid
  Just user <- runDBQuery $ GetUserByID userid
  mcompany <- getCompanyForUser user
  let stats = calculateDocStatsFromEvents statEvents
  content <- adminUserUsageStatsPage user mcompany $ do
    statisticsFieldsForASingleUser stats
  renderFromBody TopEmpty kontrakcja content

addStats :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addStats (_, t1, s1, i1) (t, t2, s2, i2) = (t, t1+t2, s1+s2, i1+i2)

sumStats :: [(Int, Int, Int, Int)] -> (Int, Int, Int, Int)
sumStats = foldl1 addStats

statEventToDocStatTuple :: DocStatEvent -> Maybe (Int, Int, Int, Int)
statEventToDocStatTuple (DocStatEvent {seTime, seQuantity, seAmount}) = case seQuantity of
  DocStatClose       -> Just (asInt seTime, seAmount, 0, 0)
  DocStatSignatures  -> Just (asInt seTime, 0, seAmount, 0)
  DocStatSend        -> Just (asInt seTime, 0, 0, seAmount)
  -- add more here when they exist, probably returning Nothing if they aren't about docs
  -- one day, this may be needed:
  -- _                       -> Nothing

-- | Stats are very simple:
-- Date
-- # of documents Closed that date
-- # of signatures on documents closed that date
-- # of documents sent that date
calculateDocStatsFromEvents :: [DocStatEvent] -> [(Int, Int, Int, Int)]
calculateDocStatsFromEvents events =
  let byDay = groupWith (\(a,_,_,_)->a) $ reverse $ sortWith (\(a,_,_,_)->a) (catMaybes $ map statEventToDocStatTuple events)
  in map sumStats byDay

-- some utility functions

-- | Two stats get created for a document close:
--   1. Count 1 for StatDocumentClose
--   2. Count the number of signatures in StatDocumentSignatures
-- Note that this will roll over (using mplus) in case there is
-- an error.
addDocumentCloseStatEvents :: Kontrakcja m => Document -> m Bool
addDocumentCloseStatEvents doc = msum [
  do
    if not $ isClosed doc then return False
      else do
      sl  <- guardJust $ getAuthorSigLink doc
      uid <- guardJust $ maybesignatory sl
      let did = documentid doc
          sigs = countSignatures doc
          signtime = getLastSignedTime doc
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = signtime
                                                        , seQuantity   = DocStatClose
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        }
      unless a $ Log.stats $ "Could not save close stat for " ++ show did
      b <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = signtime
                                                        , seQuantity   = DocStatSignatures
                                                        , seAmount     = sigs
                                                        , seDocumentID = did
                                                        }
      unless b $ Log.stats $ "Could not save signatures stat for " ++ show did
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
      a <- runDBUpdate $ AddDocStatEvent $ DocStatEvent { seUserID     = uid
                                                        , seTime       = sendtime
                                                        , seQuantity   = DocStatSend
                                                        , seAmount     = 1
                                                        , seDocumentID = did
                                                        }
      unless a $ Log.stats $ "Could not save send stat for " ++ show did
      return a
  , return False]

addAllDocsToStats :: Kontrakcja m => m KontraLink
addAllDocsToStats = onlySuperUser $ do
  docs <- query $ GetDocuments Nothing
  _ <- mapM addDocumentSendStatEvents docs
  _ <- mapM addDocumentCloseStatEvents docs
  addFlash (OperationDone, "Added all docs to stats")
  return LinkMain
