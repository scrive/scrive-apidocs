module Stats.Model
       (
         AddDocStatEvent(..),
         DocStatEvent(..),
         DocStatQuantity(..),
         FlushDocStats(..),
         GetDocStatEvents(..),
         GetDocStatEventsByCompanyID(..),
         GetDocStatEventsByUserID(..),
         UserStatEvent(..),
         AddUserStatEvent(..),
         GetUserStatEvents(..),
         UserStatQuantity(..)
       )

       where

import Database.HDBC

import DB.Classes
import DB.Derive
import DB.Utils
import MinutesTime
import User.Model
import Doc.DocStateData
import Company.Model
import API.Service.Model

-- | A named quantity in the statistics events
-- please maintain the order on this (only add to the bottom)
data DocStatQuantity = DocStatClose       -- ^ A Close Document event
                     | DocStatEmailSignatures  -- ^ The number of email signatories in a closed document
                     | DocStatSend        -- ^ A Send Document event
                     | DocStatElegSignatures
                     | DocStatCreate
                     | DocStatEmailSignaturePending
                     | DocStatElegSignaturePending
                     | DocStatCancel
                     | DocStatEmailSignatureCancel
                     | DocStatElegSignatureCancel
                     | DocStatReject
                     | DocStatEmailSignatureReject
                     | DocStatElegSignatureReject
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocStatQuantity)

-- | An even to be logged for statistical purposes
data DocStatEvent = DocStatEvent { seUserID     :: UserID          -- ^ User who generated the event
                                 , seTime       :: MinutesTime     -- ^ The time of the event
                                 , seQuantity   :: DocStatQuantity -- ^ The type of event
                                 , seAmount     :: Int             -- ^ The value of the event
                                 , seDocumentID :: DocumentID      -- ^ If the event is related to a document, it's this doc
                                 , seServiceID :: Maybe ServiceID
                                 , seCompanyID :: Maybe CompanyID
                                 , seDocumentType :: DocumentType
                                 }

data UserStatQuantity = UserSignTOS  -- When user signs TOS
                        | UserSaveAfterSign -- when user accepts the save option after signing
                        | UserRefuseSaveAfterSign -- when user refuses the save option after signing
                        | UserPhoneAfterTOS -- when a user requests a phone call after accepting the TOS
                      deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''UserStatQuantity)

data UserStatEvent = UserStatEvent { usUserID    :: UserID
                                   , usTime      :: MinutesTime
                                   , usQuantity  :: UserStatQuantity
                                   , usAmount    :: Int
                                   , usServiceID :: Maybe ServiceID
                                   , usCompanyID :: Maybe CompanyID
                                   }

selectDocStatEventsSQL :: String
selectDocStatEventsSQL = "SELECT "
 ++ "  e.user_id"
 ++ ", e.time"
 ++ ", e.quantity"
 ++ ", e.amount"
 ++ ", e.document_id"
 ++ ", e.service_id"
 ++ ", e.company_id"
 ++ ", e.document_type"
 ++ "  FROM doc_stat_events e"
 ++ " " -- always end in space to avoid problems

selectUserStatEventsSQL :: String
selectUserStatEventsSQL = "SELECT "
 ++ "  e.user_id"
 ++ ", e.time"
 ++ ", e.quantity"
 ++ ", e.amount"
 ++ ", e.service_id"
 ++ ", e.company_id"
 ++ "  FROM user_stat_events e"
 ++ " " -- always end in space to avoid problems


data GetDocStatEvents = GetDocStatEvents
instance DBQuery GetDocStatEvents [DocStatEvent] where
  dbQuery GetDocStatEvents = wrapDB $ \conn -> do
    st <- prepare conn $ selectDocStatEventsSQL
    _ <- execute st []
    fetchDocStats st []

data GetDocStatEventsByUserID = GetDocStatEventsByUserID UserID
instance DBQuery GetDocStatEventsByUserID [DocStatEvent] where
  dbQuery (GetDocStatEventsByUserID userid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectDocStatEventsSQL
      ++ " WHERE e.user_id = ?"
    _ <- execute st [toSql userid]
    fetchDocStats st []

data AddDocStatEvent = AddDocStatEvent DocStatEvent
instance DBUpdate AddDocStatEvent Bool where
  dbUpdate (AddDocStatEvent event) = wrapDB $ \conn -> do
    runRaw conn "LOCK TABLE doc_stat_events IN ACCESS EXCLUSIVE MODE"
    selectSt <- prepare conn $ selectDocStatEventsSQL
      ++ " WHERE e.document_id = ? "
      ++ " AND   e.quantity    = ? "
    _ <- execute selectSt [toSql $ seDocumentID event
                          ,toSql $ seQuantity event]
    stats <- fetchDocStats selectSt []
    case stats of
      [] -> do
        st <- prepare conn $ "INSERT INTO doc_stat_events ("
              ++ "  user_id"
              ++ ", time"
              ++ ", quantity"
              ++ ", amount"
              ++ ", document_id"
              ++ ", service_id"
              ++ ", company_id"
              ++ ", document_type"
              ++ ") VALUES (?, to_timestamp(?), ?, ?, ?, ?, ?, ?)"
        r <- execute st [toSql $ seUserID event
                        ,toSql $ seTime event
                        ,toSql $ seQuantity event
                        ,toSql $ seAmount event
                        ,toSql $ unDocumentID $ seDocumentID event
                        ,toSql $ seServiceID event
                        ,toSql $ seCompanyID event
                        ,toSql $ show $ seDocumentType event]
        oneRowAffectedGuard r
      _ -> return False

data GetUserStatEvents = GetUserStatEvents
instance DBQuery GetUserStatEvents [UserStatEvent] where
  dbQuery GetUserStatEvents = wrapDB $ \conn -> do
    st <- prepare conn $ selectUserStatEventsSQL
    _ <- execute st []
    fetchUserStats st []

data AddUserStatEvent = AddUserStatEvent UserStatEvent
instance DBUpdate AddUserStatEvent Bool where
  dbUpdate (AddUserStatEvent event) = wrapDB $ \conn -> do
    runRaw conn "LOCK TABLE user_stat_events IN ACCESS EXCLUSIVE MODE"
    selectSt <- prepare conn $ selectUserStatEventsSQL
      ++ " WHERE e.user_id = ? "
      ++ " AND   e.quantity    = ? "
    _ <- execute selectSt [toSql $ usUserID event
                          ,toSql $ usQuantity event]
    stats <- fetchUserStats selectSt []
    case stats of
      [] -> do
        st <- prepare conn $ "INSERT INTO user_stat_events ("
              ++ "  user_id"
              ++ ", time"
              ++ ", quantity"
              ++ ", amount"
              ++ ", service_id"
              ++ ", company_id"
              ++ ") VALUES (?, to_timestamp(?), ?, ?, ?, ?)"
        r <- execute st [toSql $ usUserID event
                        ,toSql $ usTime event
                        ,toSql $ usQuantity event
                        ,toSql $ usAmount event
                        ,toSql $ usServiceID event
                        ,toSql $ usCompanyID event]
        oneRowAffectedGuard r
      _ -> return False

fetchDocStats :: Statement -> [DocStatEvent] -> IO [DocStatEvent]
fetchDocStats st acc = fetchRow st >>= maybe (return acc) f
  where f [uid, time, quantity, amount, documentid, serviceid, companyid, documenttype] =
          fetchDocStats st $ DocStatEvent { seUserID       = fromSql uid
                                          , seTime         = fromSql time
                                          , seQuantity     = fromSql quantity
                                          , seAmount       = fromSql amount
                                          , seDocumentID   = DocumentID (fromSql documentid)
                                          , seServiceID    = fromSql serviceid
                                          , seCompanyID    = fromSql companyid
                                          , seDocumentType = doctypeFromString $ fromSql documenttype
                                          } : acc
        f l = error $ "fetchDocStats: unexpected row: "++show l

fetchUserStats :: Statement -> [UserStatEvent] -> IO [UserStatEvent]
fetchUserStats st acc = fetchRow st >>= maybe (return acc) f
  where f [uid, time, quantity, amount, serviceid, companyid] =
          fetchUserStats st $ UserStatEvent { usUserID       = fromSql uid
                                            , usTime         = fromSql time
                                            , usQuantity     = fromSql quantity
                                            , usAmount       = fromSql amount
                                            , usServiceID    = fromSql serviceid
                                            , usCompanyID    = fromSql companyid
                                            } : acc
        f l = error $ "fetchUserStats: unexpected row: "++show l


data GetDocStatEventsByCompanyID = GetDocStatEventsByCompanyID CompanyID
instance DBQuery GetDocStatEventsByCompanyID [DocStatEvent] where
  dbQuery (GetDocStatEventsByCompanyID companyid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectDocStatEventsSQL
      ++ " WHERE e.company_id = ?"
    _ <- execute st [toSql companyid]
    fetchDocStats st []

data FlushDocStats = FlushDocStats
instance DBUpdate FlushDocStats Bool where
  dbUpdate FlushDocStats = wrapDB $ \conn -> do
    st <- prepare conn $ "DELETE FROM doc_stat_events"
    _ <- execute st []
    return True

