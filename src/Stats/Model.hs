module Stats.Model 
       (
         AddDocStatEvent(..),
         DocStatEvent(..),
         DocStatQuantity(..),
         FlushStats(..),
         GetDocStatEvents(..),
         GetDocStatEventsByCompanyID(..),
         GetDocStatEventsByUserID(..)
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
-- please maintain the order on this
data DocStatQuantity = DocStatClose       -- ^ A Close Document event
                     | DocStatEmailSignatures  -- ^ The number of email signatories in a closed document
                     | DocStatSend        -- ^ A Send Document event
                     | DocStatElegSignatures
                     | DocStatCreate                       
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
                    ,toSql $ show $ seDocumentType]
    oneRowAffectedGuard r

fetchDocStats :: Statement -> [DocStatEvent] -> IO [DocStatEvent]
fetchDocStats st acc = fetchRow st >>= maybe (return acc)
  (\[uid, time, quantity, amount, documentid, serviceid, companyid, documenttype] -> 
    fetchDocStats st $ DocStatEvent { seUserID       = fromSql uid
                                    , seTime         = fromSql time
                                    , seQuantity     = fromSql quantity
                                    , seAmount       = fromSql amount
                                    , seDocumentID   = DocumentID (fromSql documentid)
                                    , seServiceID    = fromSql serviceid
                                    , seCompanyID    = fromSql companyid
                                    , seDocumentType = doctypeFromString $ fromSql documenttype
                                    } : acc)

data GetDocStatEventsByCompanyID = GetDocStatEventsByCompanyID CompanyID
instance DBQuery GetDocStatEventsByCompanyID [DocStatEvent] where
  dbQuery (GetDocStatEventsByCompanyID companyid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectDocStatEventsSQL
      ++ " WHERE e.company_id = ?"
    _ <- execute st [toSql companyid]
    fetchDocStats st []

data FlushStats = FlushStats
instance DBUpdate FlushStats Bool where
  dbUpdate FlushStats = wrapDB $ \conn -> do
    st <- prepare conn $ "DELETE FROM doc_stat_events"
    _ <- execute st []
    return True
    
