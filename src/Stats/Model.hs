module Stats.Model 
       (
         AddDocStatCompanyEvent(..),
         AddDocStatEvent(..),
         DocStatCompanyEvent(..),
         DocStatEvent(..),
         DocStatQuantity(..),
         GetDocStatCompanyEventsByCompanyID(..),
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

-- | A named quantity in the statistics events
-- please maintain the order on this
data DocStatQuantity = DocStatClose       -- ^ A Close Document event
                     | DocStatSignatures  -- ^ The number of signatories in a closed document
                     | DocStatSend        -- ^ A Send Document event
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocStatQuantity)

-- | An even to be logged for statistical purposes
data DocStatEvent = DocStatEvent { seUserID     :: UserID          -- ^ User who generated the event
                                 , seTime       :: MinutesTime     -- ^ The time of the event
                                 , seQuantity   :: DocStatQuantity -- ^ The type of event
                                 , seAmount     :: Int             -- ^ The value of the event
                                 , seDocumentID :: DocumentID      -- ^ If the event is related to a document, it's this doc
                                 }

-- | An even to be logged for statistical purposes
data DocStatCompanyEvent = DocStatCompanyEvent { secCompanyID  :: CompanyID
                                               , secUserID     :: UserID          -- ^ User who generated the event
                                               , secTime       :: MinutesTime     -- ^ The time of the event
                                               , secQuantity   :: DocStatQuantity -- ^ The type of event
                                               , secAmount     :: Int             -- ^ The value of the event
                                               , secDocumentID :: DocumentID      -- ^ If the event is related to a document, it's this doc
                                               }


selectDocStatEventsSQL :: String
selectDocStatEventsSQL = "SELECT "
 ++ "  e.user_id"
 ++ ", e.time"
 ++ ", e.quantity"
 ++ ", e.amount"
 ++ ", e.document_id"
 ++ "  FROM doc_stat_events e"
 ++ " " -- always end in space to avoid problems
 

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
      ++ ") VALUES (?, to_timestamp(?), ?, ?, ?)"
    r <- execute st [toSql $ seUserID event
                    ,toSql $ seTime event
                    ,toSql $ seQuantity event
                    ,toSql $ seAmount event
                    ,toSql $ unDocumentID $ seDocumentID event]
    oneRowAffectedGuard r

fetchDocStats :: Statement -> [DocStatEvent] -> IO [DocStatEvent]
fetchDocStats st acc = fetchRow st >>= maybe (return acc)
  (\[uid, time, quantity, amount, documentid] -> 
    fetchDocStats st $ DocStatEvent { seUserID     = fromSql uid
                                    , seTime       = fromSql time
                                    , seQuantity   = fromSql quantity
                                    , seAmount     = fromSql amount
                                    , seDocumentID = DocumentID (fromSql documentid)
                                    } : acc)

selectDocStatCompanyEventsSQL :: String
selectDocStatCompanyEventsSQL = "SELECT "
 ++ "  e.company_id"
 ++ ", e.user_id"
 ++ ", e.time"
 ++ ", e.quantity"
 ++ ", e.amount"
 ++ ", e.document_id"
 ++ "  FROM doc_stat_company_events e"
 ++ " " -- always end in space to avoid problems

data GetDocStatCompanyEventsByCompanyID = GetDocStatCompanyEventsByCompanyID CompanyID
instance DBQuery GetDocStatCompanyEventsByCompanyID [DocStatCompanyEvent] where
  dbQuery (GetDocStatCompanyEventsByCompanyID companyid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectDocStatCompanyEventsSQL
      ++ " WHERE e.company_id = ?"
    _ <- execute st [toSql companyid]
    fetchDocStatsCompany st []
    
data AddDocStatCompanyEvent = AddDocStatCompanyEvent DocStatCompanyEvent
instance DBUpdate AddDocStatCompanyEvent Bool where
  dbUpdate (AddDocStatCompanyEvent event) = wrapDB $ \conn -> do
    st <- prepare conn $ "INSERT INTO doc_stat_company_events ("
      ++ "  company_id"
      ++ ", user_id"
      ++ ", time"
      ++ ", quantity"
      ++ ", amount"
      ++ ", document_id"
      ++ ") VALUES (?, ?, to_timestamp(?), ?, ?, ?)"
    r <- execute st [toSql $ secCompanyID event
                    ,toSql $ secUserID event
                    ,toSql $ secTime event
                    ,toSql $ secQuantity event
                    ,toSql $ secAmount event
                    ,toSql $ unDocumentID $ secDocumentID event]
    oneRowAffectedGuard r

fetchDocStatsCompany :: Statement -> [DocStatCompanyEvent] -> IO [DocStatCompanyEvent]
fetchDocStatsCompany st acc = fetchRow st >>= maybe (return acc)
  (\[cid, uid, time, quantity, amount, documentid] -> 
    fetchDocStatsCompany st $ DocStatCompanyEvent { secCompanyID  = fromSql cid
                                                  , secUserID     = fromSql uid
                                                  , secTime       = fromSql time
                                                  , secQuantity   = fromSql quantity
                                                  , secAmount     = fromSql amount
                                                  , secDocumentID = DocumentID (fromSql documentid)
                                                  } : acc)
