module EvidenceLog.Model 
       (
         EvidenceEventType(..),
         InsertEvidenceEvent(..)
       )
       
       where

import qualified Paths_kontrakcja as Paths

import Doc.DocStateData
import User.UserID
import DB.Derive

import DB.Classes
import DB.Utils
import MinutesTime

import Database.HDBC
import Misc
import Data.List

import Database.HDBC.PostgreSQL
import Data.Version (Version(..))

-- | A machine-readable event code for different types of events.
data EvidenceEventType = A
                       | B
                       | C
                       deriving (Eq, Show, Read, Ord)
$(enumDeriveConvertible ''EvidenceEventType)

insertEvidenceEvent' :: Maybe DocumentID 
                        -> Maybe UserID
                        -> Maybe String
                        -> MinutesTime
                        -> Maybe IPAddress
                        -> Maybe IPAddress
                        -> Maybe SignatoryLinkID
                        -> String
                        -> EvidenceEventType
                        -> String -- versionid
                        -> Connection
                        -> IO Bool
insertEvidenceEvent' mdid muid meml time mip4 mip6 mslid text event vid conn = do
  st <- prepare conn $ "INSERT INTO evidence_log ("
        ++ "  document_id"
        ++ ", user_id"
        ++ ", email"
        ++ ", time"
        ++ ", request_ip_v4"
        ++ ", request_ip_v6"
        ++ ", signatory_link_id"
        ++ ", text"
        ++ ", event_type"
        ++ ", version_id"
        ++ ") VALUES (?,?,?,?,?,?,?,?,?,?)"
  res <- execute st [toSql mdid
                    ,toSql muid
                    ,toSql meml
                    ,toSql time
                    ,toSql mip4
                    ,toSql mip6
                    ,toSql mslid
                    ,toSql text
                    ,toSql event
                    ,toSql vid]
  oneRowAffectedGuard res

versionID :: String
versionID = concat $ intersperse "." $ versionTags Paths.version

data InsertEvidenceEvent = InsertEvidenceEvent 
                             EvidenceEventType       -- A code for the event
                             String                  -- The textual representation of the event
                             MinutesTime             -- The time of the event
                             (Maybe DocumentID)      -- The documentid if this event is about a document
                             (Maybe UserID)          -- If this event was enacted by a user
                             (Maybe String)          -- The email address of the person enacting this event, if known
                             (Maybe SignatoryLinkID) -- The signatorylinkid of the person enacting this event, if known
                             (Maybe IPAddress)       -- The ipv4 address of the request that enacted this event, if there was one
                             (Maybe IPAddress)       -- The ipv6 address of the request that enacted thsi event, if there was one
instance DBUpdate InsertEvidenceEvent Bool where
  dbUpdate (InsertEvidenceEvent tp txt tm mdid muid meml mslid mip4 mip6) = 
    wrapDB (insertEvidenceEvent' mdid muid meml tm mip4 mip6 mslid txt tp versionID)
