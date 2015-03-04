module EvidenceLog.Migrations where

import Data.Monoid
import Data.Maybe
import Data.Char
import Data.Int
import Data.String.Utils
import Control.Monad

import DB
import EvidenceLog.Tables
import Text.XML.HaXml(render)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty(content)
import qualified Text.XML.HaXml.Types as XML
import Text.HTML.TagSoup.Entity

import Utils.String


dropHTMLFromMessagesInEvidenceLog :: MonadDB m => Migration m
dropHTMLFromMessagesInEvidenceLog = Migration {
  mgrTable = tableEvidenceLog
, mgrFrom = 5
, mgrDo = do
    runSQL_ "SELECT id, message_text FROM evidence_log WHERE message_text IS NOT NULL AND message_text <> '' AND event_type IN (42,46,68)"
    (evidences_with_message :: [(Int64,String)]) <- fetchMany id
    forM_ evidences_with_message $ \(eid, message) -> do
      runQuery_ . sqlUpdate "evidence_log" $ do
           sqlSet "message_text" $ fixMessage $ message
           sqlWhereEq "id" eid
  }
  where
    fixMessage :: String -> String
    fixMessage xs = strip $ case xmlParse' "Migration-evidence-drop html" $ "<span>" ++ xs ++ "</span>" of
       (Right (XML.Document _ _ (XML.Elem _ _ cs) _)) -> (concatMap fixContent cs)
       _ -> let xsWithFixedBRs = replace "<BR>" "<BR/>" $ replace "<br>" "<br/>" xs
            in if xsWithFixedBRs /= xs
               then fixMessage xsWithFixedBRs
               else unescapeHTML $ justText xs
    fixContent :: XML.Content Posn -> String
    fixContent (XML.CElem (XML.Elem (XML.N "div") _ cs) _) = (concatMap fixContent cs) ++ " \n"
    fixContent (XML.CElem (XML.Elem (XML.N "p") _ cs) _)   = (concatMap fixContent cs) ++ " \n"
    fixContent (XML.CElem (XML.Elem (XML.N "br") _ cs) _)  = (concatMap fixContent cs) ++ " \n"
    fixContent (XML.CElem (XML.Elem (XML.N _) _ cs) _)     = (concatMap fixContent cs)
    fixContent x@(XML.CString _ _ _)                       = render $ content x
    fixContent (XML.CRef  (XML.RefEntity ent) _)           = fromMaybe "" $ lookupEntity ent
    fixContent (XML.CRef  (XML.RefChar i) _)               = [chr i]
    fixContent _ = ""
    justText ('<':cs) = justText $ drop 1 $ dropWhile (/= '>') cs
    justText (c:cs) = c : justText cs
    justText [] = []



evidenceLogAddActor :: MonadDB m => Migration m
evidenceLogAddActor =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 4
  , mgrDo = do
    runSQL_ $ "ALTER TABLE evidence_log ADD COLUMN actor TEXT NOT NULL DEFAULT ''"
    runSQL_ $ "ALTER TABLE evidence_log ALTER COLUMN actor DROP DEFAULT"
  }

evidenceLogFixColumns :: MonadDB m => Migration m
evidenceLogFixColumns =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 3
  , mgrDo = do
    -- ip addresses are 32bits long
    runSQL_ $ "ALTER TABLE evidence_log ALTER COLUMN request_ip_v4 TYPE INTEGER," <>
                                        -- this is completely useless
                                        "DROP COLUMN request_ip_v6," <>

                                        "ALTER COLUMN email TYPE TEXT," <>
                                        "ALTER COLUMN text TYPE TEXT," <>
                                        "ALTER COLUMN version_id TYPE TEXT," <>
                                        "ALTER COLUMN api_user TYPE TEXT," <>
                                        "ALTER COLUMN message_text TYPE TEXT," <>
                                        "ALTER COLUMN client_name TYPE TEXT"
  }

addClientTimeNameToEvidenceLog :: MonadDB m => Migration m
addClientTimeNameToEvidenceLog =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 2
  , mgrDo = do
      runSQL_ "ALTER TABLE evidence_log ADD COLUMN client_time TIMESTAMPTZ NULL"
      runSQL_ "ALTER TABLE evidence_log ADD COLUMN client_name VARCHAR NULL"
  }

expandEventsWithAffectedSignatoryAndTextMessage :: MonadDB m => Migration m
expandEventsWithAffectedSignatoryAndTextMessage =
  Migration {
    mgrTable = tableEvidenceLog
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE evidence_log ADD COLUMN affected_signatory_link_id BIGINT NULL"
      runSQL_ "ALTER TABLE evidence_log ADD COLUMN message_text   VARCHAR NULL"
  }
