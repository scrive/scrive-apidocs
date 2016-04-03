module EvidenceLog.Migrations where

import Data.Char
import Data.Int
import Data.String.Utils
import Text.HTML.TagSoup.Entity
import Text.XML.HaXml(render)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty(content)
import qualified Text.XML.HaXml.Types as XML

import DB
import EvidenceLog.Tables
import KontraPrelude
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
    fixMessage xs = strip $ replace "\160" " " $ case xmlParse' "Migration-evidence-drop html" $ "<span>" ++ xs ++ "</span>" of
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
