module EvidenceLog.Control
       (
       jsonDocumentEvidenceLog
       )
       where

import Text.JSON
import Text.JSON.Gen

import DB
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import EvidenceLog.Model
import EvidenceLog.View
import Kontra
import KontraPrelude
import ListUtil
import Utils.Prelude

jsonDocumentEvidenceLog ::  Kontrakcja m => DocumentID -> m JSValue
jsonDocumentEvidenceLog did = do
  doc <- getDocByDocID did
  evidenceLog <- dbQuery $ GetEvidenceLog $ documentid doc
  events <- eventsJSListFromEvidenceLog  doc evidenceLog
  runJSONGenT $ do
      value "list" $ for (reverse events) $ runJSONGen . (value "fields")
      value "paging" $ pagingParamsJSON (PagedList events 1000 emptyListParams (length events))
