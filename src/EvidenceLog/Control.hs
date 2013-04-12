{-# LANGUAGE CPP #-}
module EvidenceLog.Control
       (
       jsonDocumentEvidenceLog
       )
       where

import Kontra
import DB
import Doc.DocStateData
import Doc.DocumentID
import Util.MonadUtils

import Text.JSON
import Text.JSON.Gen
import Utils.Prelude

import ListUtil
import Doc.DocStateQuery
import EvidenceLog.View
import EvidenceLog.Model

jsonDocumentEvidenceLog ::  Kontrakcja m => DocumentID -> m JSValue
jsonDocumentEvidenceLog did = do
  doc <- guardRightM' $ getDocByDocID did
  evidenceLog <- dbQuery $ GetEvidenceLog $ documentid doc
  events <- eventsJSListFromEvidenceLog  doc (reverse evidenceLog)
  runJSONGenT $ do
      value "list" $ for events $ runJSONGen . (value "fields")
      value "paging" $ pagingParamsJSON (PagedList events 1000 emptyListParams (length events))
