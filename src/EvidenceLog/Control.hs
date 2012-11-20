{-# LANGUAGE CPP #-}
module EvidenceLog.Control
       (
       jsonDocumentEvidenceLog
       )
       where

import KontraLink
import Kontra
import DB
import Doc.DocStateData
import User.Model
import User.Utils
import Util.MonadUtils

import Text.JSON
import Text.JSON.Gen
import Utils.Prelude

import ListUtil
import Doc.DocStateQuery
import EvidenceLog.View
import EvidenceLog.Model

jsonDocumentEvidenceLog ::  Kontrakcja m => DocumentID -> m (Either KontraLink JSValue)
jsonDocumentEvidenceLog did = withUserGet $ do
  ctx <- getContext  
  doc <- guardRightM' $ getDocByDocID did
  evidenceLog <- dbQuery $ GetEvidenceLog $ documentid doc
  events <- eventsJSListFromEvidenceLog  (timeLocaleForLang $ ctxlang $ ctx) (ctxtime ctx) doc (reverse evidenceLog)
  runJSONGenT $ do
      value "list" $ for events $ runJSONGen . (value "fields")
      value "paging" $ pagingParamsJSON (PagedList events 1000 emptyListParams)
            

