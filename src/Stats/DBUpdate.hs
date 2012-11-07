-- | Types representing various stat insertions, and DBUpdate instances for
--   each. This differs from the StatEvent types in that each type only
--   represents an action, which is carried out entirely on the SQL server.
module Stats.DBUpdate (
    StatUpdate,
    StatEvt (..),
    statUpdate,
    docStatClose,
    docStatSignMethod,
    docStatSend,
    docStatCancel,
    docStatReject,
    docStatCreate,
    docStatTimeout
  ) where
import Database.HDBC
import DB.Env
import DB.SQL
import DB.Core
import DB.Functions
import Doc.DocumentID
import Stats.Model
import Doc.DocStateData

type APIString = String
type StatQuery = DocumentID -> APIString -> SQL
data StatUpdate = StatUpdate StatQuery DocumentID APIString

-- | Some functions are very similar, differing only in whether they're
--   recording a stat for a closed, pending, etc. document; there is already
--   a document status type, but that type has alternatives that aren't
--   involved in the stat logging and thus creates the possibility of trying
--   to log a "bad" stat. Better to use this type instead.
data StatEvt
  = DocClosed
  | DocPending
  | DocCancel
  | DocReject 
  | DocTimeout
    deriving Eq

instance MonadDB m => DBUpdate m StatUpdate Bool where
  update (StatUpdate doIt did apistring) =
      kRun01 $ doIt did apistring

-- | SQL for turning a (document.type, document.process) pair into a doctype
--   that's suitable for the doc_stat_events table. Its sole argument is the
--   name of the documents table.
--   Example:
-- > "SELECT id, " ++ docType "doc" ++ " FROM documents AS doc"
docType :: String -> String
docType doc = unlines [
    "  CONCAT(",
    "    CASE",
    "      WHEN " ++ doc ++ ".type = 1 THEN 'Signable'",
    "      ELSE 'Template'",
    "    END, ' ',",
    "    CASE",
    "      WHEN " ++ doc ++ ".process = 1 THEN 'Contract'",
    "      WHEN " ++ doc ++ ".process = 2 THEN 'Offer'",
    "      ELSE 'Order'",
    "    END)"]


-- | Create a stat database update using the given event, ID and API string.
statUpdate :: StatQuery -> DocumentID -> APIString -> StatUpdate
statUpdate = StatUpdate

-- | Register a stat event for whatever signing method was used.
docStatSignMethod :: StatEvt -> StatQuery
docStatSignMethod status did apistring =
    SQL queryStr [toSql apistring, did', did']
  where
    queryStr = unlines [
      "INSERT INTO doc_stat_events",
      " (user_id, time, amount, api_string, document_type, document_id,",
      "  company_id, quantity)",
      "SELECT sl.user_id, " ++ time ++ ", sigs, ?,",
      docType "doc" ++ ",",
      "  doc.id, sl.company_id,",
      "  (CASE",
      "    WHEN doc.authentication_method=1 AND delivery_method=2 THEN",
      "      " ++ fromSql (toSql padQty),
      "    WHEN doc.authentication_method=1 THEN",
      "      " ++ fromSql (toSql emailQty),
      "    ELSE " ++ fromSql (toSql elegQty),
      "  END)",
      "FROM documents AS doc",
      "JOIN",
      "  signatory_links",
      "  AS sl ON sl.document_id = doc.id AND sl.is_author",
      "JOIN",
      "  (SELECT MAX(sign_time) AS last_sign, " ++ countSigs ++ " AS sigs",
      "   FROM signatory_links WHERE document_id = ?)",
      "  AS sls ON TRUE",
      "WHERE doc.id = ? AND " ++ docStatusCondition]
    did' = toSql did
    -- If status == Closed, then we want to count signatures and only update
    -- if the document is closed. If the document is instead pending or
    -- cancelled, we want to count _signatories_ and _always_ insert.
    -- Also, the event IDs differ, as does the timestamp to use.
    (padQty, emailQty, elegQty, countSigs, docStatusCondition, time)
      | status == DocClosed   = (DocStatPadSignatures,
                                 DocStatEmailSignatures,
                                 DocStatElegSignatures,
                                 "COUNT (sign_time)",
                                 "doc.status = " ++ fromSql (toSql Closed),
                                 "sls.last_sign")
      | status == DocPending  = (DocStatPadSignaturePending,
                                 DocStatEmailSignaturePending,
                                 DocStatElegSignaturePending,
                                 signatoryCount,
                                 "doc.invite_time IS NOT NULL",
                                 "doc.invite_time")
      | status == DocCancel   = (DocStatPadSignatureCancel,
                                 DocStatEmailSignatureCancel,
                                 DocStatElegSignatureCancel,
                                 signatoryCount,
                                 cancelCondition,
                                 "doc.mtime")
      | status == DocReject   = (DocStatPadSignatureReject,
                                 DocStatEmailSignatureReject,
                                 DocStatElegSignatureReject,
                                 signatoryCount,
                                 "doc.rejection_time IS NOT NULL",
                                 "doc.rejection_time")
      | status == DocTimeout  = (DocStatPadSignatureTimeout,
                                 DocStatEmailSignatureTimeout,
                                 DocStatElegSignatureTimeout,
                                 signatoryCount,
                                 timeoutCondition,
                                 "doc.timeout_time")
      | otherwise             = error $  "docStatSignMethod called with "
                                      ++ "nonsensical DocumentStatus!"
    signatoryCount = "COUNT (CASE WHEN is_partner THEN TRUE ELSE NULL END)"
    cancelCondition =
        "doc.status = " ++ fromSql (toSql Canceled)
    timeoutCondition =
        concat [
            "doc.status = " ++ fromSql (toSql Timedout),
            " AND sl.user_id IS NOT NULL",
            " AND doc.timeout_time IS NOT NULL"
          ]


-- | Register an event for "another document closed".
--   This is a bit hacky; in particular, concatenating the DocumentID is really
--   bad practice. Whether this is preferrable to having a near identical copy
--   of genericStatEvent here or not is an open question.
docStatClose :: StatQuery
docStatClose did =
    genericStatEvent DocStatSend
                     ("(SELECT MAX(sign_time) FROM signatory_links WHERE document_id = " ++ show did ++ ")")
                     ("doc.status = " ++ fromSql (toSql Closed))
                     did

-- | Record a "doc sent" stat.
docStatSend :: StatQuery
docStatSend =
  genericStatEvent DocStatSend
                   "doc.invite_time"
                   "doc.invite_time IS NOT NULL"

-- | Record a "doc cancelled" stat.
docStatCancel :: StatQuery
docStatCancel =
    genericStatEvent DocStatCancel "doc.mtime" "TRUE"

-- | Record a "doc cancelled" stat.
docStatReject :: StatQuery
docStatReject =
    genericStatEvent DocStatReject
                     "doc.rejection_time"
                     "doc.rejection_time IS NOT NULL"

-- | Record a "doc created" stat.
docStatCreate :: StatQuery
docStatCreate =
    genericStatEvent DocStatCreate
                     "doc.ctime"
                     "TRUE"

-- | Record a "doc timed out" event.
docStatTimeout :: StatQuery
docStatTimeout =
    genericStatEvent DocStatTimeout
                     "doc.timeout_time"
                     (concat [
                         "doc.status = " ++ fromSql (toSql Timedout),
                         " AND sl.user_id IS NOT NULL",
                         " AND doc.timeout_time IS NOT NULL"])

-- | Generic document stat event. An "event" is a log item which:
--   * always has amount = 1;
--   * has a timestamp that can be determined by looking only at the document;
--   * can be discarded or accepted by looking only at the document; and
--   * has a static DocStatQuantity.
--   In the SQL arguments, the document is referred to as doc and the
--   signatory link of the document's author as sl.
--   See 'docStatReject' for an example of usage.
--   This is a convenience function, and it should absolutely NOT be exported
--   sicne the interface is horribly unsafe for non-internal use.
genericStatEvent :: DocStatQuantity -- ^ Event to log.
                 -> String          -- ^ SQL expression for the event timestamp.
                 -> String          -- ^ SQL expression that determines whether
                                    --   to log the stat or not.
                 -> StatQuery
genericStatEvent qty time condition did apistring =
    SQL queryStr [toSql apistring, toSql qty, toSql did]
  where
    queryStr = unlines [
      "INSERT INTO doc_stat_events",
      " (user_id, time, amount, document_id, company_id,",
      "  api_string, quantity, document_type)",
      "SELECT sl.user_id, " ++ time ++ ", 1, doc.id, sl.company_id, ?, ?,",
      docType "doc",
      "FROM documents AS doc",
      "JOIN",
      "  signatory_links",
      "  AS sl ON sl.document_id = doc.id",
      "WHERE doc.id = ? AND sl.is_author AND " ++ condition]
