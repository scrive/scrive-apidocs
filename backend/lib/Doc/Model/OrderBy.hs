module Doc.Model.OrderBy
  ( DocumentOrderBy(..)
  , DocumentOrderByRep(..)
  , documentOrderByToSQL
  , documentOrderByAscDescToSQL
  , addMTimeSorting
  ) where

import DB
import Doc.DocStateData

-- | These are possible order by clauses that make documents sorted by.
data DocumentOrderBy
  = DocumentOrderByTitle       -- ^ Order by title, alphabetically, case insensitive
  | DocumentOrderByMTime       -- ^ Order by modification time
  | DocumentOrderByCTime       -- ^ Order by creation time
  | DocumentOrderByStatus      -- ^ Order by document status.
  | DocumentOrderByStatusClass -- ^ Order by status class.
  | DocumentOrderByType        -- ^ Order by document type.
  | DocumentOrderByPartners    -- ^ Order by partner names or emails
  | DocumentOrderByAuthor      -- ^ Order by author name or email
  deriving (Eq, Show)

data DocumentOrderByRep = DocumentOrderByRep {
    dobrExpr  :: !SQL
  , dobrName  :: !SQL
  , dobrOrder :: !SQL
  }

defaultDocumentOrderByRep :: DocumentOrderByRep
defaultDocumentOrderByRep =
  DocumentOrderByRep { dobrExpr = "", dobrName = "", dobrOrder = "" }

-- | Convert DocumentOrderBy enumeration into proper SQL order by statement
documentOrderByToSQL :: DocumentOrderBy -> DocumentOrderByRep
documentOrderByToSQL DocumentOrderByTitle =
  defaultDocumentOrderByRep { dobrExpr = "documents.title", dobrName = "doc_order_title" }
documentOrderByToSQL DocumentOrderByMTime =
  defaultDocumentOrderByRep { dobrExpr = "documents.mtime", dobrName = "doc_order_mtime" }
documentOrderByToSQL DocumentOrderByCTime =
  defaultDocumentOrderByRep { dobrExpr = "documents.ctime", dobrName = "doc_order_ctime" }
documentOrderByToSQL DocumentOrderByStatus = defaultDocumentOrderByRep
  { dobrExpr = "documents.status"
  , dobrName = "doc_order_status"
  }
documentOrderByToSQL DocumentOrderByStatusClass = defaultDocumentOrderByRep
  { dobrExpr = documentStatusClassExpression
  , dobrName = "doc_order_status_class"
  }
documentOrderByToSQL DocumentOrderByType =
  defaultDocumentOrderByRep { dobrExpr = "documents.type", dobrName = "doc_order_type" }
documentOrderByToSQL DocumentOrderByPartners = defaultDocumentOrderByRep
  { dobrExpr = parenthesize
               .   selectSignatoryLinksSmartNames
               $   "documents.author_id <> signatory_links.id"
               <+> "AND signatory_links.signatory_role ="
               <?> SignatoryRoleSigningParty
  , dobrName = "doc_order_partners"
  }
documentOrderByToSQL DocumentOrderByAuthor = defaultDocumentOrderByRep
  { dobrExpr =
    parenthesize
      $ selectSignatoryLinksSmartNames "documents.author_id = signatory_links.id"
  , dobrName = "doc_order_author"
  }

selectSignatoryLinksSmartNames :: SQL -> SQL
selectSignatoryLinksSmartNames signatory_condition =
  "SELECT COALESCE(string_agg(x.value,' '),'no fields') FROM "
    <> "(SELECT ("
    <> selectSmartName
    <> ") AS value FROM signatory_links"
    <> " WHERE signatory_links.document_id = documents.id"
    <> "   AND "
    <> signatory_condition
    <> " ORDER BY signatory_links.id) AS x"
  where
    selectFieldAs xtype name =
      "(SELECT signatory_link_fields.value_text AS value"
        <+> "FROM signatory_link_fields"
        <+> "WHERE signatory_link_fields.signatory_link_id = signatory_links.id"
        <+> "AND type ="
        <?> fieldTypeFromFieldIdentity xtype
        <+> (case xtype of
              NameFI nameorder -> "AND name_order =" <?> nameorder
              _                -> "AND name_order IS NULL"
            )
        <+> "LIMIT 1)"
        <+> "AS"
        <+> name
    {-
     selectSmartName explanation:
     1. Select fields first name, last name and email into separate tables
     2. Use FULL JOIN so we get NULL when field is not there
     3. Convert possible NULLs into empty strings
     4. Concatenate first name and last name with a space between
     5. Trim that string both ends
     6. See if it is empty, if it is, convert to NULL
     7. If NULL use email address instead
     -}
    selectSmartName =
      "SELECT "
        <> "COALESCE(NULLIF(TRIM(BOTH FROM (COALESCE(first_name.value,'') "
        <> "                                || ' ' || "
        <> "                                COALESCE(last_name.value,''))), ''),email.value) "
        <> "FROM ("
        <> selectFieldAs (NameFI (NameOrder 1)) "first_name"
        <> " FULL JOIN "
        <> selectFieldAs (NameFI (NameOrder 2)) "last_name"
        <> " ON TRUE "
        <> " FULL JOIN "
        <> selectFieldAs EmailFI "email"
        <> " ON TRUE "
        <> ") WHERE signatory_links.document_id = documents.id"

documentOrderByAscDescToSQL :: AscDesc DocumentOrderBy -> DocumentOrderByRep
documentOrderByAscDescToSQL (Asc  x) = (documentOrderByToSQL x) { dobrOrder = "ASC" }
documentOrderByAscDescToSQL (Desc x) = (documentOrderByToSQL x) { dobrOrder = "DESC" }

-- | Add sorting by mtime in descending order if it's not there.
addMTimeSorting :: [AscDesc DocumentOrderBy] -> [AscDesc DocumentOrderBy]
addMTimeSorting orders = if any ((== DocumentOrderByMTime) . peekAscDesc) orders
  then orders
  else orders ++ [Desc DocumentOrderByMTime]
  where
    peekAscDesc (Asc  x) = x
    peekAscDesc (Desc x) = x
