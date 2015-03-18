module Doc.Model.OrderBy
  ( DocumentOrderBy(..)
  , documentOrderByToSQL
  , documentOrderByAscDescToSQL
  ) where

import Data.Monoid
import Data.Monoid.Utils

import DB
import Doc.DocStateData

-- | These are possible order by clauses that make documents sorted by.
data DocumentOrderBy
  = DocumentOrderByTitle       -- ^ Order by title, alphabetically, case insensitive
  | DocumentOrderByMTime       -- ^ Order by modification time
  | DocumentOrderByCTime       -- ^ Order by creation time
  | DocumentOrderByStatusClass -- ^ Order by status class.
  | DocumentOrderByType        -- ^ Order by document type.
  | DocumentOrderByPartners    -- ^ Order by partner names or emails
  | DocumentOrderByAuthor      -- ^ Order by author name or email


-- | Convert DocumentOrderBy enumeration into proper SQL order by statement
documentOrderByToSQL :: DocumentOrderBy -> SQL
documentOrderByToSQL DocumentOrderByTitle = "documents.title"
documentOrderByToSQL DocumentOrderByMTime = "documents.mtime"
documentOrderByToSQL DocumentOrderByCTime = "documents.ctime"
documentOrderByToSQL DocumentOrderByStatusClass = documentStatusClassExpression
documentOrderByToSQL DocumentOrderByType = "documents.type"
documentOrderByToSQL DocumentOrderByPartners =
  parenthesize (selectSignatoryLinksSmartNames False True)
documentOrderByToSQL DocumentOrderByAuthor =
  parenthesize (selectSignatoryLinksSmartNames True False)

selectSignatoryLinksSmartNames :: Bool -> Bool -> SQL
selectSignatoryLinksSmartNames is_author is_partner =
      "SELECT COALESCE(string_agg(x.value,' '),'no fields') FROM " <>
      "(SELECT (" <>
      selectSmartName <>
      ") AS value FROM signatory_links" <>
           " WHERE signatory_links.document_id = documents.id" <>
           "   AND signatory_links.is_author =" <?> is_author <+> "AND signatory_links.is_partner =" <?> is_partner <>
           " ORDER BY signatory_links.id) AS x"
  where
    selectFieldAs xtype name = "(SELECT signatory_link_fields.value_text AS value" <+>
                                    "FROM signatory_link_fields" <+>
                                    "WHERE signatory_link_fields.signatory_link_id = signatory_links.id" <+>
                                    "AND type =" <?> fieldTypeFromFieldIdentity xtype <+>
                                    (case xtype of
                                      NameFI nameorder -> "AND name_order =" <?> nameorder
                                      _ -> "AND name_order IS NULL"
                                    ) <+>
                                    "LIMIT 1) " <+>
                                    "AS" <+> name
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
    selectSmartName = "SELECT " <>
                           "COALESCE(NULLIF(TRIM(BOTH FROM (COALESCE(first_name.value,'') " <>
                           "                                || ' ' || " <>
                           "                                COALESCE(last_name.value,''))), ''),email.value) " <>
                           "FROM (" <>
                      selectFieldAs (NameFI (NameOrder 1)) "first_name" <>
                      " FULL JOIN " <>
                      selectFieldAs (NameFI (NameOrder 2)) "last_name" <>
                      " ON TRUE " <>
                      " FULL JOIN " <>
                      selectFieldAs EmailFI "email" <>
                      " ON TRUE " <>
                      ") WHERE signatory_links.document_id = documents.id"

documentOrderByAscDescToSQL :: AscDesc DocumentOrderBy -> SQL
documentOrderByAscDescToSQL (Asc x) = documentOrderByToSQL x
documentOrderByAscDescToSQL (Desc x) = documentOrderByToSQL x <> " DESC"
