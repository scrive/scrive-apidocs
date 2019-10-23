module Doc.Trigger
  (
    searchUpdateDocs
  , searchUpdateSignatoryLinkFields
  ) where

import DB
import DB.SQLTrigger

-- Document search triggers. These triggers keep the `archive_search_terms`
-- column of the document table up to date: whenever
-- `signatory_link_fields.value_text` or `documents.title` is changed, these
-- triggers run a function to update the `documents.archive_search_terms`
-- field. This field is a concatenation of the `documents.title` and all of the
-- `signatory_link_fields.value_text` corresponding to a particular
-- `documents.id`.

-- | Trigger that fires after insertion or update of `documents.title`.
searchUpdateDocs :: SQLTrigger
searchUpdateDocs = SQLTrigger { sqlTrigFun = searchUpdateDocsFunc
                              , sqlTrigTbl = "documents"
                              , sqlTrigCol = Just "title"
                              }
  where
    searchUpdateDocsFunc :: RawSQL () -> RawSQL ()
    searchUpdateDocsFunc trgFunName =
      "create or replace function"
        <+> trgFunName
        <>  "()"
        <+> "returns trigger as"
        <+> "$$"
        <+> "declare"
        <+> "  new_field_values text;"
        <+> "begin"
        <+> "  new_field_values := (select coalesce(archive_search_terms_func(new.id), ''));"
        <+> "  update documents"
        <+> "  set    archive_search_terms = new_field_values,"
        <+> "         archive_search_fts = post_process_search_string(new_field_values)"
        <+> "  where  id = new.id;"
        <+> "  return new;"
        <+> "end;"
        <+> "$$"
        <+> "language plpgsql"
        <+> "volatile"
        <+> "returns null on null input;"

-- | Trigger that fires after insertion or update of `signatory_link_fields.value_text`.
searchUpdateSignatoryLinkFields :: SQLTrigger
searchUpdateSignatoryLinkFields = SQLTrigger
  { sqlTrigFun = searchUpdateSignatoryLinkFieldsFunc
  , sqlTrigTbl = "signatory_link_fields"
  , sqlTrigCol = Just "value_text"
  }
  where
    searchUpdateSignatoryLinkFieldsFunc :: RawSQL () -> RawSQL ()
    searchUpdateSignatoryLinkFieldsFunc trgFunName =
      "create or replace function"
        <+> trgFunName
        <>  "()"
        <+> "returns trigger as"
        <+> "$$"
        <+> "declare"
        <+> "  doc_id bigint;"
        <+> "  new_field_values text;"
        <+> "begin"
        <+> "  doc_id := (select d.id"
        <+> "             from   documents d"
        <+> "             join   signatory_links sl on sl.document_id = d.id"
        <+> "             join   signatory_link_fields slf on sl.id ="
        <+> "                    slf.signatory_link_id and slf.id=new.id);"
        <+> "  new_field_values := (select coalesce(archive_search_terms_func(doc_id), ''));"
        <+> "  update documents"
        <+> "  set    archive_search_terms = new_field_values,"
        <+> "         archive_search_fts = post_process_search_string(new_field_values)"
        <+> "  where  id = doc_id;"
        <+> "  return new;"
        <+> "end;"
        <+> "$$"
        <+> "language plpgsql"
        <+> "volatile"
        <+> "returns null on null input;"
