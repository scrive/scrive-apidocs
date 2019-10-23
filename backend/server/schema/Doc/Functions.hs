module Doc.Functions
  (
    archiveSearchTerms
  , postProcessSearchString
  , extractEmails
  , splitEmail
  ) where

import DB
import DB.SQLFunction

-- | This function collects all text that should be searchable for one
-- document.
archiveSearchTerms :: SQLFunction
archiveSearchTerms = SQLFunction { sqlFunDef = archiveSearchTermsFunc }
  where
    archiveSearchTermsFunc :: RawSQL ()
    archiveSearchTermsFunc =
      "create or replace function archive_search_terms_func(doc_id bigint)"
        <+> "returns text as"
        <+> "$$"
        <+> "begin"
        <+> "return (coalesce(d.title, '') ||"
        <+> "        ' ' ||"
        <+> "        string_agg(coalesce(slf.value_text, ''), ' '))"
        <+> "  from signatory_link_fields slf"
        <+> "  join signatory_links sl on sl.id = slf.signatory_link_id"
        <+> "  join documents d on d.id = sl.document_id and  d.id = doc_id"
        <+> "  group by d.id;"
        <+> "end;"
        <+> "$$"
        <+> "language plpgsql"
        <+> "volatile"
        <+> "returns null on null input;"

postProcessSearchString :: SQLFunction
postProcessSearchString = SQLFunction { sqlFunDef = postProcessSearchStringFunc }
  where
    postProcessSearchStringFunc :: RawSQL ()
    postProcessSearchStringFunc =
      "create or replace function post_process_search_string(txt text)"
        <+> "returns tsvector as"
        <+> "$$"
        <+> "declare"
        <+> "  emails       text := extract_emails(txt);"
        <+> "  split_emails text := '';"
        <+> "begin"
        <+> "  split_emails = (select string_agg(split_email(eml.splits), ' ')"
        <+> "                  from   (select unnest(string_to_array(emails, ' ')) as splits ) as eml);"
        <+> "  return to_tsvector('simple', coalesce(regexp_replace(txt, ':+', ' ', 'g'), '')) ||  ' ' || (coalesce(regexp_replace(split_emails, ':+', ' ', 'g'), '')::tsvector);"
        <+> "end;"
        <+> "$$"
        <+> "language plpgsql"
        <+> "immutable;"

-- | Filter out everything but email addresses.
--
-- A note on treatment of email addresses by FTS subsystem (i.e. when run
-- through any analysing FTS function such as `to_tsvector`):
--
-- * not split up or modified by FTS (i.e. recognised as email addresses)
--   'str1@domain.com', 'str1_str2@domain.com', 'str1.str2@domain.com',
--   'str1-str2@domain.com' etc.
--
-- * these are RFC-compliant, but split up or modified by FTS:
--   'str1+str2@domain.com'    -> 'str1', 'str2@domain.com'
--   'str1_str2_@domain.com'   -> 'str1', 'str2', 'domain.com'
--   '"str1"@domain.com'       -> 'str1', 'domain.com'
--   '_f@domain.com'           -> 'f@domain.com'
--   'f__f@domain.com'         -> 'f', 'f@domain.com'
--   'f@192.168.1.1'           -> 'f', '192.168.1.1'
--   '666@domain.com'          -> '666', 'domain.com'
--   'email@[123.123.123.123]' -> '123.123.123.123', 'email'
--
-- ...and so on. Some general forbidden patterns are '.@', '_@', '-@' - any
-- email address that contain these will be split up by the PG FTS
-- subsystem. Not even `plainto_tsquery` can be used to circumvent this, so a
-- hard cast will be used in `post_process_search_string` instead.
--

-- The regexp match returns an array that will contain at most one element in
-- the array (and hence the array subcript in the string aggregation).
extractEmails :: SQLFunction
extractEmails = SQLFunction { sqlFunDef = extractEmailsFunc }
  where
    extractEmailsFunc :: RawSQL ()
    extractEmailsFunc =
      "create or replace function extract_emails(txt text)"
        <+> "returns text as"
        <+> "$$"
        <+> "begin"
        <+> "  return string_agg(emails.address[1], ' ')"
        <+> "  from   (select regexp_matches(unnest(string_to_array(txt, ' ')), '(.+@.+\\..+?)', 'g') as address)"
        <+> "  as     emails;"
        <+> "end;"
        <+> "$$"
        <+> "language plpgsql"
        <+> "immutable"
        <+> "returns null on null input;"

-- | Split an email address into its constituent parts.

splitEmail :: SQLFunction
splitEmail = SQLFunction { sqlFunDef = splitEmailFunc }
  where
    splitEmailFunc :: RawSQL ()
    splitEmailFunc =
      "create or replace function split_email(email text)"
        <+> "returns text as"
        <+> "$$"
        <+> "declare"
        <+> "  split   text[] := '{}';"
        <+> "begin"
        <+> "  split   := string_to_array(email, '@');"
        <+> "  return (split[1] ||"
        <+> "          ' ' ||"
        <+> "          split[2] ||"
        <+> "          ' ' ||"
        <+> "          email);"
        <+> "end;"
        <+> "$$"
        <+> "language plpgsql"
        <+> "immutable"
        <+> "returns null on null input;"
