
--
-- This script finds all files in the database that:
--
-- - are not yet purged (purge_time is null)
-- - are not referenced at all or all that references them is itself purged
--
-- A table with URLs to files is produced. What should happen next:
-- - files should be marked as purged
-- - a script removing those files from Amazon should be executed
--
-- Note about output syntax: output starts after some garbage at the
-- token START. Then it is lines with space separated values. First
-- one is file id, then comes file url relative to the Amazon
-- including bucket it is in.  Output ends on token FINISH.

BEGIN;


-- First of all lets show all places where foreign references to files
-- are to be expected:

CREATE TEMP TABLE expected_refs(table_name,column_name) AS
VALUES ('author_attachments',    'file_id'),
       ('documents',             'file_id'),
       ('documents',             'sealed_file_id'),
       ('mail_attachments',      'file_id'),
       ('signatory_attachments', 'file_id'),
       ('signatory_screenshots', 'file_id');

CREATE TEMP TABLE refs AS
SELECT table_name, column_name
  FROM information_schema.key_column_usage
 WHERE constraint_name IN (SELECT constraint_name
                             FROM information_schema.referential_constraints
                            WHERE unique_constraint_name = 'pk_files');

\echo Both of the below should be empty, if they are not script must be fixed to include new or missing relations

SELECT * FROM refs EXCEPT SELECT * FROM expected_refs;
SELECT * FROM expected_refs EXCEPT SELECT * FROM refs;

\pset format unaligned
\pset fieldsep ' '
\pset tuples_only on
\echo START

UPDATE files
   SET purged_time = now()
 WHERE files.content IS NULL -- were already moved to Amazon
   AND purged_time IS NULL
   -- Case 1:
   -- File is connected as source to a document that is still available to somebody.
   AND NOT EXISTS (
       SELECT TRUE
         FROM documents
         JOIN signatory_links ON signatory_links.document_id = documents.id
        WHERE documents.file_id = files.id
          AND documents.purged_time IS NULL
       )
   -- Case 2:
   -- File is connected as sealed file to a document that is still available to somebody.
   AND NOT EXISTS (
       SELECT TRUE
         FROM documents
         JOIN signatory_links ON signatory_links.document_id = documents.id
        WHERE documents.sealed_file_id = files.id
          AND documents.purged_time IS NULL
       )
   -- Case 3:
   -- File is connected as a signatory attachment to a document that is available to somebody.
   AND NOT EXISTS (
       SELECT TRUE
         FROM documents
         JOIN signatory_links ON signatory_links.document_id = documents.id
         JOIN signatory_attachments ON signatory_attachments.signatory_link_id = signatory_links.id
        WHERE signatory_attachments.file_id = files.id
          AND documents.purged_time IS NULL
       )
   -- Case 4:
   -- File is connected as an author attachment to a document that is available to somebody.
   AND NOT EXISTS (
       SELECT TRUE
         FROM documents
         JOIN signatory_links ON signatory_links.document_id = documents.id
         JOIN author_attachments ON author_attachments.document_id = documents.id
        WHERE author_attachments.file_id = files.id
          AND documents.purged_time IS NULL
       )
   -- Case 5:
   -- There is an email with this file as an attachment
   AND NOT EXISTS (
       SELECT TRUE
         FROM mail_attachments
        WHERE mail_attachments.file_id = files.id
       )
   AND NOT EXISTS (
       SELECT TRUE
         FROM documents
         JOIN signatory_links ON signatory_links.document_id = documents.id
         JOIN signatory_screenshots ON signatory_screenshots.signatory_link_id = signatory_links.id
        WHERE signatory_screenshots.file_id = files.id
          AND documents.purged_time IS NULL
       )
RETURNING id, amazon_bucket || '/' || amazon_url
   ;
\echo FINISH

ROLLBACK;
