

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
\echo START

SELECT id, amazon_url
  FROM files
 WHERE files.content IS NULL -- were already moved to Amazon
   -- Case 1:
   -- File is connected as source to a document that is still available to somebody.
   AND NOT EXISTS (
       SELECT TRUE
         FROM documents
         JOIN signatory_links ON signatory_links.document_id = documents.id
        WHERE documents.file_id = files.id
          AND signatory_links.really_deleted IS NULL
       )
   -- Case 2:
   -- File is connected as sealed file to a document that is still available to somebody.
   AND NOT EXISTS (
       SELECT TRUE
         FROM documents
         JOIN signatory_links ON signatory_links.document_id = documents.id
        WHERE documents.sealed_file_id = files.id
          AND signatory_links.really_deleted IS NULL
       )
   -- Case 3:
   -- File is connected as a signatory attachment to a document that is available to somebody.
   AND NOT EXISTS (
       SELECT TRUE
         FROM documents
         JOIN signatory_links ON signatory_links.document_id = documents.id
         JOIN signatory_attachments ON signatory_attachments.signatory_link_id = signatory_links.id
        WHERE signatory_attachments.file_id = files.id
          AND signatory_links.really_deleted IS NULL
       )
   -- Case 3:
   -- File is connected as a author attachment to a document that is available to somebody.
   AND NOT EXISTS (
       SELECT TRUE
         FROM documents
         JOIN signatory_links ON signatory_links.document_id = documents.id
         JOIN author_attachments ON author_attachments.document_id = documents.id
        WHERE author_attachments.file_id = files.id
          AND signatory_links.really_deleted IS NULL
       )
   -- Case 4:
   -- There is an email with this file as attachment
   AND NOT EXISTS (
       SELECT TRUE
         FROM mail_attachments
        WHERE mail_attachments.file_id = files.id
       )
   AND NOT EXISTS (
       SELECT TRUE
         FROM signatory_screenshots
         JOIN signatory_links ON signatory_screenshots.signatory_link_id = signatory_links.id
        WHERE signatory_screenshots.file_id = files.id
          AND signatory_links.really_deleted IS NULL
       )
   ;
\echo FINISH

ROLLBACK;
