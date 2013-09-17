

-- This SQL commans file should find all documents that should be
-- purged, then purges them, then marks them as purged.
--
-- Document is a candidate for purging when for each of signatory
-- links one of the below is true:
--
-- - signatory links is deleted and 3 months went by after that point
--
-- - signatory link is not deleted but is wasn't saved to an account
--   and 1 month went by since document reached final state (closed,
--   rejected or canceled or error)
--
-- Purging consists of:
-- - blanking values of all signatory_link_fields
-- - blanking all IP addresses related to document
--
-- Note: do not set to null any files referenced. Files themselves
-- will be purged by another script.
--
-- Note: when we have implemented company deletes then we will be able
-- to purge document based on companies.purged_time. This is a long
-- term task.
BEGIN;

SELECT count(*) AS "Total documents"
  FROM documents
     ;

SELECT count(*) AS "Sign links that were deleted"
  FROM signatory_links
 WHERE deleted IS NOT NULL
     ;

SELECT count(*) AS "Sign links that were really_deleted"
  FROM signatory_links
 WHERE really_deleted IS NOT NULL
     ;

CREATE TEMP TABLE documents_to_purge(id, title) AS
SELECT documents.id, documents.title
  FROM documents
       -- document wasn't purged yet
 WHERE documents.purged_time IS NULL
       -- has not been deleted at least in a single account
   AND NOT EXISTS(SELECT TRUE
                    FROM signatory_links
                   WHERE signatory_links.document_id = documents.id
                     AND signatory_links.user_id IS NOT NULL
                     -- not deleted or linger time hasn't elapsed
                     AND (signatory_links.deleted IS NULL OR
                          signatory_links.deleted + interval '3 month' > now()))

       -- is not saved but time to save the document went by
   AND NOT EXISTS(SELECT TRUE
                    FROM signatory_links
                   WHERE signatory_links.document_id = documents.id
                     AND signatory_links.user_id IS NULL
                     -- linger time hasn't elapsed yet
                     AND documents.mtime + interval '1 month' > now())
     ;


SELECT count(*) AS "Documents to purge"
  FROM documents_to_purge
     ;

UPDATE documents
   SET purged_time = now()
 WHERE documents.id IN (SELECT id
                          FROM documents_to_purge)
     ;

UPDATE signatory_links
   SET sign_ip = 0
     , seen_ip = 0
     , eleg_data_mismatch_first_name = ''
     , eleg_data_mismatch_last_name = ''
     , eleg_data_mismatch_personal_number = ''
 WHERE signatory_links.document_id IN (SELECT id FROM documents_to_purge)
     ;

UPDATE signatory_link_fields
   SET value = ''
 WHERE signatory_link_fields.signatory_link_id IN
       (SELECT id
          FROM signatory_links
         WHERE signatory_links.document_id IN (SELECT id FROM documents_to_purge))
     ;

ROLLBACK;
