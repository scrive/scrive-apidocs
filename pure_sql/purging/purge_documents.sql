--
-- This SQL commands file should find all documents that should be
-- purged, then purges them, then marks them as purged.
--
-- A document is a candidate for purging when is does not contain live
-- signatory links. A live signatory link is one that either:
--
-- - signatory link is in user account and not deleted
-- - signatory link is in user account and deleted no longer
--   than 3 months ago
-- - signatory link is not in user account and no longer than 1 month
--   passed since document reached final state (closed, rejected,
--   canceled or error)
--
-- Note: user accounts have lifetime extent the same as companies have
-- as user account represent current or historical fact of employment
-- and working on behalf of the company. As soon as we will be
-- supporting removal of companies document purging rules should take
-- that into account companies.purged_time.
--
-- Purging consists of blanking the following:
-- - all values of all signatory_link_fields
-- - all IP addresses related to document
-- - e-leg data
-- - removing all evidence_log data
--
-- Important note: do not set to null any files referenced. Files
-- themselves will be purged by another script.
--
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

DELETE
  FROM evidence_log
 WHERE document_id IN (SELECT id FROM documents_to_purge)
     ;

ROLLBACK;
