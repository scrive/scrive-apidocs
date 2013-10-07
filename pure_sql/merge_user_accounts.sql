

BEGIN;


-- The table 'remap' is table of tuples of email addresses, 'from_id',
-- 'from_email', 'to_id' and 'to_email'

CREATE TEMP TABLE remap AS
SELECT users.id AS from_id
     , users.email AS from_email
     , canonical_users.id AS to_id
     , canonical_users.email AS to_email
  FROM users, users AS canonical_users
 WHERE users.email = 'gustav@gamersgate.com'
   AND canonical_users.email = 'gustav.nisser@gmail.com'
     ;

-- Show where is users table referenced from. This script might need
-- to be revised if needed.
CREATE TEMP TABLE refs AS
SELECT table_name || '.' || column_name AS "Columns referencing users.id"
  FROM information_schema.key_column_usage
 WHERE constraint_name IN (SELECT constraint_name
                             FROM information_schema.referential_constraints
                            WHERE unique_constraint_name = 'pk_users');

SELECT *
  FROM refs
     ;

-- Show what will get done
SELECT *
  FROM remap
     ;
-- Show some more information about remapped users
SELECT email, account_suspended, deleted, is_company_admin
  FROM users
 WHERE users.id IN (SELECT from_id FROM remap)
    OR users.id IN (SELECT to_id FROM remap)
     ;

-- Remap signatory links
UPDATE signatory_links
   SET user_id = remap.to_id
  FROM remap
 WHERE remap.from_id = signatory_links.user_id
     ;

-- Remap attachments should there be any
UPDATE attachments
   SET user_id = remap.to_id
  FROM remap
 WHERE remap.from_id = attachments.user_id
     ;

ROLLBACK;
