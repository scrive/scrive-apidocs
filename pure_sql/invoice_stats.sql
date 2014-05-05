

BEGIN;


CREATE FUNCTION escape_for_csv(text) RETURNS text
    AS $$select '"' || regexp_replace($1,'"','""','g') || '"';$$
    LANGUAGE SQL
    IMMUTABLE
    RETURNS NULL ON NULL INPUT;

CREATE TEMP TABLE thetime(time) AS
    VALUES (date_trunc('month', now() - interval '1 month'));

CREATE TEMP TABLE results AS
SELECT escape_for_csv(companies.name) AS "Company name"
     , escape_for_csv(companies.id :: TEXT) AS "Company ID"
     , escape_for_csv(companies.number :: TEXT) AS "Company number"
     , (CASE payment_plans.plan
          WHEN 1 THEN 'free'
          WHEN 2 THEN 'team'
          WHEN 3 THEN 'form'
          WHEN 4 THEN 'enterprise'
        END) AS "Plan"
     , (CASE payment_plans.provider
          WHEN 0 THEN 'none'
          WHEN 1 THEN 'recurly'
          ELSE payment_plans.provider :: TEXT
        END) AS "Provider"
     , (SELECT count(*)
          FROM documents
         WHERE EXISTS (SELECT TRUE
                         FROM signatory_links
                         JOIN users ON users.id = signatory_links.user_id
                        WHERE signatory_links.is_author
                          AND signatory_links.document_id = documents.id
                          AND users.company_id = companies.id)) AS "All docs"
     , (SELECT count(*)
          FROM documents
         WHERE EXISTS (SELECT TRUE
                         FROM signatory_links
                         JOIN users ON users.id = signatory_links.user_id
                        WHERE signatory_links.is_author
                          AND signatory_links.document_id = documents.id
                          AND users.company_id = companies.id
                          AND date_trunc('month', documents.invite_time) = thetime.time)) AS "Docs sent"
     , (SELECT count(*)
          FROM documents
         WHERE EXISTS (SELECT TRUE
                         FROM signatory_links
                         JOIN users ON users.id = signatory_links.user_id
                        WHERE signatory_links.is_author
                          AND signatory_links.document_id = documents.id
                          AND users.company_id = companies.id
                          AND documents.status = 3 -- Closed
                          AND (SELECT date_trunc('month', max(signatory_links.sign_time))
                                 FROM signatory_links
                                WHERE signatory_links.is_partner
                                  AND signatory_links.document_id = documents.id) = thetime.time)) AS "Docs closed"
     , (SELECT count(*)
          FROM documents, signatory_links
         WHERE signatory_links.document_id = documents.id
           AND signatory_links.sign_time IS NOT NULL
           AND EXISTS (SELECT TRUE
                         FROM signatory_links
                         JOIN users ON users.id = signatory_links.user_id
                        WHERE signatory_links.is_author
                          AND signatory_links.document_id = documents.id
                          AND users.company_id = companies.id
                          AND documents.status = 3 -- Closed
                          AND (SELECT date_trunc('month', max(signatory_links.sign_time))
                                 FROM signatory_links
                                WHERE signatory_links.is_partner
                                  AND signatory_links.document_id = documents.id) = thetime.time)) AS "Sigs closed"
     , (SELECT count(*)
          FROM users
         WHERE (users.deleted IS NULL OR users.deleted > thetime.time)
           AND users.company_id = companies.id
           AND has_accepted_terms_of_service <= thetime.time) AS "Users at begin of month"
     , (SELECT count(*)
          FROM users
         WHERE (users.deleted IS NULL OR users.deleted > thetime.time + interval '1 month')
           AND users.company_id = companies.id
           AND has_accepted_terms_of_service <= thetime.time + interval '1 month') AS "Users at end of month"
     , (SELECT count(*)
          FROM users
         WHERE users.company_id = companies.id
           AND has_accepted_terms_of_service >= thetime.time
           AND has_accepted_terms_of_service < thetime.time + interval '1 month') AS "Users activated during month"
     , (SELECT count(*)
          FROM users
         WHERE users.company_id = companies.id
           AND users.deleted >= thetime.time
           AND users.deleted < thetime.time + interval '1 month') AS "Users deleted during month"
     , substring((thetime.time :: DATE :: TEXT) for 7) AS "Month"

  FROM companies
  JOIN payment_plans ON companies.id = payment_plans.company_id
  , thetime

--  FROM companies
--  JOIN payment_plans ON companies.id = payment_plans.company_id
--  JOIN users ON users.id = payment_plans.user_id

--  FROM payment_plans

-- WHERE companies.id IS NULL
-- WHERE payment_plans.account_type IS NOT NULL
--   AND payment_plans.plan IS NOT NULL
--   LIMIT 10
     ;

SELECT *
  FROM results
 WHERE "Sigs closed" > 0
    OR "Docs closed" > 0
    OR "Users at begin of month" > 0
    OR "Users at end of month" > 0
    OR "Users activated during month" > 0
    OR "Users deleted during month" > 0
 ORDER BY 1, 2
     ;

ROLLBACK;
