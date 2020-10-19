/*

* Sample usage:

  ```
  $ psql -d kontrakcja -v date_from='20170320' -v date_to='20170327' \
         -f invoice_stats.sql
  ```
  This prints the data extracted by means of the `report` table to a CSV file
  named `./report-${date_from}_${date_to}.csv`

  ```
  $ psql -d kontrakcja -v -f invoice_stats.sql
  ```

  This uses default of the beginning of the month of *previous* day up until
  (but not including) the beginning of the *current* day; cf. below.

* Parameter functionality

- `date_from` should be specified in the format 'YYYY-MM-DD' or 'YYYYMMDD'. If
  `date_from` is missing, the beginning of the month of the day *preceding*
  whatever day `date_to` evaluates to will be used.

- `date_to` should be specified in the format 'YYYY-MM-DD' or 'YYYYMMDD'. If
  `date_to` is missing, the very beginning of the current day will be
  used.

- `report_dir` is the directory where report files will be created. If left
   empty, the `monthly-invoice` will be used.

*/

\set QUIET on

BEGIN;

/*
  We initialise start and end of period with defaults as per the folklore item
  http://stackoverflow.com/a/32597876. A simple application of `coalesce` does
  *not* do it, nor a simple table with defaults, since checking for non-existing
  command line arguments crashes the script and would need exception handling.
*/

-- This will set the variable `date_to` to the *string* `:date_to` (and similarly
-- for `date_from`), unless this variable was initialised before running the
-- script (e.g. on the command line by means of `-v`). That is why we check for
-- that string in the step after this.
\set date_from  :date_from
\set date_to    :date_to
\set report_dir :report_dir

SELECT CASE
  WHEN :'date_to' = ':date_to'
  THEN (date_trunc('day', now()) :: TEXT)
  ELSE :'date_to'
END AS "date_to" \gset

SELECT CASE
  WHEN :'date_from' = ':date_from'
  THEN (date_trunc('month', timestamptz(:'date_to') - interval '1 day') :: TEXT)
  ELSE :'date_from'
END AS "date_from" \gset

SELECT CASE
  WHEN :'report_dir' = ':report_dir'
  THEN 'monthly-invoice'
  ELSE :'report_dir'
END AS "report_dir" \gset

CREATE OR REPLACE FUNCTION escape_for_csv(str text)
RETURNS text AS
$$
BEGIN
  -- let's keep the old strangeness
  str = regexp_replace(str,'"','"" & Chr(34) & ""','g');
  str = regexp_replace(str,';','','g');
  return str;
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
RETURNS NULL ON NULL INPUT;

/*
  Get the payment plan of the *first* parent that is either a bill item or an
  invoice and has one explicitly set.
*/

CREATE OR REPLACE FUNCTION get_payment_plan_from_parent(user_group_id bigint)
RETURNS smallint AS
$$
DECLARE
  payment_plan_from_parent smallint;
BEGIN
  WITH
  parents AS
  (
    SELECT unnest id, ordinality ord
      FROM user_groups ug
     CROSS JOIN LATERAL unnest(ug.parent_group_path) WITH ORDINALITY
     WHERE ug.id = $1
  ),
  candidates AS
  (
    SELECT p.id, p.ord FROM parents p
      JOIN user_group_invoicings ugi on ugi.user_group_id = p.id
     WHERE ARRAY[ugi.invoicing_type] <@ ARRAY[2, 3]::smallint[]
       AND ugi.payment_plan IS NOT NULL
  )
  SELECT INTO payment_plan_from_parent ugi.payment_plan
    FROM candidates c
    JOIN user_group_invoicings ugi ON ugi.user_group_id = c.id
   WHERE c.ord = (SELECT min(candidates.ord) FROM candidates);

  RETURN payment_plan_from_parent;
END; $$
LANGUAGE plpgsql
VOLATILE
RETURNS NULL ON NULL INPUT;

/*
  Unless a payment plan is set explicitly for this user group, retrieve it from
  the nearest parent.
*/
CREATE OR REPLACE FUNCTION get_payment_plan(user_group_id bigint)
RETURNS smallint AS
$$
DECLARE
    payment_plan_self smallint;
BEGIN
  SELECT INTO payment_plan_self payment_plan
    FROM user_group_invoicings
   WHERE user_group_invoicings.user_group_id = $1;

  RETURN
    CASE
      WHEN payment_plan_self IS NOT NULL
        THEN payment_plan_self
      ELSE get_payment_plan_from_parent($1)
    END;
END; $$
LANGUAGE plpgsql
VOLATILE
RETURNS NULL ON NULL INPUT;

/*
  Retrieve *any* of the user group admin email addresses.
*/
CREATE OR REPLACE FUNCTION get_user_group_contact(ugid bigint)
RETURNS text AS
$$
DECLARE
  contact_email text;
BEGIN
  SELECT INTO contact_email email
    FROM users
   WHERE users.is_company_admin
     AND users.user_group_id = ugid
   LIMIT 1;
  RETURN contact_email;
END; $$
LANGUAGE plpgsql
STRICT VOLATILE;


/*
  This function returns the user group ID of the *first* node from and including
  the leaf (which is the supplied `user_group_id`) to the root that has the
  specified invoicing type. It returns `null` if one of the arguments do not
  correspond to an existing entity in the database.
*/
CREATE OR REPLACE FUNCTION get_invoicing_aggregation_group(user_group_id bigint, inv_type integer)
RETURNS bigint AS
$$
DECLARE
  invoicing_user_group_id bigint;
BEGIN
    WITH
    candidates AS
    (
      SELECT unnest id, ordinality ord
        FROM user_groups ug
       CROSS JOIN LATERAL unnest(array_prepend(user_group_id, ug.parent_group_path)) WITH ORDINALITY
       WHERE ug.id = user_group_id
    ),
    candidates_invoicing AS
    (
      SELECT c.id, c.ord, ugi.invoicing_type FROM candidates c
        JOIN user_group_invoicings ugi on ugi.user_group_id = c.id
       WHERE ugi.invoicing_type = (inv_type :: smallint)
       -- ^ cast since we then need type annotation in implementation only, not
       -- at call site.
    )
    SELECT c.id into invoicing_user_group_id
      FROM candidates_invoicing c
      JOIN user_group_invoicings ugi ON ugi.user_group_id = c.id
     WHERE c.ord = (SELECT min(candidates_invoicing.ord) FROM candidates_invoicing);

    RETURN invoicing_user_group_id;
END;
$$
LANGUAGE plpgsql
VOLATILE
RETURNS NULL ON NULL INPUT;

CREATE OR replace FUNCTION get_billitem_path(user_group_id bigint)
RETURNS TABLE (
  ug_id bigint,
  inv_type smallint,
  payment_plan smallint,
  ord bigint
) AS
$$
  -- we select all groups in the parent path (+ group itself) that have
  -- either billitem or invoice set since we need to order _once_.
  WITH
  all_inclusive AS
  (
    SELECT unnest ai_ugid, ugi.invoicing_type ai_inv_type, ugi.payment_plan ai_plan, ordinality ai_ord
      FROM user_groups ug
     CROSS JOIN LATERAL unnest(array_prepend($1, ug.parent_group_path)) WITH ORDINALITY
      JOIN user_group_invoicings ugi ON ugi.user_group_id = unnest
     WHERE ug.id = $1
       AND (ugi.invoicing_type = (2 :: smallint) OR ugi.invoicing_type = (3 :: smallint))
  ),
  candidates_invoicing AS
  (
    SELECT * FROM all_inclusive WHERE ai_inv_type = (3 :: smallint)
  ),
  first_invoice_parent AS
  (
    SELECT * FROM candidates_invoicing ci
     WHERE ci.ai_ord = (SELECT min(candidates_invoicing.ai_ord) FROM candidates_invoicing)
  ),
  all_billitems AS
  (
    SELECT * FROM all_inclusive
     WHERE ai_inv_type = (2 :: smallint)
       AND ai_ord < (SELECT ai_ord FROM first_invoice_parent)
     ORDER BY ai_ord ASC
  )
  (SELECT * FROM first_invoice_parent)
  UNION
  (SELECT * FROM all_billitems)
$$
LANGUAGE SQL
VOLATILE
RETURNS NULL ON NULL INPUT;

/*
  Just a wrapper for `get_billitem_path` to convert to array form.
*/
CREATE OR REPLACE FUNCTION get_billitem_path_array(user_group_id bigint)
RETURNS bigint[] AS
$$
DECLARE
  ug_path bigint[];
BEGIN
  SELECT INTO ug_path array(SELECT ug_id FROM get_billitem_path($1) ORDER BY ord);
  RETURN ug_path;
END;
$$
LANGUAGE PLPGSQL
VOLATILE
RETURNS NULL ON NULL INPUT;

/*
  A simple helper since postgres does not define any array reversal function.
*/
CREATE OR REPLACE FUNCTION array_reverse(anyarray)
RETURNS anyarray AS
$$
SELECT array( SELECT $1[i]
                FROM generate_subscripts($1,1) AS s(i)
               ORDER BY i DESC
            );
$$
LANGUAGE SQL
STRICT IMMUTABLE;

CREATE OR REPLACE FUNCTION array_last_id(ids bigint[])
RETURNS bigint AS
$$
BEGIN
  RETURN ids[array_length(ids, 1)];
END;
$$
LANGUAGE PLPGSQL
STRICT IMMUTABLE;

-- Print the report to CSV. Unfortunately the table shape needs to be defined
-- here in the 'head' of the function, meaning any 'AS' naming of columns are
-- ignored upon returning the table. We still keep them to be able to line up
-- the columns in a human-friendly manner.

CREATE OR REPLACE FUNCTION get_report_base(date_from TIMESTAMPTZ, date_to TIMESTAMPTZ)
    RETURNS TABLE
      (
        "User group name" TEXT,
        "User group ID" BIGINT,
        "User group root ID" BIGINT,
        "Company number" TEXT,
        "Company country" TEXT,
        "Salesforce ID" TEXT,
        "User group admin" TEXT,
        "Invoice/BillItem contact email" TEXT,
        "Is invoice" TEXT,
        "Is billitem" TEXT,
        "Invoice group" TEXT,
        "Bill path" BIGINT[],
        "Parent group path" BIGINT[],
        "Invoicing type" TEXT,
        "Payment plan" TEXT,
        "First doc signed" DATE,
        "Docs sent" BIGINT,
        "Docs closed" BIGINT,
        "Sigs closed" BIGINT,
        "SMSes sent" BIGINT,
        "SMSes sent (physical)" BIGINT,
        "Swedish BankID signatures (started)" BIGINT,
        "Swedish BankID signatures (finished)" BIGINT,
        "Swedish BankID authorization (started)" BIGINT,
        "Swedish BankID authorization (finished)" BIGINT,
        "Norwegian BankID signatures (started)" BIGINT,
        "Norwegian BankID signatures (finished)" BIGINT,
        "Norwegian BankID authorization (started)" BIGINT,
        "Norwegian BankID authorization (finished)" BIGINT,
        "Danish NemID signatures (started)" BIGINT,
        "Danish NemID signatures (finished)" BIGINT,
        "Danish NemID authorization (started)" BIGINT,
        "Danish NemID authorization (finished)" BIGINT,
        "Finnish TUPAS authorization (started)" BIGINT,
        "Finnish TUPAS authorization (finished)" BIGINT,
        "Finnish FTN signatures (started)" BIGINT,
        "Finnish FTN signatures (finished)" BIGINT,
        "Verimi authentications (started)" BIGINT,
        "Verimi authentications (finished)" BIGINT,
        "Verimi QES signatures (started)" BIGINT,
        "Verimi QES signatures (finished)" BIGINT,
        "iDIN authentications (started)" BIGINT,
        "iDIN authentications (finished)" BIGINT,
        "iDIN signatures (started)" BIGINT,
        "iDIN signatures (finished)" BIGINT,
        "Onfido (document check only) signatures (started)" BIGINT,
        "Onfido (document check only) signatures (finished)" BIGINT,
        "Onfido (doc check + facial comparison) signatures (started)" BIGINT,
        "Onfido (doc check + facial comparison) signatures (finished)" BIGINT,
        "Shareable links used" BIGINT,
        "Telia SMSes sent (physical)" BIGINT,
        "Users at start of period" BIGINT,
        "Users at end of period" BIGINT,
        "Users activated during period" BIGINT,
        "Users deleted during period" BIGINT,
        "Start date" TEXT,
        "End date" TEXT
      )
    AS
    $$
    BEGIN

      CREATE TEMPORARY TABLE period AS
      SELECT date_from AS "from", date_to AS "to";

      -- Why is this table wrapped in a PL/pgSQL function? Because if the table
      -- is not in the context of a PL/pgSQL call chain it's impossible to
      -- dereference any variable given to `psql` on the command line.
      CREATE TEMPORARY TABLE report AS
      SELECT escape_for_csv(user_groups.name) AS "User group name"
           , user_groups.id AS "User group ID"
           , (select coalesce(user_groups.parent_group_path[(select array_length(user_groups.parent_group_path, 1))] :: bigint, user_groups.id :: bigint )) AS "User group root ID"
           , escape_for_csv(user_group_addresses.company_number :: TEXT) AS "Company number"
           , escape_for_csv(user_group_addresses.country :: TEXT) AS "Company country"
           , escape_for_csv(user_group_tags.value :: TEXT) AS "Salesforce ID"
           , escape_for_csv((SELECT get_user_group_contact(user_groups.id)) :: TEXT) AS "User group admin"
           , escape_for_csv(get_user_group_contact(
                 array_last_id(
                   array_reverse((SELECT * FROM get_billitem_path_array(user_groups.id))))) :: TEXT) AS "Invoice/BillItem contact email"
           , escape_for_csv((CASE user_group_invoicings.invoicing_type = 3
                               WHEN true THEN 'x'
                               WHEN false THEN null
                             END :: TEXT)) AS "Is invoice"
           , escape_for_csv((CASE user_group_invoicings.invoicing_type = 2
                               WHEN true THEN 'x'
                               WHEN false THEN null
                             END :: TEXT)) AS "Is billitem"
           , escape_for_csv(get_invoicing_aggregation_group(user_groups.id, 3):: TEXT) AS "Invoice group"
           , array_reverse( (SELECT * FROM get_billitem_path_array(user_groups.id)) ) AS "Bill path"
           , array_reverse( user_groups.parent_group_path ) AS "Parent group path"
           , escape_for_csv(CASE user_group_invoicings.invoicing_type
                WHEN 1 THEN 'none'
                WHEN 2 THEN 'billitem'
                WHEN 3 THEN 'invoice'
              END :: TEXT) AS "Invoicing type"
           , escape_for_csv(CASE (SELECT get_payment_plan(user_groups.id))
                WHEN 0 THEN 'free'
                WHEN 1 THEN 'trial'
                WHEN 2 THEN 'paid'
              END :: TEXT) AS "Payment plan"
           , (SELECT MIN(d.mtime)::date
                FROM documents d
                JOIN signatory_links sl ON d.author_id = sl.id
                JOIN users u            ON sl.user_id = u.id
               WHERE d.type = 1
                 AND d.status = 3
                 AND u.user_group_id = user_groups.id
             ) AS "First doc signed"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 6
                 AND chi.time >= period.from
                 AND chi.time < period.to) AS "Docs sent"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 8
                 AND chi.time >= period.from
                 AND chi.time < period.to) AS "Docs closed"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 9
                 AND chi.time >= period.from
                 AND chi.time < period.to) AS "Sigs closed"
           , (SELECT count(*)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 1 -- sms
                 AND chi.time >= period.from
                 AND chi.time < period.to) AS "SMSes sent"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 1 -- sms
                 AND chi.time >= period.from
                 AND chi.time < period.to) AS "SMSes sent (physical)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 20 -- eleg signatures
                 AND chi.time >= period.from
                 AND chi.time < period.to)  as "Swedish BankID signatures (started)"
           ,(SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 2 -- eleg signatures
                 AND chi.time >= period.from
                 AND chi.time < period.to)  as "Swedish BankID signatures (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 21 -- eleg authentication
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Swedish BankID authorization (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 3 -- eleg authentication
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Swedish BankID authorization (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 24
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Norwegian BankID signatures (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 10
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Norwegian BankID signatures (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 22
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Norwegian BankID authorization (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 4
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Norwegian BankID authorization (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 25
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Danish NemID signatures (started)"
           ,  (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 11
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Danish NemID signatures (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 23
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Danish NemID authorization (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 7
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Danish NemID authorization (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 26
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Finnish TUPAS authorization (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 12
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Finnish TUPAS authorization (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 27
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Finnish FTN signatures (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 17
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Finnish FTN signatures (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 28 -- CIVerimiAuthenticationStarted
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Verimi authentications (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 14 -- CIVerimiAuthenticationFinished
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Verimi authentications (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 33 -- CIVerimiQesSignatureStarted
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Verimi QES signatures (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 34 -- CIVerimiQesSignatureFinished
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Verimi QES signatures (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 29 -- CIIDINAuthenticationStarted
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "iDIN authentications (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 15 -- CIIDINAuthenticationFinished
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "iDIN authentications (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 30 -- CIIDINSignatureStarted
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "iDIN signatures (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 16 -- CIIDINSignatureFinished
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "iDIN signatures (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 31
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Onfido (document check only) signatures (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 18
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Onfido (document check only) signatures (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 32
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Onfido (doc check + facial comparison) signatures (started)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 19
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Onfido (doc check + facial comparison) signatures (finished)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 13
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Shareable links used"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 5
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "Telia SMSes sent (physical)"
           , (SELECT count(*)
                FROM users
               WHERE (users.deleted IS NULL OR users.deleted > period.from)
                 AND users.email NOT LIKE '%@scrive.com'
                 AND users.user_group_id = user_groups.id
                 AND users.has_accepted_terms_of_service <= period.from) AS "Users at start of period"
           , (SELECT count(*)
                FROM users
               WHERE (users.deleted IS NULL OR users.deleted > period.to)
                 AND users.email NOT LIKE '%@scrive.com'
                 AND users.user_group_id = user_groups.id
                 AND users.has_accepted_terms_of_service <= period.to) AS "Users at end of period"
           , (SELECT count(*)
                FROM users
               WHERE users.user_group_id = user_groups.id
                 AND users.email NOT LIKE '%@scrive.com'
                 AND has_accepted_terms_of_service >= period.from
                 AND has_accepted_terms_of_service < period.to) AS "Users activated during period"
           , (SELECT count(*)
                FROM users
               WHERE users.user_group_id = user_groups.id
                 AND users.email NOT LIKE '%@scrive.com'
                 AND users.deleted >= period.from
                 AND users.deleted < period.to) AS "Users deleted during period"
           , escape_for_csv(substring((period.from :: DATE :: TEXT) for 10)) AS "Start date"
           , escape_for_csv(substring((period.to :: DATE :: TEXT) for 10)) AS "End date"
         FROM      user_groups
              JOIN user_group_addresses ON user_groups.id = user_group_addresses.user_group_id
              JOIN user_group_invoicings ON user_groups.id = user_group_invoicings.user_group_id
         LEFT JOIN user_group_tags ON (    user_groups.id=user_group_tags.user_group_id
                                       AND user_group_tags.name='sf-account-id')
        CROSS JOIN period
        WHERE (   user_group_invoicings.payment_plan <> 0
               OR user_groups.parent_group_id IS NOT NULL);
       RETURN QUERY
          SELECT *
           FROM report
          WHERE report."Sigs closed" > 0
             OR report."Docs closed" > 0
             OR report."SMSes sent" > 0
             OR report."Swedish BankID signatures (started)" > 0
             OR report."Swedish BankID signatures (finished)" > 0
             OR report."Swedish BankID authorization (started)" > 0
             OR report."Swedish BankID authorization (finished)" > 0
             OR report."Norwegian BankID signatures (started)" > 0
             OR report."Norwegian BankID signatures (finished)" > 0
             OR report."Norwegian BankID authorization (started)" > 0
             OR report."Norwegian BankID authorization (finished)" > 0
             OR report."Danish NemID signatures (started)" > 0
             OR report."Danish NemID signatures (finished)" > 0
             OR report."Danish NemID authorization (started)" > 0
             OR report."Danish NemID authorization (finished)" > 0
             OR report."Finnish TUPAS authorization (started)" > 0
             OR report."Finnish TUPAS authorization (finished)" > 0
             OR report."Finnish FTN signatures (started)" > 0
             OR report."Finnish FTN signatures (finished)" > 0
             OR report."Verimi authentications (started)" > 0
             OR report."Verimi authentications (finished)" > 0
             OR report."Verimi QES signatures (started)" > 0
             OR report."Verimi QES signatures (finished)" > 0
             OR report."iDIN authentications (started)" > 0
             OR report."iDIN authentications (finished)" > 0
             OR report."iDIN signatures (started)" > 0
             OR report."iDIN signatures (finished)" > 0
             OR report."Onfido (document check only) signatures (started)" > 0
             OR report."Onfido (document check only) signatures (finished)" > 0
             or report."Onfido (doc check + facial comparison) signatures (started)" > 0
             or report."Onfido (doc check + facial comparison) signatures (finished)" > 0
             OR report."Shareable links used" > 0
             OR report."Users at start of period" > 0
             OR report."Users at end of period" > 0
          ORDER BY 1, 2;
    END;
    $$
    LANGUAGE 'plpgsql' VOLATILE;

/*
  We have to do the CSV output in a this roundabout way since the `\copy`
  command takes only a plain SQL result, so we trick it with `CREATE TABLE`.
  We are unable to use server-side copy too, since it requires raised
  privileges.
*/
CREATE TEMPORARY TABLE report_base AS (SELECT * FROM get_report_base(:'date_from', :'date_to'));

CREATE TEMPORARY TABLE report_master AS
(select
  "User group name",
  "User group ID",
  escape_for_csv((SELECT name FROM user_groups WHERE id="User group root ID") :: TEXT) AS "User group root name",
  "User group root ID",
  "Company country",
  "Salesforce ID",
  "User group admin",
  "Is invoice",
  "Is billitem",
  "Invoice group",
  escape_for_csv(array_to_string( "Bill path", ', ')) AS "Bill path",
  escape_for_csv(array_to_string("Parent group path", ', ')) AS "Parent group path",
  "Invoicing type",
  "Payment plan",
  "First doc signed",
  "Docs sent",
  "Docs closed",
  "Sigs closed",
  "SMSes sent",
  "SMSes sent (physical)",
  "Swedish BankID signatures (started)",
  "Swedish BankID signatures (finished)",
  "Swedish BankID authorization (started)",
  "Swedish BankID authorization (finished)",
  "Norwegian BankID signatures (started)",
  "Norwegian BankID signatures (finished)",
  "Norwegian BankID authorization (started)",
  "Norwegian BankID authorization (finished)",
  "Danish NemID signatures (started)",
  "Danish NemID signatures (finished)",
  "Danish NemID authorization (started)",
  "Danish NemID authorization (finished)",
  "Finnish TUPAS authorization (started)",
  "Finnish TUPAS authorization (finished)",
  "Finnish FTN signatures (started)",
  "Finnish FTN signatures (finished)",
  "Verimi authentications (started)",
  "Verimi authentications (finished)",
  "Verimi QES signatures (started)",
  "Verimi QES signatures (finished)",
  "iDIN authentications (started)",
  "iDIN authentications (finished)",
  "iDIN signatures (started)",
  "iDIN signatures (finished)",
  "Onfido (document check only) signatures (started)",
  "Onfido (document check only) signatures (finished)",
  "Onfido (doc check + facial comparison) signatures (started)",
  "Onfido (doc check + facial comparison) signatures (finished)",
  "Shareable links used",
  "Telia SMSes sent (physical)",
  "Users at start of period",
  "Users at end of period",
  "Users activated during period",
  "Users deleted during period",
  "Start date",
  "End date"
  from report_base);

\copy report_master TO report-master.csv WITH (FORMAT CSV, HEADER, DELIMITER ';');

CREATE TEMPORARY TABLE report_aggregated AS
  (SELECT
        escape_for_csv((SELECT name FROM user_groups WHERE id="User group root ID") :: TEXT) AS "User group root name",
        "User group root ID",
        "Invoice/BillItem contact email",
        -- The fields "Is billitem" and "Is invoice" in the master file are
        -- _not_ applicable for those user groups that have invoicing type
        -- `None`, so we recalculate. The 'Bill path' has by construction the
        -- property that
        --   * if length == 1, then the item shall be included in an invoicing parent
        --   * if length > 1, then the item shall be included in a billitem
         escape_for_csv(CASE array_length("Bill path", 1)
                          WHEN 0 THEN 'bill path error'
                          WHEN 1 THEN 'x'
                          ELSE NULL
                       END :: TEXT) AS "Is invoice (aggregated)",
         escape_for_csv(CASE array_length("Bill path", 1)
                          WHEN 0 THEN 'bill path error'
                          WHEN 1 THEN NULL
                          ELSE 'x'
                       END :: TEXT) AS "Is billitem (aggregated)",
        "Invoice group",
        escape_for_csv((SELECT name FROM user_groups WHERE id = array_last_id("Bill path")) :: TEXT) AS "Invoice heading",
        escape_for_csv(array_last_id("Bill path") :: TEXT) AS "Invoice heading ID",
        escape_for_csv(array_to_string( "Bill path", ', ')) AS "Bill path",
        "Payment plan",
        sum("Docs sent") AS "Docs sent",
        sum("Docs closed") AS "Docs closed",
        sum("Sigs closed") AS "Sigs closed",
        sum("SMSes sent") AS "SMSes sent",
        sum("SMSes sent (physical)") AS "SMSes sent (physical)",
        sum("Swedish BankID signatures (started)") AS "Swedish BankID signatures (started)",
        sum("Swedish BankID signatures (finished)") AS "Swedish BankID signatures (finished)",
        sum("Swedish BankID authorization (started)") AS "Swedish BankID authorization (started)",
        sum("Swedish BankID authorization (finished)") AS "Swedish BankID authorization (finished)",
        sum("Norwegian BankID signatures (started)") AS "Norwegian BankID signatures (started)",
        sum("Norwegian BankID signatures (finished)") AS "Norwegian BankID signatures (finished)",
        sum("Norwegian BankID authorization (started)") AS "Norwegian BankID authorization (started)",
        sum("Norwegian BankID authorization (finished)") AS "Norwegian BankID authorization (finished)",
        sum("Danish NemID signatures (started)") AS "Danish NemID signatures (started)",
        sum("Danish NemID signatures (finished)") AS "Danish NemID signatures (finished)",
        sum("Danish NemID authorization (started)") AS "Danish NemID authorization (started)",
        sum("Danish NemID authorization (finished)") AS "Danish NemID authorization (finished)",
        sum("Finnish TUPAS authorization (started)") AS "Finnish TUPAS authorization (started)",
        sum("Finnish TUPAS authorization (finished)") AS "Finnish TUPAS authorization (finished)",
        sum("Verimi authentications (started)") AS "Verimi authentications (started)",
        sum("Verimi authentications (finished)") AS "Verimi authentications (finished)",
        sum("Verimi QES signatures (started)") AS "Verimi QES signatures (started)",
        sum("Verimi QES signatures (finished)") AS "Verimi QES signatures (finished)",
        sum("iDIN authentications (started)") AS "iDIN authentications (started)",
        sum("iDIN authentications (finished)") AS "iDIN authentications (finished)",
        sum("iDIN signatures (started)") AS "iDIN signatures (started)",
        sum("iDIN signatures (finished)") AS "iDIN signatures (finished)",
        sum("Shareable links used") AS "Shareable links used",
        sum("Telia SMSes sent (physical)") AS "Telia SMSes sent (physical)",
        sum("Users at start of period") AS "Users at start of period",
        sum("Users at end of period") AS "Users at end of period",
        sum("Users activated during period") AS "Users activated during period",
        sum("Users deleted during period") AS "Users deleted during period",
        min("Start date") AS "Start date",
        max("End date") AS "End date"
   FROM report_base
  GROUP BY "Invoice/BillItem contact email",
           "Payment plan",
           "User group root ID",
           "Is invoice (aggregated)",
           "Is billitem (aggregated)",
           "Bill path",
           "Invoice group"
  ORDER BY "Invoice group");
\copy report_aggregated TO report-aggregated.csv WITH (FORMAT CSV, HEADER, DELIMITER ';');

/*
    Notes for the aggregated output:

    It would be enough to just group by "Bill path", but the other columns are
    needed in the output and are calculated to be equal and groupable where
    appropriate. The other approach would have been to write functions that
    calculate everything based on "Bill path".

    We change name of the two fields "Is invoice" and "Is billitem", in the
    aggregated output because
    1) The sets sent to the aggregate functions include any nodes in the subtree
       for which this is the closest ancestor that has invoicing or billitem set
       (whichever is closer).
    2) The grouping clause prefers the input field name to the output field name
       which does not play nice with the code we have since we recalculate those
       fields (due to `None`).
*/


-- format timestamps to use as dates in report file
SELECT to_char((:'date_from' :: TIMESTAMPTZ), ('YYYYMMDD' :: TEXT)) AS "filedate_from" \gset
SELECT to_char((:'date_to' :: TIMESTAMPTZ), ('YYYYMMDD' :: TEXT)) AS "filedate_to" \gset

-- To get date in filename of the report: write current query buffer to
-- shell, sed the dates out of it and change filename. We do it in this way since
-- 1) we don't want a separate script wrapper
-- 2) psql cannot name the file using the arguments supplied directly (in
-- `\copy` command above).
-- We add a UTF-8 BOM for the benefit of Excel imports.

-- create directory for reports
SELECT :'report_dir' AS "dir";
\w | mkdir -p `sed "s/SELECT '\(.*\)' AS \"dir\";/\1/"`

\! printf '\xef\xbb\xbf' >> report_tmp.csv;
\! cat report-master.csv >> report_tmp.csv;
-- update query buffer with current details for use in pipe; use is simply to add a date
SELECT :'report_dir' AS "dir", :'filedate_from' AS "From", :'filedate_to' AS "To";
\w | mv report_tmp.csv `sed "s/SELECT '\(.*\)' AS \"dir\".*\([0-9]\{8\}\).*\([0-9]\{8\}\).*/\1\/report-master-\2_\3.csv/"`;
\! rm report-master.csv;
\! rm -f report_tmp.csv

\! printf '\xef\xbb\xbf' >> report_tmp.csv;
\! cat report-aggregated.csv >> report_tmp.csv;
-- update query buffer with date details again
SELECT :'report_dir' AS "dir", :'filedate_from' AS "From", :'filedate_to' AS "To";
\w | mv report_tmp.csv `sed "s/SELECT '\(.*\)' AS \"dir\".*\([0-9]\{8\}\).*\([0-9]\{8\}\).*/\1\/report-aggregated-\2_\3.csv/"`;
\! rm report-aggregated.csv;
\! rm -f report_tmp.csv;
ROLLBACK;
