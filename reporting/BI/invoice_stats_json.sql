-- What is this file about? (Appears to be very out of date!)
/*

*/

\set QUIET on
\timing on
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
  str = regexp_replace(str,'"','"" & chr(34) & ""','g');
  str = regexp_replace(str,';','','g');
  return str;
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
RETURNS NULL ON NULL INPUT;

create or replace function anon_email(str text)
returns text AS
$$
begin
  str = regexp_replace(str,'.*(@.*)','email\1','g');
  return str;
end;
$$
language plpgsql
immutable
returns null on null input;

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
        "user_group_name" text,
        "user_group_id" text,
        "user_group_root_id" bigint,
        "company_number" text,
        "salesforce_id" text,
        "user_group_admin" text,
        "invoice_contact_email" text,
        "is_invoice" text,
        "is_billitem" text,
        "invoice_group" text,
        "bill_path" bigint[],
        "parent_group_path" text[],
        "invoicing_type" text,
        "payment_plan" text,
        "first_doc_signed" date,
        "docs_sent" bigint,
        "docs_closed" bigint,
        "sigs_closed" bigint,
        "smses_sent" bigint,
        "smses_sent_physical" bigint,
        "swedish_bankid_signatures" bigint,
        "swedish_bankid_authorization" bigint,
        "norwegian_bankid_signatures" bigint,
        "norwegian_bankid_authorization" bigint,
        "danish_nemid_signatures" bigint,
        "danish_nemid_authorization" bigint,
        "finnish_tupas_authorization" bigint,
        "verimi_authentications" bigint,
        "idin_authentications" bigint,
        "idin_signatures" bigint,
        "shareable_links_used" bigint,
        "telia_smses_sent_physical" bigint,
        "users_at_start_of_period" bigint,
        "users_at_end_of_period" bigint,
        "users_activated_during_period" bigint,
        "users_deleted_during_period" bigint,
        "start_date" text,
        "end_date" text
      )
    AS
    $$
    BEGIN

      CREATE TEMPORARY TABLE period AS
      SELECT date_from AS "from", date_to as "to";

      -- Why is this table wrapped in a PL/pgSQL function? Because if the table
      -- is not in the context of a PL/pgSQL call chain it's impossible to
      -- dereference any variable given to `psql` on the command line.
      CREATE TEMPORARY TABLE report AS
      SELECT escape_for_csv(user_groups.name) AS "user_group_name"
           , user_groups.id::text AS "user_group_id"
           , (select coalesce(user_groups.parent_group_path[(select array_length(user_groups.parent_group_path, 1))] :: bigint, user_groups.id :: bigint )) AS "user_group_root_id"
           , escape_for_csv(user_group_addresses.company_number :: TEXT) AS "company_number"
           , escape_for_csv(user_group_tags.value :: TEXT) AS "salesforce_id"
           , anon_email(escape_for_csv((SELECT get_user_group_contact(user_groups.id)) :: TEXT)) AS "user_group_admin"
           , anon_email(escape_for_csv(get_user_group_contact(
                 array_last_id(
                   array_reverse((SELECT * FROM get_billitem_path_array(user_groups.id))))) :: TEXT)) AS "invoice_contact_email"
           , escape_for_csv((CASE user_group_invoicings.invoicing_type = 3
                               WHEN true THEN 'x'
                               WHEN false THEN null
                             END :: TEXT)) AS "is_invoice"
           , escape_for_csv((CASE user_group_invoicings.invoicing_type = 2
                               WHEN true THEN 'x'
                               WHEN false THEN null
                             END :: TEXT)) AS "is_billitem"
           , escape_for_csv(get_invoicing_aggregation_group(user_groups.id, 3):: TEXT) AS "invoice_group"
           , array_reverse( (SELECT * FROM get_billitem_path_array(user_groups.id)) ) AS "bill_path"
           , array_reverse( user_groups.parent_group_path )::text[] AS "parent_group_path"
           , escape_for_csv(CASE user_group_invoicings.invoicing_type
                WHEN 1 THEN 'none'
                WHEN 2 THEN 'billitem'
                WHEN 3 THEN 'invoice'
              END :: TEXT) AS "invoicing_type"
           , escape_for_csv(CASE (SELECT get_payment_plan(user_groups.id))
                WHEN 0 THEN 'free'
                WHEN 1 THEN 'trial'
                WHEN 2 THEN 'paid'
              END :: TEXT) AS "payment_plan"
           , (SELECT MIN(d.mtime)::date
                FROM documents d
                JOIN signatory_links sl ON d.author_id = sl.id
                JOIN users u            ON sl.user_id = u.id
               WHERE d.type = 1
                 AND d.status = 3
                 AND u.user_group_id = user_groups.id
             ) AS "first_doc_signed"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 6
                 AND chi.time >= period.from
                 AND chi.time < period.to) AS "docs_sent"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 8
                 AND chi.time >= period.from
                 AND chi.time < period.to) AS "docs_closed"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 9
                 AND chi.time >= period.from
                 AND chi.time < period.to) AS "sigs_closed"
           , (SELECT count(*)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 1 -- sms
                 AND chi.time >= period.from
                 AND chi.time < period.to) AS "smses_sent"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 1 -- sms
                 AND chi.time >= period.from
                 AND chi.time < period.to) AS "smses_sent_physical"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 2 -- eleg signatures
                 AND chi.time >= period.from
                 AND chi.time < period.to)  as "swedish_bankid_signatures"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 3 -- eleg authentication
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "swedish_bankid_authorization"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 10
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "norwegian_bankid_signatures"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 4
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "norwegian_bankid_authorization"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 11
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "danish_nemid_signatures"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 7
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "danish_nemid_authorization"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 12
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "finnish_tupas_authorization"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 14 -- CIVerimiAuthentication
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "verimi_authentications"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 15 -- CIIDINAuthentication
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "idin_authentications"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 16 -- CIIDINSignature
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "idin_signatures"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 13
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "shareable_links_used"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 5
                 AND chi.time >= period.from
                 AND chi.time <= period.to) AS "telia_smses_sent_physical"
           , (SELECT count(*)
                FROM users
               WHERE (users.deleted IS NULL OR users.deleted > period.from)
                 AND users.email NOT LIKE '%@scrive.com'
                 AND users.user_group_id = user_groups.id
                 AND users.has_accepted_terms_of_service <= period.from) AS "users_at_start_of_period"
           , (SELECT count(*)
                FROM users
               WHERE (users.deleted IS NULL OR users.deleted > period.to)
                 AND users.email NOT LIKE '%@scrive.com'
                 AND users.user_group_id = user_groups.id
                 AND users.has_accepted_terms_of_service <= period.to) AS "users_at_end_of_period"
           , (SELECT count(*)
                FROM users
               WHERE users.user_group_id = user_groups.id
                 AND users.email NOT LIKE '%@scrive.com'
                 AND has_accepted_terms_of_service >= period.from
                 AND has_accepted_terms_of_service < period.to) AS "users_activated_during_period"
           , (SELECT count(*)
                FROM users
               WHERE users.user_group_id = user_groups.id
                 AND users.email NOT LIKE '%@scrive.com'
                 AND users.deleted >= period.from
                 AND users.deleted < period.to) AS "users_deleted_during_period"
           , escape_for_csv(substring((period.from :: DATE :: TEXT) for 10)) AS "start_date"
           , escape_for_csv(substring((period.to :: DATE :: TEXT) for 10)) AS "end_date"
         FROM user_groups
         JOIN user_group_addresses ON user_groups.id = user_group_addresses.user_group_id
         JOIN user_group_invoicings ON user_groups.id = user_group_invoicings.user_group_id
    LEFT JOIN user_group_tags ON (    user_groups.id=user_group_tags.user_group_id
                                  AND user_group_tags.name='sf-account-id')
        CROSS JOIN period
        WHERE (   user_group_invoicings.payment_plan <> 0
               OR user_groups.parent_group_id IS NOT NULL);
--  and user_groups.id = 9197237133460592416;
       RETURN QUERY
          SELECT *
           FROM report
          WHERE report."sigs_closed" > 0
             OR report."docs_closed" > 0
             OR report."smses_sent" > 0
             OR report."swedish_bankid_signatures" > 0
             OR report."swedish_bankid_authorization" > 0
             OR report."norwegian_bankid_signatures" > 0
             OR report."norwegian_bankid_authorization" > 0
             OR report."danish_nemid_signatures" > 0
             OR report."danish_nemid_authorization" > 0
             OR report."finnish_tupas_authorization" > 0
             OR report."verimi_authentications" > 0
             OR report."idin_authentications" > 0
             OR report."idin_signatures" > 0
             OR report."shareable_links_used" > 0
             OR report."users_at_start_of_period" > 0
             OR report."users_at_end_of_period" > 0
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
  "user_group_name",
  "user_group_id"::text,
  escape_for_csv((SELECT name FROM user_groups WHERE id="user_group_root_id") :: text) as "user_group_root_name",
  "user_group_root_id"::text,
  "salesforce_id",
  "user_group_admin",
  "is_invoice",
  "is_billitem",
  "invoice_group",
  escape_for_csv(array_to_string( "bill_path", ', ')) as "bill_path",
  escape_for_csv(array_to_string("parent_group_path", ', ')) as "parent_group_path",
  "invoicing_type",
  "payment_plan",
  "first_doc_signed",
  "docs_sent",
  "docs_closed",
  "sigs_closed",
  "smses_sent",
  "smses_sent_physical",
  "swedish_bankid_signatures",
  "swedish_bankid_authorization",
  "norwegian_bankid_signatures",
  "norwegian_bankid_authorization",
  "danish_nemid_signatures",
  "danish_nemid_authorization",
  "finnish_tupas_authorization",
  "verimi_authentications",
  "idin_authentications",
  "idin_signatures",
  "shareable_links_used",
  "telia_smses_sent_physical",
  "users_at_start_of_period",
  "users_at_end_of_period",
  "users_activated_during_period",
  "users_deleted_during_period",
  "start_date",
  "end_date"
  from report_base);

create temporary table report_master_json as
  (select row_to_json(report_master) from report_master);

\copy report_master_json TO master_tmp.json WITH (FORMAT TEXT);

\! /usr/local/bin/jq -s -c <master_tmp.json > master_$(date '+%Y-%m-%d_%H:%M:%S').json
\! rm master_tmp.json

CREATE TABLE report_aggregated AS
  (SELECT
        escape_for_csv((SELECT name FROM user_groups WHERE id="user_group_root_id") :: text) as "user_group_root_name",
        "user_group_root_id"::text,
        "invoice_contact_email",
        -- The fields "is billitem" and "is invoice" in the master file are
        -- _not_ applicable for those user groups that have invoicing type
        -- `None`, so we recalculate. The 'Bill path' has by construction the
        -- property that
        --   * if length == 1, then the item shall be included in an invoicing parent
        --   * if length > 1, then the item shall be included in a billitem
         escape_for_csv(CASE array_length("bill_path", 1)
                          WHEN 0 THEN 'bill path error'
                          WHEN 1 THEN 'x'
                          ELSE NULL
                       END :: TEXT) AS "is_invoice",
         escape_for_csv(CASE array_length("bill_path", 1)
                          WHEN 0 THEN 'bill path error'
                          WHEN 1 THEN NULL
                          ELSE 'x'
                       END :: TEXT) AS "is_billitem",
        "invoice_group",
        escape_for_csv((SELECT name FROM user_groups WHERE id = array_last_id("bill_path")) :: text) as "invoice_heading",
        escape_for_csv(array_last_id("bill_path") :: text) as "invoice_heading_id",
        escape_for_csv(array_to_string( "bill_path", ', ')) as "bill_path",
        "payment_plan",
        sum("docs_sent") as "docs_sent",
        sum("docs_closed") as "docs_closed",
        sum("sigs_closed") as "sigs_closed",
        sum("smses_sent") as "smses_sent",
        sum("smses_sent_physical") as "smses_sent_physical",
        sum("swedish_bankid_signatures") as "swedish_bankid_signatures",
        sum("swedish_bankid_authorization") as "swedish_bankid_authorization",
        sum("norwegian_bankid_signatures") as "norwegian_bankid_signatures",
        sum("norwegian_bankid_authorization") as "norwegian_bankid_authorization",
        sum("danish_nemid_signatures") as "danish_nemid_signatures",
        sum("danish_nemid_authorization") as "danish_nemid_authorization",
        sum("finnish_tupas_authorization") as "finnish_tupas_authorization",
        sum("verimi_authentications") as "verimi_authentications",
        sum("idin_authentications") as "idin_authentications",
        sum("idin_signatures") as "idin_signatures",
        sum("shareable_links_used") as "shareable_links_used",
        sum("telia_smses_sent_physical") as "telia_smses_sent_physical",
        sum("users_at_start_of_period") as "users_at_start_of_period",
        sum("users_at_end_of_period") as "users_at_end_of_period",
        sum("users_activated_during_period") as "users_activated_during_period",
        sum("users_deleted_during_period") as "users_deleted_during_period",
        min("start_date") as "start_date",
        max("end_date") as "end_date"
   FROM report_base
  GROUP BY "invoice_contact_email",
           "payment_plan",
           "user_group_root_id",
           "is_invoice",
           "is_billitem",
           "bill_path",
           "invoice_group"
  ORDER BY "invoice_group");

create temporary table report_aggregated_json as
  (select row_to_json(report_aggregated) from report_aggregated);

\copy report_aggregated_json to aggregated_tmp.json with (format text);
\! /usr/local/bin/jq -s -c < aggregated_tmp.json > aggregated_$(date '+%Y-%m-%d_%H:%M:%S').json
\! rm aggregated_tmp.json

ROLLBACK;
