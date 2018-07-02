-- As its predecessor, the script uses correlated subqueries all over the
-- place. If slow, convert to use joins.

\set QUIET on

BEGIN;

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
  whatever day `date_to` evaluates to will be used. Cf. @note:2

- `date_to` should be specified in the format 'YYYY-MM-DD' or 'YYYYMMDD'. If
  `date_to` is missing, the very beginning of the current day will be
  used. Cf. @note:1

* Hack notes

- At the end of the script there is an explanation as to why we do the things we
  do. Basically, `psql` cannot interpolate the arguments we send it into queries.

*/

/*
  We initialise start and end of period with defaults as per the folklore item
  http://stackoverflow.com/a/32597876. A simple application of `coalesce` does
  *not* do it, nor a simple table with defaults, since checking for non-existing
  command line arguments crashes the script and would need exception handling.
*/

\set date_from :date_from
\set date_to   :date_to

-- @note:1
SELECT CASE
  WHEN :'date_to' = ':date_to'
  THEN (date_trunc('day', now()) :: TEXT)
  ELSE :'date_to'
END AS "date_to" \gset

-- @note:2
SELECT CASE
  WHEN :'date_from' = ':date_from'
  THEN (date_trunc('month', timestamptz(:'date_to') - interval '1 day') :: TEXT)
  ELSE :'date_from'
END AS "date_from" \gset

/*
  According to http://stackoverflow.com/a/814194 to embed commas and leading zeros
  in a string it should look like "=""001,002""".
*/

CREATE FUNCTION escape_for_csv(text) RETURNS text
    AS
    $$
      SELECT '="' || regexp_replace($1,'"','"" & Chr(34) & ""','g') || '"';
    $$
    LANGUAGE SQL
    IMMUTABLE
    RETURNS NULL ON NULL INPUT;

-- Print the report to CSV. Unfortunately the table shape needs to be defined
-- here in the 'head' of the function, meaning any 'AS' naming of columns are
-- ignored upon returning the table. We still keep them to be able to line up
-- the columns in a human-friendly manner.
CREATE OR REPLACE FUNCTION print_csv(date_from TIMESTAMPTZ, date_to TIMESTAMPTZ)
    RETURNS TABLE
      (
        "Company name" TEXT,
        "Company ID" TEXT,
        "Company number" TEXT,
        "Partner ID" TEXT,
        "Partner name" TEXT,
        "Company admin" TEXT,
        "Payment plan" TEXT,
        "First doc signed" DATE,
        "Docs sent" BIGINT,
        "Docs closed" BIGINT,
        "Sigs closed" BIGINT,
        "SMSes sent" BIGINT,
        "SMSes sent (physical)" BIGINT,
        "Swedish BankID signatures" BIGINT,
        "Swedish BankID authorization" BIGINT,
        "Norwegian BankID signatures" BIGINT,
        "Norwegian BankID authorization" BIGINT,
        "Danish NemID signatures" BIGINT,
        "Danish NemID authorization" BIGINT,
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
      SELECT escape_for_csv(user_groups.name) AS "Company name"
           , escape_for_csv(user_groups.id :: TEXT) AS "Company ID"
           , escape_for_csv(user_group_addresses.company_number :: TEXT) AS "Company number"
           , escape_for_csv(user_groups.parent_group_id :: TEXT) AS "Partner ID"
           , escape_for_csv(COALESCE((SELECT pug.name FROM user_groups as pug WHERE pug.id = user_groups.parent_group_id),'Scrive') :: TEXT) AS "Partner name"
           , escape_for_csv((
             SELECT email
               FROM users
              WHERE users.is_company_admin
                AND users.user_group_id = user_groups.id
              LIMIT 1
             ) :: TEXT) AS "Company admin"
           , escape_for_csv(CASE user_group_invoicings.payment_plan
                WHEN 0 THEN 'free'
                WHEN 1 THEN 'one'
                WHEN 2 THEN 'team'
                WHEN 3 THEN 'enterprise'
                WHEN 4 THEN 'trial'
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
                 AND chi.time < period.to) as "Docs sent"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 8
                 AND chi.time >= period.from
                 AND chi.time < period.to) as "Docs closed"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 9
                 AND chi.time >= period.from
                 AND chi.time < period.to) as "Sigs closed"
           , (SELECT count(*)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 1 -- sms
                 AND chi.time >= period.from
                 AND chi.time < period.to) as "SMSes sent"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 1 -- sms
                 AND chi.time >= period.from
                 AND chi.time < period.to) as "SMSes sent (physical)"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 2 -- eleg signatures
                 AND chi.time >= period.from
                 AND chi.time < period.to)  as "Swedish BankID signatures"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 3 -- eleg authentication
                 AND chi.time >= period.from
                 AND chi.time <= period.to) as "Swedish BankID authorization"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 10
                 AND chi.time >= period.from
                 AND chi.time <= period.to) as "Norwegian BankID signatures"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 4
                 AND chi.time >= period.from
                 AND chi.time <= period.to) as "Norwegian BankID authorization"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 11
                 AND chi.time >= period.from
                 AND chi.time <= period.to) as "Danish NemID signatures"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 7
                 AND chi.time >= period.from
                 AND chi.time <= period.to) as "Danish NemID authorization"
           , (SELECT sum(chi.quantity)
                FROM chargeable_items chi
               WHERE chi.user_group_id = user_groups.id
                 AND chi.type = 5
                 AND chi.time >= period.from
                 AND chi.time <= period.to) as "Telia SMSes sent (physical)"
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
        FROM user_groups join user_group_addresses on user_groups.id = user_group_addresses.user_group_id join user_group_invoicings on user_groups.id = user_group_invoicings.user_group_id CROSS JOIN period WHERE user_group_invoicings.payment_plan <> 0;
        RETURN QUERY
          SELECT *
           FROM report
          WHERE report."Sigs closed" > 0
             OR report."Docs closed" > 0
             OR report."SMSes sent" > 0
             OR report."Swedish BankID signatures" > 0
             OR report."Swedish BankID authorization" > 0
             OR report."Norwegian BankID signatures" > 0
             OR report."Norwegian BankID authorization" > 0
             OR report."Danish NemID signatures" > 0
             OR report."Danish NemID authorization" > 0
             OR report."Users at start of period" > 0
             OR report."Users at end of period" > 0
          ORDER BY 1, 2;
    END;
    $$
    LANGUAGE 'plpgsql' VOLATILE;

-- We have to do the CSV output in a this roundabout way since the `\copy`
-- command takes only a plain SQL result, so we trick it with `CREATE TABLE`.
-- We are unable to use server-side copy too, since it requires raised
-- privileges.
CREATE TABLE output AS (SELECT * FROM print_csv(:'date_from', :'date_to'));
\copy output TO report.csv WITH (FORMAT CSV, HEADER);
-- format timestamps to use as dates in report file
SELECT to_char((:'date_from' :: TIMESTAMPTZ), ('YYYYMMDD' :: TEXT)) AS "filedate_from" \gset
SELECT to_char((:'date_to' :: TIMESTAMPTZ), ('YYYYMMDD' :: TEXT)) AS "filedate_to" \gset
-- update query buffer with current details for use in pipe
SELECT :'filedate_from' AS "From", :'filedate_to' AS "To";

-- To get date in filename of the report: write current query buffer to
-- shell, sed the dates out of it and change filename. We do it in this way since
-- 1) we don't want a separate script wrapper
-- 2) psql cannot name the file using the arguments supplied directly (in
-- `\copy` command above); cf. 'Hack notes'.
\w | mv report.csv report-$(sed "s/.*\([0-9]\{8\}\).*\([0-9]\{8\}\).*/\1_\2/g").csv

ROLLBACK;
