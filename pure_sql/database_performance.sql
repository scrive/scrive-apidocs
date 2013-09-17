

BEGIN;

CREATE FUNCTION fix_query(text) RETURNS text
    AS $$
          SELECT substring(trim(regexp_replace($1,'\s+',' ','g')) for 60);
       $$
    LANGUAGE SQL
    IMMUTABLE
    RETURNS NULL ON NULL INPUT;

SELECT pg_tables.tablename AS "Table name"
     , pg_size_pretty(pg_total_relation_size(pg_tables.tablename :: regclass :: oid)) AS "Size"
  FROM pg_tables
 WHERE pg_tables.schemaname = current_schema()
 ORDER BY pg_total_relation_size(pg_tables.tablename :: regclass :: oid) DESC
     ;

SELECT fix_query(query) AS "Query"
     , calls AS "Calls"
     , total_time::INT AS "Tot time(ms)"
     , round((1000*total_time/calls) :: NUMERIC, 2) AS "Avg time(us)"
     FROM pg_stat_statements JOIN pg_database ON pg_database.oid = pg_stat_statements.dbid
     WHERE pg_database.datname = current_database()
       AND calls > 5
     ORDER BY total_time DESC
     LIMIT 30;

SELECT fix_query(query) AS "Query"
     , calls AS "Calls"
     , total_time::INT AS "Tot time(ms)"
     , round((1000*total_time/calls) :: NUMERIC, 2) AS "Avg time(us)"
     FROM pg_stat_statements JOIN pg_database ON pg_database.oid = pg_stat_statements.dbid
     WHERE pg_database.datname = current_database()
       AND calls > 5
     ORDER BY calls DESC
     LIMIT 30;

SELECT fix_query(query) AS "Query"
     , calls AS "Calls"
     , total_time::INT AS "Tot time(ms)"
     , round((1000*total_time/calls) :: NUMERIC, 2) AS "Avg time(us)"
     FROM pg_stat_statements JOIN pg_database ON pg_database.oid = pg_stat_statements.dbid
     WHERE pg_database.datname = current_database()
       AND calls > 5
     ORDER BY total_time/calls DESC
     LIMIT 30;

ROLLBACK;
