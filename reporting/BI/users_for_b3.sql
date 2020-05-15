create or replace function array_reverse(anyarray)
returns anyarray as
$$
select array( select $1[i]
                from generate_subscripts($1,1) as s(i)
               order by i desc
            );
$$
language sql
strict immutable;

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

begin;
create temporary table report_users as
select u.id::text "user_id",
       anon_email(u.email) "user_email",
       null::text "user_phone",
       u.deleted "deleted",
       u.account_suspended "account_suspended",
       u.is_company_admin "is_accountadmin",
       u.user_group_id::text "group_id"
  from users u;

create temporary table report_users_json as
  (select row_to_json(report_users) from report_users);

\copy report_users_json to users_tmp.json with (format text);
\! sed 's/\\"/\"/g' < users_tmp.json | sed 's/\\\\t//g' > users_tmp2.json
\! jq -s -c <users_tmp2.json > users_$(date '+%Y-%m-%d_%H:%M:%S').json
\! rm users_tmp.json
\! rm users_tmp2.json
drop table report_users;
rollback;
drop function array_reverse(anyarray);


