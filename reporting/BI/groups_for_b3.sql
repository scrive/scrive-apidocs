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

begin;
create temporary table report_groups as
select ug.id::text "group_id",
       ug.name "group_name",
       array_reverse(ug.parent_group_path)::text[] "parent_group_path",
       ug.deleted "deleted",
       uga.company_number "company_number",
       uga.entity_name "company_name",
       uga.address "address",
       uga.zip "zip",
       uga.city "city",
       uga.country "country",
       (ugi.invoicing_type = 3) "is_invoice_group",
       (case ugi.invoicing_type
             when 3 then 'invoice'
             when 2 then 'billitem'
             when 1 then 'none'
             end :: text) as "invoicing_type", 
       ugt.value "salesforce_id"
  from user_groups ug
  left join user_group_addresses uga on ug.id=uga.user_group_id
  join user_group_invoicings ugi on ug.id=ugi.user_group_id
  left join user_group_tags ugt on (ug.id = ugt.user_group_id AND ugt.name = 'sf-account-id');

create temporary table report_groups_json as
  (select row_to_json(report_groups) from report_groups);

\copy report_groups_json to groups_tmp.json with (format text);
\! sed 's/\\"/\"/g' < groups_tmp.json | sed 's/\\\\t//g' > groups_tmp2.json
\! /usr/local/bin/jq -s -c <groups_tmp2.json > groups_$(date '+%Y-%m-%d_%H:%M:%S').json
\! rm groups_tmp.json
\! rm groups_tmp2.json
drop table report_groups;
rollback;
drop function array_reverse(anyarray);
