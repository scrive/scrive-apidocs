-- number of users connected to user groups with a price plan other than free.
select count(*) from users join user_group_invoicings on users.user_group_id = user_group_invoicings.user_group_id where deleted is null and payment_plan > 0;

-- number of sent/closed documents per month
select concat(extract(year from month), '-', lpad(extract(month from month)::text, 2, '0')) as month, docs_sent, docs_closed from (select date_trunc('month', time) as month, sum(case when type = 6 then quantity else 0 end) as docs_sent, sum(case when type = 8 then quantity else 0 end) as docs_closed from chargeable_items where type in (6,8) group by month order by month desc) aux;

-- number of sent/closed documents per year
select extract(year from date_trunc('year', time)) as year, sum(case when type = 6 then quantity else 0 end) as docs_sent, sum(case when type = 8 then quantity else 0 end) as docs_closed from chargeable_items where type in (6,8) group by year order by year desc;

-- 30 most popular days for docs sent
select concat(extract(year from daydate), '-', lpad(extract(month from daydate)::text, 2, '0'), '-', lpad(extract(day from daydate)::text, 2, '0')) as date, docs_sent from (select date_trunc('day', time) as daydate, sum(quantity) as docs_sent from chargeable_items where type = 6 group by daydate order by docs_sent desc limit 30) aux;

-- 30 most popular days for docs closed
select concat(extract(year from daydate), '-', lpad(extract(month from daydate)::text, 2, '0'), '-', lpad(extract(day from daydate)::text, 2, '0')) as date, docs_closed from (select date_trunc('day', time) as daydate, sum(quantity) as docs_closed from chargeable_items where type = 8 group by daydate order by docs_closed desc limit 30) aux;
