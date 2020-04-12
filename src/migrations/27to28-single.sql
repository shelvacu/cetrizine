create table single (
    shelvacu_is_awesome boolean not null primary key check(shelvacu_is_awesome),
    only_messages_after snowflake not null,
    do_downloads boolean not null
);

insert into single (shelvacu_is_awesome, only_messages_after, do_downloads) VALUES (true, 1, false);