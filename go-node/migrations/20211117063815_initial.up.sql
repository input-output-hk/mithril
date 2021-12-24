-- +migrate Up


create table mithril_certificates
(
    id              bigint not null unique,
    node_id         bigint not null,
    cert_hash       bytea  not null,
    prev_hash       bytea  not null,
    participants    jsonb  not null default '[]',
    block_number    int    not null,
    block_hash      bytea  not null,
    merkle_root     bytea  not null,
    multi_sig       bytea  not null,
    sig_started_at  timestamp with time zone,
    sig_finished_at timestamp with time zone
);

create table mithril_benchmarks
(
    node_id    bigint                   not null,
    cert_hash  bytea                    not null,
    title      varchar(512)             not null,
    start_time timestamp with time zone not null,
    end_time   timestamp with time zone not null
);