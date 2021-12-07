-- +migrate Up


create table mithril_certificates
(
    id              bigint        not null unique,
    params_k        bigint        not null,
    params_m        bigint        not null,
    params_phi      decimal(4, 2) not null,
    participants    jsonb         not null default '[]',
    block_number    int           not null,
    block_hash      bytea         not null,
    merkle_root     bytea         not null,
    multi_sig       bytea         not null,
    sig_started_at  timestamp with time zone,
    sig_finished_at timestamp with time zone
);