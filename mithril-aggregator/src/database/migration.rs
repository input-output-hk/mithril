//! Migration module
//!
use mithril_common::database::SqlMigration;

/// Get all the migrations required by this version of the software.
/// There shall be one migration per database version. There could be several
/// statements per migration.
pub fn get_migrations() -> Vec<SqlMigration> {
    vec![
        // Migration 1
        // Add the `stake_pool` table and migration data from the previous
        // `stake_store` JSON format.
        SqlMigration::new(
            1,
            r#"
create table stake_pool (
    stake_pool_id text      not null,
    epoch         integer   not null,
    stake         integer   not null,
    created_at    text      not null default current_timestamp,
    primary key (epoch, stake_pool_id)
);
create table if not exists stake (key_hash text primary key, key json not null, value json not null);
insert into stake_pool (epoch, stake_pool_id, stake) 
    select 
        stake.key as epoch, 
        stake_dis.key as stake_pool_id, 
        stake_dis.value as stake 
    from stake, json_each(stake.value) as stake_dis 
    order by epoch asc;
drop table stake;
"#,
        ),
        // Migration 2
        // Add the `epoch_setting` table and migration data from the previous
        // `protocol_parameters` JSON format.
        SqlMigration::new(
            2,
            r#"
create table epoch_setting (
    epoch_setting_id    integer     not null,
    protocol_parameters json        not null,
    primary key (epoch_setting_id)
);
create table if not exists protocol_parameters (key_hash text primary key, key json not null, value json not null);
insert into epoch_setting (epoch_setting_id, protocol_parameters) 
    select 
        protocol_parameters.key as epoch_setting_id, 
        protocol_parameters.value as protocol_parameters
    from protocol_parameters
    order by key asc;
drop table protocol_parameters;
"#,
        ),
        // Migration 3
        // Add the `signed_entity_type` table and insert first types
        SqlMigration::new(
            3,
            r#"
create table signed_entity_type (
    signed_entity_type_id       integer     not null,
    name                        text        not null,
    primary key (signed_entity_type_id)
);
insert into signed_entity_type (signed_entity_type_id, name) 
    values  (0, 'Mithril Stake Distribution'), 
            (1, 'Cardano Stake Distribution'),
            (2, 'Full Cardano Immutable Files');
"#,
        ),
        // Migration 4
        // Add the new `certificate` table and migrate data from its previous version.
        SqlMigration::new(
            4,
            r#"
create table if not exists certificate (key_hash text primary key, key json not null, value json not null);
alter table certificate rename to certificate_temp;
create table certificate (
    certificate_id              text     not null,
    parent_certificate_id       text,
    message                     text     not null,
    signature                   text     not null,
    aggregate_verification_key  text     not null,
    epoch                       integer  not null,
    beacon                      json     not null,
    protocol_version            text     not null,
    protocol_parameters         json     not null,
    protocol_message            json     not null,
    signers                     json     not null,
    initiated_at                text     not null default current_timestamp,
    sealed_at                   text     not null default current_timestamp,
    primary key (certificate_id),
    foreign key (parent_certificate_id) references certificate(certificate_id)
);
insert into certificate (certificate_id, 
                        parent_certificate_id, 
                        message, 
                        signature, 
                        aggregate_verification_key,
                        epoch,
                        beacon,
                        protocol_version,
                        protocol_parameters,
                        protocol_message,
                        signers,
                        initiated_at,
                        sealed_at)
    select 
        json_extract(c.value, '$.hash') as certificate_id,
        case 
            when json_extract(c.value, '$.multi_signature') <> '' then json_extract(c.value, '$.previous_hash') 
            else NULL 
        end as parent_certificate_id,
        json_extract(c.value, '$.signed_message') as message,
        case 
            when json_extract(c.value, '$.multi_signature') <> '' then json_extract(c.value, '$.multi_signature')
            else json_extract(c.value, '$.genesis_signature')
        end as signature,
        json_extract(c.value, '$.aggregate_verification_key') as aggregate_verification_key,
        json_extract(c.value, '$.beacon.epoch') as epoch,
        json(json_extract(c.value, '$.beacon')) as beacon,
        json_extract(c.value, '$.metadata.version') as protocol_version,
        json(json_extract(c.value, '$.metadata.parameters')) as protocol_parameters,
        json(json_extract(c.value, '$.protocol_message')) as protocol_message,
        json(json_extract(c.value, '$.metadata.signers')) as signers,
        json_extract(c.value, '$.metadata.initiated_at') as initiated_at,
        json_extract(c.value, '$.metadata.sealed_at') as sealed_at
    from certificate_temp as c;
create index epoch_index ON certificate(epoch);
drop table certificate_temp;
"#,
        ),
    ]
}
