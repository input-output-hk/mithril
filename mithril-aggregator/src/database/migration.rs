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
        // Migration 5
        // Add the `open_message` table
        SqlMigration::new(
            5,
            r#"
create table open_message (
	open_message_id         text    not null,
    epoch_setting_id        int     not null,
    beacon                  json    not null,
    signed_entity_type_id   int     not null,
    message                 text    not null,
    created_at              text    not null default current_timestamp,
    primary key (open_message_id),
    foreign key (epoch_setting_id)     references epoch_setting (epoch_setting_id),
    foreign key (signed_entity_type_id) references signed_entity_type (signed_entity_type_id)
);
"#,
        ),
        // Migration 6
        // Add the `signer_registration` table and migration data from the previous
        // `verification_key` JSON format.
        SqlMigration::new(
            6,
            r#"
create table signer_registration (
    signer_id                   text        not null,
    epoch_setting_id            integer     not null,
    verification_key            text        not null,
    verification_key_signature  text,
    operational_certificate     text,
    kes_period                  integer,
    stake                       integer,
    created_at                  text        not null default current_timestamp,
    primary key (epoch_setting_id, signer_id)
    foreign key (epoch_setting_id) references epoch_setting(epoch_setting_id)
);
create table if not exists verification_key (key_hash text primary key, key json not null, value json not null);
insert into signer_registration (signer_id, 
                                epoch_setting_id, 
                                verification_key, 
                                verification_key_signature,
                                operational_certificate, 
                                kes_period,
                                stake) 
    select 
        verification_key_signer.key as signer_id,
        verification_key.key as epoch_setting_id, 
        json_extract(verification_key_signer.value, '$.verification_key') as verification_key,
        json_extract(verification_key_signer.value, '$.verification_key_signature') as verification_key_signature,
        json_extract(verification_key_signer.value, '$.operational_certificate') as operational_certificate,
        json_extract(verification_key_signer.value, '$.kes_period') as kes_period,
        stake_pool.stake as stake
    from verification_key, json_each(verification_key.value) as verification_key_signer 
    left join stake_pool on stake_pool.stake_pool_id = verification_key_signer.key and stake_pool.epoch = verification_key.key
    order by verification_key.key, verification_key_signer.key asc;
drop table verification_key;
"#,
        ),
        // Migration 7
        // Add the `signed_entity` table and migration data from the previous
        // `snapshot` JSON format.
        SqlMigration::new(
            7,
            r#"
create table signed_entity (
    signed_entity_id            text        not null,
    signed_entity_type_id       integer     not null,
    certificate_id              text        not null,
    beacon                      json        not null,
    entity                      json        not null,
    created_at                  text        not null default current_timestamp,
    primary key (signed_entity_id)
    foreign key (signed_entity_type_id) references signed_entity_type(signed_entity_type_id)
    foreign key (certificate_id) references certificate(certificate_id)
);
create table if not exists snapshot (key_hash text primary key, key json not null, value json not null);
insert into signed_entity (signed_entity_id, 
                                signed_entity_type_id, 
                                certificate_id,
                                beacon,
                                entity) 
    select 
        json_extract(snapshot.value, '$.digest') as signed_entity_id,
        2 as signed_entity_type_id,
        json_extract(snapshot.value, '$.certificate_hash') as certificate_id,
        json_extract(snapshot.value, '$.beacon') as beacon,
        snapshot.value as entity
    from snapshot 
    order by ROWID asc;
drop table snapshot;
"#,
        ),
        // Migration 8
        // Add the `signer` table and migration data from `signer_registration` table
        SqlMigration::new(
            8,
            r#"
create table signer (
    signer_id                   text        not null,
    pool_ticker                 text,
    created_at                  text        not null default current_timestamp,
    updated_at                  text        not null default current_timestamp,
    primary key (signer_id)
);
insert into signer (signer_id, created_at, updated_at) 
    select 
        distinct(signer_registration.signer_id) as signer_id,
        min(signer_registration.created_at) as created_at,
        max(signer_registration.created_at) as updated_at
    from signer_registration 
    group by signer_registration.signer_id
    order by signer_registration.signer_id;

alter table signer_registration rename to signer_registration_temp;
create table signer_registration (
    signer_id                   text        not null,
    epoch_setting_id            integer     not null,
    verification_key            text        not null,
    verification_key_signature  text,
    operational_certificate     text,
    kes_period                  integer,
    stake                       integer,
    created_at                  text        not null default current_timestamp,
    primary key (epoch_setting_id, signer_id)
    foreign key (epoch_setting_id) references epoch_setting(epoch_setting_id)
    foreign key (signer_id) references signer(signer_id)
);
insert into signer_registration select * from signer_registration_temp order by ROWID asc;
drop table signer_registration_temp;
"#,
        ),
        // Migration 9
        // Add the `single_signature` table and rename previous table to `single_signature_legacy`
        SqlMigration::new(
            9,
            r#"
create table if not exists single_signature (key_hash text primary key, key json not null, value json not null);
alter table single_signature rename to single_signature_legacy;
create table single_signature (
    open_message_id                 text        not null,
    signer_id                       text        not null,
    registration_epoch_setting_id   integer     not null,
    lottery_indexes                 json        not null,
    signature                       text        not null,
    created_at                      text        not null default current_timestamp,
    primary key (open_message_id, signer_id, registration_epoch_setting_id)
    foreign key (open_message_id) references open_message(open_message_id) on delete cascade
    foreign key (signer_id, registration_epoch_setting_id) references signer_registration(signer_id, epoch_setting_id)
);
"#,
        ),
        // Migration 10
        // Alter `open_message` table and drop `single_signature_legacy` table
        SqlMigration::new(
            10,
            r#"
drop table single_signature_legacy;
alter table open_message drop column message;
alter table open_message add column protocol_message json not null;
alter table open_message add column is_certified bool not null default false;
"#,
        ),
        // Migration 11
        // Alter `signed_entity` table
        SqlMigration::new(
            11,
            r#"
alter table signed_entity rename to signed_entity_old;
create table signed_entity (
    signed_entity_id            text        not null,
    signed_entity_type_id       integer     not null,
    certificate_id              text        not null,
    beacon                      json        not null,
    artifact                    json        not null,
    created_at                  text        not null default current_timestamp,
    primary key (signed_entity_id)
    foreign key (signed_entity_type_id) references signed_entity_type(signed_entity_type_id)
    foreign key (certificate_id) references certificate(certificate_id)
);
insert into signed_entity (signed_entity_id, 
    signed_entity_type_id, 
    certificate_id,
    beacon,
    artifact, created_at) 
    select signed_entity_id, signed_entity_type_id, certificate_id, beacon, entity, created_at
    from signed_entity_old 
    order by rowid asc;
drop table signed_entity_old;
"#,
        ),
        // Migration 12
        // Alter `open_message` table
        SqlMigration::new(
            12,
            r#"
create unique index open_message_unique_index on open_message(signed_entity_type_id, beacon);
"#,
        ),
        // Migration 13
        // Update signed_entity.artifact type MithrilStakeDistribution to add new fields in JSON.
        SqlMigration::new(
            13,
            r#"
update signed_entity
    set artifact = json_insert(
        json_insert(
            artifact,
            '$.protocol_parameters', 
            json(certificate.protocol_parameters)
        ), 
        '$.created_at', 
        created_at
    )
from certificate 
where 
    signed_entity.signed_entity_type_id = 0
    and signed_entity.certificate_id = certificate.certificate_id
"#,
        ),
    ]
}
