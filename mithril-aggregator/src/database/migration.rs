//! Migration module
//!
use mithril_common::entities::SignedEntityTypeDiscriminants;
use mithril_persistence::database::SqlMigration;

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
        // Migration 14
        // Alter `signer`, `signer_registration`, `single_signature`, `open_message`, `certificate`,
        // `stake_pool`, and `signed_entity` tables to remove `default current_timestamp` clause
        // and migrate old data to rfc 3339 for `stake_pool` & `open_message` (other tables already
        // are stored in a compatible format).
        SqlMigration::new(
            14,
            r#"
-- disable foreign keys since we will delete tables linked using them
pragma foreign_keys=false;

-- Migrate signer
create table new_signer (
    signer_id                   text        not null,
    pool_ticker                 text,
    created_at                  text        not null,
    updated_at                  text        not null,
    primary key (signer_id)
);

insert into new_signer (signer_id, pool_ticker, created_at, updated_at)
    select              signer_id, pool_ticker, strftime('%Y-%m-%dT%H:%M:%fZ', created_at),
        strftime('%Y-%m-%dT%H:%M:%fZ', updated_at)
    from signer order by rowid asc;

drop table signer;
alter table new_signer rename to signer;

-- Migrate signer_registration
create table new_signer_registration (
    signer_id                   text        not null,
    epoch_setting_id            integer     not null,
    verification_key            text        not null,
    verification_key_signature  text,
    operational_certificate     text,
    kes_period                  integer,
    stake                       integer,
    created_at                  text        not null,
    primary key (epoch_setting_id, signer_id)
    foreign key (epoch_setting_id) references epoch_setting(epoch_setting_id)
    foreign key (signer_id) references signer(signer_id)
);
insert into new_signer_registration
        (  signer_id, epoch_setting_id, verification_key, verification_key_signature,
        operational_certificate, kes_period, stake, created_at)
    select signer_id, epoch_setting_id, verification_key, verification_key_signature,
        operational_certificate, kes_period, stake, strftime('%Y-%m-%dT%H:%M:%fZ', created_at)
    from signer_registration order by rowid asc;

drop table signer_registration;
alter table new_signer_registration rename to signer_registration;

-- Migrate single_signature
create table new_single_signature (
    open_message_id                 text        not null,
    signer_id                       text        not null,
    registration_epoch_setting_id   integer     not null,
    lottery_indexes                 json        not null,
    signature                       text        not null,
    created_at                      text        not null,
    primary key (open_message_id, signer_id, registration_epoch_setting_id)
    foreign key (open_message_id) references open_message(open_message_id) on delete cascade
    foreign key (signer_id, registration_epoch_setting_id) references signer_registration(signer_id, epoch_setting_id)
);
insert into new_single_signature
        (  open_message_id, signer_id, registration_epoch_setting_id, lottery_indexes, signature,
        created_at)
    select open_message_id, signer_id, registration_epoch_setting_id, lottery_indexes, signature,
        strftime('%Y-%m-%dT%H:%M:%fZ', created_at)
    from single_signature order by rowid asc;

drop table single_signature;
alter table new_single_signature rename to single_signature;

-- Migrate open_message
create table new_open_message (
    open_message_id         text    not null,
    epoch_setting_id        int     not null,
    beacon                  json    not null,
    signed_entity_type_id   int     not null,
    created_at              text    not null,
    protocol_message        json    not null,
    is_certified            bool    not null default false,
    primary key (open_message_id),
    foreign key (epoch_setting_id)     references epoch_setting (epoch_setting_id),
    foreign key (signed_entity_type_id) references signed_entity_type (signed_entity_type_id)
);

insert into new_open_message
        (  open_message_id, epoch_setting_id, beacon, signed_entity_type_id,
        protocol_message, is_certified,
        created_at)
    select open_message_id, epoch_setting_id, beacon, signed_entity_type_id,
        protocol_message, is_certified,
        strftime('%Y-%m-%dT%H:%M:%fZ', created_at)
    from open_message order by rowid asc;

drop table open_message;
alter table new_open_message rename to open_message;

create unique index open_message_unique_index on open_message(signed_entity_type_id, beacon);

-- Migrate certificate
create table new_certificate (
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
    initiated_at                text     not null,
    sealed_at                   text     not null,
    primary key (certificate_id),
    foreign key (parent_certificate_id) references certificate(certificate_id)
);
insert into new_certificate
        (  certificate_id, parent_certificate_id, message, signature, aggregate_verification_key,
        epoch, beacon, protocol_version, protocol_parameters, protocol_message, signers,
        initiated_at,
        sealed_at)
    select certificate_id, parent_certificate_id, message, signature, aggregate_verification_key,
        epoch, beacon, protocol_version, protocol_parameters, protocol_message, signers,
        strftime('%Y-%m-%dT%H:%M:%fZ', initiated_at),
        strftime('%Y-%m-%dT%H:%M:%fZ', sealed_at)
    from certificate order by rowid asc;

drop table certificate;
alter table new_certificate rename to certificate;

create index epoch_index on certificate(epoch);

-- Migrate stake_pool
create table new_stake_pool (
    stake_pool_id text      not null,
    epoch         integer   not null,
    stake         integer   not null,
    created_at    text      not null,
    primary key (epoch, stake_pool_id)
);

insert into new_stake_pool
        (  stake_pool_id, epoch, stake, created_at)
    select stake_pool_id, epoch, stake, strftime('%Y-%m-%dT%H:%M:%fZ', created_at)
    from stake_pool order by rowid asc;

drop table stake_pool;
alter table new_stake_pool rename to stake_pool;

-- Migrate signed_entity
create table new_signed_entity (
    signed_entity_id            text        not null,
    signed_entity_type_id       integer     not null,
    certificate_id              text        not null,
    beacon                      json        not null,
    created_at                  text        not null,
    artifact                    json        not null,
    primary key (signed_entity_id)
    foreign key (signed_entity_type_id) references signed_entity_type(signed_entity_type_id)
    foreign key (certificate_id) references certificate(certificate_id)
);
insert into new_signed_entity
        (  signed_entity_id, signed_entity_type_id, certificate_id, beacon, artifact,
        created_at)
    select signed_entity_id, signed_entity_type_id, certificate_id, beacon, artifact,
        strftime('%Y-%m-%dT%H:%M:%fZ', created_at)
    from signed_entity order by rowid asc;

drop table signed_entity;
alter table new_signed_entity rename to signed_entity;

-- reenable foreign keys
pragma foreign_key_check;
pragma foreign_keys=true;
        "#,
        ),
        // Migration 15
        // Alter `db_version` tables to remove `default current_timestamp` clause from its
        // `updated_at` field, and migrate old date data to rfc 3339.
        SqlMigration::new(
            15,
            r"
-- In some context, most likely tests, the db_version does not exist since the migrator isn't used
create table if not exists 'db_version' (application_type text not null primary key, version integer not null, updated_at text not null);

create table new_db_version (application_type text not null primary key, version integer not null, updated_at text not null);
insert into new_db_version
        (  application_type, version, updated_at)
    select application_type, version, updated_at
    from db_version order by rowid asc;

drop table db_version;
alter table new_db_version rename to db_version;
            ",
        ),
        // Migration 16
        // Update `signed_entity` table to remove `certificate_hash` and `created_at` from `artifact` JSON field.
        SqlMigration::new(
            16,
            r#"
update signed_entity
    set artifact = json_remove(
        artifact, 
        '$.certificate_hash',
        '$.created_at'
    );
"#,
        ),
        // Migration 17
        // Alter `signed_entity` table to add `compression_algorithm` in `artifact` JSON field.
        SqlMigration::new(
            17,
            r#"
update signed_entity
    set artifact = json_insert(
        json_insert(
            artifact,
            '$.compression_algorithm', 
            'gzip')
    ) 
where signed_entity.signed_entity_type_id = 2;
        "#,
        ),
        // Migration 18
        // Alter `signed_entity` table to add `cardano_node_version` in `artifact` JSON field.
        SqlMigration::new(
            18,
            r#"
update signed_entity
    set artifact = json_insert(
        json_insert(
            artifact,
            '$.cardano_node_version', 
            '8.1.2')
    ) 
where signed_entity.signed_entity_type_id = 2;
        "#,
        ),
        // Migration 19
        // Alter `signer` table to add `last_registered_at` field with base value copied from the
        // `created_at` field.
        SqlMigration::new(
            19,
            r#"
alter table signer add column last_registered_at text null;
update signer set last_registered_at = created_at; 
        "#,
        ),
        // Migration 20
        // Alter `open_message` table to add `expires_at` and 'is_expired' fields
        SqlMigration::new(
            20,
            r#"
alter table open_message add column is_expired bool not null default false;
alter table open_message add column expires_at text null;
        "#,
        ),
        // Migration 21
        // Add the `signed_entity_type` record for 'CardanoTransactions'
        SqlMigration::new(
            21,
            r#"
insert into signed_entity_type (signed_entity_type_id, name) 
    values  (3, 'Cardano Transactions');
"#,
        ),
        // Migration 22
        // Certificate table:
        // * Remove beacon
        // * Add network, immutable file number, signed_entity_type_id, signed_entity_type columns
        SqlMigration::new(
            22,
            r#"
-- disable foreign keys since we will delete tables linked using them
pragma foreign_keys=false;

CREATE TABLE IF NOT EXISTS "new_certificate" (
    certificate_id              text     not null,
    parent_certificate_id       text,
    message                     text     not null,
    signature                   text     not null,
    aggregate_verification_key  text     not null,
    epoch                       integer  not null,
    network                     text     not null,
    immutable_file_number       integer  not null,
    signed_entity_type_id       integer  not null,
    signed_entity_beacon        json     not null,
    protocol_version            text     not null,
    protocol_parameters         json     not null,
    protocol_message            json     not null,
    signers                     json     not null,
    initiated_at                text     not null,
    sealed_at                   text     not null,
    primary key (certificate_id),
    foreign key (parent_certificate_id) references certificate(certificate_id)
    foreign key (signed_entity_type_id) references signed_entity_type(signed_entity_type_id)
);

insert into new_certificate
        ( certificate_id, parent_certificate_id, message, signature, aggregate_verification_key,
        epoch,
        network,
        immutable_file_number,
        signed_entity_type_id,
        signed_entity_beacon,
        protocol_version, protocol_parameters, protocol_message,
        signers, initiated_at, sealed_at)
    select c.certificate_id, c.parent_certificate_id, c.message, c.signature, c.aggregate_verification_key,
        c.epoch,
        json_extract(c.beacon, '$.network'),
        json_extract(c.beacon, '$.immutable_file_number'),
        -- genesis certificate doesn't have a signed_entity, we can just use directly the MithrilStakeDistribution
        coalesce(s.signed_entity_type_id, 0),
        -- genesis certificate doesn't have a signed_entity, so we need to deduce it from the old certificate
        coalesce(s.beacon, c.epoch),
        c.protocol_version, c.protocol_parameters, c.protocol_message,
        c.signers, c.initiated_at, c.sealed_at
    from certificate c
        left join signed_entity s on s.certificate_id = c.certificate_id
    order by c.rowid asc;


drop table certificate;
alter table new_certificate rename to certificate;

CREATE INDEX epoch_index on certificate(epoch);

-- reenable foreign keys
pragma foreign_key_check;
pragma foreign_keys=true;
"#,
        ),
        // Migration 23
        // Alter `pending_certificate` table to use only an Epoch instead of a full beacon.
        SqlMigration::new(
            23,
            r#"
create table if not exists pending_certificate (key_hash text primary key, key json not null, value json not null);
update pending_certificate
    set value = 
        json_remove(
            json_insert(value, '$.epoch', json_extract(value, '$.beacon.epoch')),
            '$.beacon'
        );
        "#,
        ),
        // Migration 24
        // Add indexes for foreign keys
        SqlMigration::new(
            24,
            r#"
-- `certificate` table
create index certificate_parent_certificate_id_index on certificate(parent_certificate_id);

-- `signer_registration` table
create index signer_registration_epoch_setting_id_index on signer_registration(epoch_setting_id);
create index signer_registration_signer_id_index on signer_registration(signer_id);

-- `open_message` table
create index open_message_epoch_setting_id_index on open_message(epoch_setting_id);
create index open_message_signed_entity_type_id_index on open_message(signed_entity_type_id);

-- `signed_entity` table
create index signed_entity_signed_entity_type_id_index on signed_entity(signed_entity_type_id);
create index signed_entity_certificate_id_index on signed_entity(certificate_id);

-- `single_signature` table
create index single_signature_open_message_id_index on single_signature(open_message_id);
create index single_signature_signer_id_index on single_signature(signer_id);
create index single_signature_registration_epoch_setting_id_index on single_signature(registration_epoch_setting_id);
"#,
        ),
        // Migration 25
        // Remove Certificate and SignedEntity based on CardanoTransactions since we changed their beacon
        SqlMigration::new(
            25,
            format!(
                r#"
-- disable foreign keys since `certificate` and `signed_entity` are linked together
pragma foreign_keys=false;

delete from certificate
where certificate_id in (
    select s.certificate_id from signed_entity s
    where s.signed_entity_type_id = {}
);

delete from signed_entity
where signed_entity_type_id = {};

-- reenable foreign keys
pragma foreign_key_check;
pragma foreign_keys=true;
"#,
                SignedEntityTypeDiscriminants::CardanoTransactions.index(),
                SignedEntityTypeDiscriminants::CardanoTransactions.index()
            ),
        ),
    ]
}
