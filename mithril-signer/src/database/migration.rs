//! Migration module
//!
use mithril_persistence::database::SqlMigration;

/// Get all the migrations required by this version of the software.
/// There shall be one migration per database version. There could be several
/// statements per migration.
pub fn get_migrations() -> Vec<SqlMigration> {
    vec![
        // Migration 1
        // Alter `db_version` tables to remove `default current_timestamp` clause from its
        // `updated_at` field, and migrate old date data to rfc 3339.
        SqlMigration::new(
            1,
            r"
-- In some context, most likely tests, the db_version isn't created since the migrator isn't used
create table if not exists 'db_version' (application_type text not null primary key, version integer not null, updated_at text not null);

create table new_db_version (application_type text not null primary key, version integer not null, updated_at text not null);
insert into new_db_version select * from db_version order by rowid asc;

drop table db_version;
alter table new_db_version rename to db_version;
            ",
        ),
        // Migration 2
        // Add the `signed_entity_type` table and insert first types
        SqlMigration::new(
            2,
            r#"
create table signed_entity_type (
    signed_entity_type_id       integer     not null,
    name                        text        not null,
    primary key (signed_entity_type_id)
);
insert into signed_entity_type (signed_entity_type_id, name)
    values  (0, 'Mithril Stake Distribution'),
            (1, 'Cardano Stake Distribution'),
            (2, 'Full Cardano Immutable Files'),
            (3, 'Cardano Transactions');
"#,
        ),
        // Migration 3
        // Create the `signed_beacon` table.
        SqlMigration::new(
            3,
            r"
create table if not exists signed_beacon (
    epoch                       integer     not null,
    beacon                      text        not null,
    signed_entity_type_id       integer     not null,
    initiated_at                text        not null,
    signed_at                   text        not null,

    primary key (epoch, beacon, signed_entity_type_id),
    foreign key (signed_entity_type_id) references signed_entity_type (signed_entity_type_id)
);

create index signed_beacon_epoch on signed_beacon(epoch);
create index signed_beacon_signed_entity_type_id on signed_beacon(signed_entity_type_id);
            ",
        ),
        // Migration 4
        // Remove `network` from cardano immutable files full beacons in `signed_beacon` table
        SqlMigration::new(
            4,
            r#"
update signed_beacon
    set beacon = json_remove(beacon, '$.network')
    where signed_beacon.signed_entity_type_id = 2;
        "#,
        ),
        // Migration 5
        // Add the `stake_pool` table and migration data from the previous
        // `stake_store` JSON format.
        SqlMigration::new(
            5,
            r#"
create table stake_pool (
    stake_pool_id text      not null,
    epoch         integer   not null,
    stake         integer   not null,
    created_at    text      not null,
    primary key (epoch, stake_pool_id)
);
create table if not exists stake (key_hash text primary key, key json not null, value json not null);
insert into stake_pool (epoch, stake_pool_id, stake, created_at) 
    select 
        stake.key as epoch, 
        stake_dis.key as stake_pool_id, 
        stake_dis.value as stake,
        strftime('%Y-%m-%dT%H:%M:%fZ', current_timestamp)
    from stake, json_each(stake.value) as stake_dis 
    order by epoch asc;
drop table stake;
"#,
        ),
        // Migration 6
        // Add the `protocol_initializer` table and migration data from the previous
        // `protocol_initializer` JSON format.
        SqlMigration::new(
            6,
            r#"
create table new_protocol_initializer (
    epoch         integer   not null,
    protocol      json      not null,
    created_at    text      not null,
    primary key (epoch)
);
create table if not exists protocol_initializer (key_hash text primary key, key json not null, value json not null);
insert into new_protocol_initializer (epoch, protocol, created_at) 
    select 
        protocol_initializer.key as epoch, 
        protocol_initializer.value, 
        strftime('%Y-%m-%dT%H:%M:%fZ', current_timestamp)
    from protocol_initializer
    order by epoch asc;
drop table protocol_initializer;
alter table new_protocol_initializer rename to protocol_initializer;
"#,
        ),
    ]
}
