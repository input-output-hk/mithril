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
    ]
}
