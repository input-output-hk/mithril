//! Migration module
//!
use mithril_persistence::database::SqlMigration;

/// Get all the migrations required by this version of the software.
/// There shall be one migration per database version. There could be several
/// statements per migration.
pub fn get_migrations() -> Vec<SqlMigration> {
    vec![
        // Migration 1
        SqlMigration::new(
            1,
            r#"
create table if not exists event (
    event_id integer primary key asc autoincrement,
    created_at text not null,
    source text not null,
    action text not null,
    content text nul null
);
            "#,
        ),
        SqlMigration::new(
            2,
            r#"
create view if not exists metrics_per_day as select metric_date as date, action as counter_name, sum(counter) value from 
    (
        select action, json_extract(content, '$.content.value') counter, date(json_extract(content, '$.content.date')) metric_date 
        from event 
        where source='Metrics'
    ) 
group by action, date;
create index metric_date_index on event(date(json_extract(content, '$.content.date')));
            "#,
        ),
        SqlMigration::new(
            3,
            r#"
create view signer_registration_summary as with
  signer_version as (
    select
      json_extract(content, '$.content.party_id') as party_id,
      json_extract(content, '$.headers.signer-node-version') as node_version,
      json_extract(content, '$.headers.epoch') as epoch,
      json_extract(content, '$.content.stake') as stakes
    from event
    where action='register_signer'
    order by created_at desc
  ),
  stakes_version as (
    select
      party_id,
      case
        when instr(signer_version.node_version, '+') > 0
          then substr(signer_version.node_version, 0, instr(signer_version.node_version, '+'))
          else signer_version.node_version
      end as version,
      stakes,
      cast(epoch as int) as epoch
    from signer_version
    group by party_id, epoch
  ),
  summed_stakes_version as (
    select
      epoch,
      version,
      party_id,
      sum(stakes) over (partition by epoch) as total_epoch_stakes,
      sum(stakes) over (partition by epoch, version) as stakes_version
    from stakes_version
    order by epoch desc
  )
select
  epoch,
  version,
  total_epoch_stakes,
  stakes_version,
  printf('%02d %%', round((stakes_version * 100) / (total_epoch_stakes * 1.0))) as stakes_ratio,
  count(party_id) as pool_count
from summed_stakes_version
group by epoch, version
order by epoch desc, version desc;
            "#,
        ),
    ]
}
