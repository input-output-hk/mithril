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
    ]
}
