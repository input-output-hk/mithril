use std::sync::Arc;

use clap::Parser;
use cli_table::{print_stdout, WithTitle};
use config::{builder::DefaultState, Config, ConfigBuilder};
use mithril_common::{
    entities::{SignedEntity, Snapshot},
    StdError,
};

use crate::{dependencies::DependenciesBuilder, SnapshotFieldItem};

/// Clap command to show a given snapshot
#[derive(Parser, Debug, Clone)]
pub struct SnapshotShowCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,

    /// Snapshot digest
    digest: String,
}

impl SnapshotShowCommand {
    /// Snapshot Show command
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), StdError> {
        let config: Config = config_builder.build()?;
        let mut dependencies_builder = DependenciesBuilder::new(Arc::new(config));
        let snapshot_service = dependencies_builder.get_snapshot_service().await?;
        let signed_entity = snapshot_service.show(&self.digest).await?;

        if self.json {
            println!("{}", serde_json::to_string(&signed_entity.artifact)?);
        } else {
            print_stdout(convert_to_field_items(&signed_entity).with_title()).unwrap();
        }

        Ok(())
    }
}

/// Convert Snapshot to SnapshotFieldItems routine
fn convert_to_field_items(signed_entity: &SignedEntity<Snapshot>) -> Vec<SnapshotFieldItem> {
    let mut field_items = vec![
        SnapshotFieldItem::new(
            "Epoch".to_string(),
            format!("{}", signed_entity.artifact.beacon.epoch),
        ),
        SnapshotFieldItem::new(
            "Immutable File Number".to_string(),
            format!("{}", signed_entity.artifact.beacon.immutable_file_number),
        ),
        SnapshotFieldItem::new(
            "Network".to_string(),
            signed_entity.artifact.beacon.network.to_owned(),
        ),
        SnapshotFieldItem::new(
            "Digest".to_string(),
            signed_entity.artifact.digest.to_string(),
        ),
        SnapshotFieldItem::new(
            "Size".to_string(),
            format!("{}", signed_entity.artifact.size),
        ),
    ];
    for (idx, location) in signed_entity.artifact.locations.iter().enumerate() {
        field_items.push(SnapshotFieldItem::new(
            format!("Location {}", idx + 1),
            location.to_string(),
        ));
    }
    field_items.push(SnapshotFieldItem::new(
        "Created".to_string(),
        signed_entity.created_at.to_string(),
    ));
    field_items
}
