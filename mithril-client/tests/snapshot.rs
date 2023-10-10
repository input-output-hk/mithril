use std::sync::Arc;

use anyhow::Context;
use chrono::DateTime;
use config::{builder::DefaultState, ConfigBuilder};

use mithril_client::common::*;
use mithril_client::{dependencies::DependenciesBuilder, services::SnapshotService};

const AGGREGATOR_ENDPOINT: &str =
    "https://aggregator.release-preprod.api.mithril.network/aggregator";
const DIGEST: &str = "5dfec0f73b773ff15421fd357472079b6a98d9448e2f41bf3487e19b2169918a";

pub async fn get_snapshot_service() -> StdResult<Arc<dyn SnapshotService>> {
    let config_builder: ConfigBuilder<DefaultState> = ConfigBuilder::default()
        .set_default("genesis_verification_key", "WRITE THE VKEY HERE")?
        .set_default("aggregator_endpoint", AGGREGATOR_ENDPOINT)?;
    let config = Arc::new(config_builder.build()?);
    let snapshot_service = DependenciesBuilder::new(config)
        .get_snapshot_service()
        .await
        .with_context(|| "Dependencies Builder can not get Snapshot Service")?;

    Ok(snapshot_service)
}

// These tests are temporary since the data they rely on will be unavailable at
// some point.  This file aims at knowing what exports from `mithril-common` are
// needed to use the client as a library.
#[tokio::test]
pub async fn show() -> StdResult<()> {
    let snapshot_service = get_snapshot_service().await?;
    let snapshot_message = snapshot_service.show(DIGEST).await.with_context(|| {
        format!(
            "Snapshot Service can not show the snapshot for digest: '{}'",
            DIGEST
        )
    })?;

    assert_eq!(
        SnapshotMessage {
            digest: "5dfec0f73b773ff15421fd357472079b6a98d9448e2f41bf3487e19b2169918a".to_string(),
            beacon: Beacon {
                network: "preprod".to_string(),
                epoch: Epoch(99),
                immutable_file_number: 1906
            },
            certificate_hash: "12db78162fdb794b63b646bc85e675f9a17d056cd95256ac38a2f33fa96ec0b7"
                .to_string(),
            size: 1248866336,
            created_at: DateTime::parse_from_rfc3339("2023-10-10T09:06:42.238452966Z")?.into(),
            locations: vec!["https://storage.googleapis.com/cdn.aggregator.release-preprod.api.mithril.network/preprod-e99-i1906.5dfec0f73b773ff15421fd357472079b6a98d9448e2f41bf3487e19b2169918a.tar.zst".to_string()],
            compression_algorithm: Some(Zstandard),
            cardano_node_version: Some("8.1.2".to_string())
        },
        snapshot_message
    );

    Ok(())
}

#[tokio::test]
pub async fn list() -> StdResult<()> {
    let snapshot_service = get_snapshot_service().await?;
    let snapshot_messages = snapshot_service.list().await?;

    assert_eq!(20, snapshot_messages.len());

    Ok(())
}
