use anyhow::Context;
use async_trait::async_trait;
use mithril_common::{
    CardanoNetwork, StdResult,
    entities::{
        CardanoDbBeacon, CardanoNodeLedgerStateSnapshot, Certificate, ProtocolMessagePartKey,
        SignedEntityType,
    },
};
use semver::Version;

use crate::ArtifactBuilder;

pub struct CardanoNodeLedgerStateArtifactBuilder {
    network: CardanoNetwork,
    cardano_node_version: Version,
}

impl CardanoNodeLedgerStateArtifactBuilder {
    pub fn new(network: CardanoNetwork, cardano_node_version: &Version) -> Self {
        Self {
            network,
            cardano_node_version: cardano_node_version.clone(),
        }
    }
}

#[async_trait]
impl ArtifactBuilder<CardanoDbBeacon, CardanoNodeLedgerStateSnapshot>
    for CardanoNodeLedgerStateArtifactBuilder
{
    async fn compute_artifact(
        &self,
        beacon: CardanoDbBeacon,
        certificate: &Certificate,
    ) -> StdResult<CardanoNodeLedgerStateSnapshot> {
        let ledger_hash = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .with_context(|| "Can not find SnapshotDigest protocol message part in certificate")
            .with_context(|| {
                format!(
                    "Can not compute CardanoNodeLedgerState artifact for signed_entity: {:?}",
                    SignedEntityType::CardanoNodeLedgerState(beacon.clone())
                )
            })?;

        let cardano_node_ledger = CardanoNodeLedgerStateSnapshot::new(
            ledger_hash.to_string(),
            self.network,
            beacon,
            &self.cardano_node_version,
        );

        Ok(cardano_node_ledger)
    }
}
