use std::sync::Arc;

use async_trait::async_trait;

use crate::services::ProverService;

use super::ArtifactBuilder;
use anyhow::{anyhow, Context};
use mithril_common::{
    entities::{
        CardanoDbBeacon, CardanoTransactionsSnapshot, Certificate, ProtocolMessagePartKey,
        SignedEntityType,
    },
    StdResult,
};

/// A [CardanoTransactionsArtifact] builder
pub struct CardanoTransactionsArtifactBuilder {
    prover_service: Arc<dyn ProverService>,
}

impl CardanoTransactionsArtifactBuilder {
    /// CardanoTransactions artifact builder factory
    pub fn new(prover_service: Arc<dyn ProverService>) -> Self {
        Self { prover_service }
    }
}

#[async_trait]
impl ArtifactBuilder<CardanoDbBeacon, CardanoTransactionsSnapshot>
    for CardanoTransactionsArtifactBuilder
{
    async fn compute_artifact(
        &self,
        beacon: CardanoDbBeacon,
        certificate: &Certificate,
    ) -> StdResult<CardanoTransactionsSnapshot> {
        let merkle_root = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::CardanoTransactionsMerkleRoot)
            .ok_or(anyhow!(
                "Can not find CardanoTransactionsMerkleRoot protocol message part in certificate"
            ))
            .with_context(|| {
                format!(
                    "Can not compute CardanoTransactionsCommitment artifact for signed_entity: {:?}",
                    SignedEntityType::CardanoTransactions(beacon.clone())
                )
            })?;
        self.prover_service.compute_cache(&beacon).await?;

        Ok(CardanoTransactionsSnapshot::new(
            merkle_root.to_string(),
            beacon,
        ))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::ProtocolMessage, test_utils::fake_data};

    use crate::services::MockProverService;

    use super::*;

    #[tokio::test]
    async fn should_compute_valid_artifact_with_merkleroot() {
        let certificate = {
            let mut certificate = fake_data::certificate("certificate-123".to_string());
            let mut message = ProtocolMessage::new();
            message.set_message_part(
                ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
                "merkleroot".to_string(),
            );
            certificate.protocol_message = message;
            certificate
        };

        let beacon = certificate.as_cardano_db_beacon();
        let mut mock_prover = MockProverService::new();
        mock_prover.expect_compute_cache().returning(|_| Ok(()));
        let cardano_transaction_artifact_builder =
            CardanoTransactionsArtifactBuilder::new(Arc::new(mock_prover));
        let artifact = cardano_transaction_artifact_builder
            .compute_artifact(beacon.clone(), &certificate)
            .await
            .unwrap();
        let artifact_expected = CardanoTransactionsSnapshot::new("merkleroot".to_string(), beacon);
        assert_eq!(artifact_expected, artifact);
    }

    #[tokio::test]
    async fn should_fail_to_compute_artifact_without_merkle_root() {
        let certificate = {
            let mut certificate = fake_data::certificate("certificate-123".to_string());
            let message = ProtocolMessage::new();
            certificate.protocol_message = message;
            certificate
        };

        let mut mock_prover = MockProverService::new();
        mock_prover.expect_compute_cache().returning(|_| Ok(()));
        let cardano_transaction_artifact_builder =
            CardanoTransactionsArtifactBuilder::new(Arc::new(mock_prover));
        cardano_transaction_artifact_builder
            .compute_artifact(CardanoDbBeacon::default(), &certificate)
            .await
            .expect_err("The artifact building must fail since there is no CardanoTransactionsMerkleRoot part in its message.");
    }
}
