use async_trait::async_trait;

use super::ArtifactBuilder;
use anyhow::{anyhow, Context};
use mithril_common::{
    entities::{
        Beacon, CardanoTransactionsCommitment, Certificate, ProtocolMessagePartKey,
        SignedEntityType,
    },
    StdResult,
};

/// A [CardanoTransactionsArtifact] builder
pub struct CardanoTransactionsArtifactBuilder {}

impl CardanoTransactionsArtifactBuilder {
    /// CardanoTransactions artifact builder factory
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl ArtifactBuilder<Beacon, CardanoTransactionsCommitment> for CardanoTransactionsArtifactBuilder {
    async fn compute_artifact(
        &self,
        beacon: Beacon,
        certificate: &Certificate,
    ) -> StdResult<CardanoTransactionsCommitment> {
        let merkle_root = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::CardanoTransactionsMerkleRoot)
            .ok_or(anyhow!(
                "Can not find CardanoTransactionsMerkleRoot protocol message part in certificate"
            ))
            .with_context(|| {
                format!(
                    "Can not compute CardanoTransactionsCommitment artifact for signed_entity: {:?}",
                    SignedEntityType::CardanoTransactions(beacon)
                )
            })?;

        Ok(CardanoTransactionsCommitment::new(merkle_root.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::ProtocolMessage, test_utils::fake_data};

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

        let cardano_transaction_artifact_builder = CardanoTransactionsArtifactBuilder::new();
        let artifact = cardano_transaction_artifact_builder
            .compute_artifact(Beacon::default(), &certificate)
            .await
            .unwrap();
        let artifact_expected = CardanoTransactionsCommitment::new("merkleroot".to_string());
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

        let cardano_transaction_artifact_builder = CardanoTransactionsArtifactBuilder::new();
        cardano_transaction_artifact_builder
            .compute_artifact(Beacon::default(), &certificate)
            .await
            .expect_err("The artifact building must fail since there is no CardanoTransactionsMerkleRoot part in its message.");
    }
}
