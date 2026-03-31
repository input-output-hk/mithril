use std::sync::Arc;

use crate::services::ProverService;
use anyhow::Context;
use async_trait::async_trait;
use mithril_common::{
    StdResult,
    entities::{
        BlockNumber, BlockNumberOffset, CardanoBlocksTransactionsSnapshot, Certificate,
        ProtocolMessagePartKey, SignedEntityType,
    },
};

use super::ArtifactBuilder;

/// A [CardanoBlocksTransactionsArtifact] builder
pub struct CardanoBlocksTransactionsArtifactBuilder {
    prover_service: Arc<dyn ProverService>,
}

impl CardanoBlocksTransactionsArtifactBuilder {
    /// CardanoBlocksTransactions artifact builder factory
    pub fn new(prover_service: Arc<dyn ProverService>) -> Self {
        Self { prover_service }
    }
}

#[async_trait]
impl ArtifactBuilder<(BlockNumber, BlockNumberOffset), CardanoBlocksTransactionsSnapshot>
    for CardanoBlocksTransactionsArtifactBuilder
{
    async fn compute_artifact(
        &self,
        beacon: (BlockNumber, BlockNumberOffset),
        certificate: &Certificate,
    ) -> StdResult<CardanoBlocksTransactionsSnapshot> {
        let (block_number, block_number_offset) = beacon;
        let merkle_root = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot)
            .with_context(|| "Can not find CardanoBlocksTransactionsMerkleRoot protocol message part in certificate")
            .with_context(|| {
                format!(
                    "Can not compute CardanoBlocksTransactionsSnapshot artifact for signed_entity: {:?}",
                    SignedEntityType::CardanoBlocksTransactions(certificate.epoch, block_number, block_number_offset)
                )
            })?;
        self.prover_service.compute_cache(block_number).await?;

        Ok(CardanoBlocksTransactionsSnapshot::new(
            merkle_root.to_string(),
            block_number,
            block_number_offset,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::services::MockProverService;
    use mithril_common::{
        entities::{BlockNumberOffset, ProtocolMessage},
        test::double::fake_data,
    };

    use super::*;

    #[tokio::test]
    async fn should_compute_valid_artifact_with_merkleroot() {
        let mut mock_prover = MockProverService::new();
        mock_prover.expect_compute_cache().returning(|_| Ok(()));

        let cardano_blocks_transactions_artifact_builder =
            CardanoBlocksTransactionsArtifactBuilder::new(Arc::new(mock_prover));

        let certificate_with_merkle_root = {
            let mut protocol_message = ProtocolMessage::new();
            protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot,
                "merkleroot".to_string(),
            );
            Certificate {
                protocol_message,
                ..fake_data::certificate("certificate-123".to_string())
            }
        };
        let block_number = BlockNumber(100);
        let block_number_offset = BlockNumberOffset(42);

        let artifact = cardano_blocks_transactions_artifact_builder
            .compute_artifact(
                (block_number, block_number_offset),
                &certificate_with_merkle_root,
            )
            .await
            .unwrap();

        assert_eq!(
            CardanoBlocksTransactionsSnapshot::new(
                "merkleroot".to_string(),
                block_number,
                block_number_offset,
            ),
            artifact
        );
    }

    #[tokio::test]
    async fn should_fail_to_compute_artifact_without_merkle_root() {
        let mut mock_prover = MockProverService::new();
        mock_prover.expect_compute_cache().returning(|_| Ok(()));

        let cardano_blocks_transactions_artifact_builder =
            CardanoBlocksTransactionsArtifactBuilder::new(Arc::new(mock_prover));

        let certificate_without_merkle_root = Certificate {
            protocol_message: ProtocolMessage::new(),
            ..fake_data::certificate("certificate-123".to_string())
        };
        let block_number = BlockNumber(100);
        let block_number_offset = BlockNumberOffset(42);

        cardano_blocks_transactions_artifact_builder
            .compute_artifact((block_number, block_number_offset), &certificate_without_merkle_root)
            .await
            .expect_err("The artifact building must fail since there is no CardanoBlocksTransactionsMerkleRoot part in its message.");
    }
}
