use std::sync::Arc;

use anyhow::{Context, anyhow};
use async_trait::async_trait;
use mithril_common::{
    StdResult,
    entities::{
        BlockNumber, CardanoBlocksTransactionsSnapshot, Certificate, ProtocolMessagePartKey,
        SignedEntityType,
    },
};
use mithril_protocol_config::interface::MithrilNetworkConfigurationProvider;

use crate::services::ProverService;

use super::ArtifactBuilder;

/// A [CardanoBlocksTransactionsArtifact] builder
pub struct CardanoBlocksTransactionsArtifactBuilder {
    prover_service: Arc<dyn ProverService>,
    mithril_network_configuration_provider: Arc<dyn MithrilNetworkConfigurationProvider>,
}

impl CardanoBlocksTransactionsArtifactBuilder {
    /// CardanoBlocksTransactions artifact builder factory
    pub fn new(
        prover_service: Arc<dyn ProverService>,
        mithril_network_configuration_provider: Arc<dyn MithrilNetworkConfigurationProvider>,
    ) -> Self {
        Self {
            prover_service,
            mithril_network_configuration_provider,
        }
    }
}

#[async_trait]
impl ArtifactBuilder<BlockNumber, CardanoBlocksTransactionsSnapshot>
    for CardanoBlocksTransactionsArtifactBuilder
{
    async fn compute_artifact(
        &self,
        beacon: BlockNumber,
        certificate: &Certificate,
    ) -> StdResult<CardanoBlocksTransactionsSnapshot> {
        let merkle_root = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::CardanoTransactionsMerkleRoot)
            .with_context(|| "Can not find CardanoTransactionsMerkleRoot protocol message part in certificate")
            .with_context(|| {
                format!(
                    "Can not compute CardanoBlocksTransactionsSnapshot artifact for signed_entity: {:?}",
                    SignedEntityType::CardanoBlocksTransactions(certificate.epoch, beacon)
                )
            })?;
        self.prover_service.compute_cache(beacon).await?;

        let network_configuration = self
            .mithril_network_configuration_provider
            .get_network_configuration(certificate.epoch)
            .await?;

        let cardano_transactions = network_configuration
            .configuration_for_aggregation
            .signed_entity_types_config
            .cardano_transactions; //TODO: use cardano_blocks_transactions instead (when issue 2971 will be done)

        match cardano_transactions {
            Some(cardano_transactions) => Ok(CardanoBlocksTransactionsSnapshot::new(
                merkle_root.to_string(),
                beacon,
                cardano_transactions.security_parameter,
            )),
            None => Err(anyhow!(
                "There is no cardano blocks and transactions signing configuration for aggregation for Epoch {}",
                certificate.epoch
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::services::MockProverService;
    use mithril_common::{
        entities::{CardanoTransactionsSigningConfig, Epoch, ProtocolMessage},
        test::double::{Dummy, fake_data},
    };
    use mithril_protocol_config::model::{
        MithrilNetworkConfiguration, MithrilNetworkConfigurationForEpoch,
        SignedEntityTypeConfiguration,
    };
    use mockall::mock;

    use super::*;

    mock! {
        pub MithrilNetworkConfigurationProvider {}

        #[async_trait]
        impl MithrilNetworkConfigurationProvider for MithrilNetworkConfigurationProvider {
            async fn get_network_configuration(&self, epoch: Epoch) -> StdResult<MithrilNetworkConfiguration>;
        }
    }

    #[tokio::test]
    async fn should_compute_valid_artifact_with_merkleroot() {
        let mut mock_prover = MockProverService::new();
        mock_prover.expect_compute_cache().returning(|_| Ok(()));
        let mut mock_mithril_network_configuration_provider =
            MockMithrilNetworkConfigurationProvider::new();
        mock_mithril_network_configuration_provider
            .expect_get_network_configuration()
            .times(1)
            .returning(|_| {
                Ok(MithrilNetworkConfiguration {
                    configuration_for_aggregation: MithrilNetworkConfigurationForEpoch {
                        signed_entity_types_config: SignedEntityTypeConfiguration {
                            cardano_transactions: Some(CardanoTransactionsSigningConfig {
                                security_parameter: BlockNumber(15),
                                ..Dummy::dummy()
                            }),
                        },
                        ..Dummy::dummy()
                    },
                    ..Dummy::dummy()
                })
            });
        let cardano_blocks_transactions_artifact_builder =
            CardanoBlocksTransactionsArtifactBuilder::new(
                Arc::new(mock_prover),
                Arc::new(mock_mithril_network_configuration_provider),
            );

        let certificate_with_merkle_root = {
            let mut protocol_message = ProtocolMessage::new();
            protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
                "merkleroot".to_string(),
            );
            Certificate {
                protocol_message,
                ..fake_data::certificate("certificate-123".to_string())
            }
        };
        let beacon = BlockNumber(100);

        let artifact = cardano_blocks_transactions_artifact_builder
            .compute_artifact(beacon, &certificate_with_merkle_root)
            .await
            .unwrap();

        assert_eq!(
            CardanoBlocksTransactionsSnapshot::new(
                "merkleroot".to_string(),
                beacon,
                BlockNumber(15)
            ),
            artifact
        );
    }

    #[tokio::test]
    async fn should_fail_to_compute_artifact_without_merkle_root() {
        let mut mock_prover = MockProverService::new();
        mock_prover.expect_compute_cache().returning(|_| Ok(()));

        let cardano_blocks_transactions_artifact_builder =
            CardanoBlocksTransactionsArtifactBuilder::new(
                Arc::new(mock_prover),
                Arc::new(MockMithrilNetworkConfigurationProvider::new()),
            );

        let certificate_without_merkle_root = Certificate {
            protocol_message: ProtocolMessage::new(),
            ..fake_data::certificate("certificate-123".to_string())
        };
        let beacon = BlockNumber(100);

        cardano_blocks_transactions_artifact_builder
            .compute_artifact(beacon, &certificate_without_merkle_root)
            .await
            .expect_err("The artifact building must fail since there is no CardanoTransactionsMerkleRoot part in its message.");
    }

    #[tokio::test]
    async fn should_fail_to_compute_artifact_without_cardano_transactions() {
        let mut mock_prover = MockProverService::new();
        mock_prover.expect_compute_cache().returning(|_| Ok(()));
        let mut mock_mithril_network_configuration_provider =
            MockMithrilNetworkConfigurationProvider::new();
        mock_mithril_network_configuration_provider
            .expect_get_network_configuration()
            .times(1)
            .returning(|_| {
                Ok(MithrilNetworkConfiguration {
                    configuration_for_aggregation: MithrilNetworkConfigurationForEpoch {
                        signed_entity_types_config: SignedEntityTypeConfiguration {
                            cardano_transactions: None,
                        },
                        ..Dummy::dummy()
                    },
                    ..Dummy::dummy()
                })
            });
        let cardano_blocks_transactions_artifact_builder =
            CardanoBlocksTransactionsArtifactBuilder::new(
                Arc::new(mock_prover),
                Arc::new(mock_mithril_network_configuration_provider),
            );

        let certificate_with_merkle_root = {
            let mut protocol_message = ProtocolMessage::new();
            protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
                "merkleroot".to_string(),
            );
            Certificate {
                protocol_message,
                ..fake_data::certificate("certificate-123".to_string())
            }
        };
        let beacon = BlockNumber(100);

        cardano_blocks_transactions_artifact_builder
            .compute_artifact(beacon, &certificate_with_merkle_root)
            .await
            .expect_err("The artifact building must fail since there is no CardanoTransaction in MithrilNetworkConfiguration.");
    }
}
