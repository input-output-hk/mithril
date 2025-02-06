//! Infrastructure builders related to cardano node or cardano node database interactions
//!
//! Ex: [ChainObserver] as it communicate with a node, [ImmutableDigester] as it works on immutables
//! files contained in a cardano node database.

use anyhow::Context;
use std::sync::Arc;
use tokio::sync::Mutex;

use mithril_common::cardano_block_scanner::{BlockScanner, CardanoBlockScanner};
use mithril_common::cardano_transactions_preloader::{
    CardanoTransactionsPreloader, CardanoTransactionsPreloaderActivation,
};
use mithril_common::chain_observer::{
    CardanoCliRunner, ChainObserver, ChainObserverBuilder, FakeObserver,
};
use mithril_common::chain_reader::{ChainBlockReader, PallasChainReader};
use mithril_common::digesters::{
    CardanoImmutableDigester, DumbImmutableFileObserver, ImmutableDigester, ImmutableFileObserver,
    ImmutableFileSystemObserver,
};
use mithril_common::entities::SignedEntityTypeDiscriminants;

use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::ExecutionEnvironment;

impl DependenciesBuilder {
    async fn build_chain_observer(&mut self) -> Result<Arc<dyn ChainObserver>> {
        let chain_observer: Arc<dyn ChainObserver> = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                let cardano_cli_runner = &self.get_cardano_cli_runner().await?;
                let chain_observer_type = &self.configuration.chain_observer_type;
                let cardano_node_socket_path = &self.configuration.cardano_node_socket_path;
                let cardano_network = &self
                    .configuration
                    .get_network()
                    .with_context(|| "Dependencies Builder can not get Cardano network while building the chain observer")?;
                let chain_observer_builder = ChainObserverBuilder::new(
                    chain_observer_type,
                    cardano_node_socket_path,
                    cardano_network,
                    Some(cardano_cli_runner),
                );

                chain_observer_builder
                    .build()
                    .with_context(|| "Dependencies Builder can not build chain observer")?
            }
            _ => Arc::new(FakeObserver::default()),
        };

        Ok(chain_observer)
    }

    /// Get the [ChainObserver] instance
    pub async fn get_chain_observer(&mut self) -> Result<Arc<dyn ChainObserver>> {
        if self.chain_observer.is_none() {
            self.chain_observer = Some(self.build_chain_observer().await?);
        }

        Ok(self.chain_observer.as_ref().cloned().unwrap())
    }

    async fn build_cardano_cli_runner(&mut self) -> Result<Box<CardanoCliRunner>> {
        let cli_runner = CardanoCliRunner::new(
            self.configuration.cardano_cli_path.clone(),
            self.configuration.cardano_node_socket_path.clone(),
            self.configuration.get_network().with_context(|| {
                "Dependencies Builder can not get Cardano network while building cardano cli runner"
            })?,
        );

        Ok(Box::new(cli_runner))
    }

    /// Get the [CardanoCliRunner] instance
    pub async fn get_cardano_cli_runner(&mut self) -> Result<Box<CardanoCliRunner>> {
        if self.cardano_cli_runner.is_none() {
            self.cardano_cli_runner = Some(self.build_cardano_cli_runner().await?);
        }

        Ok(self.cardano_cli_runner.as_ref().cloned().unwrap())
    }

    async fn build_chain_block_reader(&mut self) -> Result<Arc<Mutex<dyn ChainBlockReader>>> {
        let chain_block_reader = PallasChainReader::new(
            &self.configuration.cardano_node_socket_path,
            self.configuration.get_network()?,
            self.root_logger(),
        );

        Ok(Arc::new(Mutex::new(chain_block_reader)))
    }

    /// Get the [ChainBlockReader] instance
    pub async fn get_chain_block_reader(&mut self) -> Result<Arc<Mutex<dyn ChainBlockReader>>> {
        if self.chain_block_reader.is_none() {
            self.chain_block_reader = Some(self.build_chain_block_reader().await?);
        }

        Ok(self.chain_block_reader.as_ref().cloned().unwrap())
    }

    async fn build_block_scanner(&mut self) -> Result<Arc<dyn BlockScanner>> {
        let block_scanner = CardanoBlockScanner::new(
            self.get_chain_block_reader().await?,
            self.configuration
                .cardano_transactions_block_streamer_max_roll_forwards_per_poll,
            self.root_logger(),
        );

        Ok(Arc::new(block_scanner))
    }

    /// Get the [BlockScanner] instance
    pub async fn get_block_scanner(&mut self) -> Result<Arc<dyn BlockScanner>> {
        if self.block_scanner.is_none() {
            self.block_scanner = Some(self.build_block_scanner().await?);
        }

        Ok(self.block_scanner.as_ref().cloned().unwrap())
    }

    async fn build_immutable_file_observer(&mut self) -> Result<Arc<dyn ImmutableFileObserver>> {
        let immutable_file_observer: Arc<dyn ImmutableFileObserver> =
            match self.configuration.environment {
                ExecutionEnvironment::Production => Arc::new(ImmutableFileSystemObserver::new(
                    &self.configuration.db_directory,
                )),
                _ => Arc::new(DumbImmutableFileObserver::default()),
            };

        Ok(immutable_file_observer)
    }

    /// Get the [ImmutableFileObserver] instance
    pub async fn get_immutable_file_observer(&mut self) -> Result<Arc<dyn ImmutableFileObserver>> {
        if self.immutable_file_observer.is_none() {
            self.immutable_file_observer = Some(self.build_immutable_file_observer().await?);
        }

        Ok(self.immutable_file_observer.as_ref().cloned().unwrap())
    }

    async fn build_immutable_digester(&mut self) -> Result<Arc<dyn ImmutableDigester>> {
        let immutable_digester_cache = match self.configuration.environment {
            ExecutionEnvironment::Production => Some(self.get_immutable_cache_provider().await?),
            _ => None,
        };
        let digester = CardanoImmutableDigester::new(
            self.configuration.get_network()?.to_string(),
            immutable_digester_cache,
            self.root_logger(),
        );

        Ok(Arc::new(digester))
    }

    /// Get the [ImmutableDigester] instance
    pub async fn get_immutable_digester(&mut self) -> Result<Arc<dyn ImmutableDigester>> {
        if self.immutable_digester.is_none() {
            self.immutable_digester = Some(self.build_immutable_digester().await?);
        }

        Ok(self.immutable_digester.as_ref().cloned().unwrap())
    }

    /// Create a [CardanoTransactionsPreloader] instance.
    pub async fn create_cardano_transactions_preloader(
        &mut self,
    ) -> Result<Arc<CardanoTransactionsPreloader>> {
        let activation = self
            .configuration
            .compute_allowed_signed_entity_types_discriminants()?
            .contains(&SignedEntityTypeDiscriminants::CardanoTransactions);
        let cardano_transactions_preloader = CardanoTransactionsPreloader::new(
            self.get_signed_entity_lock().await?,
            self.get_transactions_importer().await?,
            self.configuration
                .cardano_transactions_signing_config
                .security_parameter,
            self.get_chain_observer().await?,
            self.root_logger(),
            Arc::new(CardanoTransactionsPreloaderActivation::new(activation)),
        );

        Ok(Arc::new(cardano_transactions_preloader))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::SignedEntityTypeDiscriminants;

    use crate::Configuration;

    use super::*;

    #[tokio::test]
    async fn cardano_transactions_preloader_activated_with_cardano_transactions_signed_entity_type_in_configuration(
    ) {
        assert_cardano_transactions_preloader_activation(
            SignedEntityTypeDiscriminants::CardanoTransactions.to_string(),
            true,
        )
        .await;
        assert_cardano_transactions_preloader_activation(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution.to_string(),
            false,
        )
        .await;
    }

    async fn assert_cardano_transactions_preloader_activation(
        signed_entity_types: String,
        expected_activation: bool,
    ) {
        let configuration = Configuration {
            signed_entity_types: Some(signed_entity_types),
            ..Configuration::new_sample()
        };
        let mut dep_builder = DependenciesBuilder::new_with_stdout_logger(configuration);

        let cardano_transactions_preloader = dep_builder
            .create_cardano_transactions_preloader()
            .await
            .unwrap();

        let is_activated = cardano_transactions_preloader.is_activated().await.unwrap();
        assert_eq!(
            expected_activation, is_activated,
            "'is_activated' expected {}, but was {}",
            expected_activation, is_activated
        );
    }
}
