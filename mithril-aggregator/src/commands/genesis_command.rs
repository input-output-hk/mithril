use std::{collections::HashMap, path::PathBuf, sync::Arc};

use anyhow::Context;
use clap::{Parser, Subcommand};
use config::{ConfigBuilder, Map, Value, builder::DefaultState};
use serde::{Deserialize, Serialize};
use slog::{Logger, debug};

use mithril_cardano_node_chain::chain_observer::ChainObserverType;
use mithril_common::{
    StdResult,
    crypto_helper::{
        ProtocolGenesisSecretKey, ProtocolGenesisSigner, ProtocolGenesisVerificationKey,
    },
    entities::{HexEncodedGenesisSecretKey, HexEncodedGenesisVerificationKey},
};
use mithril_doc::{Documenter, StructDoc};

use crate::{
    ConfigurationSource, ExecutionEnvironment, dependency_injection::DependenciesBuilder,
    extract_all, tools::GenesisTools,
};

#[derive(Debug, Clone, Serialize, Deserialize, Documenter)]
pub struct GenesisCommandConfiguration {
    /// Cardano CLI tool path
    #[example = "`cardano-cli`"]
    pub cardano_cli_path: Option<PathBuf>,

    /// Path of the socket opened by the Cardano node
    #[example = "`/ipc/node.socket`"]
    pub cardano_node_socket_path: PathBuf,

    /// Cardano Network Magic number
    ///
    /// useful for TestNet & DevNet
    #[example = "`1097911063` or `42`"]
    pub network_magic: Option<u64>,

    /// Cardano network
    #[example = "`testnet` or `mainnet` or `devnet`"]
    network: String,

    /// Cardano chain observer type
    pub chain_observer_type: ChainObserverType,

    /// Directory to store aggregator data (Certificates, Snapshots, Protocol Parameters, ...)
    #[example = "`./mithril-aggregator/stores`"]
    pub data_stores_directory: PathBuf,
}

impl ConfigurationSource for GenesisCommandConfiguration {
    fn environment(&self) -> ExecutionEnvironment {
        ExecutionEnvironment::Production
    }

    fn cardano_cli_path(&self) -> PathBuf {
        match self.chain_observer_type {
            ChainObserverType::CardanoCli => {
                if let Some(path) = &self.cardano_cli_path {
                    path.clone()
                } else {
                    panic!("Cardano CLI path must be set when using Cardano CLI chain observer")
                }
            }
            _ => PathBuf::new(),
        }
    }

    fn cardano_node_socket_path(&self) -> PathBuf {
        self.cardano_node_socket_path.clone()
    }

    fn network_magic(&self) -> Option<u64> {
        self.network_magic
    }

    fn network(&self) -> String {
        self.network.clone()
    }

    fn chain_observer_type(&self) -> ChainObserverType {
        self.chain_observer_type.clone()
    }

    fn data_stores_directory(&self) -> PathBuf {
        self.data_stores_directory.clone()
    }

    fn store_retention_limit(&self) -> Option<usize> {
        None
    }
}

/// Genesis tools
#[derive(Parser, Debug, Clone)]
pub struct GenesisCommand {
    /// commands
    #[clap(subcommand)]
    pub genesis_subcommand: GenesisSubCommand,
}

impl GenesisCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        self.genesis_subcommand.execute(root_logger, config_builder).await
    }

    pub fn extract_config(command_path: String) -> HashMap<String, StructDoc> {
        extract_all!(
            command_path,
            GenesisSubCommand,
            Export = { ExportGenesisSubCommand },
            Import = { ImportGenesisSubCommand },
            Sign = { SignGenesisSubCommand },
            Bootstrap = { BootstrapGenesisSubCommand },
            GenerateKeypair = { GenerateKeypairGenesisSubCommand },
        )
    }
}

/// Genesis tools commands.
#[derive(Debug, Clone, Subcommand)]
pub enum GenesisSubCommand {
    /// Genesis certificate export command.
    Export(ExportGenesisSubCommand),

    /// Genesis certificate import command.
    Import(ImportGenesisSubCommand),

    /// Genesis certificate sign command.
    Sign(SignGenesisSubCommand),

    /// Genesis certificate bootstrap command.
    Bootstrap(BootstrapGenesisSubCommand),

    /// Genesis keypair generation command.
    GenerateKeypair(GenerateKeypairGenesisSubCommand),
}

impl GenesisSubCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        match self {
            Self::Bootstrap(cmd) => cmd.execute(root_logger, config_builder).await,
            Self::Export(cmd) => cmd.execute(root_logger, config_builder).await,
            Self::Import(cmd) => cmd.execute(root_logger, config_builder).await,
            Self::Sign(cmd) => cmd.execute(root_logger).await,
            Self::GenerateKeypair(cmd) => cmd.execute(root_logger).await,
        }
    }
}

/// Genesis certificate export command
#[derive(Parser, Debug, Clone)]
pub struct ExportGenesisSubCommand {
    /// Target path
    #[clap(long)]
    target_path: PathBuf,
}

impl ExportGenesisSubCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        let config: GenesisCommandConfiguration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        debug!(root_logger, "EXPORT GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis export payload to sign to {}",
            self.target_path.display()
        );
        let mut dependencies_builder =
            DependenciesBuilder::new(root_logger.clone(), Arc::new(config.clone()));
        let dependencies = dependencies_builder.create_genesis_container().await.with_context(
            || "Dependencies Builder can not create genesis command dependencies container",
        )?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies)
            .await
            .with_context(|| "genesis-tools: initialization error")?;
        genesis_tools
            .export_payload_to_sign(&self.target_path)
            .with_context(|| "genesis-tools: export error")?;
        Ok(())
    }

    pub fn extract_config(command_path: String) -> HashMap<String, StructDoc> {
        HashMap::from([(command_path, GenesisCommandConfiguration::extract())])
    }
}

#[derive(Parser, Debug, Clone)]
pub struct ImportGenesisSubCommand {
    /// Signed Payload Path
    #[clap(long)]
    signed_payload_path: PathBuf,

    /// Genesis Verification Key
    #[clap(long)]
    genesis_verification_key: HexEncodedGenesisVerificationKey,
}

impl ImportGenesisSubCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        let config: GenesisCommandConfiguration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        debug!(root_logger, "IMPORT GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis import signed payload from {}",
            self.signed_payload_path.to_string_lossy()
        );
        let mut dependencies_builder =
            DependenciesBuilder::new(root_logger.clone(), Arc::new(config.clone()));
        let dependencies = dependencies_builder.create_genesis_container().await.with_context(
            || "Dependencies Builder can not create genesis command dependencies container",
        )?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies)
            .await
            .with_context(|| "genesis-tools: initialization error")?;
        genesis_tools
            .import_payload_signature(
                &self.signed_payload_path,
                &ProtocolGenesisVerificationKey::from_json_hex(&self.genesis_verification_key)?,
            )
            .await
            .with_context(|| "genesis-tools: import error")?;
        Ok(())
    }

    pub fn extract_config(command_path: String) -> HashMap<String, StructDoc> {
        HashMap::from([(command_path, GenesisCommandConfiguration::extract())])
    }
}

#[derive(Parser, Debug, Clone)]
pub struct SignGenesisSubCommand {
    /// To Sign Payload Path
    #[clap(long)]
    to_sign_payload_path: PathBuf,

    /// Target Signed Payload Path
    #[clap(long)]
    target_signed_payload_path: PathBuf,

    /// Genesis Secret Key Path
    #[clap(long)]
    genesis_secret_key_path: PathBuf,
}

impl SignGenesisSubCommand {
    pub async fn execute(&self, root_logger: Logger) -> StdResult<()> {
        debug!(root_logger, "SIGN GENESIS command");
        println!(
            "Genesis sign payload from {} to {}",
            self.to_sign_payload_path.to_string_lossy(),
            self.target_signed_payload_path.to_string_lossy()
        );

        GenesisTools::sign_genesis_certificate(
            &self.to_sign_payload_path,
            &self.target_signed_payload_path,
            &self.genesis_secret_key_path,
        )
        .await
        .with_context(|| "genesis-tools: sign error")?;

        Ok(())
    }

    pub fn extract_config(_command_path: String) -> HashMap<String, StructDoc> {
        HashMap::new()
    }
}
#[derive(Parser, Debug, Clone)]
pub struct BootstrapGenesisSubCommand {
    /// Genesis Secret Key (test only)
    #[clap(long, env = "GENESIS_SECRET_KEY")]
    genesis_secret_key: HexEncodedGenesisSecretKey,
}

impl BootstrapGenesisSubCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        let config: GenesisCommandConfiguration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        debug!(root_logger, "BOOTSTRAP GENESIS command"; "config" => format!("{config:?}"));
        println!("Genesis bootstrap for test only!");
        let mut dependencies_builder =
            DependenciesBuilder::new(root_logger.clone(), Arc::new(config.clone()));
        let dependencies = dependencies_builder.create_genesis_container().await.with_context(
            || "Dependencies Builder can not create genesis command dependencies container",
        )?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies)
            .await
            .with_context(|| "genesis-tools: initialization error")?;
        let genesis_secret_key = ProtocolGenesisSecretKey::from_json_hex(&self.genesis_secret_key)
            .with_context(|| "json hex decode of genesis secret key failure")?;
        let genesis_signer = ProtocolGenesisSigner::from_secret_key(genesis_secret_key);
        genesis_tools
            .bootstrap_test_genesis_certificate(genesis_signer)
            .await
            .with_context(|| "genesis-tools: bootstrap error")?;
        Ok(())
    }

    pub fn extract_config(command_path: String) -> HashMap<String, StructDoc> {
        HashMap::from([(command_path, GenesisCommandConfiguration::extract())])
    }
}

/// Genesis keypair generation command.
#[derive(Parser, Debug, Clone)]
pub struct GenerateKeypairGenesisSubCommand {
    /// Target path for the generated keypair
    #[clap(long)]
    target_path: PathBuf,
}

impl GenerateKeypairGenesisSubCommand {
    pub async fn execute(&self, root_logger: Logger) -> StdResult<()> {
        debug!(root_logger, "GENERATE KEYPAIR GENESIS command");
        println!(
            "Genesis generate keypair to {}",
            self.target_path.to_string_lossy()
        );

        GenesisTools::create_and_save_genesis_keypair(&self.target_path)
            .with_context(|| "genesis-tools: keypair generation error")?;

        Ok(())
    }

    pub fn extract_config(_parent: String) -> HashMap<String, StructDoc> {
        HashMap::new()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use mithril_common::temp_dir;

    use crate::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn create_container_does_not_panic() {
        let config = GenesisCommandConfiguration {
            cardano_cli_path: None,
            cardano_node_socket_path: PathBuf::new(),
            network_magic: Some(42),
            network: "devnet".to_string(),
            chain_observer_type: ChainObserverType::Fake,
            data_stores_directory: temp_dir!().join("stores"),
        };
        let mut dependencies_builder =
            DependenciesBuilder::new(TestLogger::stdout(), Arc::new(config));

        dependencies_builder
            .create_genesis_container()
            .await
            .expect("Expected container creation to succeed without panicking");
    }
}
