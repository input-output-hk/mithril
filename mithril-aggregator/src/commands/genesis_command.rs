use anyhow::Context;
use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::{
    crypto_helper::{ProtocolGenesisSecretKey, ProtocolGenesisSigner},
    entities::HexEncodedGenesisSecretKey,
    StdResult,
};
use slog_scope::debug;
use std::path::PathBuf;

use crate::{dependency_injection::DependenciesBuilder, tools::GenesisTools, Configuration};

/// Genesis tools
#[derive(Parser, Debug, Clone)]
pub struct GenesisCommand {
    /// commands
    #[clap(subcommand)]
    pub genesis_subcommand: GenesisSubCommand,
}

impl GenesisCommand {
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        self.genesis_subcommand.execute(config_builder).await
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
}

impl GenesisSubCommand {
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            Self::Bootstrap(cmd) => cmd.execute(config_builder).await,
            Self::Export(cmd) => cmd.execute(config_builder).await,
            Self::Import(cmd) => cmd.execute(config_builder).await,
            Self::Sign(cmd) => cmd.execute(config_builder).await,
        }
    }
}

/// Genesis certificate export command
#[derive(Parser, Debug, Clone)]
pub struct ExportGenesisSubCommand {
    /// Target Path
    #[clap(long)]
    target_path: PathBuf,
}

impl ExportGenesisSubCommand {
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config: Configuration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        debug!("EXPORT GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis export payload to sign to {}",
            self.target_path.display()
        );
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let dependencies = dependencies_builder
            .create_genesis_container()
            .await
            .with_context(|| {
                "Dependencies Builder can not create genesis command dependencies container"
            })?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies)
            .await
            .with_context(|| "genesis-tools: initialization error")?;
        genesis_tools
            .export_payload_to_sign(&self.target_path)
            .with_context(|| "genesis-tools: export error")?;
        Ok(())
    }
}

#[derive(Parser, Debug, Clone)]
pub struct ImportGenesisSubCommand {
    /// Signed Payload Path
    #[clap(long)]
    signed_payload_path: PathBuf,
}

impl ImportGenesisSubCommand {
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config: Configuration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        debug!("IMPORT GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis import signed payload from {}",
            self.signed_payload_path.to_string_lossy()
        );
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let dependencies = dependencies_builder
            .create_genesis_container()
            .await
            .with_context(|| {
                "Dependencies Builder can not create genesis command dependencies container"
            })?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies)
            .await
            .with_context(|| "genesis-tools: initialization error")?;
        genesis_tools
            .import_payload_signature(&self.signed_payload_path)
            .await
            .with_context(|| "genesis-tools: import error")?;
        Ok(())
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
    pub async fn execute(&self, _config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        debug!("SIGN GENESIS command");
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
}
#[derive(Parser, Debug, Clone)]
pub struct BootstrapGenesisSubCommand {
    /// Genesis Secret Key (test only)
    #[clap(long, env = "GENESIS_SECRET_KEY")]
    genesis_secret_key: HexEncodedGenesisSecretKey,
}

impl BootstrapGenesisSubCommand {
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config: Configuration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        debug!("BOOTSTRAP GENESIS command"; "config" => format!("{config:?}"));
        println!("Genesis bootstrap for test only!");
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let dependencies = dependencies_builder
            .create_genesis_container()
            .await
            .with_context(|| {
                "Dependencies Builder can not create genesis command dependencies container"
            })?;

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
}
