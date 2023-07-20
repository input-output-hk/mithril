use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::{
    crypto_helper::{key_decode_hex, ProtocolGenesisSigner},
    entities::HexEncodedGenesisSecretKey,
};
use slog_scope::debug;
use std::{error::Error, path::PathBuf};

use crate::{dependency_injection::DependenciesBuilder, tools::GenesisTools, Configuration};

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
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
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
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
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
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        let config: Configuration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("EXPORT GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis export payload to sign to {}",
            self.target_path.display()
        );
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let dependencies = dependencies_builder.create_genesis_container().await?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies)
            .await
            .map_err(|err| format!("genesis-tools: initialization error: {err}"))?;
        genesis_tools
            .export_payload_to_sign(&self.target_path)
            .map_err(|err| format!("genesis-tools: export error: {err}"))?;
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
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        let config: Configuration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("IMPORT GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis import signed payload from {}",
            self.signed_payload_path.to_string_lossy()
        );
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let dependencies = dependencies_builder.create_genesis_container().await?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies)
            .await
            .map_err(|err| format!("genesis-tools: initialization error: {err}"))?;
        genesis_tools
            .import_payload_signature(&self.signed_payload_path)
            .await
            .map_err(|err| format!("genesis-tools: import error: {err}"))?;
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
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        let config: Configuration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("SIGN GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis sign payload from {} to {}",
            self.to_sign_payload_path.to_string_lossy(),
            self.target_signed_payload_path.to_string_lossy()
        );
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let dependencies = dependencies_builder.create_genesis_container().await?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies)
            .await
            .map_err(|err| format!("genesis-tools: initialization error: {err}"))?;
        genesis_tools
            .sign_genesis_certificate(
                &self.to_sign_payload_path,
                &self.target_signed_payload_path,
                &self.genesis_secret_key_path,
            )
            .await
            .map_err(|err| format!("genesis-tools: sign error: {err}"))?;

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
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        let config: Configuration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("BOOTSTRAP GENESIS command"; "config" => format!("{config:?}"));
        println!("Genesis bootstrap for test only!");
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let dependencies = dependencies_builder.create_genesis_container().await?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies)
            .await
            .map_err(|err| format!("genesis-tools: initialization error: {err}"))?;
        let genesis_secret_key = key_decode_hex(&self.genesis_secret_key)?;
        let genesis_signer = ProtocolGenesisSigner::from_secret_key(genesis_secret_key);
        genesis_tools
            .bootstrap_test_genesis_certificate(genesis_signer)
            .await
            .map_err(|err| format!("genesis-tools: bootstrap error: {err}"))?;
        Ok(())
    }
}
