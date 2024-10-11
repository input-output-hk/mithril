use std::{fs::File, io::Write, path::PathBuf};

use anyhow::Context;
use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::{
    crypto_helper::{EraMarkersSigner, EraMarkersVerifierSecretKey},
    entities::{Epoch, HexEncodedEraMarkersSecretKey},
    StdResult,
};
use slog::{debug, Logger};

use crate::tools::EraTools;

/// Era tools
#[derive(Parser, Debug, Clone)]
pub struct EraCommand {
    /// commands
    #[clap(subcommand)]
    pub era_subcommand: EraSubCommand,
}

impl EraCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        self.era_subcommand
            .execute(root_logger, config_builder)
            .await
    }
}

/// Era tools commands.
#[derive(Debug, Clone, Subcommand)]
pub enum EraSubCommand {
    /// Era list command.
    List(ListEraSubCommand),

    /// Era tx datum generate command.
    GenerateTxDatum(GenerateTxDatumEraSubCommand),
}

impl EraSubCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(root_logger, config_builder).await,
            Self::GenerateTxDatum(cmd) => cmd.execute(root_logger, config_builder).await,
        }
    }
}

/// Era list command
#[derive(Parser, Debug, Clone)]
pub struct ListEraSubCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,
}

impl ListEraSubCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        _config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        debug!(root_logger, "LIST ERA command");
        let era_tools = EraTools::new();
        let eras = era_tools.get_supported_eras_list()?;

        if self.json {
            println!("{}", serde_json::to_string(&eras)?);
        } else {
            println!("Supported Eras:");
            println!("{eras:#?}");
        }

        Ok(())
    }
}

/// Era tx datum generate command
#[derive(Parser, Debug, Clone)]
pub struct GenerateTxDatumEraSubCommand {
    /// Current Era epoch
    #[clap(long, env = "CURRENT_ERA_EPOCH")]
    current_era_epoch: u64,

    /// Next Era epoch start, if exists
    #[clap(long, env = "NEXT_ERA_EPOCH")]
    next_era_epoch: Option<u64>,

    /// Era Markers Secret Key
    #[clap(long, env = "ERA_MARKERS_SECRET_KEY")]
    era_markers_secret_key: HexEncodedEraMarkersSecretKey,

    /// Target Path
    #[clap(long)]
    target_path: PathBuf,
}

impl GenerateTxDatumEraSubCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        _config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        debug!(root_logger, "GENERATETXDATUM ERA command");
        let era_tools = EraTools::new();

        let era_markers_secret_key =
            EraMarkersVerifierSecretKey::from_json_hex(&self.era_markers_secret_key)
                .with_context(|| "json hex decode of era markers secret key failure")?;
        let era_markers_signer = EraMarkersSigner::from_secret_key(era_markers_secret_key);
        let tx_datum = era_tools.generate_tx_datum(
            Epoch(self.current_era_epoch),
            self.next_era_epoch.map(Epoch),
            &era_markers_signer,
        )?;

        let mut target_file = File::create(&self.target_path)?;
        target_file.write_all(tx_datum.as_bytes())?;

        Ok(())
    }
}
