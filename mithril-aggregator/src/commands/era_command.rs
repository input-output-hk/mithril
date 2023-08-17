use anyhow::anyhow;
use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::{
    crypto_helper::{key_decode_hex, EraMarkersSigner},
    entities::{Epoch, HexEncodedEraMarkersSecretKey},
    StdResult,
};
use slog_scope::debug;

use crate::tools::EraTools;

/// Era tools
#[derive(Parser, Debug, Clone)]
pub struct EraCommand {
    /// commands
    #[clap(subcommand)]
    pub era_subcommand: EraSubCommand,
}

impl EraCommand {
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        self.era_subcommand.execute(config_builder).await
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
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::GenerateTxDatum(cmd) => cmd.execute(config_builder).await,
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
    pub async fn execute(&self, _config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        debug!("LIST ERA command");
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
}

impl GenerateTxDatumEraSubCommand {
    pub async fn execute(&self, _config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        debug!("GENERATETXDATUM ERA command");
        let era_tools = EraTools::new();

        let era_markers_secret_key = key_decode_hex(&self.era_markers_secret_key)
            .map_err(|e| anyhow!(e).context("json hex decode of era markers secret key failure"))?;
        let era_markers_signer = EraMarkersSigner::from_secret_key(era_markers_secret_key);
        print!(
            "{}",
            era_tools.generate_tx_datum(
                Epoch(self.current_era_epoch),
                self.next_era_epoch.map(Epoch),
                &era_markers_signer
            )?
        );

        Ok(())
    }
}
