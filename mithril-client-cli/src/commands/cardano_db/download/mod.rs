mod v1;

use v1::PreparedCardanoDbV1Download;

use clap::Parser;
use std::{collections::HashMap, path::PathBuf};

use crate::{
    commands::SharedArgs,
    configuration::{ConfigError, ConfigParameters, ConfigSource},
    utils::{self, AncillaryLogMessage},
    CommandContext,
};
use mithril_client::MithrilResult;

/// Clap command to download a Cardano db and verify its associated certificate.
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbDownloadCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,

    /// Digest of the Cardano db snapshot to download  or `latest` for the latest artifact
    ///
    /// Use the `list` command to get that information.
    digest: String,

    /// Directory where the immutable and ancillary files will be downloaded.
    ///
    /// By default, a subdirectory will be created in this directory to extract and verify the
    /// certificate.
    #[clap(long)]
    download_dir: Option<PathBuf>,

    /// Genesis verification key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,

    /// Include ancillary files in the download, if set the `ancillary_verification_key` is required
    /// in order to verify the ancillary files.
    ///
    /// By default, only finalized immutable files are downloaded.
    /// The last ledger state snapshot and the last immutable file (the ancillary files) can be
    /// downloaded with this option.
    #[clap(long)]
    include_ancillary: bool,

    /// Ancillary verification key to verify the ancillary files.
    #[clap(long, env = "ANCILLARY_VERIFICATION_KEY")]
    ancillary_verification_key: Option<String>,
}

impl CardanoDbDownloadCommand {
    /// Command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let params = context.config_parameters()?.add_source(self)?;
        let prepared_command = self.prepare_v1(&params)?;

        prepared_command.execute(context.logger(), params).await
    }

    fn prepare_v1(&self, params: &ConfigParameters) -> MithrilResult<PreparedCardanoDbV1Download> {
        let ancillary_verification_key = if self.include_ancillary {
            AncillaryLogMessage::warn_ancillary_not_signed_by_mithril();
            Some(params.require("ancillary_verification_key")?)
        } else {
            AncillaryLogMessage::warn_fast_bootstrap_not_available();
            None
        };

        Ok(PreparedCardanoDbV1Download {
            shared_args: self.shared_args.clone(),
            digest: self.digest.clone(),
            download_dir: params.require("download_dir")?,
            include_ancillary: self.include_ancillary,
            ancillary_verification_key,
        })
    }
}

impl ConfigSource for CardanoDbDownloadCommand {
    fn collect(&self) -> Result<HashMap<String, String>, ConfigError> {
        let mut map = HashMap::new();

        if let Some(download_dir) = self.download_dir.clone() {
            let param = "download_dir".to_string();
            map.insert(
                param.clone(),
                utils::path_to_string(&download_dir)
                    .map_err(|e| ConfigError::Conversion(param, e))?,
            );
        }

        if let Some(genesis_verification_key) = self.genesis_verification_key.clone() {
            map.insert(
                "genesis_verification_key".to_string(),
                genesis_verification_key,
            );
        }

        if let Some(ancillary_verification_key) = self.ancillary_verification_key.clone() {
            map.insert(
                "ancillary_verification_key".to_string(),
                ancillary_verification_key,
            );
        }

        Ok(map)
    }
}

#[cfg(test)]
mod tests {
    use config::ConfigBuilder;
    use slog::Logger;

    use super::*;

    fn dummy_command() -> CardanoDbDownloadCommand {
        CardanoDbDownloadCommand {
            shared_args: SharedArgs { json: false },
            digest: "whatever_digest".to_string(),
            download_dir: Some(std::path::PathBuf::from("whatever_dir")),
            genesis_verification_key: "whatever".to_string().into(),
            include_ancillary: true,
            ancillary_verification_key: "whatever".to_string().into(),
        }
    }

    #[tokio::test]
    async fn ancillary_verification_key_is_mandatory_when_include_ancillary_is_true() {
        let command = CardanoDbDownloadCommand {
            include_ancillary: true,
            ancillary_verification_key: None,
            ..dummy_command()
        };
        let command_context = CommandContext::new(
            ConfigBuilder::default(),
            false,
            Logger::root(slog::Discard, slog::o!()),
        );

        let result = command.execute(command_context).await;

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Parameter 'ancillary_verification_key' is mandatory."
        );
    }

    #[test]
    fn ancillary_verification_key_can_be_read_through_configuration_file() {
        let command = CardanoDbDownloadCommand {
            ancillary_verification_key: None,
            ..dummy_command()
        };
        let config = config::Config::builder()
            .set_default("ancillary_verification_key", "value from config")
            .expect("Failed to build config builder");
        let command_context =
            CommandContext::new(config, false, Logger::root(slog::Discard, slog::o!()));
        let config_parameters = command_context
            .config_parameters()
            .unwrap()
            .add_source(&command)
            .unwrap();

        let result = command.prepare_v1(&config_parameters);

        assert!(result.is_ok());
    }

    #[test]
    fn db_download_dir_is_mandatory_to_execute_command() {
        let command = CardanoDbDownloadCommand {
            download_dir: None,
            ..dummy_command()
        };
        let command_context = CommandContext::new(
            ConfigBuilder::default(),
            false,
            Logger::root(slog::Discard, slog::o!()),
        );
        let config_parameters = command_context
            .config_parameters()
            .unwrap()
            .add_source(&command)
            .unwrap();

        let result = command.prepare_v1(&config_parameters);

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Parameter 'download_dir' is mandatory."
        );
    }
}
