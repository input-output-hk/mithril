use std::{collections::HashMap, path::PathBuf};

use clap::Parser;

use mithril_client::{common::ImmutableFileNumber, MithrilResult};

use crate::{
    commands::{cardano_db::CardanoDbDownloadCommand as NewCardanoDbDownloadCommand, SharedArgs},
    configuration::{ConfigError, ConfigSource},
    utils, CommandContext,
};

/// Clap command to download a Cardano db and verify its associated certificate.
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbV2DownloadCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,

    /// Hash of the Cardano db snapshot to download  or `latest` for the latest artifact
    ///
    /// Use the `list` command to get that information.
    hash: String,

    /// Directory where the immutable and ancillary files will be downloaded.
    ///
    /// By default, a subdirectory will be created in this directory to extract and verify the
    /// certificate.
    #[clap(long)]
    download_dir: Option<PathBuf>,

    /// Genesis verification key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,

    /// The first immutable file number to download.
    ///
    /// If not set, the download process will start from the first immutable file.
    #[clap(long)]
    start: Option<ImmutableFileNumber>,

    /// The last immutable file number to download.
    ///
    /// If not set, the download will continue until the last certified immutable file.
    #[clap(long)]
    end: Option<ImmutableFileNumber>,

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

    /// Allow existing files in the download directory to be overridden.
    #[clap(long)]
    allow_override: bool,
}

impl CardanoDbV2DownloadCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        // *Important*: All parameters also available in the collected config source below MUST be
        // obtained using the `params` argument.
        // No need for `require` as validation of existence will be done by the `NewCardanoDbDownloadCommand` itself.
        let params = context.config_parameters()?.add_source(self)?;
        let command = NewCardanoDbDownloadCommand::new_v2(
            self.shared_args.clone(),
            self.hash.clone(),
            params.get("download_dir").map(PathBuf::from),
            params.get("genesis_verification_key"),
            self.include_ancillary,
            params.get("ancillary_verification_key"),
            self.start,
            self.end,
            self.allow_override,
        );
        command.execute(context).await
    }
}

impl ConfigSource for CardanoDbV2DownloadCommand {
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
