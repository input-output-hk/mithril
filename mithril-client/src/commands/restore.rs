use std::{error::Error, fs, path::Path, sync::Arc};

use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder};
use directories::ProjectDirs;
use mithril_common::{
    certificate_chain::MithrilCertificateVerifier,
    crypto_helper::{key_decode_hex, ProtocolGenesisVerifier},
    digesters::cache::{ImmutableFileDigestCacheProvider, JsonImmutableFileDigestCacheProvider},
    digesters::CardanoImmutableDigester,
};
use slog_scope::{debug, info, warn};

use crate::{AggregatorHTTPClient, AggregatorHandler, Config, Runtime};

/// Check a downloaded snapshot and restore it if the signature is OK.
#[derive(Parser, Debug, Clone)]
pub struct RestoreCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,

    /// Disable immutables digests cache.
    #[clap(long)]
    disable_digests_cache: bool,

    /// If set the existing immutables digests cache will be reset.
    ///
    /// Will be ignored if set in conjunction with `--disable-digests-cache`.
    #[clap(long)]
    reset_digests_cache: bool,

    /// Digest of the snapshot to download. Use the `list` command to get that information.
    digest: String,
}

impl RestoreCommand {
    /// execute restore command
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        debug!("Restore snapshot");
        let config: Config = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {}", e))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {}", e))?;
        debug!("{:?}", config);
        let mut runtime = Runtime::new(config.network.clone());
        let aggregator_handler =
            AggregatorHTTPClient::new(config.network.clone(), config.aggregator_endpoint.clone());
        let certificate_verifier = Box::new(MithrilCertificateVerifier::new(slog_scope::logger()));
        let genesis_verification_key = key_decode_hex(&config.genesis_verification_key)?;
        let genesis_verifier =
            ProtocolGenesisVerifier::from_verification_key(genesis_verification_key);
        let unpacked_path = aggregator_handler.unpack_snapshot(&self.digest).await?;

        let digester = Box::new(CardanoImmutableDigester::new(
            Path::new(&unpacked_path).into(),
            build_digester_cache_provider(
                self.disable_digests_cache,
                self.reset_digests_cache,
                &config,
            )
            .await?,
            slog_scope::logger(),
        ));
        let output = runtime
            .restore_snapshot(
                Arc::new(aggregator_handler),
                digester,
                certificate_verifier,
                genesis_verifier,
                &self.digest,
            )
            .await?;
        println!(
            r###"Unpack success {}
to {}

Restore a Cardano Node with:

docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="{}",target=/data/db/ -e NETWORK={} inputoutput/cardano-node

"###,
            &self.digest,
            output,
            output,
            config.network.clone()
        );
        Ok(())
    }
}

async fn build_digester_cache_provider(
    disable_digests_cache: bool,
    reset_digests_cache: bool,
    config: &Config,
) -> Result<Option<Arc<dyn ImmutableFileDigestCacheProvider>>, Box<dyn Error>> {
    if disable_digests_cache {
        return Ok(None);
    }

    match ProjectDirs::from("io", "iohk", "mithril") {
        None => {
            warn!("Could not get cache directory, disabling immutables digests cache");
            Ok(None)
        }
        Some(project_dirs) => {
            let cache_dir: &Path = project_dirs.cache_dir();
            if !cache_dir.exists() {
                fs::create_dir_all(cache_dir).map_err(|e| {
                    format!(
                        "Failure when creation cache directory `{}`: {}",
                        cache_dir.display(),
                        e
                    )
                })?;
            }

            let cache_file = cache_dir.join(format!("immutables_digests_{}.json", config.network));
            let cache_provider = Arc::new(JsonImmutableFileDigestCacheProvider::new(&cache_file));

            if reset_digests_cache {
                cache_provider.reset().await.map_err(|e| {
                    format!(
                        "Failure when resetting digests cache file `{}`: {}",
                        cache_file.display(),
                        e
                    )
                })?;
            }

            info!(
                "Storing/Getting immutables digests cache from: {}",
                cache_file.display()
            );
            Ok(Some(cache_provider))
        }
    }
}
