use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder};
use directories::ProjectDirs;
use mithril_common::{
    api_version::APIVersionProvider,
    certificate_chain::MithrilCertificateVerifier,
    crypto_helper::{key_decode_hex, ProtocolGenesisVerifier},
    digesters::{
        cache::ImmutableFileDigestCacheProvider,
        cache::JsonImmutableFileDigestCacheProviderBuilder, CardanoImmutableDigester,
    },
};
use slog_scope::{debug, warn};
use std::{error::Error, path::Path, sync::Arc};

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
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("{:?}", config);
        let mut runtime = Runtime::new(config.network.clone());
        let aggregator_handler = AggregatorHTTPClient::new(
            config.clone().network,
            config.clone().aggregator_endpoint,
            APIVersionProvider::compute_all_versions_sorted()?,
        );
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
            let cache_provider = JsonImmutableFileDigestCacheProviderBuilder::new(
                project_dirs.cache_dir(),
                &format!("immutables_digests_{}.json", config.network),
            )
            .ensure_dir_exist()
            .should_reset_digests_cache(reset_digests_cache)
            .build()
            .await?;

            Ok(Some(Arc::new(cache_provider)))
        }
    }
}
