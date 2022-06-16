#![doc = include_str!("../README.md")]

use clap::Parser;

use config::{Map, Source, Value, ValueKind};
use mithril_aggregator::{
    AggregatorConfig, AggregatorRunner, AggregatorRuntime, BeaconStore, CertificatePendingStore,
    CertificateStore, Config, DependencyManager, MemoryBeaconStore, MultiSigner, MultiSignerImpl,
    Server, VerificationKeyStore,
};
use mithril_common::crypto_helper::ProtocolStakeDistribution;
use mithril_common::fake_data;
use mithril_common::store::adapter::JsonFileStoreAdapter;
use mithril_common::store::stake_store::StakeStore;
use slog::{Drain, Level, Logger};
use slog_scope::debug;
use std::env;
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio::time::Duration;

/// Node args
#[derive(Parser, Debug, Clone)]
pub struct Args {
    /// Server listening IP
    #[clap(long, default_value = "0.0.0.0")]
    server_ip: String,

    /// Server listening port
    #[clap(long, default_value_t = 8080)]
    server_port: u16,

    /// Verbosity level
    #[clap(short, long, parse(from_occurrences))]
    verbose: usize,

    /// Run mode
    #[clap(short, long, default_value = "dev")]
    run_mode: String,

    /// Runtime interval, in seconds
    /// Defaults to 10 minutes
    #[clap(long, default_value_t = 600)]
    runtime_interval: u32,

    /// Directory to snapshot
    #[clap(long, default_value = "/db")]
    db_directory: PathBuf,

    /// Directory to store snapshot
    /// Defaults to work folder
    #[clap(long, default_value = ".")]
    snapshot_directory: PathBuf,
}

impl Args {
    fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Warning,
            1 => Level::Info,
            2 => Level::Debug,
            _ => Level::Trace,
        }
    }

    fn build_logger(&self) -> Logger {
        let drain = slog_bunyan::new(std::io::stdout())
            .set_pretty(false)
            .build()
            .fuse();
        let drain = slog::LevelFilter::new(drain, self.log_level()).fuse();
        let drain = slog_async::Async::new(drain).build().fuse();

        Logger::root(Arc::new(drain), slog::o!())
    }
}

impl Source for Args {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new((*self).clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut result = Map::new();
        let uri = "clap arguments".to_string();

        let server_url = format!("http://{}:{}/", &self.server_ip, &self.server_port);
        result.insert(
            "server_url".to_string(),
            Value::new(Some(&uri), ValueKind::from(server_url)),
        );
        result.insert(
            "db_directory".to_string(),
            Value::new(
                Some(&uri),
                ValueKind::from(self.db_directory.to_str().unwrap().to_string()),
            ),
        );
        result.insert(
            "snapshot_directory".to_string(),
            Value::new(
                Some(&uri),
                ValueKind::from(self.snapshot_directory.to_str().unwrap().to_string()),
            ),
        );

        Ok(result)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    // Load args
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(args.build_logger());

    // Load config
    let run_mode = env::var("RUN_MODE").unwrap_or_else(|_| args.run_mode.clone());
    let config: Config = config::Config::builder()
        .add_source(config::File::with_name(&format!("./config/{}.json", run_mode)).required(false))
        .add_source(config::Environment::default())
        .add_source(args.clone())
        .build()
        .map_err(|e| format!("configuration build error: {}", e))?
        .try_deserialize()
        .map_err(|e| format!("configuration deserialize error: {}", e))?;
    debug!("Started"; "run_mode" => &run_mode, "config" => format!("{:?}", config));

    // Init dependencies
    let snapshot_store = config.build_snapshot_store();

    let beacon_store = Arc::new(RwLock::new(MemoryBeaconStore::new()));
    let snapshot_uploader = config.build_snapshot_uploader();
    let certificate_pending_store = Arc::new(RwLock::new(CertificatePendingStore::new(Box::new(
        JsonFileStoreAdapter::new(config.pending_certificate_store_directory.clone())?,
    ))));
    let certificate_store = Arc::new(RwLock::new(CertificateStore::new(Box::new(
        JsonFileStoreAdapter::new(config.certificate_store_directory.clone())?,
    ))));
    let verification_key_store = Arc::new(RwLock::new(VerificationKeyStore::new(Box::new(
        JsonFileStoreAdapter::new(config.verification_key_store_directory.clone())?,
    ))));
    let stake_store = Arc::new(RwLock::new(StakeStore::new(Box::new(
        JsonFileStoreAdapter::new(config.stake_store_directory.clone())?,
    ))));
    let multi_signer = Arc::new(RwLock::new(MultiSignerImpl::new(
        beacon_store.clone(),
        verification_key_store.clone(),
        stake_store.clone(),
    )));
    setup_dependencies_fake_data(multi_signer.clone(), beacon_store.clone()).await;

    // Init dependency manager
    let mut dependency_manager = DependencyManager::new(config.clone());
    dependency_manager
        .with_snapshot_store(snapshot_store.clone())
        .with_snapshot_uploader(snapshot_uploader.clone())
        .with_multi_signer(multi_signer.clone())
        .with_beacon_store(beacon_store.clone())
        .with_certificate_pending_store(certificate_pending_store.clone())
        .with_certificate_store(certificate_store.clone())
        .with_verification_key_store(verification_key_store.clone())
        .with_stake_store(stake_store.clone());
    let dependency_manager = Arc::new(dependency_manager);

    // Start snapshot uploader
    let snapshot_directory = config.snapshot_directory.clone();
    let runtime_dependencies = dependency_manager.clone();
    let handle = tokio::spawn(async move {
        let config = AggregatorConfig::new(
            args.runtime_interval,
            &config.network.clone(),
            &config.db_directory.clone(),
            &snapshot_directory,
            runtime_dependencies,
        );
        let mut runtime = AggregatorRuntime::new(
            Duration::from_secs(config.interval.into()),
            None,
            Arc::new(AggregatorRunner::new(config)),
        )
        .await
        .unwrap();
        runtime.run().await
    });

    // Start REST server
    println!("Starting server...");
    println!("Press Ctrl+C to stop...");
    let shutdown_signal = async {
        tokio::signal::ctrl_c()
            .await
            .expect("failed to install CTRL+C signal handler");
    };
    let http_server = Server::new(args.server_ip, args.server_port, dependency_manager.clone());
    http_server.start(shutdown_signal).await;

    handle.abort();

    println!("Exiting...");
    Ok(())
}

/// Setup dependencies with fake data
// TODO: remove this function when new runtime is implemented + remove protocol parameters from fake data
async fn setup_dependencies_fake_data(
    multi_signer: Arc<RwLock<dyn MultiSigner>>,
    beacon_store: Arc<RwLock<dyn BeaconStore>>,
) {
    // Set current beacon
    {
        let beacon = fake_data::beacon();
        let mut beacon_store = beacon_store.write().await;
        beacon_store
            .set_current_beacon(beacon.clone())
            .await
            .expect("fake set current beacon failed");
    }

    // Update protocol parameters
    {
        let mut multi_signer = multi_signer.write().await;
        let protocol_parameters = fake_data::protocol_parameters();
        multi_signer
            .update_protocol_parameters(&protocol_parameters.into())
            .await
            .expect("fake update protocol parameters failed");
    }

    // Update stake distribution
    {
        let mut multi_signer = multi_signer.write().await;
        let total_signers = 5;
        let stakes: ProtocolStakeDistribution = fake_data::signers_with_stakes(total_signers)
            .into_iter()
            .map(|signer| signer.into())
            .collect::<_>();
        multi_signer
            .update_stake_distribution(&stakes)
            .await
            .expect("fake stake distribution update failed");
    }
}
