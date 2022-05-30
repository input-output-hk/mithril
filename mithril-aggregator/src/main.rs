#![doc = include_str!("../README.md")]

use clap::Parser;

use config::{Map, Source, Value, ValueKind};
use mithril_aggregator::{
    AggregatorRuntime, CertificatePendingStore, Config, DependencyManager, JsonFileStoreAdapter,
    MemoryBeaconStore, MultiSigner, MultiSignerImpl, Server,
};
use mithril_common::crypto_helper::ProtocolStakeDistribution;
use mithril_common::fake_data;
use slog::{Drain, Level, Logger};
use slog_scope::debug;
use std::env;
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

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

    /// Snapshot interval, in seconds
    /// Defaults to 4 hours
    #[clap(long, default_value_t = 14400)]
    snapshot_interval: u32,

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
    let multi_signer = Arc::new(RwLock::new(init_multi_signer()));
    let beacon_store = Arc::new(RwLock::new(MemoryBeaconStore::default()));
    let snapshot_uploader = config.build_snapshot_uploader();
    let certificate_pending_store = Arc::new(RwLock::new(CertificatePendingStore::new(Box::new(
        JsonFileStoreAdapter::new(std::env::temp_dir().join("mithril_cert_db"))?,
    ))));

    // Init dependency manager
    let mut dependency_manager = DependencyManager::new(config.clone());
    dependency_manager
        .with_snapshot_store(snapshot_store.clone())
        .with_multi_signer(multi_signer.clone())
        .with_beacon_store(beacon_store.clone());
    let dependency_manager = Arc::new(dependency_manager);

    // Start snapshot uploader
    let snapshot_directory = config.snapshot_directory.clone();
    let handle = tokio::spawn(async move {
        let runtime = AggregatorRuntime::new(
            args.snapshot_interval * 1000,
            config.network.clone(),
            config.db_directory.clone(),
            snapshot_directory,
            beacon_store.clone(),
            multi_signer.clone(),
            snapshot_store.clone(),
            snapshot_uploader,
        );
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

/// Init multi signer dependency
fn init_multi_signer() -> impl MultiSigner {
    let mut multi_signer = MultiSignerImpl::new();

    // Update protocol parameters
    let protocol_parameters = fake_data::protocol_parameters();
    multi_signer
        .update_protocol_parameters(&protocol_parameters.into())
        .expect("update protocol parameters failed");

    // Update stake distribution
    let total_signers = 5;
    let stakes: ProtocolStakeDistribution = fake_data::signers_with_stakes(total_signers)
        .into_iter()
        .map(|signer| signer.into())
        .collect::<_>();
    multi_signer
        .update_stake_distribution(&stakes)
        .expect("stake distribution update failed");

    multi_signer
}
