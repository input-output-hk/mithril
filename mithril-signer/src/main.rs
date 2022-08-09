use clap::Parser;
use mithril_common::BeaconProviderImpl;
use slog::{o, Drain, Level, Logger};
use slog_scope::debug;
use std::env;
use std::error::Error;
use std::sync::Arc;
use std::time::Duration;

use mithril_common::chain_observer::{CardanoCliChainObserver, CardanoCliRunner};
use mithril_common::digesters::{CardanoImmutableDigester, ImmutableFileSystemObserver};
use mithril_common::store::adapter::JsonFileStoreAdapter;
use mithril_common::store::StakeStore;
use mithril_signer::{
    CertificateHandlerHTTPClient, Config, MithrilSingleSigner, ProtocolInitializerStore,
    SignerRunner, SignerServices, SignerState, StateMachine,
};

/// CLI args
#[derive(Parser)]
#[clap(name = "mithril-signer")]
#[clap(about = "An implementation of a Mithril Signer", long_about = None)]
pub struct Args {
    /// Run Mode
    #[clap(short, long, default_value = "dev")]
    run_mode: String,

    /// Verbosity level
    #[clap(short, long, parse(from_occurrences))]
    verbose: usize,
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
}

fn build_logger(min_level: Level) -> Logger {
    let drain = slog_bunyan::new(std::io::stdout())
        .set_pretty(false)
        .build()
        .fuse();
    let drain = slog::LevelFilter::new(drain, min_level).fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), o!())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    // Load args
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(build_logger(args.log_level()));

    // Load config
    let run_mode = env::var("RUN_MODE").unwrap_or(args.run_mode);
    let config: Config = config::Config::builder()
        .add_source(config::File::with_name(&format!("./config/{}.json", run_mode)).required(false))
        .add_source(config::Environment::default())
        .build()
        .map_err(|e| format!("configuration build error: {}", e))?
        .try_deserialize()
        .map_err(|e| format!("configuration deserialize error: {}", e))?;
    debug!("Started"; "run_mode" => &run_mode, "config" => format!("{:?}", config));

    let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(Box::new(
        JsonFileStoreAdapter::new(config.protocol_initializer_store_directory.clone())?,
    )));
    let single_signer = Arc::new(MithrilSingleSigner::new(config.party_id.clone()));
    let certificate_handler = Arc::new(CertificateHandlerHTTPClient::new(
        config.aggregator_endpoint.clone(),
    ));
    let digester = Arc::new(CardanoImmutableDigester::new(
        config.db_directory.clone(),
        slog_scope::logger(),
    ));
    let stake_store = Arc::new(StakeStore::new(Box::new(JsonFileStoreAdapter::new(
        config.stake_store_directory.clone(),
    )?)));
    let chain_observer = Arc::new(CardanoCliChainObserver::new(Box::new(
        CardanoCliRunner::new(
            config.cardano_cli_path.clone(),
            config.cardano_node_socket_path.clone(),
            config.get_network()?,
        ),
    )));
    let beacon_provider = Arc::new(BeaconProviderImpl::new(
        chain_observer.clone(),
        Arc::new(ImmutableFileSystemObserver::new(&config.db_directory)),
        config.get_network()?.to_owned(),
    ));

    let services = SignerServices {
        beacon_provider,
        certificate_handler,
        chain_observer,
        digester,
        single_signer,
        stake_store,
        protocol_initializer_store,
    };
    let mut state_machine = StateMachine::new(
        SignerState::Unregistered,
        Box::new(SignerRunner::new(config.clone(), services)),
        Duration::from_millis(config.run_interval),
    );
    state_machine.run().await
}
