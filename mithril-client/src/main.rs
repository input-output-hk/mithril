#![doc = include_str!("../README.md")]

use clap::{Parser, Subcommand};
use config::builder::DefaultState;
use config::{ConfigBuilder, Map, Source, Value, ValueKind};
use slog::{Drain, Level, Logger};
use slog_scope::debug;
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;

use mithril_client::commands::{DownloadCommand, ListCommand};

/// CLI args
#[derive(Parser, Debug, Clone)]
#[clap(name = "mithril-client")]
#[clap(about = "An implementation of a Mithril Client", long_about = None)]
pub struct Args {
    /// Available commands
    #[clap(subcommand)]
    command: Commands,

    /// Run Mode
    #[clap(long, env = "RUN_MODE", default_value = "dev")]
    run_mode: String,

    /// Verbosity level
    #[clap(short, long, parse(from_occurrences))]
    verbose: usize,

    /// Directory where configuration file is located
    #[clap(long, default_value = "./config")]
    pub config_directory: PathBuf,

    /// Is there an endpoint specified on the CLI?
    #[clap(long)]
    aggregator_endpoint: Option<String>,
}

impl Args {
    pub async fn execute(&self) -> Result<(), Box<dyn Error>> {
        debug!("Run Mode: {}", self.run_mode);
        let config: ConfigBuilder<DefaultState> = config::Config::builder()
            .add_source(
                config::File::with_name(&format!(
                    "{}/{}.json",
                    self.config_directory.display(),
                    self.run_mode
                ))
                .required(false),
            )
            .add_source(config::Environment::default())
            .add_source(self.clone());

        self.command.execute(config).await
    }

    fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Warning,
            1 => Level::Info,
            2 => Level::Debug,
            _ => Level::Trace,
        }
    }

    fn build_logger(&self) -> Logger {
        let decorator = slog_term::TermDecorator::new().build();
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog::LevelFilter::new(drain, self.log_level()).fuse();
        let drain = slog_async::Async::new(drain).build().fuse();

        Logger::root(Arc::new(drain), slog::o!())
    }
}

impl Source for Args {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut map = Map::new();
        let namespace = "clap arguments".to_string();

        if let Some(aggregator_endpoint) = self.aggregator_endpoint.clone() {
            map.insert(
                "aggregator_endpoint".to_string(),
                Value::new(Some(&namespace), ValueKind::from(aggregator_endpoint)),
            );
        }

        Ok(map)
    }
}

/// CLI command list
#[derive(Subcommand, Debug, Clone)]
enum Commands {
    /// List available snapshots
    #[clap(arg_required_else_help = false)]
    List(ListCommand),

    /// Infos about a snapshot
    #[clap(arg_required_else_help = false)]
    Show {
        /// Snapshot digest
        #[clap(required = true)]
        digest: String,

        /// JSON output mode
        #[clap(long)]
        json: bool,
    },

    /// Download a snapshot
    #[clap(arg_required_else_help = true)]
    Download(DownloadCommand),

    /// Restore a snapshot
    #[clap(arg_required_else_help = true)]
    Restore {
        /// Snapshot digest
        #[clap(required = true)]
        digest: String,
    },
}

impl Commands {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Download(cmd) => cmd.execute(config_builder).await,
            _ => todo!(),
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), String> {
    // Load args
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(args.build_logger());
    let result = args.execute().await;

    result.map_err(|e| format!("An error occured: {:?}", e))
}

/*
// Init dependencies
let aggregator_handler = Arc::new(AggregatorHTTPClient::new(
    config.network.clone(),
    config.aggregator_endpoint.clone(),
));
let certificate_verifier = Box::new(MithrilCertificateVerifier::new(slog_scope::logger()));
let genesis_verification_key = key_decode_hex(&config.genesis_verification_key)?;
let genesis_verifier = ProtocolGenesisVerifier::from_verification_key(genesis_verification_key);

// Init runtime
let mut runtime = Runtime::new(config.network.clone());

// Execute commands
    match &args.command {
        Commands::List { json } => match runtime.list_snapshots().await {
            Ok(snapshot_list_items) => {
                if *json {
                    println!("{}", serde_json::to_string(&snapshot_list_items).unwrap());
                } else {
                    print_stdout(snapshot_list_items.with_title()).unwrap();
                }
                Ok(())
            }
            Err(err) => pretty_print_error(err),
        },
        Commands::Show { digest, json } => match runtime.show_snapshot(digest).await {
            Ok(snapshot_field_items) => {
                if *json {
                    println!("{}", serde_json::to_string(&snapshot_field_items).unwrap());
                } else {
                    print_stdout(
                        convert_to_field_items(&snapshot_field_items, config.network.clone())
                            .with_title(),
                    )
                    .unwrap();
                }
                Ok(())
            }
            Err(err) => pretty_print_error(err),
        },
        Commands::Download {
            digest,
            location_index,
        } => match runtime.download_snapshot(digest, *location_index).await {
            Ok((from, to)) => {
                println!(
                    "Download success {} #{}\nfrom {}\nto {}",
                    digest, location_index, from, to
                );
                Ok(())
            }
            Err(err) => pretty_print_error(err),
        },
        Commands::Restore { digest } => match runtime.restore_snapshot(digest).await {
            Ok(to) => {
                println!(
                    r###"Unpack success {}
to {}

Restore a Cardano Node with:

docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="{}",target=/data/db/ -e NETWORK={} inputoutput/cardano-node

"###,
                    digest,
                    to,
                    to,
                    config.network.clone()
                );
                Ok(())
            }
            Err(err) => pretty_print_error(err),
        },
    }
 */
