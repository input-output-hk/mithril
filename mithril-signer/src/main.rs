use clap::Parser;
use slog::{o, Drain, Level, Logger};
use slog_scope::debug;
use std::env;
use std::sync::Arc;

use mithril_signer::{CertificateHandlerHTTPClient, Config, MithrilSingleSigner, Runtime};

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
async fn main() -> Result<(), String> {
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

    // TODO: Use serialized ProtocolInitializer here, loaded e.g. from filesystem
    let protocol_initializer_encoded = "";
    let single_signer = MithrilSingleSigner::new(config.party_id, protocol_initializer_encoded);
    let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint.clone());

    // Should the runtime loop returns an error ? If yes should we abort the loop at the first error or is their some tolerance ?
    let mut runtime = Runtime::new(Box::new(certificate_handler), Box::new(single_signer));
    runtime.infinite_loop(config.run_interval).await;

    Ok(())
}
