use crate::certificate_handler::CertificateHandlerHTTPClient;
use crate::entities::Config;
use crate::signer::Signer;
use crate::single_signer::{key_decode_hex, MithrilSingleSigner};
use clap::Parser;
use mithril_aggregator::fake_data;
use slog::{o, Drain, Level, Logger};
use slog_scope::{debug, error, info};
use std::env;
use std::sync::Arc;
use std::time::Duration;
use tokio::time::sleep;

mod certificate_handler;
mod entities;
mod signer;
mod single_signer;

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
async fn main() {
    // Load args
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(build_logger(args.log_level()));

    // Load config
    let run_mode = env::var("RUN_MODE").unwrap_or(args.run_mode);
    let config: Config = config::Config::builder()
        .add_source(config::File::with_name(&format!("./config/{}.json", run_mode)).required(false))
        .add_source(config::Environment::default())
        .build()
        .unwrap()
        .try_deserialize()
        .unwrap();
    debug!("Started"; "run_mode" => &run_mode, "config" => format!("{:?}", config));

    loop {
        let fake_signer = fake_data::signer_keys(config.party_id).unwrap();
        let single_signer = MithrilSingleSigner::new(
            fake_signer.party_id,
            key_decode_hex(&fake_signer.secret_key).unwrap(),
        );
        let certificate_handler =
            CertificateHandlerHTTPClient::new(config.aggregator_endpoint.clone());

        let mut signer = Signer::new(Box::new(certificate_handler), Box::new(single_signer));
        if let Err(e) = signer.run().await {
            error!("{:?}", e)
        }
        info!("Sleeping for {}", config.run_interval);
        sleep(Duration::from_millis(config.run_interval)).await;
    }
}
