#![doc = include_str!("../README.md")]

use clap::Parser;
use mithril_aggregator::MainOpts;
use mithril_common::StdResult;
use slog::{Drain, Logger};
use std::sync::Arc;

/// Build a logger from args.
pub fn build_logger(args: &MainOpts) -> Logger {
    let drain = slog_bunyan::new(std::io::stdout())
        .set_pretty(false)
        .build()
        .fuse();
    let drain = slog::LevelFilter::new(drain, args.log_level()).fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), slog::o!())
}

#[tokio::main]
async fn main() -> StdResult<()> {
    // Load args
    let args = MainOpts::parse();
    let _guard = slog_scope::set_global_logger(build_logger(&args));

    #[cfg(feature = "bundle_openssl")]
    openssl_probe::init_ssl_cert_env_vars();

    args.execute().await
}
