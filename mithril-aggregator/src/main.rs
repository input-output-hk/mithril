#![doc = include_str!("../README.md")]

use clap::Parser;
use mithril_aggregator::{CommandType, MainOpts};
use mithril_common::StdResult;
use slog::{Drain, Fuse, Level, Logger};
use slog_async::Async;
use std::sync::Arc;

fn build_io_logger<W: std::io::Write + Send + 'static>(log_level: Level, io: W) -> Fuse<Async> {
    let drain = slog_bunyan::new(io).set_pretty(false).build().fuse();
    let drain = slog::LevelFilter::new(drain, log_level).fuse();

    slog_async::Async::new(drain).build().fuse()
}

/// Build a logger from args.
pub fn build_logger(args: &MainOpts) -> Logger {
    let drain = match args.command.command_type() {
        CommandType::Server => build_io_logger(args.log_level(), std::io::stdout()),
        CommandType::CommandLine => build_io_logger(args.log_level(), std::io::stderr()),
    };

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
