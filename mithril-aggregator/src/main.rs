#![doc = include_str!("../README.md")]

use clap::Parser;
use slog::{Drain, Fuse, Level, Logger};
use slog_async::Async;
use std::sync::Arc;

use mithril_aggregator::{CommandType, MainOpts};
use mithril_common::StdResult;

fn build_io_logger<W: std::io::Write + Send + 'static>(log_level: Level, io: W) -> Fuse<Async> {
    let drain = slog_bunyan::with_name("mithril-aggregator", io)
        .set_pretty(false)
        .build()
        .fuse();
    let drain = slog::LevelFilter::new(drain, log_level).fuse();

    Async::new(drain).build().fuse()
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
    let root_logger = build_logger(&args);

    args.execute(root_logger).await
}
