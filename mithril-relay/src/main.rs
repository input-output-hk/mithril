#![doc = include_str!("../README.md")]

use std::sync::Arc;

use clap::Parser;
use mithril_common::StdResult;
use mithril_relay::Args;
use slog::{Drain, Level, Logger};

pub fn build_logger(min_level: Level) -> Logger {
    let drain = slog_bunyan::with_name("mithril-relay", std::io::stderr())
        .set_pretty(false)
        .build()
        .fuse();
    let drain = slog::LevelFilter::new(drain, min_level).fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), slog::o!())
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let args = Args::parse();
    let logger = build_logger(args.log_level());

    args.execute(logger).await
}
