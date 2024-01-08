#![doc = include_str!("../README.md")]

use clap::Parser;
use mithril_common::StdResult;
use mithril_relay::Args;


#[tokio::main]
async fn main() -> StdResult<()> {
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(args.build_logger());

    args.execute().await
}
