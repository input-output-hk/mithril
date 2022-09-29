#![doc = include_str!("../README.md")]

use clap::Parser;
use mithril_aggregator::MainOpts;
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    // Load args
    let args = MainOpts::parse();
    let result = args.execute().await;

    if result.is_err() {
        eprintln!("ERROR: application ends abnormaly.");
    }

    result
}
