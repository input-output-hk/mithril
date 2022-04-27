use crate::entities::Config;
use clap::Parser;
use clap_verbosity_flag::{InfoLevel, Verbosity};
use log::debug;
use std::env;

mod entities;

/// CLI args
#[derive(Parser)]
#[clap(name = "mithril-signer")]
#[clap(about = "An implementation of a Mithril Signer", long_about = None)]
pub struct Args {
    /// Run Mode
    #[clap(short, long, default_value = "dev")]
    run_mode: String,

    /// Verbosity level
    #[clap(flatten)]
    verbose: Verbosity<InfoLevel>,
}

fn main() {
    // Load args
    let args = Args::parse();

    // Init logger
    env_logger::Builder::new()
        .target(env_logger::Target::Stdout)
        .filter_level(args.verbose.log_level_filter())
        .init();

    // Load config
    let run_mode = env::var("RUN_MODE").unwrap_or(args.run_mode);
    debug!("Run Mode: {}", run_mode);
    let config: Config = config::Config::builder()
        .add_source(config::File::with_name(&format!("./config/{}.json", run_mode)).required(false))
        .add_source(config::Environment::default())
        .build()
        .unwrap()
        .try_deserialize()
        .unwrap();
    debug!("{:?}", config);

    println!("Hello, world!");
}
