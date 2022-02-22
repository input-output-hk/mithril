mod message;
mod network;
mod node_impl;

use clap::Parser;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::path::Path;

#[derive(Serialize, Deserialize)]
struct Config {
    my_address: String,
    peers: Vec<String>,
    stake: u64,
}

#[derive(Parser)]
struct Args {
    #[clap(long)]
    config_file: String,
}

fn main() {
    let args: Args = Args::parse();
    // process config

    let config: Config = {
        let path = Path::new(&args.config_file);
        let cf = File::open(path);
        let file = match cf {
            Err(e) => panic!("Couldn't open config file: {}", e),
            Ok(f) => f,
        };

        match serde_json::from_reader(file) {
            Err(e) => panic!("Couldn't parse config file: {}", e),
            Ok(c) => c,
        }
    };

    todo!("Not implemented!");
}
