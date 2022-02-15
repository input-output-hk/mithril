mod message;
mod network;
mod node_impl;

use serde::{Serialize, Deserialize};
use serde_json as json;
use std::path::Path;
use std::io::prelude::*;
use std::fs::File;
use clap::Parser;

#[derive(Serialize, Deserialize)]
struct Config {
    my_address: String,
    peers: Vec<String>,
    stake: u64,
}

#[derive(Parser)]
struct Args {
    #[clap(long)]
    configFile : String,
}

fn main() {
    let args: Args = Args::parse();
        // process config

    let config: Config =
        {
            let path = Path::new(&args.configFile);
            let cf = File::open(path);
            let file =
                match cf {
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