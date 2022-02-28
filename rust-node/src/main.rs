mod message;
mod network;
mod node_impl;
mod wsvc;
mod config;
mod print_examples;

use clap::Parser;
use std::fs::File;
use std::path::Path;
use std::collections::HashMap;
use crate::config::Config;

#[derive(Parser)]
struct Args {
    /// Path to configuration file
    #[clap(long)]
    config_file: String,

    /// Party ID to run as
    #[clap(long)]
    node_id: message::PartyId,

    #[clap(long)]
    debug: bool,
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

    // TODO: check that nodes are not duplicated
    let me_mb =
        config.nodes.iter()
                    .find(|p| p.id == args.node_id);

    let me = match me_mb {
        None => panic!("Cannot run as node {} - id does not appear in config file!", args.node_id),
        Some(p) => p
    };


    let peers =
        config.nodes.iter()
                    .filter(|p| p.id != args.node_id);
    let peer_addr_map =
        peers.map(|p| (p.id, p.address_endpoint.clone())).collect();

    let start_res = wsvc::start_ws_network(&me.address_local, &peer_addr_map, me.id);
    let network = match start_res {
        Ok(n) => n,
        Err(e) => panic!("Couldn't start webservice and/or client thread: {}", e)
    };

    let stake_dist_iter =
        config.nodes.iter()
                    .map(|p| (p.id, p.stake));

    let stake_dist = HashMap::from_iter(stake_dist_iter);

    match node_impl::node_impl(&network, &config.parameters, &stake_dist) {
        Err(e) => panic!("node_impl returned error: {}", e),
        Ok(()) => (),
    }
}

