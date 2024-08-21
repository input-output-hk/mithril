mod directory_observer;

use clap::Parser;
use mithril_common::StdResult;
use std::path::PathBuf;

#[derive(Parser, Debug, Clone)]
pub struct Args {
    #[clap(long)]
    id: String,

    /// Path to the socket file to communicate with this node.
    ///
    /// Optional: if not set it will default to `{system_temp_folder}/signatures-network-node-{id}/node.socket`
    #[clap(long)]
    socket_path: Option<PathBuf>,
}

#[tokio::main]
async fn main() -> StdResult<()> {
    println!("Hello, world!");
    Ok(())
}
