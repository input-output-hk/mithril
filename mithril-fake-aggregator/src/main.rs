mod application;
mod error;
mod handlers;
mod shared_state;

use clap::Parser;
use tracing::{debug, error, Level};

use crate::application::Application;
use crate::error::*;

use std::path::PathBuf;

type StdResult<T> = anyhow::Result<T>;

/// Possible command line options and arguments
#[derive(Debug, Parser)]
#[command(version)]
pub struct CliArguments {
    /// Directory where the response files are located.
    #[arg(short, long)]
    data_directory: Option<PathBuf>,

    /// Verbosity level  (-v WARN, -vv INFO, -vvv DEBUG, etc)
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    /// TCP port to listen on
    #[arg(short = 'p', long, default_value_t = 80)]
    tcp_port: u16,

    /// IP Address to bind server to
    #[arg(short, long, default_value = "127.0.0.1")]
    ip_address: String,

    /// Quiet mode, no log will be emitted. Critical error messages will still pop on STDERR
    #[arg(short, long, default_value_t = false)]
    quiet: bool,
}

impl CliArguments {
    pub fn get_verbosity_level(&self) -> Option<Level> {
        if self.quiet {
            None
        } else {
            match self.verbose {
                0 => Some(Level::ERROR),
                1 => Some(Level::WARN),
                2 => Some(Level::INFO),
                3 => Some(Level::DEBUG),
                _ => Some(Level::TRACE),
            }
        }
    }
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let params = CliArguments::parse();

    if let Some(level) = params.get_verbosity_level() {
        tracing_subscriber::fmt().with_max_level(level).init();
    }

    let result = Application::run(params).await;

    match &result {
        Err(e) => error!("{e}"),
        Ok(_) => debug!("soft terminated"),
    };

    result
}
