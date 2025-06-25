use anyhow::{Context, anyhow};
use clap::Parser;
use mithril_common::StdResult;
use slog::Level;
use std::{
    path::{Path, PathBuf},
    time::Duration,
};
use tokio::time::Instant;

use crate::FullNode;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct MainOpts {
    /// Location of the Cardano CLI binary
    #[arg(short, long)]
    pub cardano_cli_path: PathBuf,

    /// Temporary location for logs, databases etc.
    #[arg(short, long)]
    pub temporary_path: Option<PathBuf>,

    /// Path of the Aggregator binary
    #[arg(short, long, default_value = "./target/release")]
    pub aggregator_dir: PathBuf,

    /// Number of concurrent signers
    #[arg(long, default_value = "20")]
    pub num_signers: usize,

    /// Number of concurrent clients
    #[arg(long, default_value = "0")]
    pub num_clients: usize,

    /// Mithril technical Era
    #[arg(long, default_value = "pythagoras")]
    pub mithril_era: String,

    /// Aggregator HTTP port
    #[arg(short = 'p', long, default_value = "8080")]
    server_port: u32,

    /// Log level
    #[arg(short = 'v', action = clap::ArgAction::Count)]
    verbose: u8,
}

impl MainOpts {
    /// get log level from parameters
    pub fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Error,
            1 => Level::Warning,
            2 => Level::Info,
            3 => Level::Debug,
            _ => Level::Trace,
        }
    }
}

#[derive(Debug)]
pub struct AggregatorParameters {
    pub server_port: u32,
    pub full_node: FullNode,
    pub cardano_cli_path: PathBuf,
    pub work_dir: PathBuf,
    pub bin_dir: PathBuf,
    pub mithril_era: String,
}

impl AggregatorParameters {
    pub fn new(opts: &MainOpts, db_path: &Path) -> StdResult<Self> {
        let full_node = FullNode {
            db_path: db_path.to_path_buf(),
            socket_path: PathBuf::new(),
        };
        let tmp_dir = opts
            .temporary_path
            .clone()
            .unwrap_or_else(|| std::env::temp_dir().join("load-aggregator"));

        if tmp_dir.exists() {
            std::fs::remove_dir_all(&tmp_dir).with_context(|| {
                format!(
                    "Could not remove existing temp directory '{}'.",
                    tmp_dir.display()
                )
            })?;
        }
        std::fs::create_dir_all(&tmp_dir)
            .with_context(|| format!("Could not create temp directory '{}'.", tmp_dir.display()))?;

        let tmp_dir = tmp_dir.canonicalize().unwrap();

        let cardano_cli_path = {
            if !opts.cardano_cli_path.exists() {
                Err(anyhow!(
                    "Given cardano-cli path does not exist: '{}'",
                    opts.cardano_cli_path.display()
                ))?
            }

            opts.cardano_cli_path.canonicalize().with_context(|| {
                format!(
                    "Could not canonicalize path to the cardano-cli, path: '{}'",
                    opts.cardano_cli_path.display()
                )
            })?
        };

        let aggregator_parameters = AggregatorParameters {
            full_node,
            bin_dir: opts.aggregator_dir.clone(),
            cardano_cli_path,
            server_port: opts.server_port,
            work_dir: tmp_dir,
            mithril_era: opts.mithril_era.clone(),
        };

        Ok(aggregator_parameters)
    }

    pub fn mock_stake_distribution_file_path(&self) -> PathBuf {
        self.work_dir.join("stake_distribution.json")
    }

    pub fn mock_epoch_file_path(&self) -> PathBuf {
        self.work_dir.join("epoch.txt")
    }
}

pub struct Timing {
    phase: String,
    duration: Duration,
}

pub struct Reporter {
    number_of_signers: usize,
    number_of_clients: usize,
    timings: Vec<Timing>,
    current_timing: Option<(String, Instant)>,
}

impl Reporter {
    pub fn new(number_of_signers: usize, number_of_clients: usize) -> Self {
        Self {
            number_of_signers,
            number_of_clients,
            timings: vec![],
            current_timing: None,
        }
    }

    pub fn start(&mut self, phase: &str) {
        self.current_timing = Some((phase.to_owned(), Instant::now()));
    }

    pub fn stop(&mut self) {
        if let Some((phase, instant)) = &self.current_timing {
            let phase = if self.number_of_clients == 0 {
                format!("{phase} - without clients")
            } else {
                format!("{phase} - with clients")
            };
            let timing = Timing {
                phase: phase.clone(),
                duration: instant.elapsed(),
            };

            self.timings.push(timing);
            self.current_timing = None;
        }
    }

    pub fn print_report(&self) {
        println!("signers\tclients\tphase\tduration/ms");
        for t in &self.timings {
            println!(
                "{}\t{}\t{}\t{}",
                self.number_of_signers,
                self.number_of_clients,
                t.phase,
                t.duration.as_millis()
            );
        }
    }
}
