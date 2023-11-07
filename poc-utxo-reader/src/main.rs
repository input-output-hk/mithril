use anyhow::anyhow;
use clap::Parser;
use poc_utxo_reader::{errors::*, immutable_parser::*, ledger::Ledger};
use rayon::prelude::*;
use sqlite::Connection;
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

/// Configuration parameters
#[derive(Parser, Debug, PartialEq, Clone)]
pub struct Config {
    /// Command to run
    #[clap(long)]
    command: String,

    /// Data directory where immutable files are located
    #[clap(long)]
    data_directory: Option<String>,

    /// Database location
    #[clap(long, default_value = ":memory:")]
    db_path: String,

    /// Min immutable files scanned
    #[clap(long)]
    min_immutable_files: Option<usize>,

    /// Max immutable files scanned
    #[clap(long)]
    max_immutable_files: Option<usize>,

    /// With parallelization
    #[clap(long, default_value = "false")]
    with_parallelization: bool,

    /// Address to lookup records from
    #[clap(long)]
    address: Option<String>,

    /// Block number
    #[clap(long)]
    block_number: Option<u64>,
}

fn main() -> StdResult<()> {
    let config = Config::parse();

    match config.command.as_str() {
        "import" => run_import_command(&config)?,
        "query" => run_query_command(&config)?,
        _ => unimplemented!(),
    }

    Ok(())
}

/// Run import command
fn run_import_command(config: &Config) -> StdResult<()> {
    let db_path = config.db_path.to_owned();
    let connection = Connection::open_with_full_mutex(db_path)?;
    let ledger = Ledger::new(connection)?;

    let immutable_directory_path = config
        .data_directory
        .to_owned()
        .ok_or_else(|| anyhow!("Missing 'data_directory' configuration parameter"))?;
    eprintln!(">> Scanning {immutable_directory_path:?} immutable files...");

    let min_immutable_files = config.min_immutable_files.unwrap_or(0);
    let max_immutable_files = config.max_immutable_files.unwrap_or(usize::MAX);
    let immutable_chunk_file_paths: Vec<_> =
        list_immutable_files(Path::new(&immutable_directory_path))?
            .into_iter()
            .skip(min_immutable_files)
            .take(max_immutable_files)
            .collect();

    if config.with_parallelization {
        compute_ledger_parallel(&ledger, &immutable_chunk_file_paths)?
    } else {
        compute_ledger_sequential(&ledger, &immutable_chunk_file_paths)?
    };

    Ok(())
}

/// Run query command
fn run_query_command(config: &Config) -> StdResult<()> {
    let db_path = config.db_path.to_owned();
    let connection = Connection::open_with_full_mutex(db_path)?;
    let ledger = Ledger::new(connection)?;

    let block_number = config.block_number.unwrap_or(u64::MAX);
    if let Some(address) = config.address.to_owned() {
        println!(
            "{}",
            serde_json::to_string(&ledger.get_utxos_for_address(&address, &block_number)?)?
        );
    } else {
        println!(
            "{}",
            serde_json::to_string(&ledger.get_utxos_for_all_addresses(&block_number)?)?
        );
    }

    Ok(())
}

/// Compute ledger with sequential parsing of blocks from immutable files (slower, less memory usage)
fn compute_ledger_sequential(
    ledger: &Ledger,
    immutable_chunk_file_paths: &[PathBuf],
) -> StdResult<()> {
    for immutable_chunk_file_path in immutable_chunk_file_paths {
        eprintln!(
            ">> Compute addresses transaction history for blocks from {immutable_chunk_file_path:?}",
        );
        let blocks = read_blocks_from_immutable_file(immutable_chunk_file_path).unwrap();

        verify_blocks(blocks.iter().collect())?;

        ledger.save_blocks(&blocks)?;
    }

    Ok(())
}

/// Compute ledger with parallelized parsing of blocks from immutable files (faster, more memory usage)
fn compute_ledger_parallel(
    ledger: &Ledger,
    immutable_chunk_file_paths: &[PathBuf],
) -> StdResult<()> {
    let blocks_by_immutable_file: BTreeMap<_, _> = immutable_chunk_file_paths
        .par_iter()
        .map(|immutable_chunk_file_path| {
            (
                immutable_chunk_file_path,
                read_blocks_from_immutable_file(immutable_chunk_file_path).unwrap(),
            )
        })
        .collect();

    verify_blocks(
        blocks_by_immutable_file
            .iter()
            .flat_map(|blocks| blocks.1)
            .collect(),
    )?;

    for (immutable_file_path, blocks) in blocks_by_immutable_file {
        eprintln!(
            ">> Compute addresses transaction history for blocks from {immutable_file_path:?}",
        );
        ledger.save_blocks(&blocks)?;
    }

    Ok(())
}
