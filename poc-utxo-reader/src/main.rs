use anyhow::anyhow;
use clap::Parser;
use poc_utxo_reader::{
    entities::ImmutableFileNumber, errors::*, immutable_parser::*, ledger::Ledger,
};
use rayon::prelude::*;
use sqlite::Connection;
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

/// Configuration parameters
#[derive(Parser, Debug, PartialEq, Clone)]
#[clap(
    about = "This program imports transactions from immutable files and queries the associated UTxOs.",
    long_about = None
)]
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

    /// Min immutable file number imported
    #[clap(long)]
    min_immutable_file_number_import: Option<ImmutableFileNumber>,

    /// Max immutable file number imported
    #[clap(long)]
    max_immutable_file_number_import: Option<ImmutableFileNumber>,

    /// With parallelization
    #[clap(long, default_value = "false")]
    with_parallelization: bool,

    /// Address to lookup records from
    #[clap(long)]
    address: Option<String>,

    /// Immutable file number queried
    #[clap(long)]
    immutable_file_number_query: Option<ImmutableFileNumber>,
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
    //connection.execute("pragma foreign_keys=false")?;
    let ledger = Ledger::new(connection)?;

    let immutable_directory_path = config
        .data_directory
        .to_owned()
        .ok_or_else(|| anyhow!("Missing 'data_directory' configuration parameter"))?;
    eprintln!(">> Scanning {immutable_directory_path:?} immutable files...");

    let min_immutable_file_number_import = config.min_immutable_file_number_import.unwrap_or(0);
    let max_immutable_file_number_import = config
        .max_immutable_file_number_import
        .unwrap_or(usize::MAX);
    let immutable_chunk_file_paths: Vec<_> =
        list_immutable_files(Path::new(&immutable_directory_path))?
            .into_iter()
            .skip(min_immutable_file_number_import)
            .take(max_immutable_file_number_import)
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

    let max_immutable_file_number_query = (u32::MAX - 1) as usize;
    let immutable_file_number_query = config
        .immutable_file_number_query
        .unwrap_or(max_immutable_file_number_query);
    if let Some(address) = config.address.to_owned() {
        println!(
            "{}",
            serde_json::to_string(
                &ledger.get_utxos_for_address(&address, &immutable_file_number_query)?
            )?
        );
    } else {
        println!(
            "{}",
            serde_json::to_string(
                &ledger.get_utxos_for_all_addresses(&immutable_file_number_query)?
            )?
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
