use clap::Parser;
use poc_utxo_reader::{errors::*, immutable_parser::*, ledger::Ledger};
use rayon::prelude::*;
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

/// Configuration parameters
#[derive(Parser, Debug, PartialEq, Clone)]
pub struct Config {
    /// Data directory where immutable files are located
    #[clap(long)]
    data_directory: String,

    /// Max immutable files scanned
    #[clap(long)]
    max_immutable_files: Option<usize>,

    /// With parallelization
    #[clap(long)]
    with_parallelization: bool,

    /// Address to lookup records from
    #[clap(long)]
    address: Option<String>,
}

fn main() -> StdResult<()> {
    let config = Config::parse();

    let immutable_directory_path = config.data_directory;
    eprintln!(">> Scanning {immutable_directory_path:?} immutable files...");

    let max_immutable_files = config.max_immutable_files.unwrap_or(usize::MAX);
    let immutable_chunk_file_paths: Vec<_> =
        list_immutable_files(Path::new(&immutable_directory_path))?
            .into_iter()
            .take(max_immutable_files)
            .collect();

    let mut ledger = Ledger::default();
    if config.with_parallelization {
        compute_ledger_parallel(&mut ledger, &immutable_chunk_file_paths)?
    } else {
        compute_ledger_sequential(&mut ledger, &immutable_chunk_file_paths)?
    };

    if let Some(address) = config.address {
        println!(
            "{}",
            serde_json::to_string(&ledger.get_transactions_for_address(&address))?
        );
    } else {
        println!(
            "{}",
            serde_json::to_string(&ledger.get_transactions_for_all_addresses())?
        );
    }

    Ok(())
}

/// Compute ledger with sequential parsing of blocks from immutable files (slower, less memory usage)
fn compute_ledger_sequential(
    ledger: &mut Ledger,
    immutable_chunk_file_paths: &[PathBuf],
) -> StdResult<()> {
    for immutable_chunk_file_path in immutable_chunk_file_paths {
        eprintln!(
            ">> Compute addresses transaction history for blocks from {immutable_chunk_file_path:?}",
        );
        let blocks = read_blocks_from_immutable_file(immutable_chunk_file_path).unwrap();

        verify_blocks(blocks.iter().collect())?;

        let transactions: Vec<_> = blocks
            .into_iter()
            .flat_map(|block| block.transactions)
            .collect();
        ledger.save_transactions(&transactions)?;
    }

    Ok(())
}

/// Compute ledger with parallelized parsing of blocks from immutable files (faster, more memory usage)
fn compute_ledger_parallel(
    ledger: &mut Ledger,
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
        let transactions: Vec<_> = blocks
            .into_iter()
            .flat_map(|block| block.transactions)
            .collect();
        ledger.save_transactions(&transactions)?;
    }

    Ok(())
}
