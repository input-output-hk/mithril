use poc_utxo_reader::{errors::*, immutable_parser::*, ledger::*};
use rayon::prelude::*;
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

fn main() -> StdResult<()> {
    let immutable_directory_path = "./db/preprod-500/immutable";
    eprintln!(">> Scanning {immutable_directory_path:?} immutable files...");

    let max_immutable_files = 10000;
    let immutable_chunk_file_paths: Vec<_> =
        list_immutable_files(Path::new(immutable_directory_path))?
            .into_iter()
            .take(max_immutable_files)
            .collect();

    let fast_compute = false;
    let ledger = if fast_compute {
        compute_ledger_parallel(&immutable_chunk_file_paths)?
    } else {
        compute_ledger_sequential(&immutable_chunk_file_paths)?
    };

    println!(
        "Ledger history: {:#?}",
        ledger.get_transactions_for_all_addresses_()
    );

    Ok(())
}

/// Compute ledger with sequential parsing of blocks from immutable files (slower, less memory usage)
fn compute_ledger_sequential(immutable_chunk_file_paths: &[PathBuf]) -> StdResult<Ledger> {
    let mut ledger = Ledger::default();
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

    Ok(ledger)
}

/// Compute ledger with parallelized parsing of blocks from immutable files (faster, nore memory usage)
fn compute_ledger_parallel(immutable_chunk_file_paths: &[PathBuf]) -> StdResult<Ledger> {
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

    let mut ledger = Ledger::default();
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

    Ok(ledger)
}
