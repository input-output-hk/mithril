use crate::{entities::*, errors::*};
use anyhow::anyhow;
use pallas::ledger::traverse::{
    probe::{block_era, Outcome},
    MultiEraBlock,
};
use std::{
    cmp::min,
    fs,
    path::{Path, PathBuf},
};

/// Read blocks from immutable file
pub fn read_blocks_from_immutable_file(file_path: &Path) -> StdResult<Vec<Block>> {
    let cbor = fs::read(file_path).expect("Should have been able to read the file");

    let mut blocks_start_byte_index: Vec<_> = (0..cbor.len())
        .filter(|&byte_index| {
            let cbor_header_maybe = &cbor[byte_index..min(byte_index + 2, cbor.len())];
            match block_era(cbor_header_maybe) {
                Outcome::Matched(_) | Outcome::EpochBoundary => true,
                Outcome::Inconclusive => false,
            }
        })
        .collect();
    blocks_start_byte_index.push(cbor.len() + 1);

    let mut blocks = Vec::new();
    let mut last_start_byte_index = 0;
    for block_start_index in blocks_start_byte_index.into_iter().skip(1) {
        let maybe_end_byte_index = min(block_start_index, cbor.len());
        if let Ok(multi_era_block) =
            MultiEraBlock::decode(&cbor[last_start_byte_index..maybe_end_byte_index])
        {
            blocks.push(multi_era_block.try_into()?);
            last_start_byte_index = block_start_index;
        }
    }

    eprintln!(
        ">>>> Found {} blocks in {:?} immutable file",
        blocks.len(),
        file_path
    );

    Ok(blocks)
}

/// Verify blocks integrity
// TODO: enhance verification
pub fn verify_blocks(blocks: Vec<&Block>) -> StdResult<()> {
    let total_blocks = blocks.len() as u64;
    if total_blocks == 0 {
        return Ok(());
    }

    let (min_block_number, max_block_number, total_transactions) =
        blocks.iter().fold((u64::MAX, 0, 0), |acc, block| {
            (
                std::cmp::min(acc.0, block.number),
                std::cmp::max(acc.1, block.number),
                acc.2 + block.transactions.len(),
            )
        });
    let gap_blocks = total_blocks - 1 - (max_block_number - min_block_number);
    if gap_blocks != 0 {
        return Err(anyhow!(
            "A gap of {gap_blocks} exists in the blocks retrieved!"
        ));
    }
    eprintln!(">>>> Checked {total_blocks} blocks in range [{min_block_number},{max_block_number}] and {total_transactions} transactions",);

    Ok(())
}

// List immutable files (chunks)
pub fn list_immutable_files(directory_path: &Path) -> StdResult<Vec<PathBuf>> {
    let entries = fs::read_dir(directory_path).unwrap();
    let mut immutable_chunk_file_paths = Vec::new();
    for entry in entries {
        let path = entry.unwrap().path();
        let extension = path.extension().unwrap();
        if extension.to_str().unwrap() == "chunk" {
            immutable_chunk_file_paths.push(path);
        }
    }
    immutable_chunk_file_paths.sort();
    immutable_chunk_file_paths.pop().unwrap(); // Keep only up to penultimate chunk

    Ok(immutable_chunk_file_paths)
}
