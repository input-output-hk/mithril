#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(dead_code)]
use std::{cmp::min, fs, path::Path};

use pallas::{
    codec::minicbor::{decode::Tokenizer, Decoder},
    ledger::traverse::{
        probe::{block_era, Outcome},
        MultiEraBlock,
    },
};

fn main() {
    let immutable_directory = "./db/mainnet/immutable";

    let entries = fs::read_dir(immutable_directory).unwrap();
    let mut immutable_chunk_file_paths = Vec::new();
    for entry in entries {
        let path = entry.unwrap().path();
        let extension = path.extension().unwrap();
        if extension.to_str().unwrap() == "chunk" {
            immutable_chunk_file_paths.push(path.to_string_lossy().to_string());
        }
    }
    immutable_chunk_file_paths.sort();

    for immutable_chunk_file_path in immutable_chunk_file_paths {
        read_immutable_file(Path::new(&immutable_chunk_file_path));
    }
}

fn read_immutable_file(file_path: &Path) {
    let cbor = fs::read(file_path).expect("Should have been able to read the file");
    //let cbor = hex::decode(include_str!("blocks/test.block")).expect("invalid hex");

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

    let mut last_start_byte_index = 0;
    for block_start_index in blocks_start_byte_index.into_iter().skip(1) {
        let maybe_end_byte_index = min(block_start_index, cbor.len());
        if let Ok(block) = MultiEraBlock::decode(&cbor[last_start_byte_index..maybe_end_byte_index])
        {
            println!(
                ">> {file_path:?}: Block: #{}: Txs: {} Slot: {} Hash: {} Bytes[{last_start_byte_index}..{maybe_end_byte_index}]",
                block.number(),
                block.tx_count(),
                block.slot(),
                block.hash(),
            );
            /* for tx in &block.txs() {
                println!(">>>> Transaction: {tx:#?}");
            } */
            last_start_byte_index = block_start_index;
        }
    }
}
