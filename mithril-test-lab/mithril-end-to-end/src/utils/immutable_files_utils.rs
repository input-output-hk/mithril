use rand::seq::SliceRandom;
use std::path::Path;

use mithril_common::StdResult;
use pallas_hardano::storage::immutable::read_blocks;
use pallas_traverse::MultiEraBlock;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CardanoBlockTransactions {
    pub block_hash: String,
    pub transaction_hashes: Vec<String>,
}

pub fn retrieve_blocks_transactions_from_immutable_files(
    immutables_dir_path: &Path,
) -> StdResult<Vec<CardanoBlockTransactions>> {
    let chunk = read_blocks(immutables_dir_path)?;
    let mut block_transaction_list: Vec<CardanoBlockTransactions> = Vec::new();
    for block in chunk {
        let block = block?;
        let decoded_block = MultiEraBlock::decode(&block)?;

        block_transaction_list.push(CardanoBlockTransactions {
            block_hash: decoded_block.hash().to_string(),
            transaction_hashes: decoded_block
                .txs()
                .iter()
                .map(|tx| tx.hash().to_string())
                .collect(),
        });
    }
    Ok(block_transaction_list)
}

fn randomly_take<T: Clone>(items: &[T], count: usize) -> Vec<T> {
    let mut items = items.to_vec();
    items.shuffle(&mut rand::rng());
    items.into_iter().take(count).collect()
}

pub fn randomly_take_transactions_hashes(
    blocks_transactions: &[CardanoBlockTransactions],
    count: usize,
) -> Vec<String> {
    randomly_take(
        &blocks_transactions
            .iter()
            .flat_map(|bt| bt.transaction_hashes.clone())
            .collect::<Vec<_>>(),
        count,
    )
}

pub fn randomly_take_blocks_hashes(
    blocks_transactions: &[CardanoBlockTransactions],
    count: usize,
) -> Vec<String> {
    randomly_take(
        &blocks_transactions
            .iter()
            .map(|bt| bt.block_hash.clone())
            .collect::<Vec<_>>(),
        count,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_retrieve_blocks_transactions_from_immutable_files() {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let immutables_path = PathBuf::from(manifest_dir)
            .parent()
            .expect("Failed to get parent directory")
            .join("test_data/immutable");

        let blocks_and_transactions =
            retrieve_blocks_transactions_from_immutable_files(&immutables_path)
                .expect("Failed to retrieve blocks and transactions from immutable files");

        assert!(!blocks_and_transactions.is_empty());
        assert!(blocks_and_transactions.contains(&CardanoBlockTransactions {
            block_hash:
                "c75a3d5dbfcebd36bcbcdb2701ede41862764e856f81e8f1aafc5a960fd2283a".to_string(),
            transaction_hashes: vec![
                "ce2872d64240a25d7d43fb57efacfd1d9bd20073e91e1539bd31ef3ebc16555e".to_string()
            ],
        }));
    }

    #[test]
    fn test_randomly_take_transactions_hashes() {
        let blocks_transactions = vec![
            CardanoBlockTransactions {
                block_hash: "block1".to_string(),
                transaction_hashes: vec!["tx1".to_string(), "tx2".to_string()],
            },
            CardanoBlockTransactions {
                block_hash: "block2".to_string(),
                transaction_hashes: vec!["tx3".to_string(), "tx4".to_string()],
            },
        ];
        let transaction_hashes = randomly_take_transactions_hashes(&blocks_transactions, 3);
        assert_eq!(transaction_hashes.len(), 3);

        let all_contained = transaction_hashes.iter().all(|item| {
            blocks_transactions
                .iter()
                .any(|bt| bt.transaction_hashes.contains(item))
        });
        assert!(all_contained);
    }

    #[test]
    fn test_randomly_take_blocks_hashes() {
        let blocks_transactions = vec![
            CardanoBlockTransactions {
                block_hash: "block1".to_string(),
                transaction_hashes: vec![],
            },
            CardanoBlockTransactions {
                block_hash: "block2".to_string(),
                transaction_hashes: vec![],
            },
            CardanoBlockTransactions {
                block_hash: "block3".to_string(),
                transaction_hashes: vec![],
            },
            CardanoBlockTransactions {
                block_hash: "block4".to_string(),
                transaction_hashes: vec![],
            },
        ];
        let block_hashes = randomly_take_blocks_hashes(&blocks_transactions, 2);
        assert_eq!(block_hashes.len(), 2);

        let all_contained = block_hashes
            .iter()
            .all(|item| blocks_transactions.iter().any(|bt| bt.block_hash == *item));
        assert!(all_contained);
    }
}
