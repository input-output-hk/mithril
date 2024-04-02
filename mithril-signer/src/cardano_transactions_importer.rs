use async_trait::async_trait;
use mithril_common::cardano_transaction_parser::TransactionParser;
use mithril_common::entities::{CardanoDbBeacon, CardanoTransaction};
use mithril_common::signable_builder::{TransactionStore, TransactionsImporter};
use mithril_common::StdResult;
use slog::{debug, Logger};
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Import and store [CardanoTransaction].
pub struct CardanoTransactionsImporter {
    transaction_parser: Arc<dyn TransactionParser>,
    transaction_store: Arc<dyn TransactionStore>,
    logger: Logger,
    dirpath: PathBuf,
}

impl CardanoTransactionsImporter {
    /// Constructor
    pub fn new(
        transaction_parser: Arc<dyn TransactionParser>,
        transaction_store: Arc<dyn TransactionStore>,
        dirpath: &Path,
        logger: Logger,
    ) -> Self {
        Self {
            transaction_parser,
            transaction_store,
            logger,
            dirpath: dirpath.to_owned(),
        }
    }
}

#[async_trait]
impl TransactionsImporter for CardanoTransactionsImporter {
    async fn import(&self, beacon: &CardanoDbBeacon) -> StdResult<Vec<CardanoTransaction>> {
        let transactions = self.transaction_parser.parse(&self.dirpath, beacon).await?;
        debug!(
            self.logger,
            "Retrieved {} Cardano transactions at beacon: {beacon}",
            transactions.len()
        );

        let transaction_chunk_size = 100;
        for transactions_in_chunk in transactions.chunks(transaction_chunk_size) {
            self.transaction_store
                .store_transactions(transactions_in_chunk)
                .await?;
        }

        Ok(transactions)
    }
}
