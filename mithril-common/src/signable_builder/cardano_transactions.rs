use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use async_trait::async_trait;
use slog::{debug, Logger};

use crate::{
    cardano_transaction_parser::TransactionParser,
    entities::{Beacon, CardanoTransaction, ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::SignableBuilder,
    StdResult,
};

#[cfg(test)]
use mockall::automock;

/// Cardano transactions store
#[cfg_attr(test, automock)]
#[async_trait]
pub trait TransactionStore: Send + Sync {
    /// Store list of transactions
    async fn store_transactions(&self, transactions: &[CardanoTransaction]) -> StdResult<()>;
}

/// A [CardanoTransactionsSignableBuilder] builder
pub struct CardanoTransactionsSignableBuilder {
    transaction_parser: Arc<dyn TransactionParser>,
    transaction_store: Arc<dyn TransactionStore>,
    logger: Logger,
    dirpath: PathBuf,
}

impl CardanoTransactionsSignableBuilder {
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
impl SignableBuilder<Beacon> for CardanoTransactionsSignableBuilder {
    // TODO: return a protocol message computed from the transactions when it's ready to be implemented
    async fn compute_protocol_message(&self, beacon: Beacon) -> StdResult<ProtocolMessage> {
        debug!(
            self.logger,
            "Compute protocol message for CardanoTransactions at beacon: {beacon}"
        );

        let transactions = self
            .transaction_parser
            .parse(&self.dirpath, &beacon)
            .await?;
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

        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            format!("{beacon}-{}", transactions.len()),
        );

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use crate::cardano_transaction_parser::DumbTransactionParser;
    use crate::signable_builder::MockTransactionStore;

    use super::*;
    use slog::Drain;

    fn create_logger() -> slog::Logger {
        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        slog::Logger::root(Arc::new(drain), slog::o!())
    }

    #[tokio::test]
    async fn test_compute_signable() {
        let beacon = Beacon::default();
        let transactions_count = 0;
        let transaction_parser = Arc::new(DumbTransactionParser::new(vec![]));
        let transaction_store = Arc::new(MockTransactionStore::new());
        let cardano_transactions_signable_builder = CardanoTransactionsSignableBuilder::new(
            transaction_parser,
            transaction_store,
            Path::new("/tmp"),
            create_logger(),
        );
        let signable = cardano_transactions_signable_builder
            .compute_protocol_message(beacon.clone())
            .await
            .unwrap();
        let mut signable_expected = ProtocolMessage::new();
        signable_expected.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            format!("{beacon}-{transactions_count}"),
        );
        assert_eq!(signable_expected, signable);
    }
}
