use std::sync::Arc;

use mithril_cardano_node_chain::chain_importer::ChainDataImporter;
use mithril_common::StdResult;
use mithril_common::entities::BlockNumber;
use mithril_common::signable_builder::TransactionsImporter;

/// Services responsible for importing chain data into the signer data stores.
pub struct SignerChainDataImporter {
    inner: Arc<dyn ChainDataImporter>,
}

impl SignerChainDataImporter {
    /// Instantiate a new instance of `SignerChainDataImporter`.
    pub fn new(inner: Arc<dyn ChainDataImporter>) -> Self {
        Self { inner }
    }
}

#[async_trait::async_trait]
impl TransactionsImporter for SignerChainDataImporter {
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        self.inner.import(up_to_beacon).await
    }
}
