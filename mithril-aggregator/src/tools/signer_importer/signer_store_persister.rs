use async_trait::async_trait;
use std::collections::HashMap;

use mithril_common::StdResult;
use mithril_common::entities::PartyId;

use crate::database::repository::SignerStore;

use super::{PoolTicker, SignersImporterPersister};

#[async_trait]
impl SignersImporterPersister for SignerStore {
    async fn persist(&self, signers: HashMap<PartyId, Option<PoolTicker>>) -> StdResult<()> {
        self.import_many_signers(signers).await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;
    use std::sync::Arc;

    use crate::database::repository::SignerStore;
    use crate::database::test_helper::main_db_connection;
    use crate::test::TestLogger;
    use crate::tools::signer_importer::{MockSignersImporterRetriever, SignersImporter};

    use super::{super::test_tools::*, *};

    #[tokio::test]
    async fn persist_list_of_two_signers_one_with_ticker_the_other_without() {
        let connection = Arc::new(main_db_connection().unwrap());
        let store = Arc::new(SignerStore::new(connection.clone()));
        let mut retriever = MockSignersImporterRetriever::new();
        retriever.expect_retrieve().returning(|| {
            Ok(HashMap::from([
                ("pool1".to_string(), Some("[Pool name test]".to_string())),
                ("pool2".to_string(), None),
            ]))
        });

        let importer =
            SignersImporter::new(Arc::new(retriever), store.clone(), TestLogger::stdout());
        importer.run().await.expect("running importer should not fail");

        let result = store.get_all_test_signers().await.unwrap();
        assert_eq!(
            result,
            BTreeSet::from([
                TestSigner::with_ticker("pool1", "[Pool name test]",),
                TestSigner::without_ticker("pool2"),
            ])
        );
    }

    #[tokio::test]
    async fn persist_update_existing_data() {
        let connection = Arc::new(main_db_connection().unwrap());
        let store = Arc::new(SignerStore::new(connection.clone()));
        store
            .fill_with_test_signers(&[
                TestSigner::with_ticker("pool1", "[Pool name test]"),
                TestSigner::without_ticker("pool2"),
                TestSigner::with_ticker("pool3", "[Not updated]"),
                TestSigner::with_ticker("pool4", "[Ticker will be removed]"),
            ])
            .await
            .unwrap();
        let mut retriever = MockSignersImporterRetriever::new();
        retriever.expect_retrieve().returning(|| {
            Ok(HashMap::from([
                ("pool1".to_string(), Some("[Updated Pool name]".to_string())),
                ("pool2".to_string(), Some("[Added Pool name]".to_string())),
                ("pool3".to_string(), Some("[Not updated]".to_string())),
                ("pool4".to_string(), None),
                ("pool5".to_string(), Some("[New Pool]".to_string())),
            ]))
        });

        let importer =
            SignersImporter::new(Arc::new(retriever), store.clone(), TestLogger::stdout());
        importer.run().await.expect("running importer should not fail");

        let result = store.get_all_test_signers().await.unwrap();
        assert_eq!(
            result,
            BTreeSet::from([
                TestSigner::with_ticker("pool1", "[Updated Pool name]"),
                TestSigner::with_ticker("pool2", "[Added Pool name]"),
                TestSigner::with_ticker("pool3", "[Not updated]"),
                TestSigner::without_ticker("pool4"),
                TestSigner::with_ticker("pool5", "[New Pool]"),
            ])
        );
    }
}
