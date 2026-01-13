mod api;
mod blockfrost_retriever;
mod signer_store_persister;

pub use api::*;
pub use blockfrost_retriever::*;

#[cfg(test)]
mod test_tools {
    use std::collections::BTreeSet;

    use mithril_common::StdResult;

    use crate::database::repository::{SignerGetter, SignerStore};

    #[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
    pub(crate) struct TestSigner {
        pool_id: String,
        ticker: Option<String>,
    }

    impl TestSigner {
        pub fn with_ticker(pool_id: &str, ticker: &str) -> Self {
            Self {
                pool_id: pool_id.to_string(),
                ticker: Some(ticker.to_string()),
            }
        }

        pub fn without_ticker(pool_id: &str) -> Self {
            Self {
                pool_id: pool_id.to_string(),
                ticker: None,
            }
        }
    }

    #[async_trait::async_trait]
    pub(crate) trait TestSignerStoreExtensions {
        async fn fill_with_test_signers(&self, test_signers: &[TestSigner]) -> StdResult<()>;
        async fn get_all_test_signers(&self) -> StdResult<BTreeSet<TestSigner>>;
    }

    #[async_trait::async_trait]
    impl TestSignerStoreExtensions for SignerStore {
        async fn fill_with_test_signers(&self, test_signers: &[TestSigner]) -> StdResult<()> {
            for signer in test_signers {
                self.import_signer(signer.pool_id.clone(), signer.ticker.clone())
                    .await?;
            }

            Ok(())
        }

        async fn get_all_test_signers(&self) -> StdResult<BTreeSet<TestSigner>> {
            let signers = self
                .get_all()
                .await?
                .into_iter()
                .map(|s| TestSigner {
                    pool_id: s.signer_id,
                    ticker: s.pool_ticker,
                })
                .collect();

            Ok(signers)
        }
    }
}
