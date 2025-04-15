#[cfg(feature = "fs")]
mod cardano_db_v1;
#[cfg(all(feature = "fs", feature = "unstable"))]
mod cardano_db_v2;
mod cardano_transaction;
mod certificate;
mod mithril_stake_distribution;

#[cfg(all(feature = "fs", feature = "unstable"))]
#[allow(unused_imports)]
pub use cardano_db_v2::CardanoDatabaseSnapshotV2Fixture;

use crate::extensions::mock;
use mithril_client::certificate_client::CertificateVerifier;
use std::convert::Infallible;
use std::sync::Arc;
use tokio::sync::Mutex;
use warp::filters::path::FullPath;

pub type FakeAggregatorCalls = Arc<Mutex<Vec<String>>>;

pub struct FakeCertificateVerifier;

impl FakeCertificateVerifier {
    pub fn build_that_validate_any_certificate() -> Arc<dyn CertificateVerifier> {
        let mut mock_verifier = mock::MockCertificateVerifierImpl::new();
        mock_verifier.expect_verify_chain().returning(|_| Ok(()));

        Arc::new(mock_verifier)
    }
}

pub struct FakeAggregator {
    calls: FakeAggregatorCalls,
}

impl FakeAggregator {
    pub fn new() -> Self {
        FakeAggregator {
            calls: Arc::new(Mutex::new(vec![])),
        }
    }

    async fn get_calls(&self) -> Vec<String> {
        let calls = self.calls.lock().await;

        calls.clone()
    }

    pub async fn get_last_call(&self) -> Option<String> {
        self.get_calls().await.last().cloned()
    }

    pub async fn get_latest_calls(&self, count: usize) -> Vec<String> {
        self.get_calls()
            .await
            .into_iter()
            .rev()
            .take(count)
            .collect()
    }

    pub async fn store_call_and_return_value(
        full_path: FullPath,
        calls: FakeAggregatorCalls,
        returned_value: String,
    ) -> Result<impl warp::Reply, Infallible> {
        let mut call_list = calls.lock().await;
        call_list.push(full_path.as_str().to_string());

        Ok(returned_value)
    }

    pub async fn store_call_with_query_and_return_value(
        full_path: FullPath,
        query: String,
        calls: FakeAggregatorCalls,
        returned_value: String,
    ) -> Result<impl warp::Reply, Infallible> {
        let mut call_list = calls.lock().await;
        call_list.push(format!("{}?{}", full_path.as_str(), query));

        Ok(returned_value)
    }
}
