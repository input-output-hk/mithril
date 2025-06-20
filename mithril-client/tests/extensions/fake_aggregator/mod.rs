#[cfg(feature = "fs")]
mod cardano_db_v1;
#[cfg(feature = "fs")]
mod cardano_db_v2;
mod cardano_transaction;
mod certificate;
mod mithril_stake_distribution;

#[cfg(feature = "fs")]
#[allow(unused_imports)]
pub use cardano_db_v2::CardanoDatabaseSnapshotV2Fixture;

use std::sync::Arc;

use axum::{
    extract::{Request, State},
    middleware::{from_fn_with_state, Next},
    response::Response,
    Router,
};
use axum_test::TestServer;
use tokio::sync::Mutex;

use mithril_client::certificate_client::CertificateVerifier;

use crate::extensions::mock;

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
    test_server: TestServer,
}

impl FakeAggregator {
    fn spawn_test_server_on_random_port(router: Router) -> Self {
        let calls = Arc::new(Mutex::new(Vec::new()));
        let test_server = TestServer::builder()
            .http_transport()
            .build(router.route_layer(from_fn_with_state(calls.clone(), log_route_call)))
            .unwrap();

        Self { calls, test_server }
    }

    pub fn test_server(&self) -> &TestServer {
        &self.test_server
    }

    pub fn server_root_url(&self) -> String {
        self.test_server.server_address().unwrap().to_string()
    }

    pub fn server_url(&self, path: &str) -> String {
        self.test_server.server_url(path).unwrap().to_string()
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
}

async fn log_route_call(
    State(calls): State<FakeAggregatorCalls>,
    request: Request,
    next: Next,
) -> Response {
    let called_path = if let Some(query) = request.uri().query() {
        format!("{}?{query}", request.uri().path())
    } else {
        request.uri().path().to_string()
    };
    calls.lock().await.push(called_path);
    next.run(request).await
}
