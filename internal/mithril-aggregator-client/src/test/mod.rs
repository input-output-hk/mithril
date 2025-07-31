use httpmock::MockServer;

use crate::AggregatorClient;

#[cfg(test)]
mithril_common::define_test_logger!();

#[cfg(test)]
pub(crate) fn setup_server_and_client() -> (MockServer, AggregatorClient) {
    let server = MockServer::start();
    let client = AggregatorClient::builder(server.base_url())
        .with_logger(TestLogger::stdout())
        .build()
        .unwrap();

    (server, client)
}
