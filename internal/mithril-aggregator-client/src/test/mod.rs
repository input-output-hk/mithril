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

#[cfg(test)]
macro_rules! assert_error_matches {
    ($error:expr, $error_type:pat) => {
        assert!(
            matches!($error, $error_type),
            "Expected {} error, got '{:?}'.",
            stringify!($error_type),
            $error
        );
    };
}
#[cfg(test)]
pub(crate) use assert_error_matches;
