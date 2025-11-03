#[cfg(test)]
use httpmock::MockServer;

#[cfg(test)]
use crate::AggregatorHttpClient;

#[cfg(test)]
mithril_common::define_test_logger!();

#[cfg(test)]
pub(crate) fn setup_server_and_client() -> (MockServer, AggregatorHttpClient) {
    let server = MockServer::start();
    let client = AggregatorHttpClient::builder(server.base_url())
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

/// Define a test that checks that http compression is enabled for the client.
///
/// Requires `httpmock` in dev-dependencies
#[macro_export]
macro_rules! test_http_compression_is_enabled {
    () => {
        #[tokio::test]
        async fn test_http_compression_is_enabled_and_send_accept_encoding_header_with_correct_values() {
            let server = httpmock::MockServer::start();
            server.mock(|when, then| {
                when.is_true(|req| {
                    let headers = req.headers();
                    let accept_encoding_header = headers
                        .get("accept-encoding")
                        .expect("Accept-Encoding header not found");

                    ["gzip", "br", "deflate", "zstd"].iter().all(|&encoding| {
                        accept_encoding_header.to_str().is_ok_and(|h| h.contains(encoding))
                    })
                });

                then.status(200).body("[]");
            });

            let client = $crate::AggregatorHttpClient::builder(server.base_url()).build().unwrap();
            client
                .send($crate::query::GetCertificatesListQuery::latest())
                .await
                .expect("GET request should succeed with Accept-Encoding header");
        }
    };
}
