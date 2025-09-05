//! Define a HttpServer for test that can be configured using warp filters.

// Base code from the httpserver in reqwest tests:
// https://github.com/seanmonstar/reqwest/blob/master/tests/support/server.rs

use std::{net::SocketAddr, sync::mpsc as std_mpsc, thread};
use tokio::{runtime, sync::oneshot};
use warp::{Filter, Reply};

/// A HTTP server for test
pub struct TestHttpServer {
    address: SocketAddr,
    shutdown_tx: Option<oneshot::Sender<()>>,
}

impl TestHttpServer {
    /// Get the test server address
    pub fn address(&self) -> SocketAddr {
        self.address
    }

    /// Get the server url
    pub fn url(&self) -> String {
        format!("http://{}", self.address)
    }
}

impl Drop for TestHttpServer {
    fn drop(&mut self) {
        if let Some(tx) = self.shutdown_tx.take() {
            let _ = tx.send(());
        }
    }
}

/// Spawn a [TestHttpServer] using the given warp filters
pub fn test_http_server<F>(filters: F) -> TestHttpServer
where
    F: Filter + Clone + Send + Sync + 'static,
    F::Extract: Reply,
{
    test_http_server_with_socket_address(filters, ([127, 0, 0, 1], 0).into())
}

/// Spawn a [TestHttpServer] using the given warp filters
pub fn test_http_server_with_socket_address<F>(
    filters: F,
    socket_addr: SocketAddr,
) -> TestHttpServer
where
    F: Filter + Clone + Send + Sync + 'static,
    F::Extract: Reply,
{
    //Spawn new runtime in thread to prevent reactor execution context conflict
    thread::spawn(move || {
        let rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("new rt");
        let (shutdown_tx, shutdown_rx) = oneshot::channel();
        let listener =
            rt.block_on(async move { tokio::net::TcpListener::bind(socket_addr).await.unwrap() });
        let address = listener.local_addr().unwrap();
        let server = warp::serve(filters).incoming(listener).graceful(async {
            shutdown_rx.await.ok();
        });

        let (panic_tx, _) = std_mpsc::channel();
        let thread_name = format!(
            "test({})-support-server",
            thread::current().name().unwrap_or("<unknown>")
        );
        thread::Builder::new()
            .name(thread_name)
            .spawn(move || {
                rt.block_on(server.run());
                let _ = panic_tx.send(());
            })
            .expect("thread spawn");

        TestHttpServer {
            address,
            shutdown_tx: Some(shutdown_tx),
        }
    })
    .join()
    .unwrap()
}

#[cfg(test)]
mod tests {
    use reqwest::StatusCode;
    use serde::Deserialize;

    use super::*;

    #[tokio::test]
    async fn test_server_simple_http() {
        let expected: &'static str = "Hello Mithril !";
        let routes = warp::any().map(move || expected);
        let server = test_http_server(routes);

        let result = reqwest::get(server.url())
            .await
            .expect("should run")
            .text()
            .await
            .unwrap();

        assert_eq!(result, expected)
    }

    #[tokio::test]
    async fn test_server_simple_json() {
        #[derive(Debug, Eq, PartialEq, Deserialize)]
        struct Test {
            content: String,
        }

        let routes = warp::any().map(move || r#"{"content":"Hello Mithril !"}"#);
        let server = test_http_server(routes);

        let result = reqwest::get(server.url())
            .await
            .expect("should run")
            .json::<Test>()
            .await
            .unwrap();

        assert_eq!(
            result,
            Test {
                content: "Hello Mithril !".to_string()
            }
        )
    }

    #[tokio::test]
    async fn test_server_specific_route() {
        let expected: &'static str = "Hello Mithril !";
        let routes = warp::path("hello").map(move || expected);
        let server = test_http_server(routes);

        let result = reqwest::get(format!("{}/hello", server.url()))
            .await
            .expect("should run")
            .text()
            .await
            .unwrap();

        assert_eq!(result, expected);
    }

    #[tokio::test]
    async fn test_server_unbind_route_yield_404() {
        let expected: &'static str = "Hello Mithril !";
        let routes = warp::path("hello").map(move || expected);
        let server = test_http_server(routes);

        let result = reqwest::get(format!("{}/unbind", server.url()))
            .await
            .expect("should run");

        assert_eq!(result.status(), StatusCode::NOT_FOUND);
    }
}
