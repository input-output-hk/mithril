//! Define a HttpServer for test that can be configured using warp filters.

// Base code from the httpserver in reqwest tests:
// https://github.com/seanmonstar/reqwest/blob/master/tests/support/server.rs

use std::{net::SocketAddr, sync::mpsc as std_mpsc, thread, time::Duration};
use tokio::{runtime, sync::oneshot};
use warp::{Filter, Reply};

/// A HTTP server for test
pub struct TestHttpServer {
    address: SocketAddr,
    panic_rx: std_mpsc::Receiver<()>,
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

        if !::std::thread::panicking() {
            self.panic_rx
                .recv_timeout(Duration::from_secs(3))
                .expect("test server should not panic");
        }
    }
}

/// Spawn a [TestHttpServer] using the given warp filters
pub fn test_http_server<F>(filters: F) -> TestHttpServer
where
    F: Filter + Clone + Send + Sync + 'static,
    F::Extract: Reply,
{
    let port = 0_u16;
    test_http_server_with_port(filters, port)
}

/// Spawn a [TestHttpServer] using the given warp filters
pub fn test_http_server_with_port<F>(filters: F, port: u16) -> TestHttpServer
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
        let (address, server) = rt.block_on(async move {
            warp::serve(filters).bind_with_graceful_shutdown(([127, 0, 0, 1], port), async {
                shutdown_rx.await.ok();
            })
        });

        let (panic_tx, panic_rx) = std_mpsc::channel();
        let thread_name = format!(
            "test({})-support-server",
            thread::current().name().unwrap_or("<unknown>")
        );
        thread::Builder::new()
            .name(thread_name)
            .spawn(move || {
                rt.block_on(server);
                let _ = panic_tx.send(());
            })
            .expect("thread spawn");

        TestHttpServer {
            address,
            panic_rx,
            shutdown_tx: Some(shutdown_tx),
        }
    })
    .join()
    .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use reqwest::StatusCode;
    use serde::Deserialize;

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
