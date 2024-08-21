//! Define a HttpServer for test that can be configured using warp filters.

// Base code from the httpserver in reqwest tests:
// https://github.com/seanmonstar/reqwest/blob/master/tests/support/server.rs

use std::future::Future;
use std::path::{Path, PathBuf};
use std::{net::SocketAddr, sync::mpsc as std_mpsc, thread, time::Duration};
use tokio::net::UnixListener;
use tokio::runtime::Runtime;
use tokio::{runtime, sync::oneshot};
use tokio_stream::wrappers::UnixListenerStream;
use warp::{Filter, Reply};

/// A HTTP server for test
pub struct TestHttpServer {
    address: SocketAddr,
    panic_rx: std_mpsc::Receiver<()>,
    shutdown_tx: Option<oneshot::Sender<()>>,
}

enum ServerConfig {
    Tcp(SocketAddr),
    UnixSocket(PathBuf),
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
    test_http_server_with_config(filters, ServerConfig::Tcp(socket_addr))
}

/// Spawn a [TestHttpServer] with the given warp filters on a Unix socket
pub fn test_http_server_with_unix_socket<F>(filters: F, socket_path: &Path) -> TestHttpServer
where
    F: Filter + Clone + Send + Sync + 'static,
    F::Extract: Reply,
{
    test_http_server_with_config(filters, ServerConfig::UnixSocket(socket_path.to_path_buf()))
}

/// Spawn a [TestHttpServer] using the given warp filters
fn test_http_server_with_config<F>(filters: F, server_type: ServerConfig) -> TestHttpServer
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
        let (panic_tx, panic_rx) = std_mpsc::channel();

        let address = match server_type {
            ServerConfig::Tcp(socket_addr) => {
                let (address, server) = rt.block_on(async move {
                    warp::serve(filters).bind_with_graceful_shutdown(socket_addr, async {
                        shutdown_rx.await.ok();
                    })
                });
                spawn_server_thread(rt, panic_tx, server);

                address
            }
            ServerConfig::UnixSocket(socket_path) => {
                let server = rt.block_on(async move {
                    let listener = UnixListener::bind(socket_path).unwrap();
                    let incoming = UnixListenerStream::new(listener);

                    warp::serve(filters).serve_incoming_with_graceful_shutdown(incoming, async {
                        shutdown_rx.await.ok();
                    })
                });
                spawn_server_thread(rt, panic_tx, server);

                // Dummy address since this branch is a POC, not used for Unix sockets
                ([0, 0, 0, 0], 0).into()
            }
        };

        TestHttpServer {
            address,
            panic_rx,
            shutdown_tx: Some(shutdown_tx),
        }
    })
    .join()
    .unwrap()
}

fn spawn_server_thread<F>(rt: Runtime, panic_tx: std::sync::mpsc::Sender<()>, server_future: F)
where
    F: Future + Send + 'static,
{
    let thread_name = format!(
        "test({})-support-server",
        thread::current().name().unwrap_or("<unknown>")
    );

    thread::Builder::new()
        .name(thread_name)
        .spawn(move || {
            rt.block_on(server_future);
            let _ = panic_tx.send(());
        })
        .expect("thread spawn");
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
