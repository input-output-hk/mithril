//!

use anyhow::anyhow;
use curl::easy::{Easy2, Handler, List, WriteError};
use serde::Serialize;
use std::path::PathBuf;

use crate::StdResult;

/// Writes messages to a Unix socket using Http protocol.
pub struct HttpUnixSocketWriter {
    socket_path: PathBuf,
}

impl HttpUnixSocketWriter {
    /// Create a new `SocketWriter` that writes to the given socket path.
    pub fn new(socket_path: PathBuf) -> Self {
        Self { socket_path }
    }

    /// Write a message to the socket.
    ///
    /// Note: 'localhost/' is prepended to the url to make it a valid url.
    pub fn write<T: Serialize>(&self, url: &str, message: &T) -> StdResult<()> {
        let mut headers = List::new();
        headers.append("Content-Type: application/json")?;

        let mut curl = Easy2::new(CurlCollector(Vec::new()));
        curl.post(true)?;
        curl.http_headers(headers)?;
        curl.post_fields_copy(serde_json::to_string(message)?.as_bytes())?;
        curl.unix_socket(&self.socket_path.to_string_lossy())?;
        curl.url(&format!("localhost/{url}"))?;
        curl.perform()?;

        if curl.response_code()? == 201 {
            Ok(())
        } else {
            Err(anyhow!(
                "Unexpected response code: {}",
                curl.response_code()?
            ))
        }
    }
}

struct CurlCollector(Vec<u8>);

impl Handler for CurlCollector {
    fn write(&mut self, data: &[u8]) -> std::result::Result<usize, WriteError> {
        self.0.extend_from_slice(data);
        Ok(data.len())
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use tokio::sync::mpsc;
    use tokio::sync::mpsc::error::TryRecvError;
    use warp::Filter;

    use crate::messages::RegisterSignatureMessage;
    use crate::test_utils::test_http_server::{test_http_server_with_unix_socket, TestHttpServer};
    use crate::test_utils::TempDir;

    use super::*;

    fn listen_unix_socket_for_http_message(
        socket_path: &Path,
        url: &str,
        tx: mpsc::Sender<RegisterSignatureMessage>,
    ) -> TestHttpServer {
        let http_server = test_http_server_with_unix_socket(
            warp::path(url.to_string())
                .and(warp::post())
                .and(warp::body::json())
                .map(move |message: RegisterSignatureMessage| {
                    println!("Received signature: {:?}", message);
                    tx.try_send(message).unwrap();
                    warp::reply::with_status(warp::reply(), warp::http::StatusCode::CREATED)
                }),
            &socket_path,
        );

        http_server
    }

    #[test]
    fn write_message_to_socket() {
        let dir = TempDir::create("signature-network-node", "write_message_to_socket");
        let socket_path = dir.join("test.sock");
        let url = "register-signatures";
        let (tx, mut rx) = mpsc::channel(1);
        let _listener = listen_unix_socket_for_http_message(&socket_path, &url, tx);

        let writer = HttpUnixSocketWriter::new(socket_path);

        // No messages should have been notified yet
        assert_eq!(Err(TryRecvError::Empty), rx.try_recv());

        writer
            .write(&url, &RegisterSignatureMessage::dummy())
            .unwrap();

        // Wait for the message to be notified
        assert_eq!(Ok(RegisterSignatureMessage::dummy()), rx.try_recv(),);
    }
}