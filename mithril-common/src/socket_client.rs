//!

use std::path::{Path, PathBuf};

use anyhow::anyhow;
use curl::easy::{Easy2, Handler, List, WriteError};
use serde::de::DeserializeOwned;
use serde::Serialize;

use crate::StdResult;

/// Read & writes messages to a Unix socket using Http protocol.
pub struct HttpUnixSocketClient {
    socket_path: PathBuf,
}

impl HttpUnixSocketClient {
    /// Create a new `HttpUnixSocketClient` that read & writes to the given socket path.
    pub fn new(socket_path: &Path) -> Self {
        Self {
            socket_path: socket_path.to_path_buf(),
        }
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

    /// Read a message from the socket.
    ///
    /// Note: 'localhost/' is prepended to the url to make it a valid url.
    pub fn read<T: DeserializeOwned>(&self, url: &str) -> StdResult<T> {
        let mut curl = Easy2::new(CurlCollector(Vec::new()));
        curl.get(true)?;
        curl.unix_socket(&self.socket_path.to_string_lossy())?;
        curl.url(&format!("localhost/{url}"))?;
        curl.perform()?;

        let payload = serde_json::from_slice::<T>(&curl.get_ref().0)?;
        Ok(payload)
    }
}

struct CurlCollector(Vec<u8>);

impl Handler for CurlCollector {
    fn write(&mut self, data: &[u8]) -> Result<usize, WriteError> {
        self.0.extend_from_slice(data);
        Ok(data.len())
    }
}

#[cfg(test)]
mod tests {
    use tokio::sync::mpsc;
    use tokio::sync::mpsc::error::TryRecvError;
    use warp::Filter;

    use crate::messages::RegisterSignatureMessage;
    use crate::test_utils::test_http_server::test_http_server_with_unix_socket;
    use crate::test_utils::TempDir;

    use super::*;

    #[test]
    fn write_message_to_socket() {
        let dir = TempDir::create("signature-network-node", "write_message_to_socket");
        let socket_path = dir.join("test.sock");
        let url = "register-signatures";
        let (tx, mut rx) = mpsc::channel(1);
        let _http_server = test_http_server_with_unix_socket(
            warp::path(url)
                .and(warp::post())
                .and(warp::body::json())
                .map(move |message: RegisterSignatureMessage| {
                    tx.try_send(message).unwrap();
                    warp::reply::with_status(warp::reply(), warp::http::StatusCode::CREATED)
                }),
            &socket_path,
        );

        let client = HttpUnixSocketClient::new(&socket_path);

        // No messages should have been notified yet
        assert_eq!(Err(TryRecvError::Empty), rx.try_recv());

        client
            .write(&url, &RegisterSignatureMessage::dummy())
            .unwrap();

        // Wait for the message to be notified
        assert_eq!(Ok(RegisterSignatureMessage::dummy()), rx.try_recv(),);
    }

    #[test]
    fn read_message_from_socket() {
        let dir = TempDir::create("signature-network-node", "read_message_from_socket");
        let socket_path = dir.join("test.sock");
        let url = "get-register-signatures";
        let _http_server = test_http_server_with_unix_socket(
            warp::path(url).and(warp::get()).map(move || {
                warp::reply::with_status(
                    warp::reply::json(&RegisterSignatureMessage::dummy()),
                    warp::http::StatusCode::OK,
                )
            }),
            &socket_path,
        );

        let client = HttpUnixSocketClient::new(&socket_path);

        let message: RegisterSignatureMessage = client.read(&url).unwrap();

        assert_eq!(RegisterSignatureMessage::dummy(), message);
    }
}
