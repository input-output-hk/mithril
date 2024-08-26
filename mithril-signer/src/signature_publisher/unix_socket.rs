use async_trait::async_trait;
use mithril_common::entities::{ProtocolMessage, SignedEntityType, SingleSignatures};
use mithril_common::messages::TryToMessageAdapter;
use mithril_common::socket_client::HttpUnixSocketClient;
use mithril_common::StdResult;
use std::path::{Path, PathBuf};

use crate::message_adapters::ToRegisterSignatureMessageAdapter;
use crate::SignaturePublisher;

/// Publishes computed single signatures to a third party using unix socket.
pub struct UnixSocketSignaturePublisher {
    socket_path: PathBuf,
}

impl UnixSocketSignaturePublisher {
    const PUBLISH_ROUTE: &'static str = "register-signatures";

    /// Creates a new instance of the `UnixSocketSignaturePublisher`.
    pub fn new(socket_path: &Path) -> Self {
        Self {
            socket_path: socket_path.to_path_buf(),
        }
    }
}

#[async_trait]
impl SignaturePublisher for UnixSocketSignaturePublisher {
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signatures: &SingleSignatures,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        let message = ToRegisterSignatureMessageAdapter::try_adapt((
            signed_entity_type.to_owned(),
            signatures.to_owned(),
            protocol_message,
        ))?;

        let socket_client = HttpUnixSocketClient::new(&self.socket_path);
        socket_client.write(Self::PUBLISH_ROUTE, &message)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use tokio::sync::mpsc;
    use tokio::sync::mpsc::error::TryRecvError;
    use warp::Filter;

    use mithril_common::messages::RegisterSignatureMessage;
    use mithril_common::protocol::ToMessage;
    use mithril_common::test_utils::test_http_server::test_http_server_with_unix_socket;
    use mithril_common::test_utils::{fake_data, TempDir};

    use super::*;

    #[tokio::test]
    async fn publish() {
        let dir = TempDir::create("unix_socket_signature_publisher", "publish");
        let socket_path = dir.join("test.sock");
        let (tx, mut rx) = mpsc::channel(1);
        let _http_server = test_http_server_with_unix_socket(
            warp::path(UnixSocketSignaturePublisher::PUBLISH_ROUTE)
                .and(warp::post())
                .and(warp::body::json())
                .map(move |message: RegisterSignatureMessage| {
                    tx.try_send(message).unwrap();
                    warp::reply::with_status(warp::reply(), warp::http::StatusCode::CREATED)
                }),
            &socket_path,
        );

        let publisher = UnixSocketSignaturePublisher::new(&socket_path);

        // No messages should have been notified yet
        assert_eq!(Err(TryRecvError::Empty), rx.try_recv());

        let signatures = fake_data::single_signatures(vec![1, 2, 3]);
        publisher
            .publish(
                &SignedEntityType::dummy(),
                &signatures,
                &ProtocolMessage::default(),
            )
            .await
            .unwrap();

        // Wait for the message to be notified
        assert_eq!(
            Ok(RegisterSignatureMessage {
                signed_entity_type: SignedEntityType::dummy(),
                signature: signatures.signature.to_json_hex().unwrap(),
                party_id: signatures.party_id,
                won_indexes: signatures.won_indexes,
                signed_message: Some(ProtocolMessage::default().to_message()),
            }),
            rx.try_recv()
        );
    }
}
