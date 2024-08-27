use slog::{error, trace, Logger};
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use mithril_common::messages::{RegisterSignatureMessage, TryFromMessageAdapter};
use mithril_common::socket_client::HttpUnixSocketClient;
use mithril_common::StdResult;

use crate::message_adapters::FromRegisterSingleSignatureAdapter;
use crate::services::CertifierService;

/// A service that consumes signatures from a network through a Unix socket.
pub struct SignatureConsumer {
    unix_socket_client: HttpUnixSocketClient,
    run_interval: Duration,
    certifier_service: Arc<dyn CertifierService>,
    logger: Logger,
}

impl SignatureConsumer {
    const PULL_SIGNATURES_ROUTE: &'static str = "pull-signatures";

    /// Creates a new instance of the `SignatureConsumer`.
    pub fn new(
        socket_path: &Path,
        run_interval: Duration,
        certifier_service: Arc<dyn CertifierService>,
        parent_logger: &Logger,
    ) -> Self {
        Self {
            unix_socket_client: HttpUnixSocketClient::new(socket_path),
            run_interval,
            certifier_service,
            logger: parent_logger.new(slog::o!("src" => "signature_consumer")),
        }
    }

    /// Listens indefinitely for incoming signatures and registers them.
    pub async fn listen(&self) {
        loop {
            trace!(self.logger, "Pulling signatures");

            if let Err(error) = self.pull_then_register_signatures().await {
                error!(
                    self.logger,
                    "Error while pulling and registering signatures: {:?}", error
                );
            }

            tokio::time::sleep(self.run_interval).await;
        }
    }

    async fn pull_then_register_signatures(&self) -> StdResult<()> {
        let signatures_message: Vec<RegisterSignatureMessage> =
            self.unix_socket_client.read(Self::PULL_SIGNATURES_ROUTE)?;

        for signature_message in signatures_message {
            let signed_entity_type = signature_message.signed_entity_type.clone();
            let signature = FromRegisterSingleSignatureAdapter::try_adapt(signature_message)?;

            self.certifier_service
                .register_single_signature(&signed_entity_type, &signature)
                .await?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use tokio::sync::RwLock;
    use warp::Filter;

    use mithril_common::entities::{
        Certificate, Epoch, ProtocolMessage, SignedEntityType, SingleSignatures,
    };
    use mithril_common::test_utils::test_http_server::{
        test_http_server_with_unix_socket, TestHttpServer,
    };
    use mithril_common::test_utils::TempDir;
    use mithril_common::StdResult;

    use crate::entities::OpenMessage;
    use crate::services::SignatureRegistrationStatus;
    use crate::test_tools::TestLogger;

    use super::*;

    fn serve_dummy_signatures_on_unix_socket(socket_path: &Path) -> TestHttpServer {
        test_http_server_with_unix_socket(
            warp::path(SignatureConsumer::PULL_SIGNATURES_ROUTE)
                .and(warp::get())
                .map(move || {
                    warp::reply::with_status(
                        warp::reply::json(&vec![RegisterSignatureMessage::dummy()]),
                        warp::http::StatusCode::OK,
                    )
                }),
            socket_path,
        )
    }

    #[tokio::test]
    async fn listen_pull_signatures() {
        let dir = TempDir::create(
            "signature_consumer",
            "listen_pull_signatures_immediately_when_started",
        );
        let socket_path = dir.join("socket");
        let _http_server = serve_dummy_signatures_on_unix_socket(&socket_path);
        let certifier_service = CertifierServiceSpy::new();
        let run_interval = Duration::from_millis(2);

        let consumer = SignatureConsumer::new(
            &socket_path,
            run_interval,
            Arc::new(certifier_service.clone()),
            &TestLogger::stdout(),
        );

        tokio::spawn(async move {
            consumer.listen().await;
        });

        // Yield to let the consumer do its first pull immediately
        tokio::task::yield_now().await;

        let registered_signatures = certifier_service.get_registered_single_signatures().await;
        let expected_sig = (
            RegisterSignatureMessage::dummy().signed_entity_type,
            FromRegisterSingleSignatureAdapter::try_adapt(RegisterSignatureMessage::dummy())
                .unwrap(),
        );
        assert_eq!(vec![expected_sig.clone()], registered_signatures);

        // waiting so it can do several pulls
        tokio::time::sleep(run_interval * 3).await;

        let registered_signatures = certifier_service.get_registered_single_signatures().await;
        assert!(
            registered_signatures.len() >= 2,
            "Expected at least 2 signatures to be registered, got {}: {registered_signatures:?}",
            registered_signatures.len()
        );
    }

    /// A spy for the `CertifierService` trait that only records the calls to
    /// `register_single_signature`, all other methods are unimplemented.
    ///
    /// Note: used instead of Mockall because when a Mockall assertion fails, it does so
    /// in the thread that calls the method, which in our case is not the test thread meaning
    /// ... that the test will not fail.
    /// Also, it's easier to use a spy in this case since it allow to write the assertions at
    /// the end of the test instead of in the middle of it.
    #[derive(Clone)]
    struct CertifierServiceSpy {
        register_single_signatures: Arc<RwLock<Vec<(SignedEntityType, SingleSignatures)>>>,
    }

    impl CertifierServiceSpy {
        fn new() -> Self {
            Self {
                register_single_signatures: Arc::new(RwLock::new(Vec::new())),
            }
        }

        async fn get_registered_single_signatures(
            &self,
        ) -> Vec<(SignedEntityType, SingleSignatures)> {
            self.register_single_signatures.read().await.clone()
        }
    }

    #[async_trait::async_trait]
    impl CertifierService for CertifierServiceSpy {
        async fn inform_epoch(&self, _epoch: Epoch) -> StdResult<()> {
            unimplemented!()
        }

        async fn register_single_signature(
            &self,
            signed_entity_type: &SignedEntityType,
            signature: &SingleSignatures,
        ) -> StdResult<SignatureRegistrationStatus> {
            let mut register_single_signatures = self.register_single_signatures.write().await;
            register_single_signatures.push((signed_entity_type.to_owned(), signature.to_owned()));
            Ok(SignatureRegistrationStatus::Registered)
        }

        async fn create_open_message(
            &self,
            _signed_entity_type: &SignedEntityType,
            _protocol_message: &ProtocolMessage,
        ) -> StdResult<OpenMessage> {
            unimplemented!()
        }

        async fn get_open_message(
            &self,
            _signed_entity_type: &SignedEntityType,
        ) -> StdResult<Option<OpenMessage>> {
            unimplemented!()
        }

        async fn mark_open_message_if_expired(
            &self,
            _signed_entity_type: &SignedEntityType,
        ) -> StdResult<Option<OpenMessage>> {
            unimplemented!()
        }

        async fn create_certificate(
            &self,
            _signed_entity_type: &SignedEntityType,
        ) -> StdResult<Option<Certificate>> {
            unimplemented!()
        }

        async fn get_certificate_by_hash(&self, _hash: &str) -> StdResult<Option<Certificate>> {
            unimplemented!()
        }

        async fn get_latest_certificates(&self, _last_n: usize) -> StdResult<Vec<Certificate>> {
            unimplemented!()
        }

        async fn verify_certificate_chain(&self, _epoch: Epoch) -> StdResult<()> {
            unimplemented!()
        }
    }
}
