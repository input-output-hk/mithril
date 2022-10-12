use async_trait::async_trait;
use reqwest::{self, StatusCode};
use slog_scope::debug;
use std::io;
use thiserror::Error;
use tokio::sync::RwLock;

use mithril_common::{
    entities::{CertificatePending, EpochSettings, Signer, SingleSignatures},
    fake_data,
};

#[cfg(test)]
use mockall::automock;

/// Error structure for the Certificate Handler.
#[derive(Error, Debug)]
pub enum CertificateHandlerError {
    /// The aggregator host has returned a technical error.
    #[error("remote server technical error: '{0}'")]
    RemoteServerTechnical(String),

    /// The aggregator host responded it cannot fulfill our request.
    #[error("remote server logical error: '{0}'")]
    RemoteServerLogical(String),

    /// Could not reach aggregator.
    #[error("remote server unreachable: '{0}'")]
    RemoteServerUnreachable(String),

    /// Could not parse response.
    #[error("json parsing failed: '{0}'")]
    JsonParseFailed(String),

    /// Mostly network errors.
    #[error("io error: {0}")]
    IOError(#[from] io::Error),
}

/// Trait for mocking and testing a `CertificateHandler`
#[cfg_attr(test, automock)]
#[async_trait]
pub trait CertificateHandler: Sync + Send {
    /// Retrieves epoch settings from the aggregator
    async fn retrieve_epoch_settings(
        &self,
    ) -> Result<Option<EpochSettings>, CertificateHandlerError>;

    /// Retrieves a pending certificate from the aggregator
    async fn retrieve_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, CertificateHandlerError>;

    /// Registers signer with the aggregator.
    async fn register_signer(&self, signer: &Signer) -> Result<(), CertificateHandlerError>;

    /// Registers single signatures with the aggregator.
    async fn register_signatures(
        &self,
        signatures: &SingleSignatures,
    ) -> Result<(), CertificateHandlerError>;
}

/// CertificateHandlerHTTPClient is a http client for an aggregator
pub struct CertificateHandlerHTTPClient {
    aggregator_endpoint: String,
}

impl CertificateHandlerHTTPClient {
    /// CertificateHandlerHTTPClient factory
    pub fn new(aggregator_endpoint: String) -> Self {
        debug!("New CertificateHandlerHTTPClient created");
        Self {
            aggregator_endpoint,
        }
    }
}

#[async_trait]
impl CertificateHandler for CertificateHandlerHTTPClient {
    async fn retrieve_epoch_settings(
        &self,
    ) -> Result<Option<EpochSettings>, CertificateHandlerError> {
        debug!("Retrieve epoch settings");
        let url = format!("{}/epoch-settings", self.aggregator_endpoint);
        let response = reqwest::get(url.clone()).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<EpochSettings>().await {
                    Ok(epoch_settings) => Ok(Some(epoch_settings)),
                    Err(err) => Err(CertificateHandlerError::JsonParseFailed(err.to_string())),
                },
                _ => Err(CertificateHandlerError::RemoteServerTechnical(
                    response.text().await.unwrap_or_default(),
                )),
            },
            Err(err) => Err(CertificateHandlerError::RemoteServerUnreachable(
                err.to_string(),
            )),
        }
    }

    async fn retrieve_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, CertificateHandlerError> {
        debug!("Retrieve pending certificate");
        let url = format!("{}/certificate-pending", self.aggregator_endpoint);
        let response = reqwest::get(url.clone()).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<CertificatePending>().await {
                    Ok(pending_certificate) => Ok(Some(pending_certificate)),
                    Err(err) => Err(CertificateHandlerError::JsonParseFailed(err.to_string())),
                },
                StatusCode::NO_CONTENT => Ok(None),
                _ => Err(CertificateHandlerError::RemoteServerTechnical(
                    response.text().await.unwrap_or_default(),
                )),
            },
            Err(err) => Err(CertificateHandlerError::RemoteServerUnreachable(
                err.to_string(),
            )),
        }
    }

    async fn register_signer(&self, signer: &Signer) -> Result<(), CertificateHandlerError> {
        debug!("Register signer");
        let url = format!("{}/register-signer", self.aggregator_endpoint);
        let client = reqwest::Client::new();
        let response = client.post(url.clone()).json(signer).send().await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::CREATED => Ok(()),
                StatusCode::BAD_REQUEST => Err(CertificateHandlerError::RemoteServerLogical(
                    format!("bad request: {}", response.text().await.unwrap_or_default()),
                )),
                _ => Err(CertificateHandlerError::RemoteServerTechnical(
                    response.text().await.unwrap_or_default(),
                )),
            },
            Err(err) => Err(CertificateHandlerError::RemoteServerUnreachable(
                err.to_string(),
            )),
        }
    }

    async fn register_signatures(
        &self,
        signatures: &SingleSignatures,
    ) -> Result<(), CertificateHandlerError> {
        debug!("Register signatures");
        let url = format!("{}/register-signatures", self.aggregator_endpoint);
        let client = reqwest::Client::new();
        let response = client.post(url.clone()).json(signatures).send().await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::CREATED => Ok(()),
                StatusCode::BAD_REQUEST => Err(CertificateHandlerError::RemoteServerLogical(
                    format!("bad request: {}", response.text().await.unwrap_or_default()),
                )),
                StatusCode::CONFLICT => Err(CertificateHandlerError::RemoteServerLogical(
                    "already registered single signatures".to_string(),
                )),
                _ => Err(CertificateHandlerError::RemoteServerTechnical(
                    response.text().await.unwrap_or_default(),
                )),
            },
            Err(err) => Err(CertificateHandlerError::RemoteServerUnreachable(
                err.to_string(),
            )),
        }
    }
}

/// This certificate handler is intended to be used by test services.
/// It actually does not communicate with an aggregator host but mimics this behavior.
/// It is driven by a Tester that controls the CertificatePending it can return and it can return its internal state for testing.
pub struct DumbCertificateHandler {
    epoch_settings: RwLock<Option<EpochSettings>>,
    certificate_pending: RwLock<Option<CertificatePending>>,
    last_registered_signer: RwLock<Option<Signer>>,
}

impl DumbCertificateHandler {
    /// Instanciate a new DumbCertificateHandler.
    pub fn new() -> Self {
        Self {
            epoch_settings: RwLock::new(None),
            certificate_pending: RwLock::new(None),
            last_registered_signer: RwLock::new(None),
        }
    }

    /// this method pilots the epoch settings handler
    pub async fn set_epoch_settings(&self, epoch_settings: Option<EpochSettings>) {
        let mut epoch_settings_writer = self.epoch_settings.write().await;
        *epoch_settings_writer = epoch_settings;
    }

    /// this method pilots the certificate pending handler
    /// calling this method unsets the last registered signer
    pub async fn set_certificate_pending(&self, certificate_pending: Option<CertificatePending>) {
        let mut cert = self.certificate_pending.write().await;
        *cert = certificate_pending;
        let mut signer = self.last_registered_signer.write().await;
        *signer = None;
    }

    /// Return the last signer that called with the `register` method.
    pub async fn get_last_registered_signer(&self) -> Option<Signer> {
        self.last_registered_signer.read().await.clone()
    }
}

impl Default for DumbCertificateHandler {
    fn default() -> Self {
        Self {
            epoch_settings: RwLock::new(Some(fake_data::epoch_settings())),
            certificate_pending: RwLock::new(Some(fake_data::certificate_pending())),
            last_registered_signer: RwLock::new(None),
        }
    }
}

#[async_trait]
impl CertificateHandler for DumbCertificateHandler {
    async fn retrieve_epoch_settings(
        &self,
    ) -> Result<Option<EpochSettings>, CertificateHandlerError> {
        let epoch_settings = self.epoch_settings.read().await.clone();

        Ok(epoch_settings)
    }

    async fn retrieve_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, CertificateHandlerError> {
        let cert = self.certificate_pending.read().await.clone();

        Ok(cert)
    }

    /// Registers signer with the aggregator
    async fn register_signer(&self, signer: &Signer) -> Result<(), CertificateHandlerError> {
        let mut last_registered_signer = self.last_registered_signer.write().await;
        let signer = signer.clone();
        *last_registered_signer = Some(signer);

        Ok(())
    }

    /// Registers single signatures with the aggregator
    async fn register_signatures(
        &self,
        _signatures: &SingleSignatures,
    ) -> Result<(), CertificateHandlerError> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use httpmock::prelude::*;
    use mithril_common::entities::ClientError;
    use serde_json::json;
    use std::path::{Path, PathBuf};

    use mithril_common::fake_data;

    use crate::entities::Config;

    fn setup_test() -> (MockServer, Config) {
        let server = MockServer::start();
        let config = Config {
            cardano_cli_path: PathBuf::new().join("cardano-cli"),
            cardano_node_socket_path: PathBuf::new().join("whatever"),
            network_magic: Some(42),
            network: "testnet".to_string(),
            aggregator_endpoint: server.url(""),
            party_id: Some("0".to_string()),
            run_interval: 100,
            db_directory: Path::new("./db").to_path_buf(),
            data_stores_directory: Path::new("./stores").to_path_buf(),
            store_retention_limit: None,
            kes_secret_key_path: None,
            operational_certificate_path: None,
        };
        (server, config)
    }

    #[tokio::test]
    async fn test_epoch_settings_ok_200() {
        let (server, config) = setup_test();
        let epoch_settings_expected = fake_data::epoch_settings();
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/epoch-settings");
            then.status(200)
                .body(json!(epoch_settings_expected).to_string());
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let epoch_settings = certificate_handler.retrieve_epoch_settings().await;
        epoch_settings.as_ref().expect("unexpected error");
        assert_eq!(epoch_settings_expected, epoch_settings.unwrap().unwrap());
    }

    #[tokio::test]
    async fn test_epoch_settings_ko_500() {
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/epoch-settings");
            then.status(500).body("an error occurred");
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let epoch_settings = certificate_handler.retrieve_epoch_settings().await;
        assert_eq!(
            CertificateHandlerError::RemoteServerTechnical("an error occurred".to_string())
                .to_string(),
            epoch_settings.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn test_certificate_pending_ok_200() {
        let (server, config) = setup_test();
        let pending_certificate_expected = fake_data::certificate_pending();
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/certificate-pending");
            then.status(200)
                .body(json!(pending_certificate_expected).to_string());
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let pending_certificate = certificate_handler.retrieve_pending_certificate().await;
        pending_certificate.as_ref().expect("unexpected error");
        assert_eq!(
            pending_certificate_expected,
            pending_certificate.unwrap().unwrap()
        );
    }

    #[tokio::test]
    async fn test_certificate_pending_ok_204() {
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/certificate-pending");
            then.status(204);
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let pending_certificate = certificate_handler.retrieve_pending_certificate().await;
        assert!(pending_certificate.expect("unexpected error").is_none());
    }

    #[tokio::test]
    async fn test_certificate_pending_ko_500() {
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/certificate-pending");
            then.status(500).body("an error occurred");
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let pending_certificate = certificate_handler.retrieve_pending_certificate().await;
        assert_eq!(
            CertificateHandlerError::RemoteServerTechnical("an error occurred".to_string())
                .to_string(),
            pending_certificate.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn test_register_signer_ok_201() {
        let single_signers = fake_data::signers(1);
        let single_signer = single_signers.first().unwrap();
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signer");
            then.status(201);
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signer = certificate_handler.register_signer(single_signer).await;
        register_signer.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_register_signer_ok_400() {
        let single_signers = fake_data::signers(1);
        let single_signer = single_signers.first().unwrap();
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signer");
            then.status(400).body(
                serde_json::to_vec(&ClientError::new(
                    "error".to_string(),
                    "an error".to_string(),
                ))
                .unwrap(),
            );
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signer = certificate_handler.register_signer(single_signer).await;
        assert_eq!(
            CertificateHandlerError::RemoteServerLogical(
                "bad request: {\"label\":\"error\",\"message\":\"an error\"}".to_string()
            )
            .to_string(),
            register_signer.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn test_register_signer_ok_500() {
        let single_signers = fake_data::signers(1);
        let single_signer = single_signers.first().unwrap();
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signer");
            then.status(500).body("an error occurred");
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signer = certificate_handler.register_signer(single_signer).await;
        assert_eq!(
            CertificateHandlerError::RemoteServerTechnical("an error occurred".to_string())
                .to_string(),
            register_signer.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn test_register_signatures_ok_201() {
        let single_signatures = fake_data::single_signatures((1..5).collect());
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signatures");
            then.status(201);
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signatures = certificate_handler
            .register_signatures(&single_signatures)
            .await;
        register_signatures.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_register_signatures_ko_400() {
        let single_signatures = fake_data::single_signatures((1..5).collect());
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signatures");
            then.status(400).body(
                serde_json::to_vec(&ClientError::new(
                    "error".to_string(),
                    "an error".to_string(),
                ))
                .unwrap(),
            );
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signatures = certificate_handler
            .register_signatures(&single_signatures)
            .await;
        assert_eq!(
            CertificateHandlerError::RemoteServerLogical(
                "bad request: {\"label\":\"error\",\"message\":\"an error\"}".to_string()
            )
            .to_string(),
            register_signatures.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn test_register_signatures_ko_409() {
        let single_signatures = fake_data::single_signatures((1..5).collect());
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signatures");
            then.status(409);
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signatures = certificate_handler
            .register_signatures(&single_signatures)
            .await;
        assert_eq!(
            CertificateHandlerError::RemoteServerLogical(
                "already registered single signatures".to_string()
            )
            .to_string(),
            register_signatures.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn test_register_signatures_ko_500() {
        let single_signatures = fake_data::single_signatures((1..5).collect());
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signatures");
            then.status(500).body("an error occurred");
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signatures = certificate_handler
            .register_signatures(&single_signatures)
            .await;
        assert_eq!(
            CertificateHandlerError::RemoteServerTechnical("an error occurred".to_string())
                .to_string(),
            register_signatures.unwrap_err().to_string()
        );
    }
}
