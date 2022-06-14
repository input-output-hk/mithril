use async_trait::async_trait;
use reqwest::{self, StatusCode};
use slog_scope::debug;
use std::io;
use thiserror::Error;

use mithril_common::entities::{CertificatePending, Signer, SingleSignature};

#[cfg(test)]
use mockall::automock;

#[derive(Error, Debug)]
pub enum CertificateHandlerError {
    #[error("remote server technical error: '{0}'")]
    RemoteServerTechnical(String),
    #[error("remote server logical error: '{0}'")]
    RemoteServerLogical(String),
    #[error("remote server unreachable: '{0}'")]
    RemoteServerUnreachable(String),
    #[error("json parsing failed: '{0}'")]
    JsonParseFailed(String),
    #[error("io error:")]
    IOError(#[from] io::Error),
}

#[cfg_attr(test, automock)]
#[async_trait]
pub trait CertificateHandler {
    /// Retrieves a pending certificate from the aggregator
    async fn retrieve_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, CertificateHandlerError>;

    /// Registers signer with the aggregator
    async fn register_signer(&self, signer: &Signer) -> Result<(), CertificateHandlerError>;

    /// Registers single signatures with the aggregator
    async fn register_signatures(
        &self,
        signatures: &[SingleSignature],
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
                status_error => Err(CertificateHandlerError::RemoteServerTechnical(
                    status_error.to_string(),
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
                    "bad request".to_string(),
                )),
                StatusCode::CONFLICT => Err(CertificateHandlerError::RemoteServerLogical(
                    "already registered signer".to_string(),
                )),
                status_error => Err(CertificateHandlerError::RemoteServerTechnical(
                    status_error.to_string(),
                )),
            },
            Err(err) => Err(CertificateHandlerError::RemoteServerUnreachable(
                err.to_string(),
            )),
        }
    }

    async fn register_signatures(
        &self,
        signatures: &[SingleSignature],
    ) -> Result<(), CertificateHandlerError> {
        debug!("Register signatures");
        let url = format!("{}/register-signatures", self.aggregator_endpoint);
        let client = reqwest::Client::new();
        let response = client.post(url.clone()).json(signatures).send().await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::CREATED => Ok(()),
                StatusCode::BAD_REQUEST => Err(CertificateHandlerError::RemoteServerLogical(
                    "bad request".to_string(),
                )),
                StatusCode::CONFLICT => Err(CertificateHandlerError::RemoteServerLogical(
                    "already registered single signatures".to_string(),
                )),
                status_error => Err(CertificateHandlerError::RemoteServerTechnical(
                    status_error.to_string(),
                )),
            },
            Err(err) => Err(CertificateHandlerError::RemoteServerUnreachable(
                err.to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use httpmock::prelude::*;
    use serde_json::json;
    use std::path::Path;

    use mithril_common::fake_data;

    use crate::entities::Config;

    fn setup_test() -> (MockServer, Config) {
        let server = MockServer::start();
        let config = Config {
            network: "testnet".to_string(),
            aggregator_endpoint: server.url(""),
            party_id: 0,
            run_interval: 100,
            db_directory: Path::new("./db").to_path_buf(),
            stake_store_directory: Path::new("./stakes").to_path_buf(),
        };
        (server, config)
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
            then.status(500);
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let pending_certificate = certificate_handler.retrieve_pending_certificate().await;
        assert_eq!(
            CertificateHandlerError::RemoteServerTechnical("500 Internal Server Error".to_string())
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
        let register_signer = certificate_handler.register_signer(&single_signer).await;
        register_signer.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_register_signer_ok_400() {
        let single_signers = fake_data::signers(1);
        let single_signer = single_signers.first().unwrap();
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signer");
            then.status(400);
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signer = certificate_handler.register_signer(&single_signer).await;
        assert_eq!(
            CertificateHandlerError::RemoteServerLogical("bad request".to_string()).to_string(),
            register_signer.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn test_register_signer_ok_409() {
        let single_signers = fake_data::signers(1);
        let single_signer = single_signers.first().unwrap();
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signer");
            then.status(409);
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signer = certificate_handler.register_signer(&single_signer).await;
        assert_eq!(
            CertificateHandlerError::RemoteServerLogical("already registered signer".to_string())
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
            then.status(500);
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signer = certificate_handler.register_signer(&single_signer).await;
        assert_eq!(
            CertificateHandlerError::RemoteServerTechnical("500 Internal Server Error".to_string())
                .to_string(),
            register_signer.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn test_register_signatures_ok_201() {
        let single_signatures = fake_data::single_signatures(5);
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
        let single_signatures = fake_data::single_signatures(5);
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signatures");
            then.status(400);
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signatures = certificate_handler
            .register_signatures(&single_signatures)
            .await;
        assert_eq!(
            CertificateHandlerError::RemoteServerLogical("bad request".to_string()).to_string(),
            register_signatures.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn test_register_signatures_ko_409() {
        let single_signatures = fake_data::single_signatures(5);
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
        let single_signatures = fake_data::single_signatures(5);
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signatures");
            then.status(500);
        });
        let certificate_handler = CertificateHandlerHTTPClient::new(config.aggregator_endpoint);
        let register_signatures = certificate_handler
            .register_signatures(&single_signatures)
            .await;
        assert_eq!(
            CertificateHandlerError::RemoteServerTechnical("500 Internal Server Error".to_string())
                .to_string(),
            register_signatures.unwrap_err().to_string()
        );
    }
}
