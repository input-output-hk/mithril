use anyhow::{anyhow, Context};
use async_trait::async_trait;
use slog::{crit, Logger};
use std::sync::Arc;

use mithril_common::certificate_chain::{CertificateRetriever, CertificateRetrieverError};
use mithril_common::entities::Certificate;
use mithril_common::messages::CertificateMessage;

use crate::aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest};
use crate::certificate_client::CertificateClient;
use crate::{MithrilCertificate, MithrilCertificateListItem, MithrilResult};

#[inline]
pub(super) async fn list(
    client: &CertificateClient,
) -> MithrilResult<Vec<MithrilCertificateListItem>> {
    let response = client
        .aggregator_client
        .get_content(AggregatorRequest::ListCertificates)
        .await
        .with_context(|| "CertificateClient can not get the certificate list")?;
    let items = serde_json::from_str::<Vec<MithrilCertificateListItem>>(&response)
        .with_context(|| "CertificateClient can not deserialize certificate list")?;

    Ok(items)
}

#[inline]
pub(super) async fn get(
    client: &CertificateClient,
    certificate_hash: &str,
) -> MithrilResult<Option<MithrilCertificate>> {
    client.retriever.get(certificate_hash).await
}

/// Internal type to implement the [InternalCertificateRetriever] trait and avoid a circular
/// dependency between the [CertificateClient] and the [CommonMithrilCertificateVerifier] that need
/// a [CertificateRetriever] as a dependency.
pub(super) struct InternalCertificateRetriever {
    aggregator_client: Arc<dyn AggregatorClient>,
    logger: Logger,
}

impl InternalCertificateRetriever {
    pub(super) fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        logger: Logger,
    ) -> InternalCertificateRetriever {
        InternalCertificateRetriever {
            aggregator_client,
            logger,
        }
    }

    pub(super) async fn get(
        &self,
        certificate_hash: &str,
    ) -> MithrilResult<Option<MithrilCertificate>> {
        let response = self
            .aggregator_client
            .get_content(AggregatorRequest::GetCertificate {
                hash: certificate_hash.to_string(),
            })
            .await;

        match response {
            Err(AggregatorClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
            Ok(response) => {
                let message =
                    serde_json::from_str::<CertificateMessage>(&response).inspect_err(|e| {
                        crit!(
                            self.logger, "Could not create certificate from API message";
                            "error" => e.to_string(),
                            "raw_message" => response
                        );
                    })?;

                Ok(Some(message))
            }
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl CertificateRetriever for InternalCertificateRetriever {
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError> {
        self.get(certificate_hash)
            .await
            .map_err(CertificateRetrieverError)?
            .map(|message| message.try_into())
            .transpose()
            .map_err(CertificateRetrieverError)?
            .ok_or(CertificateRetrieverError(anyhow!(format!(
                "Certificate does not exist: '{}'",
                certificate_hash
            ))))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use crate::certificate_client::tests_utils::CertificateClientTestBuilder;

    use super::*;

    #[tokio::test]
    async fn get_certificate_list() {
        let expected = vec![
            MithrilCertificateListItem {
                hash: "cert-hash-123".to_string(),
                ..MithrilCertificateListItem::dummy()
            },
            MithrilCertificateListItem {
                hash: "cert-hash-456".to_string(),
                ..MithrilCertificateListItem::dummy()
            },
        ];
        let message = expected.clone();
        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_client_mock(|mock| {
                mock.expect_get_content()
                    .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
            })
            .build();
        let items = certificate_client.list().await.unwrap();

        assert_eq!(expected, items);
    }

    #[tokio::test]
    async fn get_certificate_empty_list() {
        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_client_mock(|mock| {
                mock.expect_get_content().return_once(move |_| {
                    Ok(serde_json::to_string::<Vec<MithrilCertificateListItem>>(&vec![]).unwrap())
                });
            })
            .build();
        let items = certificate_client.list().await.unwrap();

        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn test_show_ok_some() {
        let certificate_hash = "cert-hash-123".to_string();
        let certificate = fake_data::certificate(certificate_hash.clone());
        let expected_certificate = certificate.clone();

        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_client_mock(|mock| {
                mock.expect_get_content()
                    .return_once(move |_| {
                        let message: CertificateMessage = certificate.try_into().unwrap();
                        Ok(serde_json::to_string(&message).unwrap())
                    })
                    .times(1);
            })
            .build();

        let cert = certificate_client
            .get("cert-hash-123")
            .await
            .unwrap()
            .expect("The certificate should be found")
            .try_into()
            .unwrap();

        assert_eq!(expected_certificate, cert);
    }

    #[tokio::test]
    async fn test_show_ok_none() {
        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_client_mock(|mock| {
                mock.expect_get_content()
                    .return_once(move |_| {
                        Err(AggregatorClientError::RemoteServerLogical(anyhow!(
                            "an error"
                        )))
                    })
                    .times(1);
            })
            .build();

        assert!(certificate_client
            .get("cert-hash-123")
            .await
            .unwrap()
            .is_none());
    }

    #[tokio::test]
    async fn test_show_ko() {
        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_client_mock(|mock| {
                mock.expect_get_content()
                    .return_once(move |_| {
                        Err(AggregatorClientError::RemoteServerTechnical(anyhow!(
                            "an error"
                        )))
                    })
                    .times(1);
            })
            .build();

        certificate_client
            .get("cert-hash-123")
            .await
            .expect_err("The certificate client should fail here.");
    }
}
