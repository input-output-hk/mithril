use std::sync::Arc;

use anyhow::anyhow;
use async_trait::async_trait;
use mithril_common::certificate_chain::{CertificateRetriever, CertificateRetrieverError};
use mithril_common::entities::Certificate;

use crate::certificate_client::{CertificateAggregatorRequest, CertificateClient};
use crate::{MithrilCertificate, MithrilCertificateListItem, MithrilResult};

#[inline]
pub(super) async fn list(
    client: &CertificateClient,
) -> MithrilResult<Vec<MithrilCertificateListItem>> {
    client.aggregator_requester.list_latest().await
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
    aggregator_requester: Arc<dyn CertificateAggregatorRequest>,
}

impl InternalCertificateRetriever {
    pub(super) fn new(
        aggregator_requester: Arc<dyn CertificateAggregatorRequest>,
    ) -> InternalCertificateRetriever {
        InternalCertificateRetriever {
            aggregator_requester,
        }
    }

    pub(super) async fn get(
        &self,
        certificate_hash: &str,
    ) -> MithrilResult<Option<MithrilCertificate>> {
        self.aggregator_requester.get_by_hash(certificate_hash).await
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
    use mithril_common::test::double::{Dummy, fake_data};

    use super::*;
    use crate::certificate_client::tests_utils::CertificateClientTestBuilder;

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
            .config_aggregator_requester_mock(|mock| {
                mock.expect_list_latest().return_once(move || Ok(message));
            })
            .build();
        let items = certificate_client.list().await.unwrap();

        assert_eq!(expected, items);
    }

    #[tokio::test]
    async fn get_certificate_empty_list() {
        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_requester_mock(|mock| {
                mock.expect_list_latest().return_once(move || Ok(Vec::new()));
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
            .config_aggregator_requester_mock(|mock| {
                mock.expect_get_by_hash()
                    .return_once(move |_| {
                        let message: MithrilCertificate = certificate.try_into().unwrap();
                        Ok(Some(message))
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
            .config_aggregator_requester_mock(|mock| {
                mock.expect_get_by_hash().return_once(move |_| Ok(None)).times(1);
            })
            .build();

        assert!(certificate_client.get("cert-hash-123").await.unwrap().is_none());
    }

    #[tokio::test]
    async fn test_show_ko() {
        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_requester_mock(|mock| {
                mock.expect_get_by_hash()
                    .return_once(move |_| Err(anyhow!("an error")))
                    .times(1);
            })
            .build();

        certificate_client
            .get("cert-hash-123")
            .await
            .expect_err("The certificate client should fail here.");
    }
}
