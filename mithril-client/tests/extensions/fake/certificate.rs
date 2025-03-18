use mithril_client::{MithrilCertificate, MithrilCertificateListItem};
use mithril_common::test_utils::test_http_server::{test_http_server, TestHttpServer};

use crate::extensions::routes;

use super::FakeAggregator;

impl FakeAggregator {
    pub fn spawn_with_certificate(&self, certificate_hash_list: &[String]) -> TestHttpServer {
        let certificate_json = serde_json::to_string(&MithrilCertificate {
            hash: certificate_hash_list[0].to_string(),
            ..MithrilCertificate::dummy()
        })
        .unwrap();
        let certificate_list_json = serde_json::to_string(
            &certificate_hash_list
                .iter()
                .map(|hash| MithrilCertificateListItem {
                    hash: hash.clone(),
                    ..MithrilCertificateListItem::dummy()
                })
                .collect::<Vec<_>>(),
        )
        .unwrap();

        test_http_server(routes::certificate::routes(
            self.calls.clone(),
            Some(certificate_list_json),
            certificate_json,
        ))
    }
}
