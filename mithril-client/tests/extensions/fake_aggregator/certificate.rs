use mithril_client::{MithrilCertificate, MithrilCertificateListItem};
use mithril_common::test_utils::double::Dummy;

use crate::extensions::routes;

use super::FakeAggregator;

impl FakeAggregator {
    pub fn spawn_with_certificate(certificate_hash_list: &[String]) -> Self {
        let certificate = MithrilCertificate {
            hash: certificate_hash_list[0].to_string(),
            ..MithrilCertificate::dummy()
        };
        let certificate_list = certificate_hash_list
            .iter()
            .map(|hash| MithrilCertificateListItem {
                hash: hash.clone(),
                ..MithrilCertificateListItem::dummy()
            })
            .collect::<Vec<_>>();

        let router = routes::certificate::routes(certificate_list, certificate);

        Self::spawn_test_server_on_random_port(router)
    }
}
