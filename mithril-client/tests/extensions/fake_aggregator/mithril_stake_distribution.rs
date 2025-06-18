use warp::Filter;

use mithril_client::{
    MessageBuilder, MithrilCertificate, MithrilStakeDistribution, MithrilStakeDistributionListItem,
};
use mithril_test_http_server::{test_http_server, TestHttpServer};

use crate::extensions::routes;

use super::FakeAggregator;

impl FakeAggregator {
    pub fn spawn_with_mithril_stake_distribution(
        &self,
        msd_hash: &str,
        certificate_hash: &str,
    ) -> TestHttpServer {
        let mithril_stake_distribution = MithrilStakeDistribution {
            hash: msd_hash.to_string(),
            certificate_hash: certificate_hash.to_string(),
            ..MithrilStakeDistribution::dummy()
        };
        let mithril_stake_distribution_json =
            serde_json::to_string(&mithril_stake_distribution).unwrap();
        let mithril_stake_distribution_list_json = serde_json::to_string(&vec![
            MithrilStakeDistributionListItem {
                hash: msd_hash.to_string(),
                certificate_hash: certificate_hash.to_string(),
                ..MithrilStakeDistributionListItem::dummy()
            },
            MithrilStakeDistributionListItem::dummy(),
        ])
        .unwrap();

        let mut certificate = MithrilCertificate {
            hash: certificate_hash.to_string(),
            ..MithrilCertificate::dummy()
        };
        let message = MessageBuilder::new()
            .compute_mithril_stake_distribution_message(&certificate, &mithril_stake_distribution)
            .expect("Computing msd message should not fail");
        certificate.signed_message = message.compute_hash();
        let certificate_json = serde_json::to_string(&certificate).unwrap();

        test_http_server(
            routes::mithril_stake_distribution::routes(
                self.calls.clone(),
                mithril_stake_distribution_list_json,
                mithril_stake_distribution_json,
            )
            .or(routes::certificate::routes(
                self.calls.clone(),
                None,
                certificate_json,
            )),
        )
    }
}
