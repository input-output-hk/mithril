use mithril_client::{
    MessageBuilder, MithrilCertificate, MithrilStakeDistribution, MithrilStakeDistributionListItem,
};
use mithril_common::test::double::Dummy;

use crate::extensions::routes;

use super::FakeAggregator;

impl FakeAggregator {
    pub fn spawn_with_mithril_stake_distribution(msd_hash: &str, certificate_hash: &str) -> Self {
        let mithril_stake_distribution = MithrilStakeDistribution {
            hash: msd_hash.to_string(),
            certificate_hash: certificate_hash.to_string(),
            ..MithrilStakeDistribution::dummy()
        };
        let mithril_stake_distribution_list = vec![
            MithrilStakeDistributionListItem {
                hash: msd_hash.to_string(),
                certificate_hash: certificate_hash.to_string(),
                ..MithrilStakeDistributionListItem::dummy()
            },
            MithrilStakeDistributionListItem::dummy(),
        ];

        let mut certificate = MithrilCertificate {
            hash: certificate_hash.to_string(),
            ..MithrilCertificate::dummy()
        };
        let message = MessageBuilder::new()
            .compute_mithril_stake_distribution_message(&certificate, &mithril_stake_distribution)
            .expect("Computing msd message should not fail");
        certificate.signed_message = message.compute_hash();

        let router = routes::mithril_stake_distribution::routes(
            mithril_stake_distribution_list,
            mithril_stake_distribution,
        )
        .merge(routes::certificate::routes(Vec::new(), certificate));

        Self::spawn_test_server_on_random_port(router)
    }
}
