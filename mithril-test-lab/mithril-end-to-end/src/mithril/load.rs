use mithril_common::test_utils::MithrilFixture;
use mithril_common::test_utils::MithrilFixtureBuilder;
use mithril_common::{entities::Epoch, messages::RegisterSignerMessage};

/// Generate signer Data
pub fn generate_signer_data(number_of_signers: usize) -> MithrilFixture {
    MithrilFixtureBuilder::default()
        .with_signers(number_of_signers)
        .build()
}

/// Generate signer registration
pub fn generate_register_message(signers_fixture: &MithrilFixture) -> RegisterSignerMessage {
    let signers = signers_fixture.signers();
    let signer = signers.first().unwrap().to_owned();
    // generate HTTP payload for POST /signers
    let epoch = Epoch(2);

    RegisterSignerMessage {
        epoch: Some(epoch),
        party_id: signer.party_id,
        verification_key: signer.verification_key,
        verification_key_signature: signer.verification_key_signature,
        operational_certificate: signer.operational_certificate,
        kes_period: signer.kes_period,
    }
}

mod tests {

    use std::{
        fs,
        path::{Path, PathBuf},
        sync::Arc,
        time::Duration,
    };

    use mithril_common::{digesters::DummyImmutablesDbBuilder, entities::ProtocolParameters};
    use slog::Drain;

    use crate::{devnet::BftNode, Aggregator};

    use super::*;

    // TODO
    // * start an aggregator in the context of the test
    // * configure aggregator to use mock cardano-cli
    //   * implement mock cardano-client
    //   * set canned answers from cardano-cli
    #[tokio::test]
    async fn should_register_a_new_signer() {
        // configure logger which is needed to run subprocess
        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        let _log = slog_scope::set_global_logger(slog::Logger::root(Arc::new(drain), slog::o!()));

        let signers_fixture = generate_signer_data(1);
        let register_message = generate_register_message(&signers_fixture);

        // configure a dummy immutable db
        let immutable_db = DummyImmutablesDbBuilder::new("load-tester")
            .with_immutables(&[1, 2, 3])
            .append_immutable_trio()
            .build();

        let dummy_bft_node = BftNode {
            db_path: immutable_db.dir,
            socket_path: PathBuf::new(),
        };
        let cardano_cli_path = Path::new("/home/curry/mithril/mithril-test-lab/mock-cardano-cli");
        let tmp_dir = std::env::temp_dir().join("load-aggregator");
        if tmp_dir.exists() {
            fs::remove_dir_all(&tmp_dir).unwrap();
        }

        fs::create_dir_all(&tmp_dir).unwrap();

        let bin_dir = Path::new("../../target/release");
        let mut aggregator = Aggregator::new(
            8888,
            &dummy_bft_node,
            cardano_cli_path,
            &tmp_dir,
            bin_dir,
            "thales",
        )
        .unwrap();

        aggregator.set_protocol_parameters(&ProtocolParameters::default());
        aggregator.serve().unwrap();
        tokio::time::sleep(Duration::from_secs(10)).await;

        let result = reqwest::Client::new()
            .post(format!("{}/register-signer", aggregator.endpoint()))
            .json(&register_message)
            .send()
            .await
            .unwrap();
        assert_eq!(
            400,
            result.status(),
            "text: {}",
            result.text().await.unwrap()
        );

        // ensure POSTing payload gives 200
        aggregator.stop().await.unwrap();
    }
}
