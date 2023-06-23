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
    let epoch = Epoch(238);

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
        time::Duration,
    };

    use mithril_common::entities::ProtocolParameters;

    use crate::{devnet::BftNode, Aggregator};

    use super::*;

    // TODO
    // * start an aggregator in the context of the test
    // * configure aggregator to use mock cardano-cli
    //   * implement mock cardano-client
    //   * set canned answers from cardano-cli
    #[tokio::test]
    async fn should_register_a_new_signer() {
        let signers_fixture = generate_signer_data(1);
        let register_message = generate_register_message(&signers_fixture);
        let db_path = std::env::temp_dir().join("load-aggregator").join("db");
        fs::create_dir_all(&db_path).unwrap();

        let dummy_bft_node = BftNode {
            db_path,
            socket_path: PathBuf::new(),
        };
        let cardano_cli_path = Path::new("mock-cardano-cli");
        let tmp_dir = std::env::temp_dir().join("load-aggregator");
        fs::create_dir_all(&tmp_dir).unwrap();

        let bin_dir = Path::new("../../target/release");
        let mut aggregator = Aggregator::new(
            8888,
            &dummy_bft_node,
            cardano_cli_path,
            &tmp_dir,
            bin_dir,
            "Thales",
        )
        .unwrap();

        aggregator.set_protocol_parameters(&ProtocolParameters::default());
        aggregator.serve().unwrap();
        tokio::time::sleep(Duration::from_secs(10)).await;

        //let register_message_json = serde_json::to_string(&register_message).unwrap();
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
