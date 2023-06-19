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

    use super::*;

    #[tokio::test]
    async fn should_register_a_new_signer() {
        let signers_fixture = generate_signer_data(1);
        let register_message = generate_register_message(&signers_fixture);
        //let register_message_json = serde_json::to_string(&register_message).unwrap();
        let result = reqwest::Client::new()
            .post(
                "https://aggregator.testing-preview.api.mithril.network/aggregator/register-signer",
            )
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
    }
}
