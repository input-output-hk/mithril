mod extensions;

use mithril_client::{
    AggregatorDiscoveryType, ClientBuilder, GenesisVerificationKey, MessageBuilder,
    common::{BlockNumber, SlotNumber},
};
use mithril_common::{entities::CardanoBlockWithTransactions, test::double::fake_keys};

use crate::extensions::fake_aggregator::{FakeAggregator, FakeCertificateVerifier};

#[tokio::test]
async fn cardano_transaction_v2_proof_get_validate() {
    let genesis_verification_key = fake_keys::genesis_verification_key()[0];

    let blocks_with_txs = [
        CardanoBlockWithTransactions::new(
            "block_hash-10",
            BlockNumber(10),
            SlotNumber(15),
            vec!["tx_hash-1", "tx_hash-2"],
        ),
        CardanoBlockWithTransactions::new(
            "block_hash-15",
            BlockNumber(15),
            SlotNumber(15),
            vec!["tx_hash-4"],
        ),
    ];
    let transaction_hashes = blocks_with_txs
        .iter()
        .flat_map(|b| b.transactions_hashes.clone())
        .collect::<Vec<_>>();

    let certificate_hash = "certificate_hash";
    let fake_aggregator = FakeAggregator::spawn_with_proofs_v2(&blocks_with_txs, certificate_hash);
    let client = ClientBuilder::new(AggregatorDiscoveryType::Url(
        fake_aggregator.server_root_url(),
    ))
    .set_genesis_verification_key(GenesisVerificationKey::JsonHex(
        genesis_verification_key.to_string(),
    ))
    .with_certificate_verifier(FakeCertificateVerifier::build_that_validate_any_certificate())
    .build()
    .expect("Should be able to create a Client");
    let cardano_transaction_v2_client = client.cardano_transaction_v2();

    // 1 - get list of set proofs for wanted tx & associated certificate hash
    let proofs = cardano_transaction_v2_client
        .get_proofs(&transaction_hashes)
        .await
        .expect("Getting proof for the transactions should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/proof/v2/cardano-transaction?transaction_hashes={}",
            transaction_hashes.join(","),
        ))
    );

    // 2 - verify the proofs
    let verified_transactions = proofs.verify().expect("Proofs should be valid");

    // 3 - validate certificate chain
    let certificate = client
        .certificate()
        .verify_chain(&proofs.certificate_hash)
        .await
        .expect("Validating the chain should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!("/certificate/{}", proofs.certificate_hash.clone()))
    );

    // 4 - validate that the verified transactions proof is signed by the certificate
    let message = MessageBuilder::new()
        .compute_cardano_transactions_proofs_v2_message(&certificate, &verified_transactions);

    assert!(
        certificate.match_message(&message),
        "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
        certificate.signed_message,
        message.compute_hash()
    );
}
