mod extensions;

use crate::extensions::fake::{FakeAggregator, FakeCertificateVerifier};
use mithril_client::{aggregator_client::AggregatorRequest, ClientBuilder, MessageBuilder};

#[tokio::test]
async fn cardano_transaction_proof_get_validate() {
    let genesis_verification_key =
        mithril_common::test_utils::fake_keys::genesis_verification_key()[0];

    let transactions_hashes = ["abc", "def"];
    let certificate_hash = "certificate_hash";
    let fake_aggregator = FakeAggregator::new();
    let test_http_server =
        fake_aggregator.spawn_with_transactions_proofs(&transactions_hashes, certificate_hash);
    let client = ClientBuilder::aggregator(&test_http_server.url(), genesis_verification_key)
        .with_certificate_verifier(FakeCertificateVerifier::build_that_validate_any_certificate())
        .build()
        .expect("Should be able to create a Client");
    let cardano_transaction_proof_client = client.cardano_transaction_proof();

    // 1 - get list of set proofs for wanted tx & associated certificate hash
    let proofs = cardano_transaction_proof_client
        .get_proofs(&transactions_hashes)
        .await
        .expect("Getting proof for the transactions should not fail");
    assert_eq!(
        fake_aggregator.get_last_call().await,
        Some(format!(
            "/{}",
            AggregatorRequest::GetTransactionsProofs {
                transactions_hashes: transactions_hashes.iter().map(|h| h.to_string()).collect(),
            }
            .route()
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
        Some(format!(
            "/{}",
            AggregatorRequest::GetCertificate {
                hash: proofs.certificate_hash.clone()
            }
            .route()
        ))
    );

    // 4 - validate that the verified transactions proof is signed by the certificate
    let message = MessageBuilder::new()
        .compute_cardano_transactions_proofs_message(&certificate, &verified_transactions);

    assert!(
        certificate.match_message(&message),
        "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
        certificate.signed_message,
        message.compute_hash()
    );
}
