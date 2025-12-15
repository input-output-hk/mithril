use mithril_client::common::{BlockNumber, ProtocolMessagePartKey};
use mithril_client::{CardanoTransactionsProofs, CardanoTransactionsSetProof, MithrilCertificate};
use mithril_common::crypto_helper::{MKProof, ProtocolMkProof};
use mithril_common::test::crypto_helper::MKProofTestExtension;
use mithril_common::test::double::Dummy;

use super::FakeAggregator;
use crate::extensions::routes;

impl FakeAggregator {
    pub fn spawn_with_transactions_proofs(tx_hashes: &[&str], certificate_hash: &str) -> Self {
        let proof = MKProof::from_leaves(tx_hashes).unwrap();

        let proofs = CardanoTransactionsProofs {
            certificate_hash: certificate_hash.to_string(),
            certified_transactions: vec![CardanoTransactionsSetProof {
                transactions_hashes: tx_hashes.iter().map(|h| h.to_string()).collect(),
                proof: ProtocolMkProof::new(proof.to_owned().into()).to_json_hex().unwrap(),
            }],
            non_certified_transactions: vec![],
            latest_block_number: BlockNumber(9999),
        };

        let certificate = {
            let mut cert = MithrilCertificate {
                hash: certificate_hash.to_string(),
                ..MithrilCertificate::dummy()
            };
            cert.protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
                proof.root().to_hex(),
            );
            cert.protocol_message
                .set_message_part(ProtocolMessagePartKey::LatestBlockNumber, 9999.to_string());
            cert.signed_message = cert.protocol_message.compute_hash();
            cert
        };

        let router = routes::proof::routes(proofs)
            .merge(routes::certificate::routes(Vec::new(), certificate));

        Self::spawn_test_server_on_random_port(router)
    }
}
