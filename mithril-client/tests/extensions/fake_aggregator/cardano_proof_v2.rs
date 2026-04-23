use mithril_client::MithrilCertificate;
use mithril_client::common::ProtocolMessagePartKey;
use mithril_common::crypto_helper::{MKMap, MKTreeStoreInMemory};
use mithril_common::entities::CardanoBlockWithTransactions;
use mithril_common::test::crypto_helper::MKMapCardanoTestExtension;
use mithril_common::test::double::Dummy;

use crate::extensions::routes;

use super::FakeAggregator;

impl FakeAggregator {
    pub fn spawn_with_proofs_v2(
        blocks_with_txs: &[CardanoBlockWithTransactions],
        certificate_hash: &str,
    ) -> Self {
        let mk_map =
            MKMap::<_, _, MKTreeStoreInMemory>::from_blocks_with_transactions(blocks_with_txs)
                .unwrap();

        let blocks_hashes: Vec<_> = blocks_with_txs.iter().map(|b| &b.block_hash).collect();

        let transaction_hashes: Vec<_> =
            blocks_with_txs.iter().flat_map(|b| &b.transactions_hashes).collect();

        let transaction_proofs = mk_map
            .compute_proof_message_for_transactions_hashes(
                certificate_hash,
                &transaction_hashes,
                blocks_with_txs,
            )
            .unwrap();

        let block_proofs = mk_map
            .compute_proof_message_for_blocks_hashes(
                certificate_hash,
                &blocks_hashes,
                blocks_with_txs,
            )
            .unwrap();

        let verified_proof = transaction_proofs.verify().unwrap();

        let certificate = {
            let mut cert = MithrilCertificate {
                hash: certificate_hash.to_string(),
                ..MithrilCertificate::dummy()
            };
            cert.protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot,
                verified_proof.certified_merkle_root().to_string(),
            );
            cert.protocol_message.set_message_part(
                ProtocolMessagePartKey::LatestBlockNumber,
                verified_proof.latest_certified_block_number().to_string(),
            );
            cert.protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoBlocksTransactionsBlockNumberOffset,
                verified_proof.security_parameter().to_string(),
            );
            cert.signed_message = cert.protocol_message.compute_hash();
            cert
        };

        let router = routes::proof_v2::routes(transaction_proofs, block_proofs)
            .merge(routes::certificate::routes(Vec::new(), certificate));

        Self::spawn_test_server_on_random_port(router)
    }
}
