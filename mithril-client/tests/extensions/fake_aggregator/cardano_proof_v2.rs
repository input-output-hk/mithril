use mithril_client::common::{BlockNumber, ProtocolMessagePartKey};
use mithril_client::{CardanoBlocksProofs, CardanoTransactionsProofsV2, MithrilCertificate};
use mithril_common::crypto_helper::MKTreeStoreInMemory;
use mithril_common::entities::{CardanoBlock, CardanoTransaction, MkSetProof};
use mithril_common::test::double::Dummy;
use mithril_common::test::entities_extensions::MkSetProofTestExtension;

use crate::extensions::routes;

use super::FakeAggregator;

impl FakeAggregator {
    pub fn spawn_with_proofs_v2(
        blocks: &[CardanoBlock],
        transactions: &[CardanoTransaction],
        certificate_hash: &str,
    ) -> Self {
        let mk_set_transaction_proof =
            MkSetProof::<CardanoTransaction>::from_leaves::<MKTreeStoreInMemory>(transactions)
                .unwrap();
        let transaction_proofs = CardanoTransactionsProofsV2 {
            certificate_hash: certificate_hash.to_string(),
            certified_transactions: Some(mk_set_transaction_proof.clone().try_into().unwrap()),
            non_certified_transactions: vec![],
            latest_block_number: BlockNumber(9999),
        };

        let mk_set_blocks_proof =
            MkSetProof::<CardanoBlock>::from_leaves::<MKTreeStoreInMemory>(blocks).unwrap();
        let block_proofs = CardanoBlocksProofs {
            certificate_hash: certificate_hash.to_string(),
            certified_blocks: Some(mk_set_blocks_proof.try_into().unwrap()),
            non_certified_blocks: vec![],
            latest_block_number: BlockNumber(9999),
        };

        let certificate = {
            let mut cert = MithrilCertificate {
                hash: certificate_hash.to_string(),
                ..MithrilCertificate::dummy()
            };
            cert.protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot,
                mk_set_transaction_proof.merkle_root(),
            );
            cert.protocol_message
                .set_message_part(ProtocolMessagePartKey::LatestBlockNumber, 9999.to_string());
            cert.signed_message = cert.protocol_message.compute_hash();
            cert
        };

        let router = routes::proof_v2::routes(transaction_proofs, block_proofs)
            .merge(routes::certificate::routes(Vec::new(), certificate));

        Self::spawn_test_server_on_random_port(router)
    }
}
