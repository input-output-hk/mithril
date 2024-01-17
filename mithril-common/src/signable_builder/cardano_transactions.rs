use async_trait::async_trait;

use crate::{
    entities::{Beacon, ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::SignableBuilder,
    StdResult,
};

/// A [CardanoTransactionsSignableBuilder] builder
#[derive(Default)]
pub struct CardanoTransactionsSignableBuilder {}

#[async_trait]
impl SignableBuilder<Beacon> for CardanoTransactionsSignableBuilder {
    // TODO: return a protocol message computed from the transactions when it's ready to be implemented
    async fn compute_protocol_message(&self, beacon: Beacon) -> StdResult<ProtocolMessage> {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            format!("{beacon}"),
        );

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_compute_signable() {
        let beacon = Beacon::default();
        let cardano_transactions_signable_builder = CardanoTransactionsSignableBuilder::default();
        let signable = cardano_transactions_signable_builder
            .compute_protocol_message(beacon.clone())
            .await
            .unwrap();
        let mut signable_expected = ProtocolMessage::new();
        signable_expected.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            format!("{beacon}"),
        );
        assert_eq!(signable_expected, signable);
    }
}
