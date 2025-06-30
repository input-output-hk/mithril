use std::sync::Arc;

use anyhow::{anyhow, Context};
use blake2::{digest::consts::U64, Blake2b, Digest};
use pallas_network::miniprotocols::localmsgsubmission::DmqMsg;

use mithril_cardano_node_chain::chain_observer::ChainObserver;
use mithril_common::{
    crypto_helper::{KesSigner, TryToBytes},
    StdResult,
};

/// The TTL (Time To Live) for DMQ messages in blocks.
const DMQ_MESSAGE_TTL_IN_BLOCKS: u16 = 100;

/// A builder for creating DMQ messages.
pub struct DmqMessageBuilder {
    kes_signer: Arc<dyn KesSigner>,
    chain_observer: Arc<dyn ChainObserver>,
    ttl_blocks: u16,
}

impl DmqMessageBuilder {
    /// Creates a new instance of `DmqMessageBuilder`.
    pub fn new(kes_signer: Arc<dyn KesSigner>, chain_observer: Arc<dyn ChainObserver>) -> Self {
        Self {
            kes_signer,
            chain_observer,
            ttl_blocks: DMQ_MESSAGE_TTL_IN_BLOCKS,
        }
    }

    /// Set the TTL (Time To Live) for DMQ messages in blocks.
    pub fn set_ttl(mut self, ttl_blocks: u16) -> Self {
        self.ttl_blocks = ttl_blocks;

        self
    }

    /// Builds a DMQ message from the provided message bytes.
    pub async fn build(&self, message_bytes: &[u8]) -> StdResult<DmqMsg> {
        fn compute_msg_id(dmq_message: &DmqMsg) -> Vec<u8> {
            let mut hasher = Blake2b::<U64>::new();
            hasher.update(&dmq_message.msg_body);
            hasher.update(dmq_message.block_number.to_be_bytes());
            hasher.update(dmq_message.ttl.to_be_bytes());
            hasher.update(&dmq_message.kes_signature);
            hasher.update(&dmq_message.operational_certificate);

            hasher.finalize().to_vec()
        }

        let block_number = self
            .chain_observer
            .get_current_chain_point()
            .await
            .with_context(|| "Failed to get current chain point while building DMQ message")?
            .ok_or(anyhow!(
                "No current chain point available while building DMQ message"
            ))?
            .block_number;
        let block_number = (*block_number)
            .try_into()
            .with_context(|| "Failed to convert block number to u32")?;
        let (kes_signature, operational_certificate) = self
            .kes_signer
            .sign(message_bytes, block_number)
            .with_context(|| "Failed to KES sign message while building DMQ message")?;
        let mut dmq_message = DmqMsg {
            msg_id: vec![],
            msg_body: message_bytes.to_vec(),
            block_number,
            ttl: self.ttl_blocks,
            kes_signature: kes_signature.to_bytes_vec()?,
            operational_certificate: operational_certificate.to_bytes_vec()?,
        };
        dmq_message.msg_id = compute_msg_id(&dmq_message);

        Ok(dmq_message)
    }
}

#[cfg(test)]
mod tests {
    use mithril_cardano_node_chain::test::double::FakeChainObserver;
    use mithril_common::{
        crypto_helper::{KesSignerFake, TryToBytes},
        entities::{BlockNumber, ChainPoint, TimePoint},
    };

    use super::*;

    mod test_utils {
        use super::*;

        pub(super) struct TestMessage {
            pub(super) content: Vec<u8>,
        }

        impl TryToBytes for TestMessage {
            fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
                Ok(self.content.clone())
            }
        }
    }

    #[tokio::test]
    async fn test_build_dmq_message() {
        let (kes_signature, operational_certificate) = KesSignerFake::dummy_signature();
        let kes_signer = Arc::new(KesSignerFake::new(vec![Ok((
            kes_signature,
            operational_certificate.clone(),
        ))]));
        let chain_observer = Arc::new(FakeChainObserver::new(Some(TimePoint {
            chain_point: ChainPoint {
                block_number: BlockNumber(123),
                ..ChainPoint::dummy()
            },
            ..TimePoint::dummy()
        })));
        let builder = DmqMessageBuilder::new(kes_signer, chain_observer).set_ttl(100);
        let message = test_utils::TestMessage {
            content: b"test".to_vec(),
        };

        let dmq_message = builder
            .build(&message.to_bytes_vec().unwrap())
            .await
            .unwrap();

        assert!(!dmq_message.msg_id.is_empty());
        assert_eq!(
            DmqMsg {
                msg_id: vec![],
                msg_body: b"test".to_vec(),
                block_number: 123,
                ttl: 100,
                kes_signature: kes_signature.to_bytes_vec().unwrap(),
                operational_certificate: operational_certificate.to_bytes_vec().unwrap(),
            },
            DmqMsg {
                msg_id: vec![],
                ..dmq_message
            }
        );
    }
}
