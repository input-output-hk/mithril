use std::sync::Arc;

use anyhow::{anyhow, Context};
use blake2::{digest::consts::U64, Blake2b, Digest};
use pallas_network::miniprotocols::localmsgsubmission::DmqMsg;

use mithril_cardano_node_chain::chain_observer::ChainObserver;
use mithril_common::StdResult;

/// The TTL (Time To Live) for DMQ messages in blocks.
const DMQ_MESSAGE_TTL_IN_BLOCKS: u16 = 100;

/// A builder for creating DMQ messages.
pub struct DmqMessageBuilder {
    chain_observer: Arc<dyn ChainObserver>,
    ttl_blocks: u16,
}

impl DmqMessageBuilder {
    /// Creates a new instance of `DmqMessageBuilder`.
    pub fn new(chain_observer: Arc<dyn ChainObserver>, ttl_blocks: u16) -> Self {
        Self {
            chain_observer,
            ttl_blocks,
        }
    }

    /// Creates a new instance of `DmqMessageBuilder` with default TTL.
    pub fn new_with_default_ttl(chain_observer: Arc<dyn ChainObserver>) -> Self {
        Self {
            chain_observer,
            ttl_blocks: DMQ_MESSAGE_TTL_IN_BLOCKS,
        }
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
            .map_err(|_| anyhow!("Failed to convert block number to u32"))?;
        let kes_signature = vec![]; // TO DO: create a KES signature
        let operational_certificate = vec![]; // TO DO: create an operational certificate
        let mut dmq_message = DmqMsg {
            msg_id: vec![],
            msg_body: message_bytes.to_vec(),
            block_number,
            ttl: self.ttl_blocks,
            kes_signature,
            operational_certificate,
        };
        dmq_message.msg_id = compute_msg_id(&dmq_message);

        Ok(dmq_message)
    }
}

#[cfg(test)]
mod tests {
    use mithril_cardano_node_chain::test::double::FakeChainObserver;
    use mithril_common::{
        crypto_helper::TryToBytes,
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
        let chain_observer = Arc::new(FakeChainObserver::new(Some(TimePoint {
            chain_point: ChainPoint {
                block_number: BlockNumber(123),
                ..ChainPoint::dummy()
            },
            ..TimePoint::dummy()
        })));
        let builder = DmqMessageBuilder::new(chain_observer, 100);
        let message = test_utils::TestMessage {
            content: b"test".to_vec(),
        };

        let dmq_message = builder
            .build(&message.to_bytes_vec().unwrap())
            .await
            .unwrap();

        assert_eq!(
            DmqMsg {
                msg_id: vec![
                    26, 113, 171, 177, 174, 241, 244, 241, 209, 92, 210, 7, 119, 105, 94, 133, 93,
                    62, 82, 95, 91, 221, 146, 174, 201, 190, 140, 1, 217, 240, 228, 203, 14, 50,
                    104, 59, 252, 216, 26, 84, 231, 142, 163, 140, 11, 95, 17, 234, 242, 39, 230,
                    160, 194, 219, 128, 42, 53, 125, 218, 48, 209, 3, 210, 154
                ],
                msg_body: vec![116, 101, 115, 116],
                block_number: 123,
                ttl: 100,
                kes_signature: vec![],
                operational_certificate: vec![],
            },
            dmq_message
        );
    }
}
