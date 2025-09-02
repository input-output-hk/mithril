use std::sync::Arc;

use anyhow::Context;
use blake2::{Blake2b, Digest, digest::consts::U64};
use pallas_network::miniprotocols::localmsgsubmission::{DmqMsg, DmqMsgPayload};

use mithril_cardano_node_chain::chain_observer::ChainObserver;
use mithril_common::{
    StdResult,
    crypto_helper::{KesSigner, TryToBytes},
};

use crate::model::{DmqMessage, SystemUnixTimestampProvider, UnixTimestampProvider};

/// The TTL (Time To Live) for DMQ messages in seconds (default is 30 minutes).
const DMQ_MESSAGE_TTL_IN_SECONDS: u16 = 1800;

/// A builder for creating DMQ messages.
pub struct DmqMessageBuilder {
    kes_signer: Arc<dyn KesSigner>,
    chain_observer: Arc<dyn ChainObserver>,
    timestamp_provider: Arc<dyn UnixTimestampProvider>,
    ttl_seconds: u16,
}

impl DmqMessageBuilder {
    /// Creates a new instance of `DmqMessageBuilder`.
    pub fn new(kes_signer: Arc<dyn KesSigner>, chain_observer: Arc<dyn ChainObserver>) -> Self {
        Self {
            kes_signer,
            chain_observer,
            timestamp_provider: Arc::new(SystemUnixTimestampProvider),
            ttl_seconds: DMQ_MESSAGE_TTL_IN_SECONDS,
        }
    }

    /// Sets the timestamp provider for the DMQ message builder.
    pub fn set_timestamp_provider(
        mut self,
        timestamp_provider: Arc<dyn UnixTimestampProvider>,
    ) -> Self {
        self.timestamp_provider = timestamp_provider;
        self
    }

    /// Sets the TTL (Time To Live) for DMQ messages in seconds.
    pub fn set_ttl(mut self, ttl_seconds: u16) -> Self {
        self.ttl_seconds = ttl_seconds;

        self
    }

    /// Computes a message id for a DMQ message payload.
    fn compute_msg_id(dmq_message_payload: &DmqMsgPayload) -> Vec<u8> {
        let mut hasher = Blake2b::<U64>::new();
        hasher.update(&dmq_message_payload.msg_body);
        hasher.update(dmq_message_payload.kes_period.to_be_bytes());
        hasher.update(&dmq_message_payload.operational_certificate);
        hasher.update(&dmq_message_payload.cold_verification_key);
        hasher.update(dmq_message_payload.expires_at.to_be_bytes());

        hasher.finalize().to_vec()
    }

    /// Builds a DMQ message from the provided message bytes.
    pub async fn build(&self, message_bytes: &[u8]) -> StdResult<DmqMessage> {
        let expires_at: u32 = (self.timestamp_provider.current_timestamp()?
            + self.ttl_seconds as u64)
            .try_into()
            .with_context(|| "Failed to compute expires_at while building DMQ message")?;
        let kes_period = self
            .chain_observer
            .get_current_kes_period()
            .await
            .with_context(|| "Failed to get KES period while building DMQ message")?
            .unwrap_or_default();
        let (kes_signature, operational_certificate) = self
            .kes_signer
            .sign(message_bytes, kes_period)
            .with_context(|| "Failed to KES sign message while building DMQ message")?;

        let dmq_message = DmqMsg {
            msg_payload: {
                let mut dmq_message_payload = DmqMsgPayload {
                    msg_id: vec![],
                    msg_body: message_bytes.to_vec(),
                    kes_period: kes_period as u64,
                    operational_certificate: operational_certificate.to_bytes_vec()?, // TODO: remove the cold verification key in the op cert
                    cold_verification_key: vec![],
                    expires_at,
                };
                dmq_message_payload.msg_id = Self::compute_msg_id(&dmq_message_payload);

                dmq_message_payload
            },
            kes_signature: kes_signature.to_bytes_vec()?,
        };

        Ok(dmq_message.into())
    }
}

#[cfg(test)]
mod tests {
    use mithril_cardano_node_chain::test::double::FakeChainObserver;
    use mithril_common::{
        crypto_helper::TryToBytes,
        current_function,
        entities::{BlockNumber, ChainPoint, TimePoint},
        test::{crypto_helper::KesSignerFake, double::Dummy},
    };

    use crate::model::MockUnixTimestampProvider;

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
        let (kes_signature, operational_certificate) =
            KesSignerFake::dummy_signature(current_function!());
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
        let builder = DmqMessageBuilder::new(kes_signer, chain_observer)
            .set_ttl(1000)
            .set_timestamp_provider(Arc::new({
                let mut mock_timestamp_provider = MockUnixTimestampProvider::new();
                mock_timestamp_provider
                    .expect_current_timestamp()
                    .returning(|| Ok(234));

                mock_timestamp_provider
            }));
        let message = test_utils::TestMessage {
            content: b"test".to_vec(),
        };

        let dmq_message = builder.build(&message.to_bytes_vec().unwrap()).await.unwrap();

        let DmqMsg {
            msg_payload,
            kes_signature: _,
        } = &*dmq_message;
        assert_eq!(
            DmqMsg {
                msg_payload: DmqMsgPayload {
                    msg_id: DmqMessageBuilder::compute_msg_id(msg_payload),
                    msg_body: b"test".to_vec(),
                    kes_period: 0,
                    operational_certificate: operational_certificate.to_bytes_vec().unwrap(),
                    cold_verification_key: vec![], // TODO: fix
                    expires_at: 1234,
                },
                kes_signature: kes_signature.to_bytes_vec().unwrap(),
            },
            dmq_message.into()
        );
    }
}
