use std::sync::Arc;

use async_trait::async_trait;
use mithril_common::{
    digesters::ImmutableDigester,
    entities::{Beacon, ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::SignableBuilder,
    StdResult,
};

pub struct ImmutableSignableBuilder {
    immutable_digester: Arc<dyn ImmutableDigester>,
}

impl ImmutableSignableBuilder {
    /// Constructor
    pub fn new(immutable_digester: Arc<dyn ImmutableDigester>) -> Self {
        Self { immutable_digester }
    }
}

#[async_trait]
impl SignableBuilder<Beacon, ProtocolMessage> for ImmutableSignableBuilder {
    async fn compute_signable(&self, beacon: Beacon) -> StdResult<ProtocolMessage> {
        let digest = self.immutable_digester.compute_digest(&beacon).await?;
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use async_trait::async_trait;
    use mithril_common::digesters::{ImmutableDigester, ImmutableDigesterError};
    use mithril_common::entities::Beacon;

    #[derive(Default)]
    pub struct ImmutableDigesterImpl;

    #[async_trait]
    impl ImmutableDigester for ImmutableDigesterImpl {
        async fn compute_digest(&self, beacon: &Beacon) -> Result<String, ImmutableDigesterError> {
            Ok(format!("immutable {}", beacon.immutable_file_number))
        }
    }
    #[tokio::test]
    async fn compute_signable() {
        let digester = ImmutableDigesterImpl::default();
        let signable_builder = ImmutableSignableBuilder::new(Arc::new(digester));
        let protocol_message = signable_builder
            .compute_signable(Beacon::default())
            .await
            .unwrap();

        assert_eq!(
            &"immutable 0".to_string(),
            protocol_message
                .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
                .unwrap()
        );
    }
}
