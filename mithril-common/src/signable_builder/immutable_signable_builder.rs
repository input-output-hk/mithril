use std::sync::Arc;

use crate::{
    digesters::ImmutableDigester,
    entities::{Beacon, ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::SignableBuilder,
    StdResult,
};
use async_trait::async_trait;
use slog::{debug, info, Logger};

/// This structure is responsible of calculating the message for Cardano immutable files snapshots.
pub struct ImmutableSignableBuilder {
    immutable_digester: Arc<dyn ImmutableDigester>,
    logger: Logger,
}

impl ImmutableSignableBuilder {
    /// Constructor
    pub fn new(immutable_digester: Arc<dyn ImmutableDigester>, logger: Logger) -> Self {
        Self {
            immutable_digester,
            logger,
        }
    }
}

#[async_trait]
impl SignableBuilder<Beacon, ProtocolMessage> for ImmutableSignableBuilder {
    async fn compute_signable(&self, beacon: Beacon) -> StdResult<ProtocolMessage> {
        debug!(self.logger, "SignableBuilder::compute_signable({beacon:?})");
        let digest = self.immutable_digester.compute_digest(&beacon).await?;
        info!(self.logger, "SignableBuilder: digest = '{digest}'.");
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::digesters::{ImmutableDigester, ImmutableDigesterError};
    use crate::entities::Beacon;
    use async_trait::async_trait;
    use slog::Drain;

    #[derive(Default)]
    pub struct ImmutableDigesterImpl;

    #[async_trait]
    impl ImmutableDigester for ImmutableDigesterImpl {
        async fn compute_digest(&self, beacon: &Beacon) -> Result<String, ImmutableDigesterError> {
            Ok(format!("immutable {}", beacon.immutable_file_number))
        }
    }

    fn create_logger() -> slog::Logger {
        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        slog::Logger::root(Arc::new(drain), slog::o!())
    }
    #[tokio::test]
    async fn compute_signable() {
        let digester = ImmutableDigesterImpl::default();
        let signable_builder = ImmutableSignableBuilder::new(Arc::new(digester), create_logger());
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
