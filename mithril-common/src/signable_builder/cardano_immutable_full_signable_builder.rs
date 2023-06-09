use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    digesters::ImmutableDigester,
    entities::{Beacon, ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::SignableBuilder,
    StdResult,
};
use async_trait::async_trait;
use slog::{debug, info, Logger};

/// This structure is responsible of calculating the message for Cardano immutable files snapshots.
pub struct CardanoImmutableFilesFullSignableBuilder {
    immutable_digester: Arc<dyn ImmutableDigester>,
    logger: Logger,
    dirpath: PathBuf,
}

impl CardanoImmutableFilesFullSignableBuilder {
    /// Constructor
    pub fn new(
        immutable_digester: Arc<dyn ImmutableDigester>,
        dirpath: &Path,
        logger: Logger,
    ) -> Self {
        Self {
            immutable_digester,
            logger,
            dirpath: dirpath.to_owned(),
        }
    }
}

#[async_trait]
impl SignableBuilder<Beacon> for CardanoImmutableFilesFullSignableBuilder {
    async fn compute_protocol_message(&self, beacon: Beacon) -> StdResult<ProtocolMessage> {
        debug!(self.logger, "SignableBuilder::compute_signable({beacon:?})");
        let digest = self
            .immutable_digester
            .compute_digest(&self.dirpath, &beacon)
            .await?;
        info!(self.logger, "SignableBuilder: digest = '{digest}'.");
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    use crate::digesters::{ImmutableDigester, ImmutableDigesterError};
    use crate::entities::Beacon;
    use async_trait::async_trait;
    use slog::Drain;

    #[derive(Default)]
    pub struct ImmutableDigesterImpl;

    #[async_trait]
    impl ImmutableDigester for ImmutableDigesterImpl {
        async fn compute_digest(
            &self,
            _dirpath: &Path,
            beacon: &Beacon,
        ) -> Result<String, ImmutableDigesterError> {
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
        let signable_builder = CardanoImmutableFilesFullSignableBuilder::new(
            Arc::new(digester),
            Path::new(""),
            create_logger(),
        );
        let protocol_message = signable_builder
            .compute_protocol_message(Beacon::default())
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
