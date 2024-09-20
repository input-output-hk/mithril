use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    digesters::ImmutableDigester,
    entities::{CardanoDbBeacon, ProtocolMessage, ProtocolMessagePartKey},
    logging::LoggerExtensions,
    signable_builder::SignableBuilder,
    StdResult,
};
use anyhow::Context;
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
            logger: logger.new_with_component_name::<Self>(),
            dirpath: dirpath.to_owned(),
        }
    }
}

#[async_trait]
impl SignableBuilder<CardanoDbBeacon> for CardanoImmutableFilesFullSignableBuilder {
    async fn compute_protocol_message(
        &self,
        beacon: CardanoDbBeacon,
        seed_protocol_message: ProtocolMessage,
    ) -> StdResult<ProtocolMessage> {
        debug!(self.logger, "compute_signable({beacon:?})");
        let digest = self
            .immutable_digester
            .compute_digest(&self.dirpath, &beacon)
            .await
            .with_context(|| {
                format!(
                    "Cardano Immutable Files Full Signable Builder can not compute digest of '{}'",
                    &self.dirpath.display()
                )
            })?;
        info!(self.logger, "digest = '{digest}'.");
        let mut protocol_message = seed_protocol_message;
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use async_trait::async_trait;
    use std::path::Path;

    use crate::digesters::{ImmutableDigester, ImmutableDigesterError};
    use crate::entities::CardanoDbBeacon;
    use crate::test_utils::TestLogger;

    use super::*;

    #[derive(Default)]
    pub struct ImmutableDigesterImpl;

    #[async_trait]
    impl ImmutableDigester for ImmutableDigesterImpl {
        async fn compute_digest(
            &self,
            _dirpath: &Path,
            beacon: &CardanoDbBeacon,
        ) -> Result<String, ImmutableDigesterError> {
            Ok(format!("immutable {}", beacon.immutable_file_number))
        }
    }

    #[tokio::test]
    async fn compute_signable() {
        let digester = ImmutableDigesterImpl;
        let signable_builder = CardanoImmutableFilesFullSignableBuilder::new(
            Arc::new(digester),
            Path::new(""),
            TestLogger::stdout(),
        );
        let seed_protocol_message = ProtocolMessage::new();
        let protocol_message = signable_builder
            .compute_protocol_message(CardanoDbBeacon::default(), seed_protocol_message)
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
