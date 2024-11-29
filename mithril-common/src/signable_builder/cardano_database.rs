use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use async_trait::async_trait;
use slog::{info, Logger};

use crate::{
    digesters::ImmutableDigester,
    entities::{CardanoDbBeacon, ProtocolMessage, ProtocolMessagePartKey},
    logging::LoggerExtensions,
    signable_builder::SignableBuilder,
    StdResult,
};

/// This structure is responsible for calculating the message for incremental Cardano database.
pub struct CardanoDatabaseSignableBuilder {
    digester: Arc<dyn ImmutableDigester>,
    logger: Logger,
    dirpath: PathBuf,
}

impl CardanoDatabaseSignableBuilder {
    /// Constructor
    pub fn new(digester: Arc<dyn ImmutableDigester>, dirpath: &Path, logger: Logger) -> Self {
        Self {
            digester,
            logger: logger.new_with_component_name::<Self>(),
            dirpath: dirpath.to_owned(),
        }
    }
}

#[async_trait]
impl SignableBuilder<CardanoDbBeacon> for CardanoDatabaseSignableBuilder {
    async fn compute_protocol_message(
        &self,
        beacon: CardanoDbBeacon,
    ) -> StdResult<ProtocolMessage> {
        let merkle_tree = self
            .digester
            .compute_merkle_tree(&self.dirpath, &beacon)
            .await
            .with_context(|| {
                format!(
                    "Cardano Database Signable Builder can not compute merkle tree of '{}'",
                    &self.dirpath.display()
                )
            })?;

        let merkle_root = merkle_tree.compute_root()?.to_hex();
        info!(
            self.logger,
            "Computed Cardano database Merkle root = '{merkle_root}'"
        );

        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
            merkle_root,
        );

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{
        crypto_helper::{MKTree, MKTreeStoreInMemory},
        digesters::DumbImmutableDigester,
        entities::{CardanoDbBeacon, ProtocolMessagePartKey},
        test_utils::TestLogger,
    };

    use super::*;

    #[tokio::test]
    async fn compute_signable() {
        let digests = vec!["digest-1".to_string(), "digest-2".to_string()];
        let digester = DumbImmutableDigester::default().with_merkle_tree(digests.clone());
        let signable_builder = CardanoDatabaseSignableBuilder::new(
            Arc::new(digester),
            Path::new(""),
            TestLogger::stdout(),
        );

        let protocol_message = signable_builder
            .compute_protocol_message(CardanoDbBeacon::default())
            .await
            .unwrap();

        let expected_mktree: MKTree<MKTreeStoreInMemory> = MKTree::new(&digests).unwrap();
        let mut expected_message = ProtocolMessage::new();
        expected_message.set_message_part(
            ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
            expected_mktree.compute_root().unwrap().to_hex(),
        );
        assert_eq!(expected_message, protocol_message);
    }
}
