use anyhow::Context;
use slog::{Logger, o};
#[cfg(feature = "fs")]
use std::path::Path;
#[cfg(feature = "fs")]
use std::sync::Arc;

#[cfg(feature = "fs")]
use crate::cardano_database_client::{ImmutableFileRange, VerifiedDigests};
#[cfg(feature = "fs")]
use mithril_cardano_node_internal_database::digesters::{
    CardanoImmutableDigester, ImmutableDigester,
};
use mithril_common::protocol::SignerBuilder;
use mithril_common::signable_builder::CardanoStakeDistributionSignableBuilder;
#[cfg(feature = "fs")]
use mithril_common::{
    crypto_helper::MKTreeNode, entities::SignedEntityType, messages::CertificateMessage,
};
use mithril_common::{logging::LoggerExtensions, messages::CardanoDatabaseSnapshotMessage};

use crate::{
    CardanoStakeDistribution, MithrilCertificate, MithrilResult, MithrilSigner,
    MithrilStakeDistribution, VerifiedCardanoTransactions,
    common::{ProtocolMessage, ProtocolMessagePartKey},
};

/// A [MessageBuilder] can be used to compute the message of Mithril artifacts.
pub struct MessageBuilder {
    #[cfg(feature = "fs")]
    immutable_digester: Option<Arc<dyn ImmutableDigester>>,
    logger: Logger,
}

impl MessageBuilder {
    /// Constructs a new `MessageBuilder`.
    pub fn new() -> MessageBuilder {
        let logger = Logger::root(slog::Discard, o!());
        Self {
            #[cfg(feature = "fs")]
            immutable_digester: None,
            logger,
        }
    }

    /// Set the [Logger] to use.
    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = logger.new_with_component_name::<Self>();
        self
    }

    cfg_fs! {
        fn get_immutable_digester(&self, network: &str) -> Arc<dyn ImmutableDigester> {
            match self.immutable_digester.as_ref() {
                None => Arc::new(CardanoImmutableDigester::new(network.to_owned(),None, self.logger.clone())),
                Some(digester) => digester.clone(),
            }
        }

        /// Set the [ImmutableDigester] to be used for the message computation for snapshot.
        ///
        /// If not set a default implementation will be used.
        pub fn with_immutable_digester(
            mut self,
            immutable_digester: Arc<dyn ImmutableDigester>,
        ) -> Self {
            self.immutable_digester = Some(immutable_digester);
            self
        }

        /// Compute message for a snapshot (based on the directory where it was unpacked).
        ///
        /// Warning: this operation can be quite long depending on the snapshot size.
        pub async fn compute_snapshot_message(
            &self,
            snapshot_certificate: &MithrilCertificate,
            unpacked_snapshot_directory: &Path,
        ) -> MithrilResult<ProtocolMessage> {
            let digester = self.get_immutable_digester(&snapshot_certificate.metadata.network);
            let beacon =
                match &snapshot_certificate.signed_entity_type {
                SignedEntityType::CardanoImmutableFilesFull(beacon) => {Ok(beacon)},
                other => {
                    Err(anyhow::anyhow!(
                    "Can't compute message: Given certificate `{}` does not certify a snapshot, certificate signed entity: {:?}",
                    snapshot_certificate.hash,
                    other
                        )
                    )},
            }?;

            let mut message = snapshot_certificate.protocol_message.clone();

            let digest = digester
                .compute_digest(unpacked_snapshot_directory, &beacon.clone())
                .await
                .with_context(|| {
                    format!(
                        "Snapshot digest computation failed: unpacked_dir: '{}'",
                        unpacked_snapshot_directory.display()
                    )
                })?;
            message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);

            Ok(message)
        }

        /// Compute message for a Cardano database.
        pub async fn compute_cardano_database_message(
            &self,
            certificate: &CertificateMessage,
            cardano_database_snapshot: &CardanoDatabaseSnapshotMessage,
            immutable_file_range: &ImmutableFileRange,
            database_dir: &Path,
            verified_digests: &VerifiedDigests,
        ) -> MithrilResult<ProtocolMessage> {
            let network = certificate.metadata.network.clone();
            let immutable_file_number_range = immutable_file_range
                .to_range_inclusive(cardano_database_snapshot.beacon.immutable_file_number)?;
            let immutable_digester = CardanoImmutableDigester::new(network, None, self.logger.clone());
            let computed_digests = immutable_digester
                .compute_digests_for_range(database_dir, &immutable_file_number_range)
                .await?
                .entries
                .values()
                .map(MKTreeNode::from)
                .collect::<Vec<_>>();

            let merkle_proof = verified_digests.merkle_tree.compute_proof(&computed_digests)?;
            merkle_proof
                .verify()
                .with_context(|| "Merkle proof verification failed")?;

            let mut message = certificate.protocol_message.clone();
            message.set_message_part(
                ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
                merkle_proof.root().to_hex(),
            );

            Ok(message)
        }
    }

    /// Compute message for a Mithril stake distribution.
    pub fn compute_mithril_stake_distribution_message(
        &self,
        certificate: &MithrilCertificate,
        mithril_stake_distribution: &MithrilStakeDistribution,
    ) -> MithrilResult<ProtocolMessage> {
        let signers =
            MithrilSigner::try_into_signers(mithril_stake_distribution.signers_with_stake.clone())
                .with_context(|| "Could not compute message: conversion failure")?;

        let signer_builder =
            SignerBuilder::new(&signers, &mithril_stake_distribution.protocol_parameters)
                .with_context(
                    || "Could not compute message: aggregate verification key computation failed",
                )?;

        let avk = signer_builder
            .compute_aggregate_verification_key()
            .to_json_hex()
            .with_context(
                || "Could not compute message: aggregate verification key encoding failed",
            )?;

        let mut message = certificate.protocol_message.clone();
        message.set_message_part(ProtocolMessagePartKey::NextAggregateVerificationKey, avk);

        Ok(message)
    }

    /// Compute message for a Cardano Transactions Proofs.
    pub fn compute_cardano_transactions_proofs_message(
        &self,
        transactions_proofs_certificate: &MithrilCertificate,
        verified_transactions: &VerifiedCardanoTransactions,
    ) -> ProtocolMessage {
        let mut message = transactions_proofs_certificate.protocol_message.clone();
        verified_transactions.fill_protocol_message(&mut message);
        message
    }

    /// Compute message for a Cardano stake distribution.
    pub fn compute_cardano_stake_distribution_message(
        &self,
        certificate: &MithrilCertificate,
        cardano_stake_distribution: &CardanoStakeDistribution,
    ) -> MithrilResult<ProtocolMessage> {
        let mk_tree =
            CardanoStakeDistributionSignableBuilder::compute_merkle_tree_from_stake_distribution(
                cardano_stake_distribution.stake_distribution.clone(),
            )?;

        let mut message = certificate.protocol_message.clone();
        message.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionEpoch,
            cardano_stake_distribution.epoch.to_string(),
        );
        message.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionMerkleRoot,
            mk_tree.compute_root()?.to_hex(),
        );

        Ok(message)
    }
}

impl Default for MessageBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {

    use std::ops::RangeInclusive;
    use std::{collections::BTreeMap, path::PathBuf};

    use mithril_cardano_node_internal_database::test::DummyCardanoDbBuilder;
    use mithril_common::{
        entities::{CardanoDbBeacon, Epoch, ImmutableFileNumber},
        messages::CertificateMessage,
    };

    use crate::cardano_database_client::ImmutableFileRange;
    use crate::{cardano_database_client::VerifiedDigests, test_utils::TestLogger};

    use super::*;

    async fn prepare_db_and_verified_digests(
        dir_name: &str,
        beacon: &CardanoDbBeacon,
        immutable_file_range: &RangeInclusive<ImmutableFileNumber>,
    ) -> (PathBuf, CertificateMessage, VerifiedDigests) {
        let cardano_db = DummyCardanoDbBuilder::new(dir_name)
            .with_immutables(&immutable_file_range.clone().collect::<Vec<_>>())
            .append_immutable_trio()
            .build();
        let database_dir = cardano_db.get_dir();
        let immutable_digester =
            CardanoImmutableDigester::new("whatever".to_string(), None, TestLogger::stdout());
        let computed_digests = immutable_digester
            .compute_digests_for_range(database_dir, immutable_file_range)
            .await
            .unwrap();

        let digests = computed_digests
            .entries
            .iter()
            .map(|(immutable_file, digest)| (immutable_file.filename.clone(), digest.clone()))
            .collect::<BTreeMap<_, _>>();

        let merkle_tree = immutable_digester
            .compute_merkle_tree(database_dir, beacon)
            .await
            .unwrap();

        let verified_digests = VerifiedDigests {
            digests,
            merkle_tree,
        };

        let certificate = {
            let protocol_message_merkle_root =
                verified_digests.merkle_tree.compute_root().unwrap().to_hex();
            let mut protocol_message = ProtocolMessage::new();
            protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
                protocol_message_merkle_root,
            );

            CertificateMessage {
                protocol_message: protocol_message.clone(),
                signed_message: protocol_message.compute_hash(),
                ..CertificateMessage::dummy()
            }
        };

        (database_dir.to_owned(), certificate, verified_digests)
    }

    #[tokio::test]
    async fn compute_cardano_database_message_succeeds() {
        let beacon = CardanoDbBeacon {
            epoch: Epoch(123),
            immutable_file_number: 10,
        };
        let immutable_file_range = 1..=15;
        let immutable_file_range_to_prove = ImmutableFileRange::Range(2, 4);
        let (database_dir, certificate, verified_digests) = prepare_db_and_verified_digests(
            "compute_cardano_database_message_succeeds",
            &beacon,
            &immutable_file_range,
        )
        .await;

        let message = MessageBuilder::new()
            .compute_cardano_database_message(
                &certificate,
                &CardanoDatabaseSnapshotMessage::dummy(),
                &immutable_file_range_to_prove,
                &database_dir,
                &verified_digests,
            )
            .await
            .unwrap();

        assert!(certificate.match_message(&message));
    }
}
