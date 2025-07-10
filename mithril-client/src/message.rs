use anyhow::Context;
use slog::{Logger, o};
#[cfg(feature = "fs")]
use std::fmt;
#[cfg(feature = "fs")]
use std::ops::RangeInclusive;
#[cfg(feature = "fs")]
use std::path::Path;
use std::path::PathBuf;
#[cfg(feature = "fs")]
use std::sync::Arc;
#[cfg(feature = "fs")]
use thiserror::Error;

#[cfg(feature = "fs")]
use crate::cardano_database_client::{ImmutableFileRange, VerifiedDigests};
#[cfg(feature = "fs")]
use mithril_cardano_node_internal_database::{
    IMMUTABLE_DIR,
    digesters::{CardanoImmutableDigester, ImmutableDigester, ImmutableDigesterError},
};
use mithril_common::protocol::SignerBuilder;
use mithril_common::signable_builder::CardanoStakeDistributionSignableBuilder;
#[cfg(feature = "fs")]
use mithril_common::{
    crypto_helper::MKTreeNode,
    entities::{ImmutableFileName, ImmutableFileNumber, SignedEntityType},
    messages::CertificateMessage,
};
use mithril_common::{logging::LoggerExtensions, messages::CardanoDatabaseSnapshotMessage};

#[cfg(feature = "fs")]
use crate::MithrilError;
use crate::{
    CardanoStakeDistribution, MithrilCertificate, MithrilResult, MithrilSigner,
    MithrilStakeDistribution, VerifiedCardanoTransactions,
    common::{ProtocolMessage, ProtocolMessagePartKey},
};

#[derive(Debug, PartialEq)]
pub struct ImmutableFilesLists {
    pub dir_path: PathBuf,
    pub missing: Vec<ImmutableFileName>,
    pub tampered: Vec<ImmutableFileName>,
}

#[derive(Error, Debug)]
pub enum ComputeCardanoDatabaseMessageError {
    ImmutableFilesVerification(ImmutableFilesLists),

    ImmutableFilesDigester(#[from] ImmutableDigesterError),

    MerkleProofVerification(#[source] MithrilError),

    ImmutableFilesRange(#[source] MithrilError),
}

impl fmt::Display for ComputeCardanoDatabaseMessageError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComputeCardanoDatabaseMessageError::ImmutableFilesVerification(files) => {
                write!(f, "immutable files: {:?}", files)
            }
            ComputeCardanoDatabaseMessageError::ImmutableFilesDigester(e) => {
                write!(f, "Immutable files digester error: {}", e)
            }
            ComputeCardanoDatabaseMessageError::MerkleProofVerification(e) => {
                write!(f, "Merkle proof verification error: {}", e)
            }
            ComputeCardanoDatabaseMessageError::ImmutableFilesRange(e) => {
                write!(f, "Immutable files range error: {}", e)
            }
        }
    }
}

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

        fn immutable_dir(db_dir: &Path) -> PathBuf {
            db_dir.join(IMMUTABLE_DIR)
        }

        fn list_missing_immutable_files(
            database_dir: &Path,
            immutable_file_number_range: &RangeInclusive<ImmutableFileNumber>,
        ) -> Vec<ImmutableFileName> {
            let immutable_dir = Self::immutable_dir(database_dir);
            let mut missing_files = Vec::new();

            for immutable_file_number in immutable_file_number_range.clone() {
                for immutable_type in ["chunk", "primary", "secondary"] {
                    let file_name = format!("{immutable_file_number:05}.{immutable_type}");
                    if !immutable_dir.join(&file_name).exists() {
                        missing_files.push(ImmutableFileName::from(file_name));
                    }
                }
            }

            missing_files
        }

        /// Compute message for a Cardano database.
        pub async fn compute_cardano_database_message(
            &self,
            certificate: &CertificateMessage,
            cardano_database_snapshot: &CardanoDatabaseSnapshotMessage,
            immutable_file_range: &ImmutableFileRange,
            database_dir: &Path,
            verified_digests: &VerifiedDigests,
        ) -> Result<ProtocolMessage, ComputeCardanoDatabaseMessageError> {
            let network = certificate.metadata.network.clone();
            let immutable_file_number_range = immutable_file_range
                .to_range_inclusive(cardano_database_snapshot.beacon.immutable_file_number)
                .map_err(ComputeCardanoDatabaseMessageError::ImmutableFilesRange)?;
            let missing_immutable_files =
                Self::list_missing_immutable_files(database_dir, &immutable_file_number_range);
            let immutable_digester = CardanoImmutableDigester::new(network, None, self.logger.clone());
            let computed_digest_entries = immutable_digester
                .compute_digests_for_range(database_dir, &immutable_file_number_range)
                .await?
                .entries;
            let computed_digests = computed_digest_entries
                .values()
                .map(MKTreeNode::from)
                .collect::<Vec<_>>();

            let proof_result = verified_digests.merkle_tree.compute_proof(&computed_digests);
            if let Ok(ref merkle_proof) = proof_result
                && missing_immutable_files.is_empty()
            {
                merkle_proof
                    .verify()
                    .map_err(ComputeCardanoDatabaseMessageError::MerkleProofVerification)?;

                let mut message = certificate.protocol_message.clone();
                message.set_message_part(
                    ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
                    merkle_proof.root().to_hex(),
                );

                return Ok(message);
            }

            let tampered_files = match proof_result {
                // TODO: we need to handle the error properly.
                // It's not possible yet since we cannot filter/match on a returned error type since the `compute_proof` method returns an `anyhow::Error`
                Err(_e) => {
                    verified_digests
                        .list_immutable_files_not_verified(&computed_digest_entries)
                        .tampered_files
                }
                Ok(_) => vec![],
            };

            Err(
                ComputeCardanoDatabaseMessageError::ImmutableFilesVerification(ImmutableFilesLists {
                    dir_path: Self::immutable_dir(database_dir),
                    missing: missing_immutable_files,
                    tampered: tampered_files,
                }),
            )
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
    use super::*;

    #[cfg(feature = "fs")]
    mod fs {
        use mithril_cardano_node_internal_database::test::DummyCardanoDbBuilder;

        use super::*;

        fn remove_immutable_files<T: AsRef<Path>>(database_dir: &Path, immutable_file_names: &[T]) {
            for immutable_file_name in immutable_file_names {
                let immutable_file_path =
                    MessageBuilder::immutable_dir(database_dir).join(immutable_file_name.as_ref());
                std::fs::remove_file(immutable_file_path).unwrap();
            }
        }

        fn tamper_immutable_files<T: AsRef<Path>>(database_dir: &Path, immutable_file_names: &[T]) {
            for immutable_file_name in immutable_file_names {
                let immutable_file_path =
                    MessageBuilder::immutable_dir(database_dir).join(immutable_file_name.as_ref());
                std::fs::write(immutable_file_path, "tampered content").unwrap();
            }
        }

        mod list_missing_immutable_files {
            use mithril_common::temp_dir_create;

            use super::*;

            #[test]
            fn list_missing_immutable_files_should_return_empty_list_if_no_missing_files() {
                let immutable_files_in_db = 1..=10;
                let range_to_verify = 3..=5;
                let cardano_db =
                    DummyCardanoDbBuilder::new(&format!("{}", temp_dir_create!().display()))
                        .with_immutables(&immutable_files_in_db.collect::<Vec<_>>())
                        .append_immutable_trio()
                        .build();

                let missing_files = MessageBuilder::list_missing_immutable_files(
                    cardano_db.get_dir(),
                    &range_to_verify,
                );

                assert!(missing_files.is_empty());
            }

            #[test]
            fn list_missing_immutable_files_should_return_empty_list_if_missing_files_outside_range()
             {
                let immutable_files_in_db = 1..=10;
                let range_to_verify = 3..=5;
                let cardano_db =
                    DummyCardanoDbBuilder::new(&format!("{}", temp_dir_create!().display()))
                        .with_immutables(&immutable_files_in_db.collect::<Vec<_>>())
                        .append_immutable_trio()
                        .build();
                let files_to_remove = vec!["00002.chunk", "00006.primary"];
                remove_immutable_files(cardano_db.get_dir(), &files_to_remove);

                let missing_files = MessageBuilder::list_missing_immutable_files(
                    cardano_db.get_dir(),
                    &range_to_verify,
                );

                assert!(missing_files.is_empty());
            }

            #[test]
            fn list_missing_immutable_files_should_return_list_of_missing_files_inside_range() {
                let immutable_files_in_db = 1..=10;
                let range_to_verify = 3..=5;
                let cardano_db =
                    DummyCardanoDbBuilder::new(&format!("{}", temp_dir_create!().display()))
                        .with_immutables(&immutable_files_in_db.collect::<Vec<_>>())
                        .append_immutable_trio()
                        .build();
                let files_to_remove = vec!["00004.chunk", "00005.primary"];
                remove_immutable_files(cardano_db.get_dir(), &files_to_remove);

                let missing_files = MessageBuilder::list_missing_immutable_files(
                    cardano_db.get_dir(),
                    &range_to_verify,
                );

                assert_eq!(missing_files, files_to_remove);
            }
        }

        mod cardano_database_message {

            use std::{collections::BTreeMap, path::PathBuf};

            use mithril_common::{
                entities::{CardanoDbBeacon, Epoch},
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
                let immutable_digester = CardanoImmutableDigester::new(
                    "whatever".to_string(),
                    None,
                    TestLogger::stdout(),
                );
                let computed_digests = immutable_digester
                    .compute_digests_for_range(database_dir, immutable_file_range)
                    .await
                    .unwrap();

                let digests = computed_digests
                    .entries
                    .iter()
                    .map(|(immutable_file, digest)| {
                        (immutable_file.filename.clone(), digest.clone())
                    })
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

            fn to_vec_immutable_file_name(list: &[&str]) -> Vec<ImmutableFileName> {
                list.iter().map(|s| ImmutableFileName::from(*s)).collect()
            }

            #[tokio::test]
            async fn compute_cardano_database_message_succeeds() {
                let beacon = CardanoDbBeacon {
                    epoch: Epoch(123),
                    immutable_file_number: 10,
                };
                let immutable_file_range = 1..=15;
                let immutable_file_range_to_prove = ImmutableFileRange::Range(2, 4);
                let (database_dir, certificate, verified_digests) =
                    prepare_db_and_verified_digests(
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

            #[tokio::test]
            async fn compute_cardano_database_message_should_fail_if_immutable_is_missing() {
                let beacon = CardanoDbBeacon {
                    epoch: Epoch(123),
                    immutable_file_number: 10,
                };
                let immutable_file_range = 1..=15;
                let immutable_file_range_to_prove = ImmutableFileRange::Range(2, 4);
                let (database_dir, certificate, verified_digests) =
                    prepare_db_and_verified_digests(
                        "compute_cardano_database_message_should_fail_if_immutable_is_missing",
                        &beacon,
                        &immutable_file_range,
                    )
                    .await;

                let files_to_remove = vec!["00003.chunk", "00004.primary"];
                remove_immutable_files(&database_dir, &files_to_remove);

                let error = MessageBuilder::new()
                    .compute_cardano_database_message(
                        &certificate,
                        &CardanoDatabaseSnapshotMessage::dummy(),
                        &immutable_file_range_to_prove,
                        &database_dir,
                        &verified_digests,
                    )
                    .await
                    .expect_err(
                        "compute_cardano_database_message should fail if a immutable is missing",
                    );

                let error_lists = match error {
                    ComputeCardanoDatabaseMessageError::ImmutableFilesVerification(lists) => lists,
                    _ => panic!("Expected ImmutableFilesVerification error, got: {error}"),
                };

                assert_eq!(
                    error_lists,
                    ImmutableFilesLists {
                        dir_path: MessageBuilder::immutable_dir(&database_dir),
                        missing: to_vec_immutable_file_name(&files_to_remove),
                        tampered: vec![],
                    }
                );
            }

            #[tokio::test]
            async fn compute_cardano_database_message_should_fail_if_immutable_is_tampered() {
                let beacon = CardanoDbBeacon {
                    epoch: Epoch(123),
                    immutable_file_number: 10,
                };
                let immutable_file_range = 1..=15;
                let immutable_file_range_to_prove = ImmutableFileRange::Range(2, 4);
                let (database_dir, certificate, verified_digests) =
                    prepare_db_and_verified_digests(
                        "compute_cardano_database_message_should_fail_if_immutable_is_tampered",
                        &beacon,
                        &immutable_file_range,
                    )
                    .await;

                let files_to_tamper = vec!["00003.chunk", "00004.primary"];
                tamper_immutable_files(&database_dir, &files_to_tamper);

                let error = MessageBuilder::new()
                    .compute_cardano_database_message(
                        &certificate,
                        &CardanoDatabaseSnapshotMessage::dummy(),
                        &immutable_file_range_to_prove,
                        &database_dir,
                        &verified_digests,
                    )
                    .await
                    .expect_err(
                        "compute_cardano_database_message should fail if a immutable is missing",
                    );

                let error_lists = match error {
                    ComputeCardanoDatabaseMessageError::ImmutableFilesVerification(lists) => lists,
                    _ => panic!("Expected ImmutableFilesVerification error, got: {error}"),
                };
                assert_eq!(
                    error_lists,
                    ImmutableFilesLists {
                        dir_path: MessageBuilder::immutable_dir(&database_dir),
                        missing: vec![],
                        tampered: to_vec_immutable_file_name(&files_to_tamper),
                    }
                )
            }

            #[tokio::test]
            async fn compute_cardano_database_message_should_fail_if_immutables_are_missing_and_tampered()
             {
                let beacon = CardanoDbBeacon {
                    epoch: Epoch(123),
                    immutable_file_number: 10,
                };
                let immutable_file_range = 1..=15;
                let immutable_file_range_to_prove = ImmutableFileRange::Range(2, 4);
                let (database_dir, certificate, verified_digests) =
                    prepare_db_and_verified_digests(
                        "compute_cardano_database_message_should_fail_if_immutables_are_missing_and_tampered",
                        &beacon,
                        &immutable_file_range,
                    )
                    .await;

                let files_to_remove = vec!["00003.chunk"];
                let files_to_tamper = vec!["00004.primary"];
                remove_immutable_files(&database_dir, &files_to_remove);
                tamper_immutable_files(&database_dir, &files_to_tamper);

                let error = MessageBuilder::new()
                    .compute_cardano_database_message(
                        &certificate,
                        &CardanoDatabaseSnapshotMessage::dummy(),
                        &immutable_file_range_to_prove,
                        &database_dir,
                        &verified_digests,
                    )
                    .await
                    .expect_err(
                        "compute_cardano_database_message should fail if a immutable is missing",
                    );

                let error_lists = match error {
                    ComputeCardanoDatabaseMessageError::ImmutableFilesVerification(lists) => lists,
                    _ => panic!("Expected ImmutableFilesVerification error, got: {error}"),
                };
                assert_eq!(
                    error_lists,
                    ImmutableFilesLists {
                        dir_path: MessageBuilder::immutable_dir(&database_dir),
                        missing: to_vec_immutable_file_name(&files_to_remove),
                        tampered: to_vec_immutable_file_name(&files_to_tamper),
                    }
                )
            }
        }
    }
}
