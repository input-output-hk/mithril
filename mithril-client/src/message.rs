use anyhow::Context;
use slog::{Logger, o};
#[cfg(feature = "fs")]
use {
    slog::warn,
    std::{
        fmt,
        ops::RangeInclusive,
        path::{Path, PathBuf},
        sync::Arc,
    },
    thiserror::Error,
};

#[cfg(feature = "fs")]
use mithril_cardano_node_internal_database::{
    IMMUTABLE_DIR,
    digesters::{CardanoImmutableDigester, ImmutableDigester, ImmutableDigesterError},
};
#[cfg(feature = "fs")]
use mithril_common::{
    crypto_helper::MKTreeNode,
    entities::{ImmutableFileName, ImmutableFileNumber, SignedEntityType},
    messages::{CardanoDatabaseSnapshotMessage, CertificateMessage},
};
use mithril_common::{
    logging::LoggerExtensions, protocol::SignerBuilder,
    signable_builder::CardanoStakeDistributionSignableBuilder,
};

use crate::{
    CardanoStakeDistribution, MithrilCertificate, MithrilResult, MithrilSigner,
    MithrilStakeDistribution, VerifiedCardanoTransactions,
    common::{ProtocolMessage, ProtocolMessagePartKey},
};
#[cfg(feature = "fs")]
use crate::{
    MithrilError,
    cardano_database_client::{ImmutableFileRange, VerifiedDigests},
};

cfg_fs! {
    //TODO: TO REMOVE (Moved to proving.rs)
    const MERKLE_PROOF_COMPUTATION_ERROR:&str = "Merkle proof computation failed";

    //TODO: TO REMOVE (Moved to proving.rs)
    /// Type containing the lists of immutable files that are missing or tampered.
    #[derive(Debug, PartialEq)]
    pub struct ImmutableVerificationResult {
        /// The immutables files directory.
        pub immutables_dir: PathBuf,
        /// List of missing immutable files.
        pub missing: Vec<ImmutableFileName>,
        /// List of tampered immutable files.
        pub tampered: Vec<ImmutableFileName>,
        /// List of non-verifiable immutable files.
        pub non_verifiable: Vec<ImmutableFileName>,
    }

    //TODO: TO REMOVE (Moved to proving.rs)
    /// Compute Cardano database message related errors.
    #[derive(Error, Debug)]
    pub enum ComputeCardanoDatabaseMessageError {
        /// Error related to the verification of immutable files.
        ImmutableFilesVerification(ImmutableVerificationResult),

        /// Error related to the immutable files digests computation.
        DigestsComputation(#[from] ImmutableDigesterError),

        /// Error related to the Merkle proof verification.
        MerkleProofVerification(#[source] MithrilError),

        /// Error related to the immutable files range.
        ImmutableFilesRangeCreation(#[source] MithrilError),
    }

    impl fmt::Display for ComputeCardanoDatabaseMessageError {

        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                ComputeCardanoDatabaseMessageError::ImmutableFilesVerification(lists) => {
                    fn get_first_10_files_path(
                        files: &[ImmutableFileName],
                        immutables_dir: &Path,
                    ) -> String {
                        files
                            .iter()
                            .take(10)
                            .map(|file| immutables_dir.join(file).to_string_lossy().to_string())
                            .collect::<Vec<_>>()
                            .join("\n")
                    }

                    if !lists.missing.is_empty() {
                        let missing_files_subset = get_first_10_files_path(&lists.missing, &lists.immutables_dir);
                        writeln!(
                            f,
                            "Number of missing immutable files: {}",
                            lists.missing.len()
                        )?;
                        writeln!(f, "First 10 missing immutable files paths:")?;
                        writeln!(f, "{missing_files_subset}")?;
                    }
                    if !lists.missing.is_empty() && !lists.tampered.is_empty() {
                        writeln!(f)?;
                    }
                    if !lists.tampered.is_empty() {
                        let tampered_files_subset = get_first_10_files_path(&lists.tampered, &lists.immutables_dir);
                        writeln!(f,"Number of tampered immutable files: {}",lists.tampered.len())?;
                        writeln!(f, "First 10 tampered immutable files paths:")?;
                        writeln!(f, "{tampered_files_subset}")?;
                    }
                    if (!lists.missing.is_empty() || !lists.tampered.is_empty()) && !lists.non_verifiable.is_empty() {
                        writeln!(f)?;
                    }
                    if !lists.non_verifiable.is_empty() {
                        let non_verifiable_files_subset = get_first_10_files_path(&lists.non_verifiable, &lists.immutables_dir);
                        writeln!(f, "Number of non verifiable immutable files: {}", lists.non_verifiable.len())?;
                        writeln!(f, "First 10 non verifiable immutable files paths:")?;
                        writeln!(f, "{non_verifiable_files_subset}")?;
                    }
                    Ok(())
                }
                ComputeCardanoDatabaseMessageError::DigestsComputation(e) => {
                    write!(f, "Immutable files digester error: {e:?}")
                }
                ComputeCardanoDatabaseMessageError::MerkleProofVerification(e) => {
                    write!(f, "Merkle proof verification error: {e:?}")
                }
                ComputeCardanoDatabaseMessageError::ImmutableFilesRangeCreation(e) => {
                    write!(f, "Immutable files range error: {e:?}")
                }
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

        //TODO: TO REMOVE OR MUTUALIZE WITH proving.rs
        fn immutable_dir(db_dir: &Path) -> PathBuf {
            db_dir.join(IMMUTABLE_DIR)
        }

        //TODO: TO REMOVE
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

        ///TODO: TO REMOVE
        /// Compute message for a Cardano database.
        ///
        /// This function first lists missing immutables files (if `allow_missing` is false)
        /// then computes the digests for the given range and finally computes the Merkle proof.
        pub async fn compute_cardano_database_message(
            &self,
            certificate: &CertificateMessage,
            cardano_database_snapshot: &CardanoDatabaseSnapshotMessage,
            immutable_file_range: &ImmutableFileRange,
            allow_missing: bool,
            database_dir: &Path,
            verified_digests: &VerifiedDigests,
        ) -> Result<ProtocolMessage, ComputeCardanoDatabaseMessageError> {
            let network = certificate.metadata.network.clone();
            let immutable_file_number_range = immutable_file_range
                .to_range_inclusive(cardano_database_snapshot.beacon.immutable_file_number)
                .map_err(ComputeCardanoDatabaseMessageError::ImmutableFilesRangeCreation)?;
            let missing_immutable_files = if allow_missing {
                vec![]
            } else {
                Self::list_missing_immutable_files(database_dir, &immutable_file_number_range)
            };
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

            let (tampered, non_verifiable) = match proof_result {
                Err(e) => {
                    warn!(self.logger, "{MERKLE_PROOF_COMPUTATION_ERROR}: {e:}");
                    let verified_digests = verified_digests
                        .list_immutable_files_not_verified(&computed_digest_entries);

                    (verified_digests.tampered_files, verified_digests.non_verifiable_files)
                }
                Ok(_) => (vec![], vec![]),
            };
            Err(
                ComputeCardanoDatabaseMessageError::ImmutableFilesVerification(ImmutableVerificationResult {
                    immutables_dir: Self::immutable_dir(database_dir),
                    missing: missing_immutable_files,
                    tampered,
                    non_verifiable,
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
#[cfg(feature = "fs")]
mod tests {
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
        fn list_missing_immutable_files_should_return_empty_list_if_missing_files_outside_range() {
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
            test::double::Dummy,
        };

        use crate::cardano_database_client::ImmutableFileRange;
        use crate::{cardano_database_client::VerifiedDigests, test_utils::TestLogger};

        use super::*;

        //TODO TO REMOVE (Moved to proving.rs)
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

        fn to_vec_immutable_file_name(list: &[&str]) -> Vec<ImmutableFileName> {
            list.iter().map(|s| ImmutableFileName::from(*s)).collect()
        }

        //TODO TO REMOVE (Moved to proving.rs)
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
                    false,
                    &database_dir,
                    &verified_digests,
                )
                .await
                .unwrap();

            assert!(certificate.match_message(&message));
        }

        //TODO TO REMOVE (Moved to proving.rs)
        #[tokio::test]
        async fn compute_cardano_database_message_should_fail_if_immutable_is_missing_and_allow_missing_not_set()
         {
            let beacon = CardanoDbBeacon {
                epoch: Epoch(123),
                immutable_file_number: 10,
            };
            let immutable_file_range = 1..=15;
            let immutable_file_range_to_prove = ImmutableFileRange::Range(2, 4);
            let (database_dir, certificate, verified_digests) = prepare_db_and_verified_digests(
                "compute_cardano_database_message_should_fail_if_immutable_is_missing_and_allow_missing_not_set",
                &beacon,
                &immutable_file_range,
            )
            .await;

            let files_to_remove = vec!["00003.chunk", "00004.primary"];
            remove_immutable_files(&database_dir, &files_to_remove);

            let allow_missing = false;
            let error = MessageBuilder::new()
                .compute_cardano_database_message(
                    &certificate,
                    &CardanoDatabaseSnapshotMessage::dummy(),
                    &immutable_file_range_to_prove,
                    allow_missing,
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
                ImmutableVerificationResult {
                    immutables_dir: MessageBuilder::immutable_dir(&database_dir),
                    missing: to_vec_immutable_file_name(&files_to_remove),
                    tampered: vec![],
                    non_verifiable: vec![],
                }
            );
        }

        #[tokio::test]
        async fn compute_cardano_database_message_should_success_if_immutable_is_missing_and_allow_missing_is_set()
         {
            let beacon = CardanoDbBeacon {
                epoch: Epoch(123),
                immutable_file_number: 10,
            };
            let immutable_file_range = 1..=15;
            let immutable_file_range_to_prove = ImmutableFileRange::Range(2, 4);
            let (database_dir, certificate, verified_digests) = prepare_db_and_verified_digests(
                "compute_cardano_database_message_should_success_if_immutable_is_missing_and_allow_missing_is_set",
                &beacon,
                &immutable_file_range,
            )
            .await;

            let files_to_remove = vec!["00003.chunk", "00004.primary"];
            remove_immutable_files(&database_dir, &files_to_remove);

            let allow_missing = true;
            MessageBuilder::new()
                .compute_cardano_database_message(
                    &certificate,
                    &CardanoDatabaseSnapshotMessage::dummy(),
                    &immutable_file_range_to_prove,
                    allow_missing,
                    &database_dir,
                    &verified_digests,
                )
                .await
                .expect(
                    "compute_cardano_database_message should succeed if a immutable is missing but 'allow_missing' is set",
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
            let (database_dir, certificate, verified_digests) = prepare_db_and_verified_digests(
                "compute_cardano_database_message_should_fail_if_immutable_is_tampered",
                &beacon,
                &immutable_file_range,
            )
            .await;

            let files_to_tamper = vec!["00003.chunk", "00004.primary"];
            tamper_immutable_files(&database_dir, &files_to_tamper);

            let (logger, log_inspector) = TestLogger::memory();

            let error = MessageBuilder::new()
                .with_logger(logger)
                .compute_cardano_database_message(
                    &certificate,
                    &CardanoDatabaseSnapshotMessage::dummy(),
                    &immutable_file_range_to_prove,
                    false,
                    &database_dir,
                    &verified_digests,
                )
                .await
                .expect_err(
                    "compute_cardano_database_message should fail if a immutable is missing",
                );

            assert!(log_inspector.contains_log(MERKLE_PROOF_COMPUTATION_ERROR));

            let error_lists = match error {
                ComputeCardanoDatabaseMessageError::ImmutableFilesVerification(lists) => lists,
                _ => panic!("Expected ImmutableFilesVerification error, got: {error}"),
            };
            assert_eq!(
                error_lists,
                ImmutableVerificationResult {
                    immutables_dir: MessageBuilder::immutable_dir(&database_dir),
                    missing: vec![],
                    tampered: to_vec_immutable_file_name(&files_to_tamper),
                    non_verifiable: vec![],
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
                    false,
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
                ImmutableVerificationResult {
                    immutables_dir: MessageBuilder::immutable_dir(&database_dir),
                    missing: to_vec_immutable_file_name(&files_to_remove),
                    tampered: to_vec_immutable_file_name(&files_to_tamper),
                    non_verifiable: vec![],
                }
            )
        }

        #[tokio::test]
        async fn compute_cardano_database_message_should_fail_if_there_is_more_local_immutable_than_verified_digest()
         {
            let last_verified_digest_number = 10;
            let last_local_immutable_file_number = 15;
            let range_of_non_verifiable_files =
                last_verified_digest_number + 1..=last_local_immutable_file_number;

            let expected_non_verifiable_files: Vec<ImmutableFileName> =
                (range_of_non_verifiable_files)
                    .flat_map(|i| {
                        [
                            format!("{i:05}.chunk"),
                            format!("{i:05}.primary"),
                            format!("{i:05}.secondary"),
                        ]
                    })
                    .collect();

            let beacon = CardanoDbBeacon {
                epoch: Epoch(123),
                immutable_file_number: last_verified_digest_number,
            };
            //create verified digests for immutable files 1 to 10
            let (_, certificate, verified_digests) = prepare_db_and_verified_digests(
                "database_dir_for_verified_digests",
                &beacon,
                &(1..=last_verified_digest_number),
            )
            .await;
            //create a local database with immutable files 1 to 15
            let (database_dir, _, _) = prepare_db_and_verified_digests(
                "database_dir_for_local_immutables",
                &beacon,
                &(1..=last_local_immutable_file_number),
            )
            .await;

            let error = MessageBuilder::new()
                .compute_cardano_database_message(
                    &certificate,
                    &CardanoDatabaseSnapshotMessage::dummy(),
                    &ImmutableFileRange::Range(1, 15),
                    false,
                    &database_dir,
                    &verified_digests,
                )
                .await
                .expect_err(
                    "compute_cardano_database_message should fail if there is more local immutable than verified digest",
                );

            let error_lists = match error {
                ComputeCardanoDatabaseMessageError::ImmutableFilesVerification(lists) => lists,
                _ => panic!("Expected ImmutableFilesVerification error, got: {error}"),
            };
            assert_eq!(
                error_lists,
                ImmutableVerificationResult {
                    immutables_dir: MessageBuilder::immutable_dir(&database_dir),
                    missing: vec![],
                    tampered: vec![],
                    non_verifiable: expected_non_verifiable_files,
                }
            );
        }
    }

    //TODO: TO REMOVE (Moved to proving.rs)
    mod compute_cardano_database_message_error {
        use super::*;

        fn generate_immutable_files_verification_error(
            missing_range: Option<RangeInclusive<usize>>,
            tampered_range: Option<RangeInclusive<usize>>,
            non_verifiable_range: Option<RangeInclusive<usize>>,
            immutable_path: &str,
        ) -> ComputeCardanoDatabaseMessageError {
            let missing: Vec<ImmutableFileName> = match missing_range {
                Some(range) => range
                    .map(|i| ImmutableFileName::from(format!("{i:05}.chunk")))
                    .collect(),
                None => vec![],
            };
            let tampered: Vec<ImmutableFileName> = match tampered_range {
                Some(range) => range
                    .map(|i| ImmutableFileName::from(format!("{i:05}.chunk")))
                    .collect(),
                None => vec![],
            };

            let non_verifiable: Vec<ImmutableFileName> = match non_verifiable_range {
                Some(range) => range
                    .map(|i| ImmutableFileName::from(format!("{i:05}.chunk")))
                    .collect(),
                None => vec![],
            };

            ComputeCardanoDatabaseMessageError::ImmutableFilesVerification(
                ImmutableVerificationResult {
                    immutables_dir: PathBuf::from(immutable_path),
                    missing,
                    tampered,
                    non_verifiable,
                },
            )
        }

        fn normalize_path_separators(s: &str) -> String {
            s.replace('\\', "/")
        }

        #[test]
        fn display_immutable_files_verification_error_should_displayed_lists_with_10_elements() {
            let error = generate_immutable_files_verification_error(
                Some(1..=15),
                Some(20..=31),
                Some(40..=41),
                "/path/to/immutables",
            );

            let display = normalize_path_separators(&format!("{error}"));

            assert_eq!(
                display,
                r###"Number of missing immutable files: 15
First 10 missing immutable files paths:
/path/to/immutables/00001.chunk
/path/to/immutables/00002.chunk
/path/to/immutables/00003.chunk
/path/to/immutables/00004.chunk
/path/to/immutables/00005.chunk
/path/to/immutables/00006.chunk
/path/to/immutables/00007.chunk
/path/to/immutables/00008.chunk
/path/to/immutables/00009.chunk
/path/to/immutables/00010.chunk

Number of tampered immutable files: 12
First 10 tampered immutable files paths:
/path/to/immutables/00020.chunk
/path/to/immutables/00021.chunk
/path/to/immutables/00022.chunk
/path/to/immutables/00023.chunk
/path/to/immutables/00024.chunk
/path/to/immutables/00025.chunk
/path/to/immutables/00026.chunk
/path/to/immutables/00027.chunk
/path/to/immutables/00028.chunk
/path/to/immutables/00029.chunk

Number of non verifiable immutable files: 2
First 10 non verifiable immutable files paths:
/path/to/immutables/00040.chunk
/path/to/immutables/00041.chunk
"###
            );
        }

        #[test]
        fn display_immutable_files_should_display_tampered_files_only() {
            let error = generate_immutable_files_verification_error(
                None,
                Some(1..=1),
                None,
                "/path/to/immutables",
            );

            let display = normalize_path_separators(&format!("{error}"));

            assert_eq!(
                display,
                r###"Number of tampered immutable files: 1
First 10 tampered immutable files paths:
/path/to/immutables/00001.chunk
"###
            );
        }

        #[test]
        fn display_immutable_files_should_display_missing_files_only() {
            let error = generate_immutable_files_verification_error(
                Some(1..=1),
                None,
                None,
                "/path/to/immutables",
            );

            let display = normalize_path_separators(&format!("{error}"));

            assert_eq!(
                display,
                r###"Number of missing immutable files: 1
First 10 missing immutable files paths:
/path/to/immutables/00001.chunk
"###
            );
        }

        #[test]
        fn display_immutable_files_should_display_non_verifiable_files_only() {
            let error = generate_immutable_files_verification_error(
                None,
                None,
                Some(1..=5),
                "/path/to/immutables",
            );

            let display = normalize_path_separators(&format!("{error}"));

            assert_eq!(
                display,
                r###"Number of non verifiable immutable files: 5
First 10 non verifiable immutable files paths:
/path/to/immutables/00001.chunk
/path/to/immutables/00002.chunk
/path/to/immutables/00003.chunk
/path/to/immutables/00004.chunk
/path/to/immutables/00005.chunk
"###
            );
        }
    }
}
