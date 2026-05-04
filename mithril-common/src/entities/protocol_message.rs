use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::{collections::BTreeMap, fmt::Display};
use thiserror::Error;

#[cfg(test)]
use crate::entities::Epoch;

/// Error returned by [ProtocolMessage::check_rigid_integrity] when a rigid-segment value
/// in a [ProtocolMessage] does not match the fixed-size SNARK-friendly slot it must fill.
#[derive(Debug, Error, PartialEq, Eq)]
pub enum RigidProtocolMessageIntegrityError {
    /// The decoded value of a fixed-size rigid field does not match the expected byte length.
    #[error("rigid `{field}` value must decode to exactly {expected} bytes (got {actual})")]
    UnexpectedFieldLength {
        /// Name of the rigid field whose decoded bytes do not match the expected length.
        field: &'static str,
        /// Expected byte length for the rigid slot.
        expected: usize,
        /// Actual byte length of the decoded value.
        actual: usize,
    },

    /// The required `current_epoch` entry is missing from the protocol message.
    #[error(
        "rigid `current_epoch` value is required but no entry was found in the protocol message"
    )]
    MissingCurrentEpoch,

    /// The `current_epoch` value cannot be parsed as a base-10 unsigned 64-bit integer.
    #[error("rigid `current_epoch` value must be a base-10 unsigned 64-bit integer: {0}")]
    InvalidCurrentEpoch(String),

    /// The required `next_aggregate_verification_key` entry is missing from the protocol message.
    #[error(
        "rigid `next_aggregate_verification_key` value is required but no entry was found in the protocol message"
    )]
    MissingNextSnarkAggregateVerificationKey,

    /// The required `next_protocol_parameters` entry is missing from the protocol message.
    #[error(
        "rigid `next_protocol_parameters` value is required but no entry was found in the protocol message"
    )]
    MissingNextProtocolParameters,
}

/// Decode the wire SNARK aggregate verification key value (CBOR-prefixed AVK encoding produced
/// by `ProtocolKey::new(snark_avk).to_bytes_hex()`) and project it into the canonical
/// [RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES]-byte rigid-slot layout.
///
/// Producers keep emitting the full AVK CBOR encoding so the certificate verifier can still
/// deserialize the previous-cert announcement back to a `ProtocolAggregateVerificationKeyForSnark`
/// for full-equality comparison. The rigid hash assembler (and the integrity check) decode that
/// wire value and project the AVK into the fixed-size rigid slot via
/// `AggregateVerificationKeyForSnark::to_rigid_slot_bytes`.
#[cfg(feature = "future_snark")]
fn decode_snark_avk_to_rigid_slot_bytes(
    value: &str,
) -> Result<[u8; RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES], RigidProtocolMessageIntegrityError> {
    let snark_avk = crate::crypto_helper::ProtocolAggregateVerificationKeyForSnark::try_from(value)
        .map_err(|err| {
            RigidProtocolMessageIntegrityError::InvalidSnarkAggregateVerificationKey(
                err.to_string(),
            )
        })?;
    snark_avk.to_rigid_slot_bytes().map_err(|err| {
        RigidProtocolMessageIntegrityError::InvalidSnarkAggregateVerificationKey(err.to_string())
    })
}

/// Decode the wire protocol parameters value (hex-encoded fixed-size buffer, falling back to
/// raw UTF-8 bytes when hex decoding fails) and project it into the
/// [RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES]-byte rigid slot.
#[cfg(feature = "future_snark")]
fn decode_protocol_parameters_to_rigid_slot_bytes(
    value: &str,
) -> Result<[u8; RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES], RigidProtocolMessageIntegrityError> {
    let bytes = hex::decode(value).unwrap_or_else(|_| value.as_bytes().to_vec());
    bytes.as_slice().try_into().map_err(|_| {
        RigidProtocolMessageIntegrityError::UnexpectedFieldLength {
            field: "next_protocol_parameters",
            expected: RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES,
            actual: bytes.len(),
        }
    })
}

/// [ProtocolMessagePartKey] entries projected into a fixed-size segment of the rigid preimage,
/// and therefore stripped from the dynamic-parts digest segment.
const RIGID_SEGMENT_KEYS: &[ProtocolMessagePartKey] = &[
    ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
    ProtocolMessagePartKey::NextProtocolParameters,
    ProtocolMessagePartKey::CurrentEpoch,
];

/// Byte length of the `digest` value segment in the rigid preimage.
pub const RIGID_DIGEST_BYTES: usize = 32;

/// Byte length of the `next_aggregate_verification_key` value segment in the rigid preimage.
///
/// The slot holds the SNARK-friendly aggregate verification key bytes (the value sourced from
/// [ProtocolMessagePartKey::NextSnarkAggregateVerificationKey]).
pub const RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES: usize = 44;

/// Byte length of the `next_protocol_parameters` value segment in the rigid preimage.
pub const RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES: usize = 32;

/// Byte length of the `current_epoch` value segment in the rigid preimage.
pub const RIGID_CURRENT_EPOCH_BYTES: usize = 8;

/// ASCII label written immediately before the `digest` value segment in the rigid preimage.
const RIGID_DIGEST_LABEL: &[u8] = b"digest";

/// ASCII label written immediately before the `next_aggregate_verification_key` value segment in the rigid preimage.
const RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_LABEL: &[u8] = b"next_aggregate_verification_key";

/// ASCII label written immediately before the `next_protocol_parameters` value segment in the rigid preimage.
const RIGID_NEXT_PROTOCOL_PARAMETERS_LABEL: &[u8] = b"next_protocol_parameters";

/// ASCII label written immediately before the `current_epoch` value segment in the rigid preimage.
const RIGID_CURRENT_EPOCH_LABEL: &[u8] = b"current_epoch";

/// Byte length of the full rigid preimage that [ProtocolMessage::rigid_preimage] assembles when
/// the hash scheme is [ProtocolMessageHashScheme::Rigid]. Each named segment is prefixed by its ASCII
/// label, mirroring the layout consumed by the IVC SNARK gadget.
const RIGID_PROTOCOL_MESSAGE_PREIMAGE_BYTES: usize = RIGID_DIGEST_LABEL.len()
    + RIGID_DIGEST_BYTES
    + RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_LABEL.len()
    + RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES
    + RIGID_NEXT_PROTOCOL_PARAMETERS_LABEL.len()
    + RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES
    + RIGID_CURRENT_EPOCH_LABEL.len()
    + RIGID_CURRENT_EPOCH_BYTES;

/// Hash scheme of a [ProtocolMessage], driving which hash scheme is applied by
/// [ProtocolMessage::compute_hash].
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq, Eq)]
pub enum ProtocolMessageHashScheme {
    /// Pre-Lagrange hash scheme. Kept as the default so missing-field deserializations of
    /// existing certificates remain byte-identical.
    #[default]
    #[serde(rename = "legacy")]
    Legacy,

    /// Lagrange SNARK-friendly hash scheme.
    #[serde(rename = "rigid")]
    Rigid,
}

impl ProtocolMessageHashScheme {
    fn is_legacy(&self) -> bool {
        matches!(self, Self::Legacy)
    }
}

/// The key of a [ProtocolMessage] part
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProtocolMessagePartKey {
    /// The ProtocolMessage part key associated to the Snapshot Digest
    #[serde(rename = "snapshot_digest")]
    SnapshotDigest,

    /// The ProtocolMessage part key associated to the Cardano Transactions Merkle Root
    #[serde(rename = "cardano_transactions_merkle_root")]
    CardanoTransactionsMerkleRoot,

    /// The ProtocolMessage part key associated to the Cardano Blocks and Transactions Merkle Root
    #[serde(rename = "cardano_blocks_transactions_merkle_root")]
    CardanoBlocksTransactionsMerkleRoot,

    /// The ProtocolMessage part key associated to the Next epoch aggregate verification key
    ///
    /// The AVK that will be allowed to be used to sign during the next epoch
    /// aka AVK(n-1)
    #[serde(rename = "next_aggregate_verification_key")]
    NextAggregateVerificationKey,

    /// The ProtocolMessage part key associated to the Next epoch protocol parameters
    ///
    /// The protocol parameters that will be allowed to be used to sign during the next epoch
    /// aka PPARAMS(n-1)
    #[serde(rename = "next_protocol_parameters")]
    NextProtocolParameters,

    /// The ProtocolMessage part key associated to the current epoch
    ///
    /// aka EPOCH(n)
    #[serde(rename = "current_epoch")]
    CurrentEpoch,

    /// The ProtocolMessage part key associated to the latest block number signed
    #[serde(rename = "latest_block_number")]
    LatestBlockNumber,

    /// The ProtocolMessage part key associated to the security parameter block number offset of the CardanoBlocksTransactions
    #[serde(rename = "cardano_blocks_transactions_block_number_offset")]
    CardanoBlocksTransactionsBlockNumberOffset,

    /// The ProtocolMessage part key associated to the epoch for which the Cardano stake distribution is computed
    #[serde(rename = "cardano_stake_distribution_epoch")]
    CardanoStakeDistributionEpoch,

    /// The ProtocolMessage part key associated to the Cardano stake distribution Merkle root
    #[serde(rename = "cardano_stake_distribution_merkle_root")]
    CardanoStakeDistributionMerkleRoot,

    /// The ProtocolMessage part key associated to the Cardano database Merkle root
    #[serde(rename = "cardano_database_merkle_root")]
    CardanoDatabaseMerkleRoot,

    /// The ProtocolMessage part key associated to the Next epoch SNARK aggregate verification key
    ///
    /// The SNARK AVK that will be allowed to be used to sign during the next epoch
    /// aka AVKS(n-1)
    #[serde(rename = "next_aggregate_verification_key_snark")]
    NextSnarkAggregateVerificationKey,
}

impl Display for ProtocolMessagePartKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::SnapshotDigest => write!(f, "snapshot_digest"),
            Self::NextAggregateVerificationKey => write!(f, "next_aggregate_verification_key"),
            Self::NextProtocolParameters => write!(f, "next_protocol_parameters"),
            Self::CurrentEpoch => write!(f, "current_epoch"),
            Self::CardanoTransactionsMerkleRoot => write!(f, "cardano_transactions_merkle_root"),
            Self::CardanoBlocksTransactionsMerkleRoot => {
                write!(f, "cardano_blocks_transactions_merkle_root")
            }
            Self::LatestBlockNumber => write!(f, "latest_block_number"),
            Self::CardanoBlocksTransactionsBlockNumberOffset => {
                write!(f, "cardano_blocks_transactions_block_number_offset")
            }
            Self::CardanoStakeDistributionEpoch => write!(f, "cardano_stake_distribution_epoch"),
            Self::CardanoStakeDistributionMerkleRoot => {
                write!(f, "cardano_stake_distribution_merkle_root")
            }
            Self::CardanoDatabaseMerkleRoot => write!(f, "cardano_database_merkle_root"),
            Self::NextSnarkAggregateVerificationKey => {
                write!(f, "next_aggregate_verification_key_snark")
            }
        }
    }
}

/// The value of a [ProtocolMessage] part
pub type ProtocolMessagePartValue = String;

/// ProtocolMessage represents a message that is signed (or verified) by the Mithril protocol
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ProtocolMessage {
    /// Map of the messages combined into the digest
    /// aka MSG(p,n)
    pub message_parts: BTreeMap<ProtocolMessagePartKey, ProtocolMessagePartValue>,

    /// Hash scheme used to derive the protocol message digest
    #[serde(default, skip_serializing_if = "ProtocolMessageHashScheme::is_legacy")]
    pub hash_scheme: ProtocolMessageHashScheme,
}

impl ProtocolMessage {
    /// [ProtocolMessage] factory returning the default (pre-Lagrange) [ProtocolMessageHashScheme::Legacy]
    /// variant.
    pub fn new() -> ProtocolMessage {
        ProtocolMessage::default()
    }

    /// [ProtocolMessage] factory returning the rigid (Lagrange) [ProtocolMessageHashScheme::Rigid]
    /// variant.
    pub fn new_rigid() -> ProtocolMessage {
        ProtocolMessage {
            message_parts: BTreeMap::new(),
            hash_scheme: ProtocolMessageHashScheme::Rigid,
        }
    }

    /// Set the message part associated with a key
    ///
    /// Returns the previously associated value if it existed.
    pub fn set_message_part(
        &mut self,
        key: ProtocolMessagePartKey,
        value: ProtocolMessagePartValue,
    ) -> Option<ProtocolMessagePartValue> {
        self.message_parts.insert(key, value)
    }

    /// Get the message part associated with a key
    pub fn get_message_part(
        &self,
        key: &ProtocolMessagePartKey,
    ) -> Option<&ProtocolMessagePartValue> {
        self.message_parts.get(key)
    }

    /// Return `true` if the protocol message uses the [ProtocolMessageHashScheme::Rigid] hash
    /// scheme.
    pub fn is_rigid(&self) -> bool {
        matches!(self.hash_scheme, ProtocolMessageHashScheme::Rigid)
    }

    /// Compute the hex-encoded SHA-256 hash of the protocol message.
    ///
    /// Dispatches over [ProtocolMessage::hash_scheme]: the legacy scheme keeps the pre-Lagrange
    /// byte-identical output, the rigid scheme hashes the SNARK-friendly
    /// [rigid_preimage](ProtocolMessage::rigid_preimage).
    pub fn compute_hash(&self) -> String {
        match self.hash_scheme {
            ProtocolMessageHashScheme::Legacy => self.compute_legacy_hash(),
            ProtocolMessageHashScheme::Rigid => self.compute_rigid_hash(),
        }
    }

    fn compute_legacy_digest_bytes(&self) -> [u8; RIGID_DIGEST_BYTES] {
        let mut hasher = Sha256::new();
        for (key, value) in self.message_parts.iter() {
            hasher.update(key.to_string().as_bytes());
            hasher.update(value.as_bytes());
        }
        hasher.finalize().into()
    }

    fn compute_legacy_hash(&self) -> String {
        hex::encode(self.compute_legacy_digest_bytes())
    }

    fn compute_rigid_hash(&self) -> String {
        hex::encode(Sha256::digest(self.rigid_preimage()))
    }

    /// Assemble the SNARK-friendly rigid preimage from the [ProtocolMessage::message_parts] as
    /// `"digest" || digest_value || "next_aggregate_verification_key" || avk_value
    /// || "next_protocol_parameters" || protocol_parameters_value
    /// || "current_epoch" || current_epoch_value`.
    pub fn rigid_preimage(&self) -> Vec<u8> {
        let mut preimage = Vec::with_capacity(RIGID_PROTOCOL_MESSAGE_PREIMAGE_BYTES);
        preimage.extend_from_slice(RIGID_DIGEST_LABEL);
        preimage.extend_from_slice(&self.rigid_digest_field());
        preimage.extend_from_slice(RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_LABEL);
        preimage.extend_from_slice(&self.rigid_next_aggregate_verification_key_field());
        preimage.extend_from_slice(RIGID_NEXT_PROTOCOL_PARAMETERS_LABEL);
        preimage.extend_from_slice(&self.rigid_next_protocol_parameters_field());
        preimage.extend_from_slice(RIGID_CURRENT_EPOCH_LABEL);
        preimage.extend_from_slice(&self.rigid_current_epoch_field());
        preimage
    }

    /// Build the dynamic-parts projection used to compute the rigid preimage `digest` segment:
    /// strips the rigid-segment keys (next AVK, next protocol parameters, current epoch) and
    /// forces [ProtocolMessageHashScheme::Legacy] so the digest segment is hashed via the
    /// legacy preimage routine.
    #[cfg(feature = "future_snark")]
    fn stripped_for_rigid_digest(mut self) -> ProtocolMessage {
        self.hash_scheme = ProtocolMessageHashScheme::Legacy;
        for key in RIGID_SEGMENT_KEYS {
            self.message_parts.remove(key);
        }
        self
    }

    /// Validate that every rigid-segment value fits its fixed-size slot.
    ///
    /// No-op for [ProtocolMessageHashScheme::Legacy]. For [ProtocolMessageHashScheme::Rigid],
    /// surfaces the first [RigidProtocolMessageIntegrityError] among missing entry,
    /// byte-length mismatch, or non-decimal epoch. An empty dynamic-parts projection is
    /// allowed and collapses the rigid `digest` segment to a SHA-256 hash of no input.
    ///
    /// Intended for producers to call at construction time so layout violations surface before
    /// hashing or signing.
    #[cfg(feature = "future_snark")]
    pub fn check_rigid_integrity(&self) -> Result<(), RigidProtocolMessageIntegrityError> {
        if !self.is_rigid() {
            return Ok(());
        }

        let snark_avk = self
            .message_parts
            .get(&ProtocolMessagePartKey::NextSnarkAggregateVerificationKey)
            .ok_or(RigidProtocolMessageIntegrityError::MissingNextSnarkAggregateVerificationKey)?;
        let snark_avk_bytes = legacy_value_to_bytes(snark_avk);
        if snark_avk_bytes.len() != RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES {
            return Err(RigidProtocolMessageIntegrityError::UnexpectedFieldLength {
                field: "next_aggregate_verification_key",
                expected: RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES,
                actual: snark_avk_bytes.len(),
            });
        }

        let protocol_parameters = self
            .message_parts
            .get(&ProtocolMessagePartKey::NextProtocolParameters)
            .ok_or(RigidProtocolMessageIntegrityError::MissingNextProtocolParameters)?;
        decode_protocol_parameters_to_rigid_slot_bytes(protocol_parameters)?;

        let current_epoch = self
            .message_parts
            .get(&ProtocolMessagePartKey::CurrentEpoch)
            .ok_or(RigidProtocolMessageIntegrityError::MissingCurrentEpoch)?;
        current_epoch.parse::<u64>().map_err(|err| {
            RigidProtocolMessageIntegrityError::InvalidCurrentEpoch(err.to_string())
        })?;

        Ok(())
    }

    /// SHA-256 fingerprint of the dynamic message parts in the rigid `digest` segment. An
    /// empty projection is allowed and collapses to the SHA-256 hash of no input.
    #[cfg(feature = "future_snark")]
    fn rigid_digest_field(&self) -> [u8; RIGID_DIGEST_BYTES] {
        self.clone().stripped_for_rigid_digest().compute_legacy_digest_bytes()
    }

    /// Project the [ProtocolMessagePartKey::NextSnarkAggregateVerificationKey] entry into the
    /// fixed-size [RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES] rigid slot.
    ///
    /// Missing entries collapse to a zero-padded buffer and oversized decoded values are
    /// truncated to the slot width; both cases are accepted silently here. Producers running
    /// [ProtocolMessage::check_rigid_integrity] reject these as
    /// [RigidProtocolMessageIntegrityError::MissingNextSnarkAggregateVerificationKey] and
    /// [RigidProtocolMessageIntegrityError::UnexpectedFieldLength] at construction time. If the
    /// strict check is bypassed, the verifier recomputes a preimage that does not match the
    /// signed bytes and the signature-vs-message check fails as a `match_message` mismatch
    /// instead of a typed error.
    fn rigid_next_aggregate_verification_key_field(
        &self,
    ) -> [u8; RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES] {
        let mut buffer = [0u8; RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES];
        if let Some(value) = self
            .message_parts
            .get(&ProtocolMessagePartKey::NextSnarkAggregateVerificationKey)
        {
            let bytes = legacy_value_to_bytes(value);
            let length = bytes.len().min(RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES);
            buffer[..length].copy_from_slice(&bytes[..length]);
        }
        buffer
    }

    /// Project the [ProtocolMessagePartKey::NextProtocolParameters] entry into the rigid slot.
    /// Missing or out-of-width entries silently collapse to zeros;
    /// [ProtocolMessage::check_rigid_integrity] surfaces the typed error.
    #[cfg(feature = "future_snark")]
    fn rigid_next_protocol_parameters_field(&self) -> [u8; RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES] {
        self.message_parts
            .get(&ProtocolMessagePartKey::NextProtocolParameters)
            .and_then(|value| decode_protocol_parameters_to_rigid_slot_bytes(value).ok())
            .unwrap_or([0u8; RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES])
    }

    /// Encode the [ProtocolMessagePartKey::CurrentEpoch] entry as a little-endian `u64` in the
    /// rigid slot. Missing or non-decimal entries silently collapse to zeros;
    /// [ProtocolMessage::check_rigid_integrity] surfaces the typed error.
    #[cfg(feature = "future_snark")]
    fn rigid_current_epoch_field(&self) -> [u8; RIGID_CURRENT_EPOCH_BYTES] {
        self.message_parts
            .get(&ProtocolMessagePartKey::CurrentEpoch)
            .and_then(|raw| raw.parse::<u64>().ok())
            .map(|epoch| epoch.to_le_bytes())
            .unwrap_or([0u8; RIGID_CURRENT_EPOCH_BYTES])
    }

    /// Get the current epoch signed into the protocol message.
    ///
    /// Returns `None` when the [ProtocolMessagePartKey::CurrentEpoch] value is missing or not a
    /// valid base-10 unsigned integer.
    #[cfg(test)]
    pub fn get_current_epoch(&self) -> Option<Epoch> {
        self.message_parts
            .get(&ProtocolMessagePartKey::CurrentEpoch)
            .and_then(|raw| raw.parse::<u64>().ok())
            .map(Epoch)
    }

    /// Return `true` when the protocol message carries a next-epoch SNARK aggregate verification
    /// key entry.
    #[cfg(test)]
    pub fn has_next_snark_aggregate_verification_key(&self) -> bool {
        self.message_parts
            .contains_key(&ProtocolMessagePartKey::NextSnarkAggregateVerificationKey)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_protocol_message_compute_hash_include_next_aggregate_verification_key() {
        let protocol_message = ProtocolMessage::new();
        let hash_before_change = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-456".to_string(),
        );

        assert_ne!(hash_before_change, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_protocol_message_compute_hash_include_snapshot_digest() {
        let protocol_message = ProtocolMessage::new();
        let hash_before_change = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-456".to_string(),
        );

        assert_ne!(hash_before_change, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_protocol_message_compute_hash_include_cardano_transactions_merkle_root() {
        let protocol_message = ProtocolMessage::new();
        let hash_before_change = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            "ctx-merke-root-456".to_string(),
        );

        assert_ne!(hash_before_change, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_protocol_message_compute_hash_include_cardano_blocks_transactions_merkle_root() {
        let protocol_message = ProtocolMessage::new();
        let hash_before_change = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot,
            "cardano-blocks-tx-merkle-root-456".to_string(),
        );

        assert_ne!(hash_before_change, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_protocol_message_compute_hash_include_cardano_stake_distribution_epoch() {
        let protocol_message = ProtocolMessage::new();
        let hash_before_change = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionEpoch,
            "cardano-stake-distribution-epoch-456".to_string(),
        );

        assert_ne!(hash_before_change, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_protocol_message_compute_hash_include_cardano_stake_distribution_merkle_root() {
        let protocol_message = ProtocolMessage::new();
        let hash_before_change = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionMerkleRoot,
            "cardano-stake-distribution-merkle-root-456".to_string(),
        );

        assert_ne!(hash_before_change, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_protocol_message_compute_hash_include_latest_block_number() {
        let protocol_message = ProtocolMessage::new();
        let hash_before_change = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::LatestBlockNumber,
            "latest-immutable-file-number-456".to_string(),
        );

        assert_ne!(hash_before_change, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_protocol_message_compute_hash_include_cardano_database_merkle_root() {
        let protocol_message = ProtocolMessage::new();
        let hash_before_change = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
            "cardano-database-merkle-root-456".to_string(),
        );

        assert_ne!(hash_before_change, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_protocol_message_compute_hash_include_next_snark_aggregate_verification_key() {
        let protocol_message = ProtocolMessage::new();
        let hash_before_change = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
            "next-snark-avk-456".to_string(),
        );

        assert_ne!(hash_before_change, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_protocol_message_compute_hash_include_next_protocol_parameters() {
        let protocol_message = build_protocol_message_reference();
        let hash_expected = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            "latest-protocol-parameters-456".to_string(),
        );

        assert_ne!(hash_expected, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_set_message_part_calling_order_have_no_influence_on_hash_computed() {
        let mut protocol_message_a_b = build_protocol_message_reference();
        protocol_message_a_b.set_message_part(
            ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot,
            "A".to_string(),
        );
        protocol_message_a_b.set_message_part(
            ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
            "B".to_string(),
        );

        let mut protocol_message_b_a = build_protocol_message_reference();
        protocol_message_b_a.set_message_part(
            ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
            "B".to_string(),
        );
        protocol_message_b_a.set_message_part(
            ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot,
            "A".to_string(),
        );

        assert_eq!(
            protocol_message_a_b.compute_hash(),
            protocol_message_b_a.compute_hash()
        );
    }

    #[test]
    fn test_protocol_message_compute_hash_same_hash_with_same_protocol_message() {
        assert_eq!(
            build_protocol_message_reference().compute_hash(),
            build_protocol_message_reference().compute_hash()
        );
    }

    fn build_protocol_message_reference() -> ProtocolMessage {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            "next-protocol-parameters-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            "ctx-merkle-root-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot,
            "cardano-blocks-tx-merkle-root-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::LatestBlockNumber,
            "latest-immutable-file-number-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionEpoch,
            "cardano-stake-distribution-epoch-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionMerkleRoot,
            "cardano-stake-distribution-merkle-root-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
            "cardano-database-merkle-root-123".to_string(),
        );

        protocol_message
    }

    fn build_rigid_protocol_message_reference() -> ProtocolMessage {
        let mut message = ProtocolMessage::new_rigid();
        message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            hex::encode([0xAAu8; 16]),
        );
        message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            hex::encode([0xBBu8; 32]),
        );
        message.set_message_part(
            ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
            hex::encode([0xCCu8; RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES]),
        );
        message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            hex::encode([0xDDu8; RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES]),
        );
        message.set_message_part(ProtocolMessagePartKey::CurrentEpoch, "42".to_string());
        message
    }

    #[test]
    fn new_returns_a_message_with_legacy_hash_scheme_by_default() {
        let protocol_message = ProtocolMessage::new();

        assert!(!protocol_message.is_rigid());
        assert_eq!(
            protocol_message.hash_scheme,
            ProtocolMessageHashScheme::Legacy
        );
    }

    #[test]
    fn new_rigid_returns_a_message_with_rigid_hash_scheme() {
        let protocol_message = ProtocolMessage::new_rigid();

        assert!(protocol_message.is_rigid());
        assert_eq!(
            protocol_message.hash_scheme,
            ProtocolMessageHashScheme::Rigid
        );
    }

    #[test]
    fn set_message_part_works_same_on_every_hash_scheme() {
        let mut legacy = ProtocolMessage::new();
        let mut rigid = ProtocolMessage::new_rigid();

        legacy.set_message_part(ProtocolMessagePartKey::SnapshotDigest, "snap".to_string());
        rigid.set_message_part(ProtocolMessagePartKey::SnapshotDigest, "snap".to_string());

        assert_eq!(
            legacy.get_message_part(&ProtocolMessagePartKey::SnapshotDigest),
            Some(&"snap".to_string()),
        );
        assert_eq!(
            rigid.get_message_part(&ProtocolMessagePartKey::SnapshotDigest),
            Some(&"snap".to_string()),
        );
    }

    #[test]
    fn legacy_and_rigid_compute_hash_outputs_do_not_collide_on_same_map() {
        let mut legacy = build_protocol_message_reference();
        let mut rigid = legacy.clone();
        rigid.hash_scheme = ProtocolMessageHashScheme::Rigid;

        assert_ne!(legacy.compute_hash(), rigid.compute_hash());
        legacy.hash_scheme = ProtocolMessageHashScheme::Rigid;
        assert_eq!(legacy.compute_hash(), rigid.compute_hash());
    }

    #[test]
    fn rigid_compute_hash_produces_a_hex_encoded_32_bytes_digest() {
        let rigid = build_rigid_protocol_message_reference();

        let hash = rigid.compute_hash();

        assert_eq!(hash.len(), 64);
        let decoded = hex::decode(&hash).unwrap();
        assert_eq!(decoded.len(), 32);
    }

    #[test]
    fn rigid_preimage_has_expected_fixed_byte_length() {
        let rigid = ProtocolMessage::new_rigid();

        let preimage = rigid.rigid_preimage();

        assert_eq!(preimage.len(), RIGID_PROTOCOL_MESSAGE_PREIMAGE_BYTES);
    }

    #[test]
    fn rigid_preimage_concatenates_labeled_segments_in_a_fixed_order() {
        let rigid = build_rigid_protocol_message_reference();

        let mut expected = Vec::new();
        expected.extend_from_slice(b"digest");
        expected.extend_from_slice(&rigid.rigid_digest_field());
        expected.extend_from_slice(b"next_aggregate_verification_key");
        expected.extend_from_slice(&rigid.rigid_next_aggregate_verification_key_field());
        expected.extend_from_slice(b"next_protocol_parameters");
        expected.extend_from_slice(&rigid.rigid_next_protocol_parameters_field());
        expected.extend_from_slice(b"current_epoch");
        expected.extend_from_slice(&rigid.rigid_current_epoch_field());

        assert_eq!(rigid.rigid_preimage(), expected);
    }

    #[test]
    fn rigid_preimage_layout_pins_label_offsets_and_segment_lengths() {
        let rigid = build_rigid_protocol_message_reference();

        let preimage = rigid.rigid_preimage();

        let digest_label = b"digest";
        let avk_label = b"next_aggregate_verification_key";
        let protocol_parameters_label = b"next_protocol_parameters";
        let current_epoch_label = b"current_epoch";

        let mut offset = 0usize;

        assert_eq!(
            &preimage[offset..offset + digest_label.len()],
            digest_label,
            "the rigid preimage must start with the `digest` ASCII label"
        );
        offset += digest_label.len();
        assert_eq!(
            &preimage[offset..offset + RIGID_DIGEST_BYTES],
            &rigid.rigid_digest_field()[..],
            "the digest segment must follow its ASCII label"
        );
        offset += RIGID_DIGEST_BYTES;

        assert_eq!(
            &preimage[offset..offset + avk_label.len()],
            avk_label,
            "the rigid preimage must include the `next_aggregate_verification_key` label"
        );
        offset += avk_label.len();
        assert_eq!(
            &preimage[offset..offset + RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES],
            &rigid.rigid_next_aggregate_verification_key_field()[..],
            "the next aggregate verification key segment must follow its ASCII label"
        );
        offset += RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES;

        assert_eq!(
            &preimage[offset..offset + protocol_parameters_label.len()],
            protocol_parameters_label,
            "the rigid preimage must include the `next_protocol_parameters` label"
        );
        offset += protocol_parameters_label.len();
        assert_eq!(
            &preimage[offset..offset + RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES],
            &rigid.rigid_next_protocol_parameters_field()[..],
            "the next protocol parameters segment must follow its ASCII label"
        );
        offset += RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES;

        assert_eq!(
            &preimage[offset..offset + current_epoch_label.len()],
            current_epoch_label,
            "the rigid preimage must include the `current_epoch` label"
        );
        offset += current_epoch_label.len();
        assert_eq!(
            &preimage[offset..offset + RIGID_CURRENT_EPOCH_BYTES],
            &rigid.rigid_current_epoch_field()[..],
            "the current epoch segment must follow its ASCII label"
        );
        offset += RIGID_CURRENT_EPOCH_BYTES;

        assert_eq!(
            offset,
            preimage.len(),
            "the rigid preimage must contain only the four labeled segments"
        );
    }

    #[test]
    fn rigid_preimage_total_byte_length_is_pinned_to_one_hundred_ninety() {
        let rigid = build_rigid_protocol_message_reference();

        assert_eq!(
            rigid.rigid_preimage().len(),
            190,
            "the rigid preimage must be 190 bytes: 4 ASCII labels (6+31+24+13) plus the four value segments (32+44+32+8)"
        );
    }

    #[test]
    fn rigid_preimage_sources_aggregate_verification_key_segment_from_snark_avk_value() {
        let mut message = ProtocolMessage::new_rigid();
        let snark_avk = [0xCDu8; RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES];
        message.set_message_part(
            ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
            hex::encode(snark_avk),
        );

        assert_eq!(
            message.rigid_next_aggregate_verification_key_field(),
            snark_avk,
            "the rigid AVK segment must be the raw bytes of the SNARK aggregate verification key"
        );
    }

    #[test]
    fn serde_round_trips_legacy_shape() {
        let protocol_message = build_protocol_message_reference();

        let json = serde_json::to_string(&protocol_message).unwrap();
        let restored: ProtocolMessage = serde_json::from_str(&json).unwrap();

        assert_eq!(protocol_message, restored);
        assert_eq!(restored.hash_scheme, ProtocolMessageHashScheme::Legacy);
    }

    #[test]
    fn serde_round_trips_rigid_shape() {
        let protocol_message = build_rigid_protocol_message_reference();

        let json = serde_json::to_string(&protocol_message).unwrap();
        let restored: ProtocolMessage = serde_json::from_str(&json).unwrap();

        assert_eq!(protocol_message, restored);
        assert_eq!(restored.hash_scheme, ProtocolMessageHashScheme::Rigid);
    }

    #[test]
    fn legacy_wire_shape_omits_hash_scheme_field_for_backward_compatibility() {
        let protocol_message = build_protocol_message_reference();

        let json_value: serde_json::Value = serde_json::to_value(&protocol_message).unwrap();

        let object = json_value
            .as_object()
            .expect("legacy wire shape must be a JSON object");
        assert!(
            !object.contains_key("hash_scheme"),
            "legacy protocol message must not emit a `hash_scheme` field so pre-Lagrange JSON stays byte-identical"
        );
        assert!(
            object.contains_key("message_parts"),
            "legacy wire shape must still expose the `message_parts` field"
        );
    }

    #[test]
    fn rigid_wire_shape_exposes_hash_scheme_discriminator() {
        let protocol_message = build_rigid_protocol_message_reference();

        let json_value: serde_json::Value = serde_json::to_value(&protocol_message).unwrap();

        assert_eq!(
            json_value.get("hash_scheme").and_then(|v| v.as_str()),
            Some("rigid")
        );
    }

    #[test]
    fn deserializing_a_payload_without_hash_scheme_defaults_to_legacy() {
        let legacy_json = serde_json::json!({
            "message_parts": { "snapshot_digest": "abc" }
        });

        let protocol_message: ProtocolMessage = serde_json::from_value(legacy_json).unwrap();

        assert_eq!(
            protocol_message.hash_scheme,
            ProtocolMessageHashScheme::Legacy
        );
        assert_eq!(
            protocol_message.get_message_part(&ProtocolMessagePartKey::SnapshotDigest),
            Some(&"abc".to_string())
        );
    }

    #[test]
    fn legacy_wire_shape_is_pinned_to_its_pre_lagrange_json_representation() {
        let pinned_json_before_rework =
            r#"{"message_parts":{"snapshot_digest":"snapshot-digest-123"}}"#;
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-123".to_string(),
        );

        let json = serde_json::to_string(&protocol_message).unwrap();

        assert_eq!(
            json, pinned_json_before_rework,
            "the legacy wire shape must stay byte-identical to the pre-Lagrange serialization"
        );
    }

    #[test]
    fn legacy_compute_hash_is_pinned_to_its_pre_lagrange_byte_identical_output() {
        let hash_pinned_before_rework =
            "9b9c1b930b151abea9e3ddbd101b156d037c6f003dfbf5391d56b5d5a56b6138";

        assert_eq!(
            build_protocol_message_reference().compute_hash(),
            hash_pinned_before_rework,
            "the legacy protocol message hash must stay byte-identical to the pre-Lagrange output"
        );
    }

    #[test]
    fn rigid_next_protocol_parameters_field_holds_raw_hex_decoded_bytes() {
        let mut message = ProtocolMessage::new_rigid();
        let raw = [0xDDu8; RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES];
        message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            hex::encode(raw),
        );

        assert_eq!(message.rigid_next_protocol_parameters_field(), raw);
    }

    #[test]
    fn rigid_current_epoch_field_is_little_endian_encoded_parsed_integer() {
        let mut message = ProtocolMessage::new_rigid();
        message.set_message_part(ProtocolMessagePartKey::CurrentEpoch, "7".to_string());

        assert_eq!(message.rigid_current_epoch_field(), 7u64.to_le_bytes());
    }

    #[test]
    fn stripped_for_rigid_digest_drops_only_rigid_segment_keys_and_forces_legacy_hash_scheme() {
        let mut rigid = build_rigid_protocol_message_reference();
        rigid.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-keep".to_string(),
        );

        let stripped = rigid.stripped_for_rigid_digest();

        assert_eq!(stripped.hash_scheme, ProtocolMessageHashScheme::Legacy);
        for key in RIGID_SEGMENT_KEYS {
            assert!(
                stripped.get_message_part(key).is_none(),
                "rigid segment key {key} must be stripped from the digest projection"
            );
        }
        assert_eq!(
            stripped.get_message_part(&ProtocolMessagePartKey::SnapshotDigest),
            Some(&"snapshot-digest-keep".to_string()),
            "non-rigid keys must survive the stripping step"
        );
    }

    #[test]
    fn rigid_digest_field_is_invariant_under_changes_of_rigid_segment_keys() {
        let mut base = build_rigid_protocol_message_reference();
        let baseline = base.rigid_digest_field();

        for key in RIGID_SEGMENT_KEYS {
            base.set_message_part(*key, "tampered".to_string());
        }

        assert_eq!(
            base.rigid_digest_field(),
            baseline,
            "the rigid digest segment must depend only on dynamic (non-rigid) message parts"
        );
    }

    #[test]
    fn rigid_compute_hash_changes_when_digest_related_parts_change() {
        let mut base = build_rigid_protocol_message_reference();
        let base_hash = base.compute_hash();

        base.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            hex::encode([0xFEu8; 16]),
        );

        assert_ne!(base_hash, base.compute_hash());
    }

    #[test]
    fn has_next_snark_aggregate_verification_key_detects_presence() {
        let mut message = ProtocolMessage::new();
        assert!(!message.has_next_snark_aggregate_verification_key());

        message.set_message_part(
            ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
            hex::encode([0xABu8; 44]),
        );
        assert!(message.has_next_snark_aggregate_verification_key());
    }

    #[test]
    fn get_current_epoch_parses_stored_decimal_value() {
        let mut message = ProtocolMessage::new();
        assert_eq!(message.get_current_epoch(), None);

        message.set_message_part(ProtocolMessagePartKey::CurrentEpoch, "42".to_string());
        assert_eq!(message.get_current_epoch(), Some(Epoch(42)));

        message.set_message_part(ProtocolMessagePartKey::CurrentEpoch, "oops".to_string());
        assert_eq!(message.get_current_epoch(), None);
    }

    #[test]
    fn rigid_preimage_is_byte_identical_to_a_hand_built_labeled_concatenation() {
        let snark_avk_bytes = [5u8; RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES];
        let protocol_params_bytes = [3u8; RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES];
        let epoch_value = 12345u64;

        let mut rigid = ProtocolMessage::new_rigid();
        rigid.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-source".to_string(),
        );
        rigid.set_message_part(
            ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
            hex::encode(snark_avk_bytes),
        );
        rigid.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            hex::encode(protocol_params_bytes),
        );
        rigid.set_message_part(
            ProtocolMessagePartKey::CurrentEpoch,
            epoch_value.to_string(),
        );

        let mut expected = Vec::new();
        expected.extend_from_slice(b"digest");
        expected.extend_from_slice(&rigid.rigid_digest_field());
        expected.extend_from_slice(b"next_aggregate_verification_key");
        expected.extend_from_slice(&snark_avk_bytes);
        expected.extend_from_slice(b"next_protocol_parameters");
        expected.extend_from_slice(&protocol_params_bytes);
        expected.extend_from_slice(b"current_epoch");
        expected.extend_from_slice(&epoch_value.to_le_bytes());

        assert_eq!(
            expected,
            rigid.rigid_preimage(),
            "the rigid preimage must be the byte-identical concatenation of each ASCII label followed by its raw value bytes"
        );
    }

    #[test]
    fn check_rigid_integrity_is_a_no_op_for_legacy_protocol_messages() {
        let legacy = build_protocol_message_reference();

        legacy
            .check_rigid_integrity()
            .expect("legacy protocol message must skip the rigid layout check");
    }

    #[test]
    fn check_rigid_integrity_succeeds_on_a_well_formed_rigid_protocol_message() {
        let rigid = build_rigid_protocol_message_reference();

        rigid
            .check_rigid_integrity()
            .expect("a well-formed rigid protocol message must pass the integrity check");
    }

    #[test]
    fn check_rigid_integrity_allows_an_empty_dynamic_digest_projection() {
        let mut rigid = ProtocolMessage::new_rigid();
        rigid.set_message_part(
            ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
            build_snark_avk_wire_value_for_test([0xCCu8; 32], 0),
        );
        rigid.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            hex::encode([0xDDu8; RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES]),
        );
        rigid.set_message_part(ProtocolMessagePartKey::CurrentEpoch, "1".to_string());

        rigid
            .check_rigid_integrity()
            .expect("an empty dynamic-parts projection must be accepted");
    }

    #[test]
    fn check_rigid_integrity_fails_when_next_snark_avk_entry_is_missing() {
        let mut rigid = build_rigid_protocol_message_reference();
        rigid
            .message_parts
            .remove(&ProtocolMessagePartKey::NextSnarkAggregateVerificationKey);

        let error = rigid
            .check_rigid_integrity()
            .expect_err("missing rigid SNARK AVK entry must surface an error");

        assert_eq!(
            error,
            RigidProtocolMessageIntegrityError::MissingNextSnarkAggregateVerificationKey
        );
    }

    #[test]
    fn check_rigid_integrity_fails_when_next_snark_avk_decodes_to_unexpected_length() {
        let mut rigid = build_rigid_protocol_message_reference();
        rigid.set_message_part(
            ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
            hex::encode([0xCCu8; RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES - 1]),
        );

        let error = rigid
            .check_rigid_integrity()
            .expect_err("ill-formed rigid SNARK AVK entry must surface an error");

        assert_eq!(
            error,
            RigidProtocolMessageIntegrityError::UnexpectedFieldLength {
                field: "next_aggregate_verification_key",
                expected: RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES,
                actual: RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_BYTES - 1,
            }
        );
    }

    #[test]
    fn check_rigid_integrity_fails_when_next_protocol_parameters_entry_is_missing() {
        let mut rigid = build_rigid_protocol_message_reference();
        rigid
            .message_parts
            .remove(&ProtocolMessagePartKey::NextProtocolParameters);

        let error = rigid
            .check_rigid_integrity()
            .expect_err("missing rigid protocol parameters entry must surface an error");

        assert_eq!(
            error,
            RigidProtocolMessageIntegrityError::MissingNextProtocolParameters
        );
    }

    #[test]
    fn check_rigid_integrity_fails_when_next_protocol_parameters_decodes_to_unexpected_length() {
        let mut rigid = build_rigid_protocol_message_reference();
        rigid.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            hex::encode([0xDDu8; RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES + 1]),
        );

        let error = rigid
            .check_rigid_integrity()
            .expect_err("ill-formed rigid protocol parameters entry must surface an error");

        assert_eq!(
            error,
            RigidProtocolMessageIntegrityError::UnexpectedFieldLength {
                field: "next_protocol_parameters",
                expected: RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES,
                actual: RIGID_NEXT_PROTOCOL_PARAMETERS_BYTES + 1,
            }
        );
    }

    #[test]
    fn check_rigid_integrity_fails_when_current_epoch_entry_is_missing() {
        let mut rigid = build_rigid_protocol_message_reference();
        rigid.message_parts.remove(&ProtocolMessagePartKey::CurrentEpoch);

        let error = rigid
            .check_rigid_integrity()
            .expect_err("missing rigid current epoch entry must surface an error");

        assert_eq!(
            error,
            RigidProtocolMessageIntegrityError::MissingCurrentEpoch
        );
    }

    #[test]
    fn check_rigid_integrity_fails_when_current_epoch_is_not_a_decimal_unsigned_integer() {
        let mut rigid = build_rigid_protocol_message_reference();
        rigid.set_message_part(ProtocolMessagePartKey::CurrentEpoch, "oops".to_string());

        let error = rigid
            .check_rigid_integrity()
            .expect_err("non-decimal rigid current epoch entry must surface an error");

        assert!(
            matches!(
                error,
                RigidProtocolMessageIntegrityError::InvalidCurrentEpoch(_)
            ),
            "unexpected error variant: {error:?}"
        );
    }
}
