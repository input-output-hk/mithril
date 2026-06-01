//! Test/generator-side protocol message builder for Halo2 IVC circuit assets.

use std::collections::BTreeMap;

use anyhow::{Context, anyhow};
use sha2::{Digest as Sha2Digest, Sha256};

use crate::{AggregateVerificationKeyForSnark, MembershipDigest, StmResult};

use super::PREIMAGE_SIZE;

// Rigid labels match `mithril-common::ProtocolMessage::rigid_preimage()` byte-for-byte.
// These are NOT derived from the key Display strings; the Display strings are only
// used in the dynamic-parts SHA256 computation.
const RIGID_DIGEST_LABEL: &[u8] = b"digest";
const RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_LABEL: &[u8] = b"next_aggregate_verification_key";
const RIGID_NEXT_PROTOCOL_PARAMETERS_LABEL: &[u8] = b"next_protocol_parameters";
const RIGID_CURRENT_EPOCH_LABEL: &[u8] = b"current_epoch";

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ProtocolMessagePartKey {
    SnapshotDigest,
    NextSnarkAggregateVerificationKey,
    NextProtocolParameters,
    CurrentEpoch,
}

impl std::fmt::Display for ProtocolMessagePartKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SnapshotDigest => write!(f, "snapshot_digest"),
            Self::NextSnarkAggregateVerificationKey => {
                write!(f, "next_aggregate_verification_key_snark")
            }
            Self::NextProtocolParameters => write!(f, "next_protocol_parameters"),
            Self::CurrentEpoch => write!(f, "current_epoch"),
        }
    }
}

pub(crate) struct ProtocolMessage {
    message_parts: BTreeMap<ProtocolMessagePartKey, String>,
}

/// Formats the SNARK AVK protocol-message value like `mithril-common` does through
/// `ProtocolAggregateVerificationKeyForSnark::to_bytes_hex()`.
pub(crate) fn aggregate_verification_key_message_part<D: MembershipDigest>(
    aggregate_verification_key: &AggregateVerificationKeyForSnark<D>,
) -> StmResult<String> {
    let bytes = aggregate_verification_key
        .to_bytes()
        .context("aggregate verification key serialization failed")?;
    Ok(hex::encode(bytes))
}

impl ProtocolMessage {
    pub(crate) fn new() -> Self {
        Self {
            message_parts: BTreeMap::new(),
        }
    }

    pub(crate) fn set_message_part(&mut self, key: ProtocolMessagePartKey, value: String) {
        self.message_parts.insert(key, value);
    }

    fn required_message_part(
        &self,
        key: ProtocolMessagePartKey,
        name: &'static str,
    ) -> StmResult<&str> {
        self.message_parts
            .get(&key)
            .map(String::as_str)
            .ok_or_else(|| anyhow!("{name} slot is required"))
    }

    /// Assembles the 190-byte rigid preimage that matches `mithril-common::ProtocolMessage::rigid_preimage()`.
    ///
    /// Layout:
    /// ```text
    /// "digest"                           (6 bytes)
    /// || SHA256(dynamic_parts)           (32 bytes)
    /// || "next_aggregate_verification_key" (31 bytes)
    /// || avk_slot                        (44 bytes)
    /// || "next_protocol_parameters"      (24 bytes)
    /// || params_slot                     (32 bytes)
    /// || "current_epoch"                 (13 bytes)
    /// || epoch_slot                      (8 bytes)
    /// = 190 bytes
    /// ```
    pub(crate) fn try_rigid_preimage<D: MembershipDigest>(&self) -> StmResult<[u8; PREIMAGE_SIZE]> {
        let dynamic_hash = self.compute_dynamic_parts_hash();
        let avk_slot = self.avk_slot_from_key::<D>()?;
        let params_slot = self.params_slot_from_key()?;
        let epoch_slot = self.epoch_slot_from_key()?;

        let mut preimage = [0u8; PREIMAGE_SIZE];
        let mut cursor = 0;

        // offset 0..6
        preimage[cursor..cursor + RIGID_DIGEST_LABEL.len()].copy_from_slice(RIGID_DIGEST_LABEL);
        cursor += RIGID_DIGEST_LABEL.len();

        // offset 6..38
        preimage[cursor..cursor + 32].copy_from_slice(&dynamic_hash);
        cursor += 32;

        // offset 38..69
        preimage[cursor..cursor + RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_LABEL.len()]
            .copy_from_slice(RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_LABEL);
        cursor += RIGID_NEXT_AGGREGATE_VERIFICATION_KEY_LABEL.len();

        // offset 69..113 — AVK slot; circuit reads Merkle root from PREIMAGE_NEXT_MERKLE_ROOT_BYTES (69..101)
        preimage[cursor..cursor + avk_slot.len()].copy_from_slice(&avk_slot);
        cursor += avk_slot.len();

        // offset 113..137
        preimage[cursor..cursor + RIGID_NEXT_PROTOCOL_PARAMETERS_LABEL.len()]
            .copy_from_slice(RIGID_NEXT_PROTOCOL_PARAMETERS_LABEL);
        cursor += RIGID_NEXT_PROTOCOL_PARAMETERS_LABEL.len();

        // offset 137..169 — params slot; circuit reads from PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES (137..169)
        preimage[cursor..cursor + params_slot.len()].copy_from_slice(&params_slot);
        cursor += params_slot.len();

        // offset 169..182
        preimage[cursor..cursor + RIGID_CURRENT_EPOCH_LABEL.len()]
            .copy_from_slice(RIGID_CURRENT_EPOCH_LABEL);
        cursor += RIGID_CURRENT_EPOCH_LABEL.len();

        // offset 182..190 — epoch slot; circuit reads from PREIMAGE_CURRENT_EPOCH_BYTES (182..190)
        preimage[cursor..cursor + epoch_slot.len()].copy_from_slice(&epoch_slot);

        Ok(preimage)
    }

    /// SHA256 of the dynamic parts: all keys except the three rigid fixed-slot keys,
    /// in BTreeMap order: `SHA256(key.to_string() || value)` for each remaining entry.
    fn compute_dynamic_parts_hash(&self) -> [u8; 32] {
        let mut hasher = Sha256::new();
        for (key, value) in &self.message_parts {
            if matches!(
                key,
                ProtocolMessagePartKey::NextSnarkAggregateVerificationKey
                    | ProtocolMessagePartKey::NextProtocolParameters
                    | ProtocolMessagePartKey::CurrentEpoch
            ) {
                continue;
            }
            hasher.update(key.to_string().as_bytes());
            hasher.update(value.as_bytes());
        }
        hasher.finalize().into()
    }

    fn avk_slot_from_key<D: MembershipDigest>(&self) -> StmResult<[u8; 44]> {
        let value = self.required_message_part(
            ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
            "next SNARK aggregate verification key",
        )?;
        let bytes =
            hex::decode(value).context("invalid next SNARK aggregate verification key hex")?;
        let avk = AggregateVerificationKeyForSnark::<D>::from_bytes(&bytes)
            .context("invalid next SNARK aggregate verification key bytes")?;
        avk.to_rigid_slot_bytes()
    }

    fn params_slot_from_key(&self) -> StmResult<[u8; 32]> {
        let value = self.required_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            "next protocol parameters",
        )?;
        let bytes = hex::decode(value).context("invalid next protocol parameters hex")?;
        let actual = bytes.len();
        bytes.try_into().map_err(|_| {
            anyhow!("next protocol parameters slot must be exactly 32 bytes, got {actual}")
        })
    }

    fn epoch_slot_from_key(&self) -> StmResult<[u8; 8]> {
        let value =
            self.required_message_part(ProtocolMessagePartKey::CurrentEpoch, "current epoch")?;
        let epoch: u64 = value.parse().context("invalid current epoch slot")?;
        Ok(epoch.to_le_bytes())
    }
}
