use crate::circuits::halo2_ivc::helpers::merkle_tree::MerkleTreeCommitment;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fmt::Display;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProtocolMessagePartKey {
    /// The ProtocolMessage part key associated to the general Digest
    #[serde(rename = "digest")]
    Digest,

    /// The ProtocolMessage part key associated to the Snapshot Digest
    #[serde(rename = "snapshot_digest")]
    SnapshotDigest,

    /// The ProtocolMessage part key associated to the Cardano Transactions Merkle Root
    #[serde(rename = "cardano_transactions_merkle_root")]
    CardanoTransactionsMerkleRoot,

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

    /// The ProtocolMessage part key associated to the epoch for which the Cardano stake distribution is computed
    #[serde(rename = "cardano_stake_distribution_epoch")]
    CardanoStakeDistributionEpoch,

    /// The ProtocolMessage part key associated to the Cardano stake distribution Merkle root
    #[serde(rename = "cardano_stake_distribution_merkle_root")]
    CardanoStakeDistributionMerkleRoot,

    /// The ProtocolMessage part key associated to the Cardano database Merkle root
    #[serde(rename = "cardano_database_merkle_root")]
    CardanoDatabaseMerkleRoot,
}

impl Display for ProtocolMessagePartKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Digest => write!(f, "digest"),
            Self::SnapshotDigest => write!(f, "snapshot_digest"),
            Self::NextAggregateVerificationKey => write!(f, "next_aggregate_verification_key"),
            Self::NextProtocolParameters => write!(f, "next_protocol_parameters"),
            Self::CurrentEpoch => write!(f, "current_epoch"),
            Self::CardanoTransactionsMerkleRoot => write!(f, "cardano_transactions_merkle_root"),
            Self::LatestBlockNumber => write!(f, "latest_block_number"),
            Self::CardanoStakeDistributionEpoch => write!(f, "cardano_stake_distribution_epoch"),
            Self::CardanoStakeDistributionMerkleRoot => {
                write!(f, "cardano_stake_distribution_merkle_root")
            }
            Self::CardanoDatabaseMerkleRoot => write!(f, "cardano_database_merkle_root"),
        }
    }
}

pub type ProtocolMessagePartValue = Vec<u8>;

/// ProtocolMessage represents a message that is signed (or verified) by the Mithril protocol
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ProtocolMessage {
    /// Map of the messages combined into the digest
    /// aka MSG(p,n)
    pub message_parts: BTreeMap<ProtocolMessagePartKey, ProtocolMessagePartValue>,
}

impl ProtocolMessage {
    /// ProtocolMessage factory
    pub fn new() -> ProtocolMessage {
        ProtocolMessage {
            message_parts: BTreeMap::new(),
        }
    }

    /// Set the message part associated with a key
    /// Returns previously set value if it exists
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

    pub fn get_preimage(&self) -> Vec<u8> {
        let mut preimage = Vec::new();
        self.message_parts.iter().for_each(|(k, v)| {
            preimage.extend_from_slice(k.to_string().as_bytes());
            preimage.extend_from_slice(&v);
        });
        preimage
    }

    /// Computes the hash of the protocol message
    pub fn compute_hash(&self) -> Vec<u8> {
        let preimage = self.get_preimage();
        Sha256::digest(&preimage).to_vec()
    }
}

#[derive(Debug, Clone)]
pub struct AggregateVerificationKey {
    mt_commit: MerkleTreeCommitment,
    total_stake: u64,
}

impl AggregateVerificationKey {
    pub fn new(mt_commit: MerkleTreeCommitment, total_stake: u64) -> Self {
        Self {
            mt_commit,
            total_stake,
        }
    }
}

impl From<AggregateVerificationKey> for Vec<u8> {
    fn from(avk: AggregateVerificationKey) -> Vec<u8> {
        let mut bytes = Vec::from(avk.mt_commit);
        bytes.extend_from_slice(&avk.total_stake.to_le_bytes());
        bytes
    }
}

impl TryFrom<&[u8]> for AggregateVerificationKey {
    type Error = &'static str;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if bytes.len() != 44 {
            return Err("Invalid byte length for AggregateVerificationKey");
        }

        let mt_commit = bytes[0..36].try_into().unwrap();
        let total_stake = u64::from_le_bytes(bytes[36..44].try_into().unwrap());

        Ok(AggregateVerificationKey {
            mt_commit,
            total_stake,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Epoch(pub u64);

impl From<Epoch> for Vec<u8> {
    fn from(epoch: Epoch) -> Vec<u8> {
        epoch.0.to_le_bytes().to_vec()
    }
}

impl TryFrom<&[u8]> for Epoch {
    type Error = &'static str;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if bytes.len() != 8 {
            return Err("Invalid byte length for AggregateVerificationKey");
        }

        let num = u64::from_le_bytes(bytes.try_into().unwrap());
        Ok(Epoch(num))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn all_key() -> ([ProtocolMessagePartKey; 10], [usize; 10]) {
        let keys = [
            ProtocolMessagePartKey::Digest,
            ProtocolMessagePartKey::SnapshotDigest,
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            ProtocolMessagePartKey::NextProtocolParameters,
            ProtocolMessagePartKey::CurrentEpoch,
            ProtocolMessagePartKey::LatestBlockNumber,
            ProtocolMessagePartKey::CardanoStakeDistributionEpoch,
            ProtocolMessagePartKey::CardanoStakeDistributionMerkleRoot,
            ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
        ];
        let lens = [6, 15, 32, 31, 24, 13, 19, 32, 38, 28];
        (keys, lens)
    }

    fn build_protocol_message_reference() -> ProtocolMessage {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, vec![0u8; 32]);
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            vec![0u8; 44],
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            vec![0u8; 32],
        );
        protocol_message.set_message_part(ProtocolMessagePartKey::CurrentEpoch, vec![0u8; 8]);
        protocol_message
    }

    #[test]
    fn test_protocol_message_hash() {
        let protocol_message = build_protocol_message_reference();
        let hash = protocol_message.compute_hash();

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            vec![3u8; 44],
        );

        assert_ne!(hash, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_protocol_message_keys() {
        let (keys, lens) = all_key();
        assert_eq!(keys.len(), lens.len());

        for (key, len) in keys.iter().zip(lens.iter()) {
            let s = key.to_string();
            assert_eq!(s.len(), *len);
        }
    }
}
