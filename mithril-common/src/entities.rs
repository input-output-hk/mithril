//! The entities used by, and exchanged between, the aggregator, signers and client.

use crate::crypto_helper::{key_decode_hex, ProtocolSingleSignature};
use fixed::types::U8F24;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::{collections::BTreeMap, collections::HashMap, fmt::Display};
use thiserror::Error;

/// Epoch represents a Cardano epoch
pub type Epoch = u64;

/// ImmutableFileNumber represents the id of immutable files in the Cardano node database
pub type ImmutableFileNumber = u64;

/// PartyId represents a signing party in Mithril protocol
pub type PartyId = String;

/// Stake represents the stakes of a participant in the Cardano chain
pub type Stake = u64;

/// StakeDistribution represents the stakes of multiple participants in the Cardano chain
pub type StakeDistribution = HashMap<PartyId, Stake>;

/// LotteryIndex represents the index of a Mithril single signature lottery
pub type LotteryIndex = u64;

/// Cardano Network magic identifier
pub type MagicId = u64;

/// Protocol version
pub type ProtocolVersion = String;

/// The Cardano Network that is being targeted
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize, Hash, Eq, PartialOrd)]
pub enum CardanoNetwork {
    /// The Cardano mainnet
    MainNet,

    /// The Cardano testnet
    TestNet(MagicId),

    /// A Cardano private devnet
    DevNet(MagicId),
}

impl Display for CardanoNetwork {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            CardanoNetwork::MainNet => write!(f, "mainnet"),
            CardanoNetwork::DevNet(_) => write!(f, "devnet"),
            CardanoNetwork::TestNet(_) => write!(f, "testnet"),
        }
    }
}

/// Beacon represents a point in the Cardano chain at which a Mithril certificate should be produced
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize, Hash)]
pub struct Beacon {
    /// Cardano network
    #[serde(rename = "network")]
    pub network: String,

    /// Cardano chain epoch number
    #[serde(rename = "epoch")]
    pub epoch: Epoch,

    /// Number of the last included immutable files for the digest computation
    #[serde(rename = "immutable_file_number")]
    pub immutable_file_number: ImmutableFileNumber,
}

impl PartialOrd for Beacon {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.network.partial_cmp(&other.network) {
            Some(core::cmp::Ordering::Equal) => {}
            _ord => return None,
        };
        match self.epoch.partial_cmp(&other.epoch) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.immutable_file_number
            .partial_cmp(&other.immutable_file_number)
    }
}

impl Beacon {
    /// Beacon factory
    pub fn new(
        network: String,
        epoch: Epoch,
        immutable_file_number: ImmutableFileNumber,
    ) -> Beacon {
        Beacon {
            network,
            epoch,
            immutable_file_number,
        }
    }

    /// Computes the hash of a Beacon
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.network.as_bytes());
        hasher.update(self.epoch.to_be_bytes());
        hasher.update(self.immutable_file_number.to_be_bytes());
        hex::encode(hasher.finalize())
    }

    /// Computes a new Beacon by applying an epoch offset
    pub fn compute_beacon_with_epoch_offset(&self, epoch_offset: i64) -> Result<Self, BeaconError> {
        let mut beacon = self.clone();
        let epoch_new = beacon.epoch as i64 + epoch_offset;
        if epoch_new < 0 {
            return Err(BeaconError::EpochOffset(beacon.epoch, epoch_offset));
        }
        beacon.epoch = epoch_new as u64;
        Ok(beacon)
    }
}

/// BeaconError is an error triggerred by a Beacon
#[derive(Error, Debug)]
pub enum BeaconError {
    /// Error raised when the computation of an epoch using an offset fails.
    #[error("epoch offset error")]
    EpochOffset(u64, i64),
}

/// CertificatePending represents a pending certificate in the process of production
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct CertificatePending {
    /// Current Beacon
    #[serde(rename = "beacon")]
    pub beacon: Beacon,

    /// Current Protocol parameters
    #[serde(rename = "protocol")]
    pub protocol_parameters: ProtocolParameters,

    /// Current Signers
    #[serde(rename = "signers")]
    pub signers: Vec<Signer>,
}

impl CertificatePending {
    /// CertificatePending factory
    pub fn new(
        beacon: Beacon,
        protocol_parameters: ProtocolParameters,
        signers: Vec<Signer>,
    ) -> CertificatePending {
        CertificatePending {
            beacon,
            protocol_parameters,
            signers,
        }
    }
}

/// The key of a ProtocolMessage
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProtocolMessagePartKey {
    /// The ProtocolMessage part key associated to the Snapshot Digest
    #[serde(rename = "snapshot_digest")]
    SnapshotDigest,

    /// The ProtocolMessage part key associated to the Next epoch aggregate verification key
    /// The AVK that will be allowed to be used to sign during the next epoch
    /// aka AVK(n-1)
    #[serde(rename = "next_aggregate_verification_key")]
    NextAggregateVerificationKey,
}

impl Display for ProtocolMessagePartKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::SnapshotDigest => write!(f, "snapshot_digest"),
            Self::NextAggregateVerificationKey => write!(f, "next_aggregate_verification_key"),
        }
    }
}

/// The value of a ProtocolMessage
pub type ProtocolMessagePartValue = String;

/// ProtocolMessage represents a message that is signed (or verified) by the Mithril protocol
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct ProtocolMessage {
    /// Map of the messages combined into the digest
    /// aka MSG(p,n)
    #[serde(rename = "message_parts")]
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

    /// Computes the hash of the protocol message
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        self.message_parts.iter().for_each(|(k, v)| {
            hasher.update(k.to_string().as_bytes());
            hasher.update(v.as_bytes());
        });
        hex::encode(hasher.finalize())
    }
}

/// CertificateMetadata represents the metadata associated to a Certificate
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct CertificateMetadata {
    /// Protocol Version (semver)
    /// Useful to achieve backward compatibility of the certificates (including of the multi signature)
    /// part of METADATA(p,n)
    #[serde(rename = "version")]
    pub protocol_version: ProtocolVersion,

    /// Protocol parameters
    /// part of METADATA(p,n)
    #[serde(rename = "parameters")]
    pub protocol_parameters: ProtocolParameters,

    /// Date and time when the certificate was initiated
    /// Represents the time at which the single signatures registration is opened
    /// part of METADATA(p,n)
    #[serde(rename = "initiated_at")]
    pub initiated_at: String,

    /// Date and time when the certificate was sealed
    /// Represents the time at which the quorum of single signatures was reached so that they were aggregated into a multi signature
    /// part of METADATA(p,n)
    #[serde(rename = "sealed_at")]
    pub sealed_at: String,

    /// The list of the active signers with their stakes and verification keys
    /// part of METADATA(p,n)
    #[serde(rename = "signers")]
    pub signers: Vec<SignerWithStake>,
}

impl CertificateMetadata {
    /// CertificateMetadata factory
    pub fn new(
        protocol_version: ProtocolVersion,
        protocol_parameters: ProtocolParameters,
        initiated_at: String,
        sealed_at: String,
        signers: Vec<SignerWithStake>,
    ) -> CertificateMetadata {
        CertificateMetadata {
            protocol_version,
            protocol_parameters,
            initiated_at,
            sealed_at,
            signers,
        }
    }

    /// Computes the hash of the certificate metadata
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.protocol_version.as_bytes());
        hasher.update(&self.protocol_parameters.compute_hash().as_bytes());
        hasher.update(self.initiated_at.as_bytes());
        hasher.update(self.sealed_at.as_bytes());
        self.signers
            .iter()
            .for_each(|signer| hasher.update(signer.compute_hash().as_bytes()));
        hex::encode(hasher.finalize())
    }
}

/// Certificate represents a Mithril certificate embedding a Mithril STM multisignature
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Certificate {
    /// Hash of the current certificate
    /// Computed from the other fields of the certificate
    /// aka H(Cp,n))
    #[serde(rename = "hash")]
    pub hash: String,

    /// Hash of the previous certificate in the chain
    /// This is either the hash of the first certificate of the epoch in the chain
    /// Or the first certificate of the previous epoch in the chain (if the certificate is the first of its epoch)
    /// aka H(FC(n))
    #[serde(rename = "previous_hash")]
    pub previous_hash: String,

    /// Mithril beacon on the Cardano chain
    /// aka BEACON(p,n)
    #[serde(rename = "beacon")]
    pub beacon: Beacon,

    /// Certificate metadata
    /// aka METADATA(p,n)
    #[serde(rename = "metadata")]
    pub metadata: CertificateMetadata,

    /// Structured message that is used to created the signed message
    /// aka MSG(p,n) U AVK(n-1)
    #[serde(rename = "protocol_message")]
    pub protocol_message: ProtocolMessage,

    /// Message that is signed by the signers
    /// aka H(MSG(p,n) || AVK(n-1))
    #[serde(rename = "signed_message")]
    pub signed_message: String,

    /// Aggregate verification key
    /// The AVK used to sign during the current epoch
    /// aka AVK(n-2)
    #[serde(rename = "aggregate_verification_key")]
    pub aggregate_verification_key: String,

    /// STM multi signature created from a quorum of single signatures from the signers
    /// aka MULTI_SIG(H(MSG(p,n) || AVK(n-1)))
    #[serde(rename = "multi_signature")]
    pub multi_signature: String,

    /// Genesis signature created from the original stake distribution
    /// aka GENESIS_SIG(AVK(-1))
    #[serde(rename = "genesis_signature")]
    pub genesis_signature: String,
}

impl Certificate {
    /// Certificate factory
    pub fn new(
        previous_hash: String,
        beacon: Beacon,
        metadata: CertificateMetadata,
        protocol_message: ProtocolMessage,
        aggregate_verification_key: String,
        multi_signature: String,
        genesis_signature: String,
    ) -> Certificate {
        let signed_message = (&protocol_message.compute_hash()).to_owned();
        let mut certificate = Certificate {
            hash: "".to_string(),
            previous_hash,
            beacon,
            metadata,
            protocol_message,
            signed_message,
            aggregate_verification_key,
            multi_signature,
            genesis_signature,
        };
        certificate.hash = certificate.compute_hash();
        certificate
    }

    /// Computes the hash of a Certificate
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.previous_hash.as_bytes());
        hasher.update(self.beacon.compute_hash().as_bytes());
        hasher.update(self.metadata.compute_hash().as_bytes());
        hasher.update(self.protocol_message.compute_hash().as_bytes());
        hasher.update(self.signed_message.as_bytes());
        hasher.update(self.aggregate_verification_key.as_bytes());
        hasher.update(self.multi_signature.as_bytes());
        hasher.update(self.genesis_signature.as_bytes());
        hex::encode(hasher.finalize())
    }
}

/// Internal error representation
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Error {
    /// error message
    #[serde(rename = "message")]
    pub message: String,
}

impl Error {
    /// Error factory
    pub fn new(message: String) -> Error {
        Error { message }
    }
}

/// Protocol cryptographic parameters
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ProtocolParameters {
    /// Quorum parameter
    #[serde(rename = "k")]
    pub k: u64,

    /// Security parameter (number of lotteries)
    #[serde(rename = "m")]
    pub m: u64,

    /// f in phi(w) = 1 - (1 - f)^w, where w is the stake of a participant
    #[serde(rename = "phi_f")]
    pub phi_f: f32,
}

impl ProtocolParameters {
    /// ProtocolParameters factory
    pub fn new(k: u64, m: u64, phi_f: f32) -> ProtocolParameters {
        ProtocolParameters { k, m, phi_f }
    }

    /// phi_f_fixed is a fixed decimal representatio of phi_f
    /// used for PartialEq and Hash implementation
    pub fn phi_f_fixed(&self) -> U8F24 {
        U8F24::from_num(self.phi_f)
    }

    /// Computes the hash of ProtocolParameters
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.k.to_be_bytes());
        hasher.update(self.m.to_be_bytes());
        hasher.update(self.phi_f_fixed().to_be_bytes());
        hex::encode(hasher.finalize())
    }
}

impl PartialEq<ProtocolParameters> for ProtocolParameters {
    fn eq(&self, other: &ProtocolParameters) -> bool {
        self.k == other.k && self.m == other.m && self.phi_f_fixed() == other.phi_f_fixed()
    }
}

/// Signer represents a signing participant in the network
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Signer {
    /// The unique identifier of the signer
    #[serde(rename = "party_id")]
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    #[serde(rename = "verification_key")]
    pub verification_key: String,
}

impl Signer {
    /// Signer factory
    pub fn new(party_id: PartyId, verification_key: String) -> Signer {
        Signer {
            party_id,
            verification_key,
        }
    }

    /// Computes the hash of Signer
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.party_id.as_bytes());
        hasher.update(self.verification_key.as_bytes());
        hex::encode(hasher.finalize())
    }
}

/// Signer represents a signing party in the network (including its stakes)
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct SignerWithStake {
    /// The unique identifier of the signer
    #[serde(rename = "party_id")]
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    #[serde(rename = "verification_key")]
    pub verification_key: String,

    /// The signer stake
    #[serde(rename = "stake")]
    pub stake: Stake,
}

impl SignerWithStake {
    /// SignerWithStake factory
    pub fn new(party_id: PartyId, verification_key: String, stake: Stake) -> SignerWithStake {
        SignerWithStake {
            party_id,
            verification_key,
            stake,
        }
    }

    /// Computes the hash of SignerWithStake
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.party_id.as_bytes());
        hasher.update(self.verification_key.as_bytes());
        hasher.update(self.stake.to_be_bytes());
        hex::encode(hasher.finalize())
    }
}

/// SingleSignatures represent single signatures originating from a participant in the network
/// for a digest at won lottery indexes
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct SingleSignatures {
    /// The unique identifier of the signer
    #[serde(rename = "party_id")]
    pub party_id: PartyId,

    /// The single signature of the digest
    #[serde(rename = "signature")]
    pub signature: String,

    /// The indexes of the won lotteries that lead to the single signatures
    #[serde(rename = "indexes")]
    pub won_indexes: Vec<LotteryIndex>,
}

impl SingleSignatures {
    /// SingleSignature factory
    pub fn new(
        party_id: PartyId,
        signature: String,
        won_indexes: Vec<LotteryIndex>,
    ) -> SingleSignatures {
        SingleSignatures {
            party_id,
            signature,
            won_indexes,
        }
    }

    /// Convert this [SingleSignatures] to its corresponding [MithrilCore Signature][ProtocolSingleSignature].
    pub fn to_protocol_signature(&self) -> Result<ProtocolSingleSignature, String> {
        match key_decode_hex::<ProtocolSingleSignature>(&self.signature) {
            Ok(signature) => Ok(signature),
            Err(error) => Err(format!(
                "Could not decode signature: {}, signature: {}",
                error, self.signature
            )),
        }
    }
}

/// Snapshot represents a snapshot file and its metadata
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Snapshot {
    /// Digest that is signed by the signer participants
    #[serde(rename = "digest")]
    pub digest: String,

    /// Mithril beacon on the Cardano chain
    #[serde(rename = "beacon")]
    pub beacon: Beacon,

    /// Hash of the associated certificate
    #[serde(rename = "certificate_hash")]
    pub certificate_hash: String,

    /// Size of the snapshot file in Bytes
    #[serde(rename = "size")]
    pub size: u64,

    /// Date and time at which the snapshot was created
    #[serde(rename = "created_at")]
    pub created_at: String,

    /// Locations where the binary content of the snapshot can be retrieved
    #[serde(rename = "locations")]
    pub locations: Vec<String>,
}

impl Snapshot {
    /// Snapshot factory
    pub fn new(
        digest: String,
        beacon: Beacon,
        certificate_hash: String,
        size: u64,
        created_at: String,
        locations: Vec<String>,
    ) -> Snapshot {
        Snapshot {
            digest,
            beacon,
            certificate_hash,
            size,
            created_at,
            locations,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::key_encode_hex;
    use crate::crypto_helper::tests_setup::{setup_message, setup_signers};
    use std::cmp::Ordering;

    use super::*;

    #[test]
    fn test_beacon_partial_ord_different_network() {
        let beacon1: Beacon = Beacon {
            network: "A".to_string(),
            epoch: 0,
            immutable_file_number: 0,
        };
        let beacon2: Beacon = Beacon {
            network: "B".to_string(),
            epoch: 0,
            immutable_file_number: 0,
        };

        assert!(beacon1.partial_cmp(&beacon2).is_none());
    }

    #[test]
    fn test_beacon_partial_ord_equal() {
        let beacon1: Beacon = Beacon {
            network: "A".to_string(),
            epoch: 0,
            immutable_file_number: 0,
        };

        assert_eq!(Some(Ordering::Equal), beacon1.partial_cmp(&beacon1));
    }

    #[test]
    fn test_beacon_partial_ord_same_epoch_less() {
        let beacon1: Beacon = Beacon {
            network: "A".to_string(),
            epoch: 0,
            immutable_file_number: 0,
        };
        let beacon2: Beacon = Beacon {
            network: "A".to_string(),
            epoch: 0,
            immutable_file_number: 1,
        };

        assert_eq!(Some(Ordering::Less), beacon1.partial_cmp(&beacon2));
    }

    #[test]
    fn test_beacon_partial_ord_same_epoch_greater() {
        let beacon1: Beacon = Beacon {
            network: "A".to_string(),
            epoch: 0,
            immutable_file_number: 1,
        };
        let beacon2: Beacon = Beacon {
            network: "A".to_string(),
            epoch: 0,
            immutable_file_number: 0,
        };

        assert_eq!(Some(Ordering::Greater), beacon1.partial_cmp(&beacon2));
    }

    #[test]
    fn test_beacon_partial_ord_cmp_epochs_less() {
        let beacon1: Beacon = Beacon {
            network: "A".to_string(),
            epoch: 0,
            immutable_file_number: 99,
        };
        let beacon2: Beacon = Beacon {
            network: "A".to_string(),
            epoch: 1,
            immutable_file_number: 99,
        };

        assert_eq!(Some(Ordering::Less), beacon1.partial_cmp(&beacon2));
    }

    #[test]
    fn test_protocol_parameters_partialeq() {
        assert_eq!(
            ProtocolParameters::new(1000, 100, 0.123001),
            ProtocolParameters::new(1000, 100, 0.123001)
        );
        assert_ne!(
            ProtocolParameters::new(1000, 100, 0.1230011),
            ProtocolParameters::new(1000, 100, 0.1230012)
        );
        assert_ne!(
            ProtocolParameters::new(1000, 100, 0.12301),
            ProtocolParameters::new(1000, 100, 0.12300)
        );
        assert_ne!(
            ProtocolParameters::new(1001, 100, 0.12300),
            ProtocolParameters::new(1000, 100, 0.12300)
        );
        assert_ne!(
            ProtocolParameters::new(1000, 101, 0.12300),
            ProtocolParameters::new(1000, 100, 0.12300)
        );
    }

    #[test]
    fn single_signatures_should_convert_to_protocol_signatures() {
        let message = setup_message();
        let signers = setup_signers(1);
        let (party_id, _, _, signer, _) = signers.first().unwrap();
        let protocol_sigs = signer.sign(message.compute_hash().as_bytes()).unwrap();

        let signature = SingleSignatures::new(
            party_id.to_owned(),
            key_encode_hex(&protocol_sigs).unwrap(),
            protocol_sigs.indexes.clone(),
        );

        assert_eq!(protocol_sigs, signature.to_protocol_signature().unwrap());
    }

    #[test]
    fn test_beacon_compute_hash() {
        let hash_expected = "48cbf709b56204d8315aefd3a416b45398094f6fd51785c5b7dcaf7f35aacbfb";

        assert_eq!(
            hash_expected,
            Beacon::new("testnet".to_string(), 10, 100).compute_hash()
        );
        assert_ne!(
            hash_expected,
            Beacon::new("mainnet".to_string(), 10, 100).compute_hash()
        );
        assert_ne!(
            hash_expected,
            Beacon::new("testnet".to_string(), 20, 100).compute_hash()
        );
        assert_ne!(
            hash_expected,
            Beacon::new("testnet".to_string(), 10, 200).compute_hash()
        );
    }

    #[test]
    fn test_protocol_parameters_compute_hash() {
        let hash_expected = "ace019657cd995b0dfbb1ce8721a1092715972c4ae0171cc636ab4a44e6e4279";

        assert_eq!(
            hash_expected,
            ProtocolParameters::new(1000, 100, 0.123).compute_hash()
        );
        assert_ne!(
            hash_expected,
            ProtocolParameters::new(2000, 100, 0.123).compute_hash()
        );
        assert_ne!(
            hash_expected,
            ProtocolParameters::new(1000, 200, 0.123).compute_hash()
        );
        assert_ne!(
            hash_expected,
            ProtocolParameters::new(1000, 100, 0.124).compute_hash()
        );
    }

    #[test]
    fn test_signer_compute_hash() {
        let hash_expected = "1a71566d70060d38ed94cc7760b0c38d34dd2729a1a1ea70ef983d2c780a4d77";

        assert_eq!(
            hash_expected,
            Signer::new("1".to_string(), "verification-key-123".to_string()).compute_hash()
        );
        assert_ne!(
            hash_expected,
            Signer::new("0".to_string(), "verification-key-123".to_string()).compute_hash()
        );
        assert_ne!(
            hash_expected,
            Signer::new("1".to_string(), "verification-key-456".to_string()).compute_hash()
        );
    }

    #[test]
    fn test_signer_with_stake_compute_hash() {
        let hash_expected = "16362ace34bdb40c10d79c08fcfa5b0b14c74b6681635723c89aee52d4134971";

        assert_eq!(
            hash_expected,
            SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10)
                .compute_hash()
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::new("0".to_string(), "verification-key-123".to_string(), 10)
                .compute_hash()
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::new("1".to_string(), "verification-key-456".to_string(), 10)
                .compute_hash()
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 20)
                .compute_hash()
        );
    }

    #[test]
    fn test_certificate_metadata_compute_hash() {
        let hash_expected = "512632ae13b4527486303a78b500fb328b9f353b8a635a32f74fe1d899439ae8";

        assert_eq!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0-modified".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(2000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at-modified".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at-modified".to_string(),
                vec![
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new(
                        "1-modified".to_string(),
                        "verification-key-123".to_string(),
                        10
                    ),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![SignerWithStake::new(
                    "1".to_string(),
                    "verification-key-123".to_string(),
                    10
                ),],
            )
            .compute_hash()
        );
    }

    #[test]
    fn test_protocol_message_compute_hash() {
        let hash_expected = "71dee1e558cd647cdbc219a24b766940f568e7e8287c30a8292209ef11666e03";

        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-123".to_string(),
        );
        assert_eq!(hash_expected, protocol_message.compute_hash());

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-456".to_string(),
        );
        assert_ne!(hash_expected, protocol_message_modified.compute_hash());

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-456".to_string(),
        );
        assert_ne!(hash_expected, protocol_message_modified.compute_hash());
    }

    #[test]
    fn test_certificate_compute_hash() {
        let hash_expected = "7d714a2005ceb4778efe8805c9efe139a65aa5c607b38befeee97a4518928a0c";

        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-123".to_string(),
        );
        assert_eq!(
            hash_expected,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    "initiated_at".to_string(),
                    "sealed_at".to_string(),
                    vec![
                        SignerWithStake::new(
                            "1".to_string(),
                            "verification-key-123".to_string(),
                            10
                        ),
                        SignerWithStake::new(
                            "2".to_string(),
                            "verification-key-456".to_string(),
                            20
                        )
                    ],
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            Certificate::new(
                "previous_hash-modified".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    "initiated_at".to_string(),
                    "sealed_at".to_string(),
                    vec![
                        SignerWithStake::new(
                            "1".to_string(),
                            "verification-key-123".to_string(),
                            10
                        ),
                        SignerWithStake::new(
                            "2".to_string(),
                            "verification-key-456".to_string(),
                            20
                        )
                    ],
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet-modified".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    "initiated_at".to_string(),
                    "sealed_at".to_string(),
                    vec![
                        SignerWithStake::new(
                            "1".to_string(),
                            "verification-key-123".to_string(),
                            10
                        ),
                        SignerWithStake::new(
                            "2".to_string(),
                            "verification-key-456".to_string(),
                            20
                        )
                    ],
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0-modified".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    "initiated_at".to_string(),
                    "sealed_at".to_string(),
                    vec![
                        SignerWithStake::new(
                            "1".to_string(),
                            "verification-key-123".to_string(),
                            10
                        ),
                        SignerWithStake::new(
                            "2".to_string(),
                            "verification-key-456".to_string(),
                            20
                        )
                    ],
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-456".to_string(),
        );
        assert_ne!(
            hash_expected,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    "initiated_at".to_string(),
                    "sealed_at".to_string(),
                    vec![
                        SignerWithStake::new(
                            "1".to_string(),
                            "verification-key-123".to_string(),
                            10
                        ),
                        SignerWithStake::new(
                            "2".to_string(),
                            "verification-key-456".to_string(),
                            20
                        )
                    ],
                ),
                protocol_message_modified.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    "initiated_at".to_string(),
                    "sealed_at".to_string(),
                    vec![
                        SignerWithStake::new(
                            "1".to_string(),
                            "verification-key-123".to_string(),
                            10
                        ),
                        SignerWithStake::new(
                            "2".to_string(),
                            "verification-key-456".to_string(),
                            20
                        )
                    ],
                ),
                protocol_message.clone(),
                "aggregate_verification_key-modified".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    "initiated_at".to_string(),
                    "sealed_at".to_string(),
                    vec![
                        SignerWithStake::new(
                            "1".to_string(),
                            "verification-key-123".to_string(),
                            10
                        ),
                        SignerWithStake::new(
                            "2".to_string(),
                            "verification-key-456".to_string(),
                            20
                        )
                    ],
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature-modified".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    "initiated_at".to_string(),
                    "sealed_at".to_string(),
                    vec![
                        SignerWithStake::new(
                            "1".to_string(),
                            "verification-key-123".to_string(),
                            10
                        ),
                        SignerWithStake::new(
                            "2".to_string(),
                            "verification-key-456".to_string(),
                            20
                        )
                    ],
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature-modified".to_string(),
            )
            .compute_hash()
        );
    }
}
