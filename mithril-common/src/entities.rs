// TODO: to be removed later
#![allow(dead_code)]

use fixed::types::U8F24;
use serde::{Deserialize, Serialize};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub type ImmutableFileNumber = u64;

/// Beacon represents a point in the Cardano chain at which a Mithril certificate should be produced
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize, Hash)]
pub struct Beacon {
    /// Cardano network
    #[serde(rename = "network")]
    pub network: String,

    /// Cardano chain epoch number
    #[serde(rename = "epoch")]
    pub epoch: u64,

    /// Number of the last included immutable files for the digest computation
    #[serde(rename = "immutable_file_number")]
    pub immutable_file_number: ImmutableFileNumber,
}

impl Beacon {
    /// Beacon factory
    pub fn new(network: String, epoch: u64, immutable_file_number: u64) -> Beacon {
        Beacon {
            network,
            epoch,
            immutable_file_number,
        }
    }
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

    /// Hash of the previous certificate
    #[serde(rename = "previous_hash")]
    pub previous_hash: String,

    /// Current Signers with stakes
    // TODO: Should return Vec<Signer> instead, will be updated when stake distribution is real
    #[serde(rename = "signers")]
    pub signers: Vec<SignerWithStake>,
}

impl CertificatePending {
    /// CertificatePending factory
    pub fn new(
        beacon: Beacon,
        protocol_parameters: ProtocolParameters,
        previous_hash: String,
        signers: Vec<SignerWithStake>,
    ) -> CertificatePending {
        CertificatePending {
            beacon,
            protocol_parameters,
            previous_hash,
            signers,
        }
    }
}

/// Certificate represents a Mithril certificate embedding a Mithril STM multisignature
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize, Hash)]
pub struct Certificate {
    /// Hash of the current certificate
    #[serde(rename = "hash")]
    pub hash: String,

    /// Hash of the previous certificate
    #[serde(rename = "previous_hash")]
    pub previous_hash: String,

    /// Cardano chain beacon
    #[serde(rename = "beacon")]
    pub beacon: Beacon,

    /// Protocol parameters
    #[serde(rename = "protocol")]
    pub protocol_parameters: ProtocolParameters,

    /// Digest that is signed by the signer participants
    #[serde(rename = "digest")]
    pub digest: String,

    /// Date and time at which the certificate was initialized and ready to accept single signatures from signers
    #[serde(rename = "started_at")]
    pub started_at: String,

    /// Date and time at which the certificate was completed (when the quorum of single signatures was reached so that a multisignature could be aggregated from them)
    #[serde(rename = "completed_at")]
    pub completed_at: String,

    /// The list of the signers with their stakes and verification keys
    #[serde(rename = "signers")]
    pub signers: Vec<SignerWithStake>,

    /// Aggregate verification key
    #[serde(rename = "aggregate_verification_key")]
    pub aggregate_verification_key: String,

    /// STM multisignature created from a quorum of single signatures from the signers
    #[serde(rename = "multisignature")]
    pub multisignature: String,
}

impl Certificate {
    /// Certificate factory
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        previous_hash: String,
        beacon: Beacon,
        protocol_parameters: ProtocolParameters,
        digest: String,
        started_at: String,
        completed_at: String,
        signers: Vec<SignerWithStake>,
        aggregate_verification_key: String,
        multisignature: String,
    ) -> Certificate {
        let mut certificate = Certificate {
            hash: "".to_string(),
            previous_hash,
            beacon,
            protocol_parameters,
            digest,
            started_at,
            completed_at,
            signers,
            aggregate_verification_key,
            multisignature,
        };
        let mut hasher = DefaultHasher::new();
        certificate.hash(&mut hasher);
        certificate.hash = format!("{:x}", hasher.finish());
        certificate
    }
}

/// Internal error representation
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Error {
    /// error code
    #[serde(rename = "code")]
    pub code: String,

    /// error message
    #[serde(rename = "message")]
    pub message: String,
}

impl Error {
    /// Error factory
    pub fn new(code: String, message: String) -> Error {
        Error { code, message }
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
}

impl PartialEq<ProtocolParameters> for ProtocolParameters {
    fn eq(&self, other: &ProtocolParameters) -> bool {
        self.k == other.k && self.m == other.m && self.phi_f_fixed() == other.phi_f_fixed()
    }
}

impl Hash for ProtocolParameters {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.k.hash(state);
        self.m.hash(state);
        self.phi_f_fixed().hash(state);
    }
}

/// Signer represents a signing participant in the network
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize, Hash)]
pub struct Signer {
    /// The unique identifier of the signer
    #[serde(rename = "party_id")]
    pub party_id: u64,

    /// The public key used to authenticate signer signature
    #[serde(rename = "verification_key")]
    pub verification_key: String,
}

impl Signer {
    /// Signer factory
    pub fn new(party_id: u64, verification_key: String) -> Signer {
        Signer {
            party_id,
            verification_key,
        }
    }
}

/// Signer represents a signing party in the network (including its stakes)
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize, Hash)]
pub struct SignerWithStake {
    /// The unique identifier of the signer
    #[serde(rename = "party_id")]
    pub party_id: u64,

    /// The public key used to authenticate signer signature
    #[serde(rename = "verification_key")]
    pub verification_key: String,

    #[serde(rename = "stake")]
    pub stake: u64,
}

impl SignerWithStake {
    /// SignerWithStake factory
    pub fn new(party_id: u64, verification_key: String, stake: u64) -> SignerWithStake {
        SignerWithStake {
            party_id,
            verification_key,
            stake,
        }
    }
}

/// SingleSignature represents a single signature originating from a participant in the network for a digest at a specific lottery index
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct SingleSignature {
    /// The unique identifier of the signer
    #[serde(rename = "party_id")]
    pub party_id: u64,

    /// The index of the lottery won that lead to the single signature
    #[serde(rename = "index")]
    pub index: u64,

    /// The single signature of the digest
    #[serde(rename = "signature")]
    pub signature: String,
}

impl SingleSignature {
    /// SingleSignature factory
    pub fn new(party_id: u64, index: u64, signature: String) -> SingleSignature {
        SingleSignature {
            party_id,
            index,
            signature,
        }
    }
}

/// Snapshot represents a snapshot file and its metadata
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Snapshot {
    /// Digest that is signed by the signer participants
    #[serde(rename = "digest")]
    pub digest: String,

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
        certificate_hash: String,
        size: u64,
        created_at: String,
        locations: Vec<String>,
    ) -> Snapshot {
        Snapshot {
            digest,
            certificate_hash,
            size,
            created_at,
            locations,
        }
    }
}

/// Stake represents the stakes of a participant in the Cardano chain
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Stake {
    #[serde(rename = "stake")]
    pub stake: u64,
}

impl Stake {
    /// Stake factory
    pub fn new(stake: u64) -> Stake {
        Stake { stake }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    fn test_protocol_parameters_hash() {
        let hash_expected = "946adce9d3be7833";

        let compute_hash = |protocol_parameters: ProtocolParameters| -> String {
            let mut hasher = DefaultHasher::new();
            protocol_parameters.hash(&mut hasher);
            format!("{:x}", hasher.finish())
        };

        assert_eq!(
            hash_expected,
            compute_hash(ProtocolParameters::new(1000, 100, 0.123))
        );
        assert_ne!(
            hash_expected,
            compute_hash(ProtocolParameters::new(2000, 100, 0.123))
        );
        assert_ne!(
            hash_expected,
            compute_hash(ProtocolParameters::new(1000, 200, 0.123))
        );
        assert_ne!(
            hash_expected,
            compute_hash(ProtocolParameters::new(1000, 100, 0.124))
        );
    }
}
