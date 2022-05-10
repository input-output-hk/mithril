// TODO: to be removed later
#![allow(dead_code)]

use serde::{Deserialize, Serialize};

/// Beacon represents a point in the Cardano chain at which a Mithril certificate should be produced
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Beacon {
    /// Cardano network
    #[serde(rename = "network")]
    pub network: String,

    /// Cardano chain epoch number
    #[serde(rename = "epoch")]
    pub epoch: u64,

    /// Cardano chain block number
    #[serde(rename = "immutable_number")]
    pub immutable_number: u64,
}

impl Beacon {
    /// Beacon factory
    pub fn new(network: String, epoch: u64, immutable_number: u64) -> Beacon {
        Beacon {
            network,
            epoch,
            immutable_number,
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
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct Certificate {
    /// Hash of the current certificate
    #[serde(rename = "hash")]
    pub hash: String,

    /// Hash of the previous certificate
    #[serde(rename = "previous_hash")]
    pub previous_hash: String,

    /// Cardano chain block number
    #[serde(rename = "block")]
    pub block: u64,

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

    /// The list of the participants (potential signers) with their stakes and verification keys
    #[serde(rename = "participants")]
    pub participants: Vec<SignerWithStake>,

    /// STM multisignature created from a quorum of single signatures from the signers
    #[serde(rename = "multisignature")]
    pub multisignature: String,
}

impl Certificate {
    /// Certificate factory
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        hash: String,
        previous_hash: String,
        block: u64,
        protocol_parameters: ProtocolParameters,
        digest: String,
        started_at: String,
        completed_at: String,
        participants: Vec<SignerWithStake>,
        multisignature: String,
    ) -> Certificate {
        Certificate {
            hash,
            previous_hash,
            block,
            protocol_parameters,
            digest,
            started_at,
            completed_at,
            participants,
            multisignature,
        }
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
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
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
}

/// Signer represents a signing participant in the network
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
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
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
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
