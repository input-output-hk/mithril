use fixed::types::U8F24;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

pub type ImmutableFileNumber = u64;

/// Beacon represents a point in the Cardano chain at which a Mithril certificate should be produced
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize, Hash, PartialOrd)]
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

    /// Computes the hash of a Beacon
    pub fn compute_hash(beacon: &Self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(beacon.network.as_bytes());
        hasher.update(beacon.epoch.to_be_bytes());
        hasher.update(beacon.immutable_file_number.to_be_bytes());
        hex::encode(hasher.finalize())
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

    /// Current Signers
    #[serde(rename = "signers")]
    pub signers: Vec<Signer>,
}

impl CertificatePending {
    /// CertificatePending factory
    pub fn new(
        beacon: Beacon,
        protocol_parameters: ProtocolParameters,
        previous_hash: String,
        signers: Vec<Signer>,
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
        certificate.hash = Self::compute_hash(&certificate);
        certificate
    }

    /// Computes the hash of a certificate
    pub fn compute_hash(certificate: &Self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(certificate.previous_hash.as_bytes());
        hasher.update(Beacon::compute_hash(&certificate.beacon).as_bytes());
        hasher
            .update(ProtocolParameters::compute_hash(&certificate.protocol_parameters).as_bytes());
        hasher.update(certificate.digest.as_bytes());
        hasher.update(certificate.started_at.as_bytes());
        hasher.update(certificate.completed_at.as_bytes());
        certificate.signers.iter().for_each(|s| {
            hasher.update(SignerWithStake::compute_hash(s));
        });
        hasher.update(certificate.aggregate_verification_key.as_bytes());
        hasher.update(certificate.multisignature.as_bytes());
        hex::encode(hasher.finalize())
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

    /// Computes the hash of ProtocolParameters
    pub fn compute_hash(protocol_parameters: &Self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(protocol_parameters.k.to_be_bytes());
        hasher.update(protocol_parameters.m.to_be_bytes());
        hasher.update(protocol_parameters.phi_f_fixed().to_be_bytes());
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

    /// Computes the hash of Signer
    pub fn compute_hash(signer: &Self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(signer.party_id.to_be_bytes());
        hasher.update(signer.verification_key.as_bytes());
        hex::encode(hasher.finalize())
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

    /// Computes the hash of SignerWithStake
    pub fn compute_hash(signer_with_stake: &Self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(signer_with_stake.party_id.to_be_bytes());
        hasher.update(signer_with_stake.verification_key.as_bytes());
        hasher.update(signer_with_stake.stake.to_be_bytes());
        hex::encode(hasher.finalize())
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

    #[test]
    fn test_beacon_compute_hash() {
        let hash_expected = "48cbf709b56204d8315aefd3a416b45398094f6fd51785c5b7dcaf7f35aacbfb";

        assert_eq!(
            hash_expected,
            Beacon::compute_hash(&Beacon::new("testnet".to_string(), 10, 100))
        );
        assert_ne!(
            hash_expected,
            Beacon::compute_hash(&Beacon::new("mainnet".to_string(), 10, 100))
        );
        assert_ne!(
            hash_expected,
            Beacon::compute_hash(&Beacon::new("testnet".to_string(), 20, 100))
        );
        assert_ne!(
            hash_expected,
            Beacon::compute_hash(&Beacon::new("testnet".to_string(), 10, 200))
        );
    }

    #[test]
    fn test_protocol_parameters_compute_hash() {
        let hash_expected = "ace019657cd995b0dfbb1ce8721a1092715972c4ae0171cc636ab4a44e6e4279";

        assert_eq!(
            hash_expected,
            ProtocolParameters::compute_hash(&ProtocolParameters::new(1000, 100, 0.123))
        );
        assert_ne!(
            hash_expected,
            ProtocolParameters::compute_hash(&ProtocolParameters::new(2000, 100, 0.123))
        );
        assert_ne!(
            hash_expected,
            ProtocolParameters::compute_hash(&ProtocolParameters::new(1000, 200, 0.123))
        );
        assert_ne!(
            hash_expected,
            ProtocolParameters::compute_hash(&ProtocolParameters::new(1000, 100, 0.124))
        );
    }

    #[test]
    fn test_signer_compute_hash() {
        let hash_expected = "e5b71e4fc8052671ec42ed7da3bed0f949ab376f89d843f5264a5acf4ee5f695";

        assert_eq!(
            hash_expected,
            Signer::compute_hash(&Signer::new(1, "verification-key-123".to_string()))
        );
        assert_ne!(
            hash_expected,
            Signer::compute_hash(&Signer::new(0, "verification-key-123".to_string()))
        );
        assert_ne!(
            hash_expected,
            Signer::compute_hash(&Signer::new(1, "verification-key-456".to_string()))
        );
    }

    #[test]
    fn test_signer_with_stake_compute_hash() {
        let hash_expected = "7715aa991702ca996d1d0a5335435d23ed280038679de489c58f6550af262644";

        assert_eq!(
            hash_expected,
            SignerWithStake::compute_hash(&SignerWithStake::new(
                1,
                "verification-key-123".to_string(),
                10
            ))
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::compute_hash(&SignerWithStake::new(
                0,
                "verification-key-123".to_string(),
                10
            ))
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::compute_hash(&SignerWithStake::new(
                1,
                "verification-key-456".to_string(),
                10
            ))
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::compute_hash(&SignerWithStake::new(
                1,
                "verification-key-123".to_string(),
                20
            ))
        );
    }

    #[test]
    fn test_certificate_compute_hash() {
        let hash_expected = "16393ea9ffcbd50dbd1cb3d2b44736870edbc8175116c0eb501ded50ea7ed114";

        assert_eq!(
            hash_expected,
            Certificate::compute_hash(&Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                ProtocolParameters::new(1000, 100, 0.123),
                "digest".to_string(),
                "started_at".to_string(),
                "completed_at".to_string(),
                vec![
                    SignerWithStake::new(1, "verification-key-123".to_string(), 10),
                    SignerWithStake::new(2, "verification-key-456".to_string(), 20)
                ],
                "aggregate_verification_key".to_string(),
                "multisignature".to_string(),
            ))
        );

        assert_ne!(
            hash_expected,
            Certificate::compute_hash(&Certificate::new(
                "modified-previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                ProtocolParameters::new(1000, 100, 0.123),
                "digest".to_string(),
                "started_at".to_string(),
                "completed_at".to_string(),
                vec![
                    SignerWithStake::new(1, "verification-key-123".to_string(), 10),
                    SignerWithStake::new(2, "verification-key-456".to_string(), 20)
                ],
                "aggregate_verification_key".to_string(),
                "multisignature".to_string(),
            ))
        );

        assert_ne!(
            hash_expected,
            Certificate::compute_hash(&Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("modified-testnet".to_string(), 10, 100),
                ProtocolParameters::new(1000, 100, 0.123),
                "digest".to_string(),
                "started_at".to_string(),
                "completed_at".to_string(),
                vec![
                    SignerWithStake::new(1, "verification-key-123".to_string(), 10),
                    SignerWithStake::new(2, "verification-key-456".to_string(), 20)
                ],
                "aggregate_verification_key".to_string(),
                "multisignature".to_string(),
            ))
        );

        assert_ne!(
            hash_expected,
            Certificate::compute_hash(&Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                ProtocolParameters::new(2000, 100, 0.123),
                "digest".to_string(),
                "started_at".to_string(),
                "completed_at".to_string(),
                vec![
                    SignerWithStake::new(1, "verification-key-123".to_string(), 10),
                    SignerWithStake::new(2, "verification-key-456".to_string(), 20)
                ],
                "aggregate_verification_key".to_string(),
                "multisignature".to_string(),
            ))
        );

        assert_ne!(
            hash_expected,
            Certificate::compute_hash(&Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                ProtocolParameters::new(1000, 100, 0.123),
                "modified-digest".to_string(),
                "started_at".to_string(),
                "completed_at".to_string(),
                vec![
                    SignerWithStake::new(1, "verification-key-123".to_string(), 10),
                    SignerWithStake::new(2, "verification-key-456".to_string(), 20)
                ],
                "aggregate_verification_key".to_string(),
                "multisignature".to_string(),
            ))
        );

        assert_ne!(
            hash_expected,
            Certificate::compute_hash(&Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                ProtocolParameters::new(1000, 100, 0.123),
                "digest".to_string(),
                "modified-started_at".to_string(),
                "completed_at".to_string(),
                vec![
                    SignerWithStake::new(1, "verification-key-123".to_string(), 10),
                    SignerWithStake::new(2, "verification-key-456".to_string(), 20)
                ],
                "aggregate_verification_key".to_string(),
                "multisignature".to_string(),
            ))
        );

        assert_ne!(
            hash_expected,
            Certificate::compute_hash(&Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                ProtocolParameters::new(1000, 100, 0.123),
                "digest".to_string(),
                "started_at".to_string(),
                "modified-completed_at".to_string(),
                vec![
                    SignerWithStake::new(1, "verification-key-123".to_string(), 10),
                    SignerWithStake::new(2, "verification-key-456".to_string(), 20)
                ],
                "aggregate_verification_key".to_string(),
                "multisignature".to_string(),
            ))
        );

        assert_ne!(
            hash_expected,
            Certificate::compute_hash(&Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                ProtocolParameters::new(1000, 100, 0.123),
                "digest".to_string(),
                "started_at".to_string(),
                "completed_at".to_string(),
                vec![
                    SignerWithStake::new(1, "modified-verification-key-123".to_string(), 10),
                    SignerWithStake::new(2, "verification-key-456".to_string(), 20)
                ],
                "aggregate_verification_key".to_string(),
                "multisignature".to_string(),
            ))
        );

        assert_ne!(
            hash_expected,
            Certificate::compute_hash(&Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                ProtocolParameters::new(1000, 100, 0.123),
                "digest".to_string(),
                "started_at".to_string(),
                "completed_at".to_string(),
                vec![
                    SignerWithStake::new(1, "verification-key-123".to_string(), 10),
                    SignerWithStake::new(2, "verification-key-456".to_string(), 20)
                ],
                "modified-aggregate_verification_key".to_string(),
                "multisignature".to_string(),
            ))
        );

        assert_ne!(
            hash_expected,
            Certificate::compute_hash(&Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                ProtocolParameters::new(1000, 100, 0.123),
                "digest".to_string(),
                "started_at".to_string(),
                "completed_at".to_string(),
                vec![
                    SignerWithStake::new(1, "verification-key-123".to_string(), 10),
                    SignerWithStake::new(2, "verification-key-456".to_string(), 20)
                ],
                "aggregate_verification_key".to_string(),
                "modified-multisignature".to_string(),
            ))
        );
    }
}
