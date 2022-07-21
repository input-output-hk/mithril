use crate::crypto_helper::{key_decode_hex, ProtocolSingleSignature};
use fixed::types::U8F24;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::{collections::HashMap, fmt::Display};

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

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize, Hash, Eq, PartialOrd)]
pub enum CardanoNetwork {
    MainNet,
    DevNet(MagicId),
    TestNet(MagicId),
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
    pub fn compute_hash(signer: &Self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(signer.party_id.as_bytes());
        hasher.update(signer.verification_key.as_bytes());
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
    pub fn compute_hash(signer_with_stake: &Self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(signer_with_stake.party_id.as_bytes());
        hasher.update(signer_with_stake.verification_key.as_bytes());
        hasher.update(signer_with_stake.stake.to_be_bytes());
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
        let protocol_sigs = signer.sign(message.as_bytes()).unwrap();

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
        let hash_expected = "1a71566d70060d38ed94cc7760b0c38d34dd2729a1a1ea70ef983d2c780a4d77";

        assert_eq!(
            hash_expected,
            Signer::compute_hash(&Signer::new(
                "1".to_string(),
                "verification-key-123".to_string()
            ))
        );
        assert_ne!(
            hash_expected,
            Signer::compute_hash(&Signer::new(
                "0".to_string(),
                "verification-key-123".to_string()
            ))
        );
        assert_ne!(
            hash_expected,
            Signer::compute_hash(&Signer::new(
                "1".to_string(),
                "verification-key-456".to_string()
            ))
        );
    }

    #[test]
    fn test_signer_with_stake_compute_hash() {
        let hash_expected = "16362ace34bdb40c10d79c08fcfa5b0b14c74b6681635723c89aee52d4134971";

        assert_eq!(
            hash_expected,
            SignerWithStake::compute_hash(&SignerWithStake::new(
                "1".to_string(),
                "verification-key-123".to_string(),
                10
            ))
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::compute_hash(&SignerWithStake::new(
                "0".to_string(),
                "verification-key-123".to_string(),
                10
            ))
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::compute_hash(&SignerWithStake::new(
                "1".to_string(),
                "verification-key-456".to_string(),
                10
            ))
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::compute_hash(&SignerWithStake::new(
                "1".to_string(),
                "verification-key-123".to_string(),
                20
            ))
        );
    }

    #[test]
    fn test_certificate_compute_hash() {
        let hash_expected = "5d08129c86948f3082bc9ef3d0d2fbe628e907790db1fa58eadf13f0940c8d52";

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
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
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
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
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
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
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
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
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
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
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
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
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
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
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
                    SignerWithStake::new(
                        "1".to_string(),
                        "modified-verification-key-123".to_string(),
                        10
                    ),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
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
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
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
                    SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10),
                    SignerWithStake::new("2".to_string(), "verification-key-456".to_string(), 20)
                ],
                "aggregate_verification_key".to_string(),
                "modified-multisignature".to_string(),
            ))
        );
    }
}
