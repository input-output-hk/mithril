use hex::ToHex;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::Write;
use std::path;

use mithril_common::crypto_helper::{
    key_decode_hex, key_encode_hex, ProtocolClerk, ProtocolInitializer, ProtocolKeyRegistration,
    ProtocolMultiSignature, ProtocolParameters, ProtocolPartyId, ProtocolSigner,
    ProtocolSignerVerificationKey, ProtocolSingleSignature, ProtocolStake,
};

/// Player artifacts
#[derive(Debug, Serialize, Deserialize)]
struct PlayerArtifact {
    party_id: ProtocolPartyId,
    stake: ProtocolStake,
    verification_key: String,
    initializer: String,
}

/// Single Signature artifacts
#[derive(Debug, Serialize, Deserialize)]
struct SingleSignatureArtifact {
    party_id: ProtocolPartyId,
    message: String,
    lotteries: Vec<u64>,
    signature: String,
}

/// Multi Signature artifacts
#[derive(Debug, Serialize, Deserialize)]
struct MultiSignatureArtifact {
    party_id: ProtocolPartyId,
    message: String,
    signature: String,
}

/// Party represents a signing protocol participant
#[derive(Debug)]
pub struct Party {
    /// Party's identifier
    party_id: ProtocolPartyId,
    /// Party's stake
    stake: ProtocolStake,
    /// Protocol parameters
    params: Option<ProtocolParameters>,
    /// Protocol signer
    signer: Option<ProtocolSigner>,
    /// Protocol clerk
    clerk: Option<ProtocolClerk>,
    /// Multi signatures
    msigs: HashMap<Vec<u8>, ProtocolMultiSignature>,
}

impl Party {
    /// Party factory
    pub fn new(party_id: usize, stake: u64) -> Self {
        println!("Party #{}: party created with {} stakes", party_id, stake);
        Self {
            party_id: format!("{}", party_id) as ProtocolPartyId,
            stake: stake as ProtocolStake,
            params: None,
            signer: None,
            clerk: None,
            msigs: HashMap::new(),
        }
    }

    /// Update protocol parameters
    pub fn update_params(&mut self, params: &ProtocolParameters) {
        println!(
            "Party #{}: protocol params updated to {:?}",
            self.party_id, params
        );
        self.params = Some(*params);
    }

    /// Register keys
    pub fn register_keys(
        &mut self,
        players_with_keys: &[(
            ProtocolPartyId,
            ProtocolStake,
            ProtocolSignerVerificationKey,
        )],
    ) {
        let players = players_with_keys
            .iter()
            .map(|(party_id, stake, _verification_key)| (party_id.to_owned(), *stake))
            .collect::<Vec<_>>();
        println!(
            "Party #{}: protocol keys registration from {:?}",
            self.party_id, players
        );

        let mut key_reg = ProtocolKeyRegistration::init();
        for (_party_id, stake, verification_key) in players_with_keys {
            key_reg.register(*stake, *verification_key).unwrap();
        }
        let closed_reg = key_reg.close();

        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let p = ProtocolInitializer::setup(self.params.unwrap(), self.stake, &mut rng);
        self.signer = Some(p.new_signer(closed_reg));
        self.clerk = Some(ProtocolClerk::from_signer(self.signer.as_ref().unwrap()));
    }

    /// Individually sign a message through lottery
    pub fn sign_message(&mut self, message: &[u8]) -> Option<ProtocolSingleSignature> {
        println!(
            "Party #{}: sign message {}",
            self.party_id,
            message.encode_hex::<String>()
        );

        let signature = self.signer.as_ref().unwrap().sign(message);
        println!(
            "Party #{}: {}",
            self.party_id,
            if signature.is_some() {
                format!("lottery #{:?} won", signature.as_ref().unwrap().indexes)
            } else {
                "lost all lotteries".to_string()
            }
        );
        signature
    }

    /// Aggregate signatures
    pub fn sign_aggregate(
        &mut self,
        message: &Vec<u8>,
        signatures: &[ProtocolSingleSignature],
    ) -> Option<&ProtocolMultiSignature> {
        let msig = self.clerk.as_ref().unwrap().aggregate(signatures, message);
        match msig {
            Ok(aggregate_signature) => {
                println!("Party #{}: aggregate signature computed", self.party_id);
                self.msigs.insert(message.clone(), aggregate_signature);
                self.get_aggregate(message)
            }
            Err(_) => {
                println!(
                    "Party #{}: not enough signatures to compute aggregate",
                    self.party_id
                );
                None
            }
        }
    }

    /// Retrieve agreggate signature associated to a message
    pub fn get_aggregate(&self, message: &Vec<u8>) -> Option<&ProtocolMultiSignature> {
        self.msigs.get(message)
    }

    /// Verify a certificate
    pub fn verify_message(&self, message: &Vec<u8>) -> Result<(), String> {
        match self.get_aggregate(message) {
            Some(msig) => match msig.verify(
                message,
                &self.clerk.as_ref().unwrap().compute_avk(),
                &self.params.unwrap(),
            ) {
                Ok(_) => {
                    println!(
                        "Party #{}: aggregate signature successfully verified for {}!",
                        self.party_id,
                        message.encode_hex::<String>()
                    );
                    Ok(())
                }
                Err(_) => {
                    println!(
                        "Party #{}: aggregate signature verification failed {}",
                        self.party_id,
                        message.encode_hex::<String>()
                    );
                    Err(String::from("aggregate signature tampered"))
                }
            },
            None => {
                println!(
                    "Party #{}: aggregate signature not found {}",
                    self.party_id,
                    message.encode_hex::<String>()
                );
                Err(String::from("aggregate signature not found"))
            }
        }
    }
}

/// Verifier represents a participant that is not a signer in the protocol
#[derive(Debug)]
pub struct Verifier {
    /// Protocol parameters
    params: Option<ProtocolParameters>,
    /// Protocol clerk
    clerk: Option<ProtocolClerk>,
}

impl Verifier {
    /// Verifier factory
    pub fn new() -> Self {
        println!("Verifier: verifier created");
        Self {
            params: None,
            clerk: None,
        }
    }

    /// Update protocol parameters
    pub fn update_params(&mut self, params: &ProtocolParameters) {
        println!("Verifier: protocol params updated to {:?}", params);
        self.params = Some(*params);
    }

    /// Register keys
    pub fn register_keys(
        &mut self,
        players_with_keys: &[(
            ProtocolPartyId,
            ProtocolStake,
            ProtocolSignerVerificationKey,
        )],
    ) {
        let players = players_with_keys
            .iter()
            .map(|(party_id, stake, _verification_key)| (party_id.to_owned(), *stake))
            .collect::<Vec<_>>();
        println!("Verifier: protocol keys registration from {:?}", players);

        let mut key_reg = ProtocolKeyRegistration::init();
        for (_party_id, stake, verification_key) in players_with_keys {
            key_reg.register(*stake, *verification_key).unwrap();
        }
        let closed_reg = key_reg.close();

        self.clerk = Some(ProtocolClerk::from_registration(
            self.params.unwrap(),
            closed_reg,
        ));
    }

    /// Verify a message
    pub fn verify_message(
        &self,
        message: &Vec<u8>,
        msig: &ProtocolMultiSignature,
    ) -> Result<(), String> {
        match msig.verify(
            message,
            &self.clerk.as_ref().unwrap().compute_avk(),
            &self.params.unwrap(),
        ) {
            Ok(_) => {
                println!(
                    "Verifier: aggregate signature successfully verified for {}!",
                    message.encode_hex::<String>()
                );
                Ok(())
            }
            Err(_) => {
                println!(
                    "Verifier: aggregate signature verification failed {}",
                    message.encode_hex::<String>()
                );
                Err(String::from("aggregate signature tampered"))
            }
        }
    }
}

/// Demonstrator is a Mithril protocol demonstrator implementation
#[derive(Debug)]
pub struct Demonstrator {
    /// Configuration of the demonstrator
    config: crate::Config,
    /// List of protocol participants
    parties: Vec<Party>,
    /// Protocol external verifier
    verifier: Option<Verifier>,
    /// List of messages to sign
    messages: Vec<Vec<u8>>,
    /// Protocol parameters
    params: Option<ProtocolParameters>,
}

impl Demonstrator {
    /// Demonstrator factory
    pub fn new(config: &crate::Config) -> Self {
        // Generate parties
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let parties = (0..config.nparties)
            .into_iter()
            .map(|party_id| Party::new(party_id, 1 + rng.next_u64() % 999))
            .collect::<Vec<_>>();
        // Generate messages
        let messages = (0..config.nmessages)
            .into_iter()
            .map(|_| {
                let mut msg = [0u8; 16];
                rng.fill_bytes(&mut msg);
                msg.to_vec()
            })
            .collect::<Vec<Vec<u8>>>();
        Self {
            config: *config,
            parties,
            verifier: None,
            messages,
            params: None,
        }
    }
}

pub trait ProtocolDemonstrator {
    /// Establish phase of the protocol
    fn establish(&mut self);

    /// Initialization phase of the protocol
    fn initialize(&mut self);

    /// Issue certificates
    fn issue_certificates(&mut self);

    /// Verify certificates
    fn verify_certificates(&self) -> Result<(), String>;
}

impl ProtocolDemonstrator for Demonstrator {
    /// Establish phase of the protocol
    fn establish(&mut self) {
        self.params = Some(ProtocolParameters {
            m: self.config.m,
            k: self.config.k,
            phi_f: self.config.phi_f,
        });
        println!("Protocol established to {:?}", self.params.unwrap());
    }

    /// Initialization phase of the protocol
    fn initialize(&mut self) {
        // Retrieve protocol parameters
        let mut verifier = Verifier::new();
        verifier.update_params(&self.params.unwrap());
        for party in self.parties.iter_mut() {
            party.update_params(&self.params.unwrap());
        }

        // Register keys
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let players = self
            .parties
            .iter()
            .map(|party| (party.party_id.to_owned(), party.stake))
            .collect::<Vec<_>>();
        let mut players_artifacts = Vec::new();
        for (party_id, stake) in players {
            let protocol_initializer =
                ProtocolInitializer::setup(self.params.unwrap(), stake, &mut rng);
            let verification_key: ProtocolSignerVerificationKey =
                protocol_initializer.verification_key();
            players_artifacts.push(PlayerArtifact {
                party_id,
                stake: protocol_initializer.stake(),
                verification_key: key_encode_hex(verification_key).unwrap(),
                initializer: key_encode_hex(protocol_initializer).unwrap(),
            })
        }
        let players_with_keys = players_artifacts
            .iter()
            .map(|player| {
                (
                    player.party_id.to_owned(),
                    player.stake,
                    key_decode_hex(&player.verification_key).unwrap(),
                )
            })
            .collect::<Vec<(
                ProtocolPartyId,
                ProtocolStake,
                ProtocolSignerVerificationKey,
            )>>();
        verifier.register_keys(&players_with_keys);
        for party in self.parties.iter_mut() {
            party.register_keys(&players_with_keys);
        }
        self.verifier = Some(verifier);

        // Write artifacts
        write_artifacts("parties-keys", &players_artifacts);
    }

    /// Issue certificates
    fn issue_certificates(&mut self) {
        let mut single_signature_artifacts = Vec::new();
        let mut multi_signature_artifacts = Vec::new();
        for (i, message) in self.messages.iter().enumerate() {
            // Issue certificates
            println!("Message #{} to sign: {:?}", i, message);
            let mut signatures = Vec::<ProtocolSingleSignature>::new();
            for party in self.parties.iter_mut() {
                if let Some(party_signature) = party.sign_message(message) {
                    single_signature_artifacts.push(SingleSignatureArtifact {
                        party_id: party.party_id.to_owned(),
                        message: message.encode_hex::<String>(),
                        lotteries: party_signature.indexes.clone(),
                        signature: key_encode_hex(&party_signature).unwrap(),
                    });
                    signatures.push(party_signature);
                }
            }
            for party in self.parties.iter_mut() {
                let party_id = party.party_id.to_owned();
                if let Some(multi_signature) = party.sign_aggregate(message, &signatures) {
                    multi_signature_artifacts.push(MultiSignatureArtifact {
                        party_id,
                        message: message.encode_hex::<String>(),
                        signature: key_encode_hex(multi_signature).unwrap(),
                    })
                }
            }
        }

        // Write artifacts
        write_artifacts("single-signatures", &single_signature_artifacts);
        write_artifacts("multi-signatures", &multi_signature_artifacts);
    }

    /// Verify certificates
    fn verify_certificates(&self) -> Result<(), String> {
        for (i, message) in self.messages.iter().enumerate() {
            println!(
                "Message #{} to verify: {}",
                i,
                message.encode_hex::<String>()
            );
            for party in self.parties.iter() {
                match party.verify_message(message) {
                    Ok(_) => (),
                    Err(err) => return Err(err),
                }
                let msig = party.get_aggregate(message).unwrap();
                match self
                    .verifier
                    .as_ref()
                    .unwrap()
                    .verify_message(message, msig)
                {
                    Ok(_) => (),
                    Err(err) => return Err(err),
                }
            }
        }
        Ok(())
    }
}

/// Write artifacts helper
pub fn write_artifacts<T: Serialize>(artifact_name: &str, value: &T) {
    let artifacts_file_path_name = format!("artifacts/{}.json", artifact_name);
    let artifacts_file_path = env::current_dir()
        .unwrap()
        .join(path::Path::new(&artifacts_file_path_name));
    fs::create_dir_all(&artifacts_file_path.parent().unwrap()).unwrap();
    let mut artifacts_file = fs::File::create(&artifacts_file_path).unwrap();
    write!(
        artifacts_file,
        "{}",
        serde_json::to_string_pretty(value).unwrap()
    )
    .unwrap();
    println!("Artifacts written to {}", artifacts_file_path_name);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_protocol_parameters() -> ProtocolParameters {
        ProtocolParameters {
            m: 100,
            k: 10,
            phi_f: 0.65,
        }
    }

    fn default_config() -> crate::Config {
        let protocol_parameters = setup_protocol_parameters();
        crate::Config {
            m: protocol_parameters.m,
            k: protocol_parameters.k,
            phi_f: protocol_parameters.phi_f,
            nparties: 5,
            nmessages: 2,
        }
    }

    #[test]
    fn test_demonstrator_new() {
        let config = default_config();
        let demo = Demonstrator::new(&config);
        assert_eq!(demo.config, config);
    }

    #[test]
    fn test_demonstrator_establish() {
        let config = default_config();
        let mut demo = Demonstrator::new(&config);
        demo.establish();
        assert_eq!(demo.params.unwrap().m, config.m);
        assert_eq!(demo.params.unwrap().k, config.k);
        assert_eq!(demo.params.unwrap().phi_f, config.phi_f);
    }

    #[test]
    fn test_demonstrator_initialize() {
        let config = default_config();
        let mut demo = Demonstrator::new(&config);
        demo.establish();
        demo.initialize();
        assert_eq!(demo.parties.len(), config.nparties);
        assert_eq!(demo.messages.len(), config.nmessages);
        for party in demo.parties {
            assert_ne!(party.stake, 0);
            assert!(party.signer.is_some());
            assert!(party.clerk.is_some());
        }
        assert!(demo.verifier.is_some());
    }

    #[test]
    fn test_demonstrator_issue_certificates_ok() {
        let config = default_config();
        let mut demo = Demonstrator::new(&config);
        demo.establish();
        demo.initialize();
        demo.issue_certificates();
        assert_eq!(demo.parties.len(), config.nparties);
        assert_eq!(demo.messages.len(), config.nmessages);
        for party in demo.parties {
            assert_eq!(party.msigs.len(), config.nmessages);
        }
    }

    #[test]
    fn test_demonstrator_issue_certificates_ko() {
        let mut config = default_config();
        config.k = 10000;
        config.m = 10;
        let mut demo = Demonstrator::new(&config);
        demo.establish();
        demo.initialize();
        demo.issue_certificates();
        assert_eq!(demo.parties.len(), config.nparties);
        assert_eq!(demo.messages.len(), config.nmessages);
        for party in demo.parties {
            assert_eq!(party.msigs.len(), 0);
        }
    }

    #[test]
    fn test_demonstrator_verify_certificates_ok() {
        let config = default_config();
        let mut demo = Demonstrator::new(&config);
        demo.establish();
        demo.initialize();
        demo.issue_certificates();
        assert_eq!(demo.parties.len(), config.nparties);
        assert_eq!(demo.messages.len(), config.nmessages);
        assert!(demo.verify_certificates().is_ok())
    }

    #[test]
    fn test_demonstrator_verify_certificates_ko() {
        let mut config = default_config();
        config.k = 10000;
        config.m = 10;
        let mut demo = Demonstrator::new(&config);
        demo.establish();
        demo.initialize();
        demo.issue_certificates();
        assert_eq!(demo.parties.len(), config.nparties);
        assert_eq!(demo.messages.len(), config.nmessages);
        assert!(demo.verify_certificates().is_err())
    }
}
