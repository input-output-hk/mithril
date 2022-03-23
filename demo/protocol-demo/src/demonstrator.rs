use std::collections::HashMap;

use ark_bls12_377::Bls12_377;
use mithril::key_reg::KeyReg;
use mithril::merkle_tree::MTHashLeaf;
use mithril::mithril_proof::concat_proofs::{ConcatProof, TrivialEnv};
use mithril::msp::MspPk;
use mithril::stm::{
    AggregationFailure, MTValue, PartyId, Stake, StmClerk, StmInitializer, StmMultiSig,
    StmParameters, StmSig, StmSigner,
};

use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

type H = blake2::Blake2b;
type F = <H as MTHashLeaf<MTValue<Bls12_377>>>::F;

pub type Bytes = Vec<u8>;
pub type ProtocolParameters = StmParameters;
pub type ProtocolSigner = StmSigner<H, Bls12_377>;
pub type ProtocolInitializer = StmInitializer<Bls12_377>;
pub type ProtocolClerk = StmClerk<H, Bls12_377, TrivialEnv>;
pub type ProtocolVerificationKey = MspPk<Bls12_377>;
pub type ProtocolProof = ConcatProof<Bls12_377, H, F>;
pub type ProtocolSingleSig = StmSig<Bls12_377, F>;
pub type ProtocolMultiSig = StmMultiSig<Bls12_377, ProtocolProof>;

/// Party represents a signing protocol participant
#[derive(Debug)]
pub struct Party {
    /// Party's identifier
    party_id: PartyId,
    /// Party's stake
    stake: Stake,
    /// Protocol parameters
    params: Option<ProtocolParameters>,
    /// Protocol signer
    signer: Option<ProtocolSigner>,
    /// Protocol clerk
    clerk: Option<ProtocolClerk>,
    /// Multi signatures
    msigs: HashMap<Bytes, ProtocolMultiSig>,
}

impl Party {
    /// Party factory
    pub fn new(party_id: usize, stake: u64) -> Self {
        println!("Party #{}: party created with {} stakes", party_id, stake);
        Self {
            party_id: party_id as PartyId,
            stake: stake as Stake,
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
    pub fn register_keys(&mut self, players_with_keys: &[(PartyId, Stake, ProtocolVerificationKey)]) {
        let players = players_with_keys
            .into_iter()
            .map(|(party_id, stake, _verification_key)| (*party_id, *stake))
            .collect::<Vec<_>>();
        println!(
            "Party #{}: protocol keys registration from {:?}",
            self.party_id, players
        );

        let mut key_reg = KeyReg::new(&players);
        for (party_id, _stake, verification_key) in players_with_keys {
            key_reg.register(*party_id, *verification_key).unwrap();
        }
        let closed_reg = key_reg.close();

        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let p = StmInitializer::setup(self.params.unwrap(), self.party_id, self.stake, &mut rng);
        self.signer = Some(p.new_signer(closed_reg));
        self.clerk = Some(StmClerk::from_signer(
            &self.signer.as_ref().unwrap(),
            TrivialEnv,
        ));
    }

    /// Individually sign a message through lottery
    pub fn sign_message(&mut self, message: &Bytes) -> Vec<(ProtocolSingleSig, u64)> {
        let mut signatures = Vec::new();
        println!(
            "Party #{}: sign message {:?}",
            self.party_id,
            message.as_slice().to_vec()
        );
        for i in 1..self.params.unwrap().m {
            if let Some(signature) = self.signer.as_ref().unwrap().sign(&message, i) {
                println!("Party #{}: lottery #{} won", self.party_id, i);
                signatures.push((signature, i));
            }
        }
        signatures
    }

    /// Aggregate signatures
    pub fn sign_aggregate(
        &mut self,
        message: &Bytes,
        signatures: &Vec<(ProtocolSingleSig, u64)>,
    ) {
        let unzipped_signatures: (Vec<_>, Vec<_>) = signatures.iter().cloned().unzip();
        let msig = self
            .clerk
            .as_ref()
            .unwrap()
            .aggregate::<ProtocolProof>(
                &unzipped_signatures.0,
                &unzipped_signatures.1,
                message,
            );
        match msig {
            Ok(aggregate_signature) => {
                println!("Party #{}: aggregate signature computed", self.party_id);
                self.msigs.insert(message.clone(), aggregate_signature);
            }
            Err(AggregationFailure::NotEnoughSignatures(_n, _k)) => {
                println!(
                    "Party #{}: not enough signatures to compute aggregate",
                    self.party_id
                );
            }
        }
    }

    /// Retrieve agreggate signature associated to a message
    pub fn get_aggregate(&self, message: &Bytes) -> Option<&ProtocolMultiSig> {
        self.msigs.get(message)
    }

    /// Verify a certificate
    pub fn verify_message(&self, message: &Bytes) -> Result<(), String> {
        match self.get_aggregate(message) {
            Some(msig) => {
                match self
                    .clerk
                    .as_ref()
                    .unwrap()
                    .verify_msig::<ProtocolProof>(&msig, message)
                {
                    Ok(_) => {
                        println!(
                            "Party #{}: aggregate signature successfully verified for {:?}!",
                            self.party_id, message
                        );
                        Ok(())
                    }
                    Err(_) => {
                        println!(
                            "Party #{}: aggregate signature verification failed {:?}",
                            self.party_id, message
                        );
                        Err(String::from("aggregate signature tampered"))
                    }
                }
            }
            None => {
                println!(
                    "Party #{}: aggregate signature not found {:?}",
                    self.party_id, message
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
        println!(
            "Verifier: protocol params updated to {:?}",
            params
        );
        self.params = Some(*params);
    }

    /// Register keys
    pub fn register_keys(&mut self, players_with_keys: &[(PartyId, Stake, ProtocolVerificationKey)]) {
        let players = players_with_keys
            .into_iter()
            .map(|(party_id, stake, _verification_key)| (*party_id, *stake))
            .collect::<Vec<_>>();
        println!(
            "Verifier: protocol keys registration from {:?}",
            players
        );

        let mut key_reg = KeyReg::new(&players);
        for (party_id, _stake, verification_key) in players_with_keys {
            key_reg.register(*party_id, *verification_key).unwrap();
        }
        let closed_reg = key_reg.close();

        self.clerk = Some(StmClerk::from_registration(
            self.params.unwrap(),
            TrivialEnv,
            closed_reg
        ));
    }

    /// Verify a message
    pub fn verify_message(&self, message: &Bytes, msig: &ProtocolMultiSig) -> Result<(), String> {
        match self
                    .clerk
                    .as_ref()
                    .unwrap()
                    .verify_msig::<ProtocolProof>(msig, message)
                {
                    Ok(_) => {
                        println!(
                            "Verifier: aggregate signature successfully verified for {:?}!",
                            message
                        );
                        Ok(())
                    }
                    Err(_) => {
                        println!(
                            "Verifier: aggregate signature verification failed {:?}",
                            message
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
    messages: Vec<Bytes>,
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
            .collect::<Vec<Bytes>>();
        Self {
            config: config.clone(),
            parties: parties,
            verifier: None,
            messages: messages,
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
        for party in self.parties.iter_mut() {
            party.update_params(&self.params.unwrap());
        }
        let mut verifier = Verifier::new();
        verifier.update_params(&self.params.unwrap());

        // Register keys
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let players = self
            .parties
            .iter()
            .map(|party| (party.party_id, party.stake))
            .collect::<Vec<_>>();
        let mut players_with_keys = Vec::new();
        for (party_id, stake) in players {
            let p: ProtocolInitializer =
                StmInitializer::setup(self.params.unwrap(), party_id, stake, &mut rng);
            players_with_keys.push((p.party_id(), p.stake(), p.verification_key()))
        }
        for party in self.parties.iter_mut() {
            party.register_keys(&players_with_keys);
        }
        verifier.register_keys(&players_with_keys);
        self.verifier = Some(verifier);
    }

    /// Issue certificates
    fn issue_certificates(&mut self) {
        for (i, message) in self.messages.iter().enumerate() {
            println!("Message #{} to sign: {:?}", i, message);
            let mut signatures = Vec::<(ProtocolSingleSig, u64)>::new();
            for party in self.parties.iter_mut() {
                let party_signatures = party.sign_message(&message);
                signatures.extend(party_signatures);
            }
            for party in self.parties.iter_mut() {
                party.sign_aggregate(&message, &signatures);
            }
        }
    }

    /// Verify certificates
    fn verify_certificates(&self) -> Result<(), String> {
        for (i, message) in self.messages.iter().enumerate() {
            println!("Message #{} to verify: {:?}", i, message);
            for party in self.parties.iter() {
                match party.verify_message(&message) {
                    Ok(_) => (),
                    Err(err) => return Err(err),
                }
                let msig = party.get_aggregate(&message).unwrap();
                match self.verifier.as_ref().unwrap().verify_message(&message, msig) {
                    Ok(_) => (),
                    Err(err) => return Err(err),
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn default_config() -> crate::Config {
        crate::Config {
            m: 100,
            k: 10,
            phi_f: 0.5,
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
