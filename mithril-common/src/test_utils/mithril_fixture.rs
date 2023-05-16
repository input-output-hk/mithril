use std::sync::Arc;

use crate::{
    certificate_chain::CertificateGenesisProducer,
    crypto_helper::{
        key_decode_hex, key_encode_hex, OpCert, ProtocolAggregateVerificationKey, ProtocolClerk,
        ProtocolGenesisSigner, ProtocolInitializer, ProtocolKeyRegistration, ProtocolSigner,
        ProtocolSignerVerificationKey, ProtocolSignerVerificationKeySignature,
        ProtocolStakeDistribution,
    },
    entities::{
        Beacon, Certificate, HexEncodedAgregateVerificationKey, ProtocolMessage,
        ProtocolParameters, Signer, SignerWithStake, SingleSignatures, StakeDistribution,
    },
};

/// A fixture of Mithril data types.
#[derive(Debug, Clone)]
pub struct MithrilFixture {
    protocol_parameters: ProtocolParameters,
    signers: Vec<SignerFixture>,
    stake_distribution: ProtocolStakeDistribution,
}

/// A signer fixture, containing a [signer entity][SignerWithStake] with its
/// corresponding protocol [signer][ProtocolSigner] and
/// [initializer][ProtocolInitializer]
#[derive(Debug, Clone)]
pub struct SignerFixture {
    /// A [SignerWithStake].
    pub signer_with_stake: SignerWithStake,
    /// A [ProtocolSigner].
    pub protocol_signer: ProtocolSigner,
    /// A [ProtocolSigner].
    pub protocol_initializer: ProtocolInitializer,
}

impl From<SignerFixture> for SignerWithStake {
    fn from(fixture: SignerFixture) -> Self {
        fixture.signer_with_stake
    }
}

impl From<&SignerFixture> for SignerWithStake {
    fn from(fixture: &SignerFixture) -> Self {
        fixture.signer_with_stake.clone()
    }
}

impl MithrilFixture {
    /// [MithrilFixture] factory.
    pub fn new(
        protocol_parameters: ProtocolParameters,
        signers: Vec<SignerFixture>,
        stake_distribution: ProtocolStakeDistribution,
    ) -> Self {
        Self {
            protocol_parameters,
            signers,
            stake_distribution,
        }
    }

    /// Get the fixture protocol parameters.
    pub fn protocol_parameters(&self) -> ProtocolParameters {
        self.protocol_parameters.clone()
    }

    /// Get the fixture signers.
    pub fn signers_fixture(&self) -> Vec<SignerFixture> {
        self.signers.clone()
    }

    /// Get the fixture signers.
    pub fn signers(&self) -> Vec<Signer> {
        self.signers
            .clone()
            .into_iter()
            .map(|s| s.signer_with_stake.into())
            .collect()
    }

    /// Get the fixture signers with stake.
    pub fn signers_with_stake(&self) -> Vec<SignerWithStake> {
        self.signers
            .iter()
            .map(|s| &s.signer_with_stake)
            .cloned()
            .collect()
    }

    /// Get the fixture stake distribution.
    pub fn stake_distribution(&self) -> StakeDistribution {
        StakeDistribution::from_iter(self.stake_distribution.clone())
    }

    /// Get the fixture protocol stake distribution.
    pub fn protocol_stake_distribution(&self) -> ProtocolStakeDistribution {
        self.stake_distribution.clone()
    }

    /// Create a [ProtocolClerk] based on this fixture protocol parameters & signers
    pub fn create_clerk(&self) -> ProtocolClerk {
        let mut key_registration = ProtocolKeyRegistration::init(&self.stake_distribution);

        for signer in self.signers.clone() {
            key_registration
                .register(
                    Some(signer.signer_with_stake.party_id.to_owned()),
                    signer.operational_certificate(),
                    signer.verification_key_signature(),
                    signer.signer_with_stake.kes_period,
                    signer.verification_key(),
                )
                .unwrap();
        }
        let closed_registration = key_registration.close();

        ProtocolClerk::from_registration(
            &self.protocol_parameters.clone().into(),
            &closed_registration,
        )
    }

    /// Compute the Aggregate Verification Key for this fixture.
    pub fn compute_avk(&self) -> ProtocolAggregateVerificationKey {
        let clerk = self.create_clerk();
        clerk.compute_avk()
    }

    /// Compute the Aggregate Verification Key for this fixture and returns it has a [HexEncodedAgregateVerificationKey].
    pub fn compute_and_encode_avk(&self) -> HexEncodedAgregateVerificationKey {
        let avk = self.compute_avk();
        key_encode_hex(avk).unwrap()
    }

    /// Create a genesis certificate using the fixture signers for the given beacon
    pub fn create_genesis_certificate(&self, beacon: &Beacon) -> Certificate {
        let genesis_avk = self.compute_avk();
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let genesis_producer = CertificateGenesisProducer::new(Some(Arc::new(genesis_signer)));
        let genesis_protocol_message =
            CertificateGenesisProducer::create_genesis_protocol_message(&genesis_avk).unwrap();
        let genesis_signature = genesis_producer
            .sign_genesis_protocol_message(genesis_protocol_message)
            .unwrap();

        CertificateGenesisProducer::create_genesis_certificate(
            self.protocol_parameters.clone(),
            beacon.clone(),
            genesis_avk,
            genesis_signature,
        )
        .unwrap()
    }
}

impl SignerFixture {
    /// Sign the given protocol message.
    pub fn sign(&self, protocol_message: &ProtocolMessage) -> Option<SingleSignatures> {
        self.protocol_signer
            .sign(protocol_message.compute_hash().as_bytes())
            .map(|signature| {
                let won_indexes = signature.indexes.clone();

                SingleSignatures::new(
                    self.signer_with_stake.party_id.to_owned(),
                    key_encode_hex(signature).unwrap(),
                    won_indexes,
                )
            })
    }

    /// Decode this signer operational certificate if any
    pub fn operational_certificate(&self) -> Option<OpCert> {
        match &self.signer_with_stake.operational_certificate {
            Some(operational_certificate) => key_decode_hex(operational_certificate).unwrap(),
            _ => None,
        }
    }

    /// Decode this signer verification key certificate
    pub fn verification_key(&self) -> ProtocolSignerVerificationKey {
        key_decode_hex(&self.signer_with_stake.verification_key).unwrap()
    }

    /// Decode this signer verification key signature certificate if any
    pub fn verification_key_signature(&self) -> Option<ProtocolSignerVerificationKeySignature> {
        self.signer_with_stake
            .verification_key_signature
            .as_ref()
            .map(|verification_key_signature| key_decode_hex(verification_key_signature).unwrap())
    }
}
