use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    certificate_chain::CertificateGenesisProducer,
    crypto_helper::{
        ProtocolAggregateVerificationKey, ProtocolClosedKeyRegistration, ProtocolGenesisSigner,
        ProtocolInitializer, ProtocolOpCert, ProtocolSigner, ProtocolSignerVerificationKey,
        ProtocolSignerVerificationKeySignature, ProtocolStakeDistribution,
    },
    entities::{
        Certificate, Epoch, HexEncodedAggregateVerificationKey, PartyId, ProtocolParameters,
        Signer, SignerWithStake, SingleSignatures, Stake, StakeDistribution,
        StakeDistributionParty,
    },
    protocol::{SignerBuilder, ToMessage},
    StdResult,
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
    /// A [ProtocolClosedKeyRegistration].
    pub protocol_closed_key_registration: ProtocolClosedKeyRegistration,
    /// The path to this signer kes secret key file
    pub kes_secret_key_path: Option<PathBuf>,
}

impl SignerFixture {
    /// Create a new SignerFixture with specific protocol parameters.
    /// This is useful to simulate some adversarial behaviors.
    pub fn try_new_with_protocol_parameters(
        self,
        protocol_parameters: ProtocolParameters,
    ) -> StdResult<Self> {
        let mut protocol_initializer = self.protocol_initializer.clone();
        protocol_initializer.override_protocol_parameters(&protocol_parameters.into());
        let protocol_signer =
            protocol_initializer.new_signer(self.protocol_closed_key_registration.clone())?;
        Ok(Self {
            protocol_signer,
            ..self
        })
    }
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

/// Represent the output of the `stake-snapshot` command of the cardano-cli
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CardanoCliStakeDistribution {
    #[serde(rename = "pools")]
    pub signers: HashMap<String, CardanoCliSignerStake>,
}

/// Represent the stakes of a party in the output of the `stake-snapshot`
/// command of the cardano-cli
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CardanoCliSignerStake {
    #[serde(rename = "stakeMark")]
    actual_stake: Stake,
    #[serde(rename = "stakeSet")]
    previous_stake: Stake,
    #[serde(rename = "stakeGo")]
    penultimate_stake: Stake,
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

    /// Get certificate metadata signers
    pub fn stake_distribution_parties(&self) -> Vec<StakeDistributionParty> {
        self.signers
            .iter()
            .map(|s| StakeDistributionParty {
                party_id: s.signer_with_stake.party_id.clone(),
                stake: s.signer_with_stake.stake,
            })
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

    /// Get the stake distribution formated as a cardano-cli `stake-snapshot` output.
    ///
    /// Note: will fail if the signers certification was disabled
    pub fn cardano_cli_stake_distribution(&self) -> CardanoCliStakeDistribution {
        let signers = HashMap::from_iter(self.signers_fixture().into_iter().map(|signer| {
            (
                signer.compute_protocol_party_id_as_hash(),
                CardanoCliSignerStake {
                    actual_stake: signer.signer_with_stake.stake,
                    previous_stake: signer.signer_with_stake.stake,
                    penultimate_stake: signer.signer_with_stake.stake,
                },
            )
        }));

        CardanoCliStakeDistribution { signers }
    }

    /// Compute the Aggregate Verification Key for this fixture.
    pub fn compute_avk(&self) -> ProtocolAggregateVerificationKey {
        SignerBuilder::new(&self.signers_with_stake(), &self.protocol_parameters)
            .unwrap()
            .compute_aggregate_verification_key()
    }

    /// Compute the Aggregate Verification Key for this fixture and returns it has a [HexEncodedAggregateVerificationKey].
    pub fn compute_and_encode_avk(&self) -> HexEncodedAggregateVerificationKey {
        let avk = self.compute_avk();
        avk.to_json_hex().unwrap()
    }

    /// Create a genesis certificate using the fixture signers for the given beacon
    pub fn create_genesis_certificate<T: Into<String>>(
        &self,
        network: T,
        epoch: Epoch,
    ) -> Certificate {
        let genesis_avk = self.compute_avk();
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let genesis_producer = CertificateGenesisProducer::new(Some(Arc::new(genesis_signer)));
        let genesis_protocol_message = CertificateGenesisProducer::create_genesis_protocol_message(
            &self.protocol_parameters,
            &genesis_avk,
            &epoch,
        )
        .unwrap();
        let genesis_signature = genesis_producer
            .sign_genesis_protocol_message(genesis_protocol_message)
            .unwrap();

        CertificateGenesisProducer::create_genesis_certificate(
            self.protocol_parameters.clone(),
            network,
            epoch,
            genesis_avk,
            genesis_signature,
        )
        .unwrap()
    }

    /// Make all underlying signers sign the given message, filter the resulting list to remove
    /// the signers that did not sign because they loosed the lottery.
    pub fn sign_all<T: ToMessage>(&self, message: &T) -> Vec<SingleSignatures> {
        self.signers
            .par_iter()
            .filter_map(|s| s.sign(message))
            .collect()
    }
}

impl From<MithrilFixture> for Vec<Signer> {
    fn from(fixture: MithrilFixture) -> Self {
        fixture.signers()
    }
}

impl From<MithrilFixture> for Vec<SignerWithStake> {
    fn from(fixture: MithrilFixture) -> Self {
        fixture.signers_with_stake()
    }
}

impl From<MithrilFixture> for Vec<SignerFixture> {
    fn from(fixture: MithrilFixture) -> Self {
        fixture.signers_fixture()
    }
}

impl SignerFixture {
    /// Sign the given protocol message.
    pub fn sign<T: ToMessage>(&self, message: &T) -> Option<SingleSignatures> {
        let message = message.to_message();
        self.protocol_signer
            .sign(message.as_bytes())
            .map(|signature| {
                let won_indexes = signature.indexes.clone();

                SingleSignatures::new(
                    self.signer_with_stake.party_id.to_owned(),
                    signature.into(),
                    won_indexes,
                )
            })
    }

    /// Shortcut to get the party id from the inner signer with stake
    pub fn party_id(&self) -> PartyId {
        self.signer_with_stake.party_id.clone()
    }

    /// Decode this signer operational certificate if any
    pub fn operational_certificate(&self) -> Option<ProtocolOpCert> {
        self.signer_with_stake.operational_certificate.clone()
    }

    /// Compute the party id hash
    ///
    /// Note: will fail if the signers certification was disabled
    pub fn compute_protocol_party_id_as_hash(&self) -> String {
        self.operational_certificate()
            .unwrap()
            .compute_protocol_party_id_as_hash()
    }

    /// Decode this signer verification key certificate
    pub fn verification_key(&self) -> ProtocolSignerVerificationKey {
        self.signer_with_stake.verification_key
    }

    /// Decode this signer verification key signature certificate if any
    pub fn verification_key_signature(&self) -> Option<ProtocolSignerVerificationKeySignature> {
        self.signer_with_stake.verification_key_signature
    }

    /// Get the path to this signer kes secret key
    pub fn kes_secret_key_path(&self) -> Option<&Path> {
        self.kes_secret_key_path.as_deref()
    }
}
