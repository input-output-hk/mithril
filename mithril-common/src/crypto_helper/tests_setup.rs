//! Test data builders for Mithril STM types, for testing purpose.
use super::{genesis::*, types::*, OpCert, SerDeShelleyFileFormat};
use crate::{
    entities::{Certificate, ProtocolMessage, ProtocolMessagePartKey, SignerWithStake, Stake},
    test_utils::{CertificateChainBuilder, SignerFixture, TempDir},
};

use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::{fs, path::PathBuf};

/// Create or retrieve a temporary directory for storing cryptographic material for a signer, use this for tests only.
pub fn setup_temp_directory_for_signer(
    party_id: &ProtocolPartyId,
    auto_create: bool,
) -> Option<PathBuf> {
    let temp_dir = TempDir::new("tests_setup", "mithril_crypto_helper_material")
        .build_path()
        .join(party_id);

    if auto_create {
        fs::create_dir_all(&temp_dir).expect("temp dir creation should not fail");
    }
    temp_dir.exists().then_some(temp_dir)
}

/// Instantiate a [ProtocolMessage] using fake data, use this for tests only.
pub fn setup_message() -> ProtocolMessage {
    let mut protocol_message = ProtocolMessage::new();
    protocol_message.set_message_part(
        ProtocolMessagePartKey::SnapshotDigest,
        "message_to_sign_123".to_string(),
    );
    protocol_message.set_message_part(
        ProtocolMessagePartKey::NextAggregateVerificationKey,
        "next-avk-123".to_string(),
    );
    protocol_message
}

/// Instantiate a [ProtocolParameters], use this for tests only.
pub fn setup_protocol_parameters() -> ProtocolParameters {
    ProtocolParameters {
        m: 100,
        k: 5,
        phi_f: 0.65,
    }
}

fn setup_protocol_initializer(
    party_id: &str,
    kes_secret_key_path: Option<PathBuf>,
    stake: Stake,
    protocol_parameters: &ProtocolParameters,
) -> ProtocolInitializer {
    let protocol_initializer_seed: [u8; 32] = format!("{party_id:<032}").as_bytes()[..32]
        .try_into()
        .unwrap();
    let mut protocol_initializer_rng = ChaCha20Rng::from_seed(protocol_initializer_seed);
    let kes_period = kes_secret_key_path.as_ref().map(|_| 0);
    let protocol_initializer: ProtocolInitializer = ProtocolInitializer::setup(
        *protocol_parameters,
        kes_secret_key_path,
        kes_period,
        stake,
        &mut protocol_initializer_rng,
    )
    .expect("protocol initializer setup should not fail");

    protocol_initializer
}

fn setup_signer_with_stake(
    party_id: &str,
    stake: Stake,
    protocol_initializer: &ProtocolInitializer,
    operational_certificate: Option<ProtocolOpCert>,
    kes_period: u32,
) -> SignerWithStake {
    let kes_period = operational_certificate.as_ref().and(Some(kes_period));

    SignerWithStake::new(
        party_id.to_owned(),
        protocol_initializer.verification_key().into(),
        protocol_initializer.verification_key_signature(),
        operational_certificate,
        kes_period,
        stake,
    )
}

fn decode_op_cert_in_dir(dir: Option<PathBuf>) -> Option<ProtocolOpCert> {
    dir.as_ref().map(|dir| {
        OpCert::from_file(dir.join("opcert.cert"))
            .expect("operational certificate decoding should not fail")
            .into()
    })
}

/// Instantiate a list of protocol signers based on the given [ProtocolStakeDistribution] and [ProtocolParameters], use this for tests only.
pub fn setup_signers_from_stake_distribution(
    stake_distribution: &ProtocolStakeDistribution,
    protocol_parameters: &ProtocolParameters,
) -> Vec<SignerFixture> {
    let mut key_registration = ProtocolKeyRegistration::init(stake_distribution);
    let mut signers: Vec<(SignerWithStake, ProtocolInitializer, Option<PathBuf>)> = vec![];

    for (party_id, stake) in stake_distribution {
        let kes_period = 0;
        let temp_dir = setup_temp_directory_for_signer(party_id, false);
        let kes_secret_key_path: Option<PathBuf> = temp_dir.as_ref().map(|dir| dir.join("kes.sk"));
        let protocol_initializer = setup_protocol_initializer(
            party_id,
            kes_secret_key_path.clone(),
            *stake,
            protocol_parameters,
        );
        let operational_certificate = decode_op_cert_in_dir(temp_dir);
        let signer_with_stake = setup_signer_with_stake(
            party_id,
            *stake,
            &protocol_initializer,
            operational_certificate.clone(),
            kes_period,
        );

        key_registration
            .register(
                Some(signer_with_stake.party_id.to_owned()),
                operational_certificate,
                protocol_initializer.verification_key_signature(),
                Some(kes_period),
                protocol_initializer.verification_key().into(),
            )
            .expect("key registration should have succeeded");

        signers.push((signer_with_stake, protocol_initializer, kes_secret_key_path));
    }

    let closed_key_registration = key_registration.close();

    signers
        .into_iter()
        .map(
            |(signer_with_stake, protocol_initializer, kes_secret_key_path)| {
                let protocol_closed_key_registration = closed_key_registration.clone();
                let protocol_signer = protocol_initializer
                    .clone()
                    .new_signer(protocol_closed_key_registration.clone())
                    .expect("creating a new protocol signer should not fail");

                SignerFixture {
                    signer_with_stake,
                    protocol_signer,
                    protocol_initializer,
                    protocol_closed_key_registration,
                    kes_secret_key_path,
                }
            },
        )
        .collect::<_>()
}

/// Instantiate a certificate chain, use this for tests only.
pub fn setup_certificate_chain(
    total_certificates: u64,
    certificates_per_epoch: u64,
) -> (Vec<Certificate>, ProtocolGenesisVerifier) {
    let certificate_chain_builder = CertificateChainBuilder::new()
        .with_total_certificates(total_certificates)
        .with_certificates_per_epoch(certificates_per_epoch)
        .with_protocol_parameters(setup_protocol_parameters());

    certificate_chain_builder.build()
}
