//! Test data builders for Mithril STM types, for testing purpose.
use super::{genesis::*, key_encode_hex, types::*, OpCert, SerDeShelleyFileFormat};
use crate::{
    certificate_chain::CertificateGenesisProducer,
    entities::{
        Certificate, CertificateSignature, Epoch, ProtocolMessage, ProtocolMessagePartKey,
        SignerWithStake, Stake,
    },
    test_utils::{fake_data, MithrilFixtureBuilder, SignerFixture},
};

use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::{cmp::min, fs, sync::Arc};

use std::{collections::HashMap, path::PathBuf};

/// Create or retrieve a temporary directory for storing cryptographic material for a signer, use this for tests only.
pub fn setup_temp_directory_for_signer(
    party_id: &ProtocolPartyId,
    auto_create: bool,
) -> Option<PathBuf> {
    let temp_dir = std::env::temp_dir()
        .join("mithril_crypto_helper_material")
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
    let protocol_initializer_seed: [u8; 32] = party_id.as_bytes()[..32].try_into().unwrap();
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
    operational_certificate: Option<OpCert>,
    kes_period: u32,
) -> SignerWithStake {
    SignerWithStake::new(
        party_id.to_owned(),
        protocol_initializer.verification_key().into(),
        protocol_initializer
            .verification_key_signature()
            .as_ref()
            .map(|verification_key_signature| {
                key_encode_hex(verification_key_signature)
                    .expect("key_encode_hex of verification_key_signature should not fail")
            }),
        operational_certificate
            .as_ref()
            .map(|operational_certificate| {
                key_encode_hex(operational_certificate)
                    .expect("key_encode_hex of operational_certificate should not fail")
            }),
        operational_certificate.as_ref().map(|_| kes_period),
        stake,
    )
}

fn decode_op_cert_in_dir(dir: Option<PathBuf>) -> Option<OpCert> {
    dir.as_ref().map(|dir| {
        OpCert::from_file(dir.join("opcert.cert"))
            .expect("operational certificate decoding should not fail")
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
                let protocol_signer = protocol_initializer
                    .clone()
                    .new_signer(closed_key_registration.clone())
                    .expect("creating a new protocol signer should not fail");

                SignerFixture {
                    signer_with_stake,
                    protocol_signer,
                    protocol_initializer,
                    kes_secret_key_path,
                }
            },
        )
        .collect::<_>()
}

/// Instantiate a Genesis Signer and its associated Verifier
pub fn setup_genesis() -> (ProtocolGenesisSigner, ProtocolGenesisVerifier) {
    let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
    let genesis_verifier = genesis_signer.create_genesis_verifier();
    (genesis_signer, genesis_verifier)
}

/// Instantiate a certificate chain, use this for tests only.
pub fn setup_certificate_chain(
    total_certificates: u64,
    certificates_per_epoch: u64,
) -> (Vec<Certificate>, ProtocolGenesisVerifier) {
    let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
    let genesis_verifier = genesis_signer.create_genesis_verifier();
    let genesis_producer = CertificateGenesisProducer::new(Some(Arc::new(genesis_signer)));
    let protocol_parameters = setup_protocol_parameters();
    let mut epochs = (1..total_certificates + 2)
        .map(|i| match certificates_per_epoch {
            0 => panic!("expected at least 1 certificate per epoch"),
            1 => Epoch(i),
            _ => Epoch(i / certificates_per_epoch + 1),
        })
        .collect::<Vec<_>>();
    let fixture_per_epoch = epochs
        .clone()
        .into_iter()
        .map(|epoch| {
            (
                epoch,
                MithrilFixtureBuilder::default()
                    .with_protocol_parameters(protocol_parameters.into())
                    .with_signers(min(2 + epoch.0 as usize, 5))
                    .build(),
            )
        })
        .collect::<HashMap<_, _>>();
    let clerk_for_signers = |signers: &[SignerFixture]| -> ProtocolClerk {
        let first_signer = &signers[0].protocol_signer;
        ProtocolClerk::from_signer(first_signer)
    };
    let avk_for_signers = |signers: &[SignerFixture]| -> ProtocolAggregateVerificationKey {
        let clerk = clerk_for_signers(signers);
        clerk.compute_avk()
    };
    let avk_encode =
        |avk: &ProtocolAggregateVerificationKey| -> String { key_encode_hex(avk).unwrap() };
    epochs.pop();
    let certificates = epochs
        .into_iter()
        .enumerate()
        .map(|(i, epoch)| {
            let immutable_file_number = i as u64 * 10;
            let digest = format!("digest{i}");
            let certificate_hash = format!("certificate_hash-{i}");
            let fixture = fixture_per_epoch.get(&epoch).unwrap();
            let next_fixture = fixture_per_epoch.get(&(epoch + 1)).unwrap();
            let avk = avk_for_signers(&fixture.signers_fixture());
            let next_avk = avk_for_signers(&next_fixture.signers_fixture());
            let mut fake_certificate = fake_data::certificate(certificate_hash);
            fake_certificate.beacon.epoch = epoch;
            fake_certificate.beacon.immutable_file_number = immutable_file_number;
            fake_certificate
                .protocol_message
                .set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);
            fake_certificate.protocol_message.set_message_part(
                ProtocolMessagePartKey::NextAggregateVerificationKey,
                avk_encode(&next_avk),
            );
            fake_certificate.aggregate_verification_key = avk_encode(&avk);
            fake_certificate.signed_message = fake_certificate.protocol_message.compute_hash();
            fake_certificate.previous_hash = "".to_string();
            match i {
                0 => {
                    let genesis_protocol_message =
                        CertificateGenesisProducer::create_genesis_protocol_message(&next_avk)
                            .unwrap();
                    let genesis_signature = genesis_producer
                        .sign_genesis_protocol_message(genesis_protocol_message)
                        .unwrap();
                    fake_certificate = CertificateGenesisProducer::create_genesis_certificate(
                        fake_certificate.metadata.protocol_parameters,
                        fake_certificate.beacon,
                        next_avk,
                        genesis_signature.into(),
                    )
                    .unwrap()
                }
                _ => {
                    fake_certificate.metadata.signers = fixture.signers_with_stake();

                    let single_signatures = fixture
                        .signers_fixture()
                        .iter()
                        .filter_map(|s| {
                            s.protocol_signer
                                .sign(fake_certificate.signed_message.as_bytes())
                        })
                        .collect::<Vec<_>>();
                    let clerk = clerk_for_signers(&fixture.signers_fixture());
                    let multi_signature = clerk
                        .aggregate(
                            &single_signatures,
                            fake_certificate.signed_message.as_bytes(),
                        )
                        .unwrap();
                    fake_certificate.signature =
                        CertificateSignature::MultiSignature(multi_signature.into());
                }
            }
            fake_certificate
        })
        .collect::<Vec<Certificate>>();
    let mut certificates_new: Vec<Certificate> = Vec::new();
    certificates
        .iter()
        .enumerate()
        .for_each(|(i, certificate)| {
            let mut certificate_new = certificate.clone();
            if i > 0 {
                if let Some(previous_certificate) = certificates_new.get(i - 1) {
                    certificate_new.previous_hash = previous_certificate.compute_hash();
                }
            }
            certificate_new.hash = certificate_new.compute_hash();
            certificates_new.push(certificate_new);
        });
    certificates_new.reverse();
    (certificates_new, genesis_verifier)
}
