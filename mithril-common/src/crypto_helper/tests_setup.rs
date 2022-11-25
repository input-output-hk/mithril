//! Test data builders for Mithril STM types, for testing purpose.
use super::cardano::ColdKeyGenerator;
use super::{genesis::*, key_encode_hex, types::*, OpCert, SerDeShelleyFileFormat};
use crate::certificate_chain::CertificateGenesisProducer;
use crate::{
    entities::{Certificate, Epoch, ProtocolMessage, ProtocolMessagePartKey, SignerWithStake},
    fake_data,
};

use kes_summed_ed25519::kes::Sum6Kes;
use kes_summed_ed25519::traits::KesSk;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
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

/// Instantiate a list of protocol signers, use this for tests only.
pub fn setup_signers(
    total: u64,
    protocol_parameters: &ProtocolParameters,
) -> Vec<(SignerWithStake, ProtocolSigner, ProtocolInitializer)> {
    let mut stake_rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut kes_keys_seed = [0u8; 32];

    let stake_distribution = (0..total)
        .into_iter()
        .map(|party_idx| {
            let party_id = if party_idx % 2 == 0
                || cfg!(not(feature = "allow_uncertified_signer_registration"))
            {
                // 50% of signers with key certification if allow unverified signer registration
                // Or 100% of signers otherwise
                let keypair = ColdKeyGenerator::create_deterministic_keypair([party_idx as u8; 32]);
                let (kes_secret_key, kes_verification_key) = Sum6Kes::keygen(&mut kes_keys_seed);
                let operational_certificate = OpCert::new(kes_verification_key, 0, 0, keypair);
                let party_id = operational_certificate
                    .compute_protocol_party_id()
                    .expect("compute protocol party id should not fail");
                let temp_dir = setup_temp_directory_for_signer(&party_id, true)
                    .expect("setup temp directory should return a value");
                if !temp_dir.join("kes.sk").exists() {
                    kes_secret_key
                        .to_file(temp_dir.join("kes.sk"))
                        .expect("KES secret key file export should not fail");
                }
                if !temp_dir.join("opcert.cert").exists() {
                    operational_certificate
                        .to_file(temp_dir.join("opcert.cert"))
                        .expect("operational certificate file export should not fail");
                }
                party_id
            } else {
                // 50% of signers without key certification (legacy) if allow unverified signer registration
                // Or 0% of signers otherwise
                // TODO: Should be removed once the signer certification is fully deployed
                format!("{:<032}", party_idx)
            };

            let stake = 1 + stake_rng.next_u64() % 999;
            (party_id, stake)
        })
        .collect::<Vec<_>>();
    setup_signers_from_stake_distribution(&stake_distribution, protocol_parameters)
}

/// Instantiate a list of protocol signers based on the given [ProtocolStakeDistribution] and [ProtocolParameters], use this for tests only.
pub fn setup_signers_from_stake_distribution(
    stake_distribution: &ProtocolStakeDistribution,
    protocol_parameters: &ProtocolParameters,
) -> Vec<(SignerWithStake, ProtocolSigner, ProtocolInitializer)> {
    let signers = stake_distribution
        .iter()
        .map(|(party_id, stake)| {
            let temp_dir = setup_temp_directory_for_signer(party_id, false);
            let protocol_initializer_seed: [u8; 32] = party_id.as_bytes()[..32].try_into().unwrap();
            let mut protocol_initializer_rng = ChaCha20Rng::from_seed(protocol_initializer_seed);
            let kes_secret_key_path: Option<PathBuf> =
                temp_dir.as_ref().map(|dir| dir.join("kes.sk"));
            let kes_period = temp_dir.as_ref().map(|_| 0);
            let protocol_initializer: ProtocolInitializer = ProtocolInitializer::setup(
                *protocol_parameters,
                kes_secret_key_path,
                kes_period,
                *stake,
                &mut protocol_initializer_rng,
            )
            .expect("protocol initializer setup should not fail");
            (
                party_id.clone() as ProtocolPartyId,
                *stake as ProtocolStake,
                protocol_initializer,
            )
        })
        .collect::<Vec<_>>();

    let mut key_registration = ProtocolKeyRegistration::init(stake_distribution);
    signers
        .iter()
        .for_each(|(party_id, _stake, protocol_initializer)| {
            let temp_dir = setup_temp_directory_for_signer(party_id, false);
            let operational_certificate = temp_dir.as_ref().map(|dir| {
                OpCert::from_file(dir.join("opcert.cert"))
                    .expect("operational certificate decoding should not fail")
            });
            let verification_key = protocol_initializer.verification_key();
            let kes_signature = protocol_initializer.verification_key_signature();
            let kes_period = 0;
            key_registration
                .register(
                    Some(party_id.to_owned()),
                    operational_certificate,
                    kes_signature,
                    Some(kes_period),
                    verification_key,
                )
                .expect("key registration should have succeeded");
        });
    let closed_key_registration = key_registration.close();
    signers
        .into_iter()
        .map(|(party_id, stake, protocol_initializer)| {
            let temp_dir = setup_temp_directory_for_signer(&party_id, false);
            let operational_certificate: Option<OpCert> = temp_dir.as_ref().map(|dir| {
                OpCert::from_file(dir.join("opcert.cert"))
                    .expect("operational certificate decoding should not fail")
            });
            let kes_period = 0;
            (
                SignerWithStake::new(
                    party_id,
                    key_encode_hex(protocol_initializer.verification_key())
                        .expect("key_encode_hex of verification_key should not fail"),
                    protocol_initializer
                        .verification_key_signature()
                        .as_ref()
                        .map(|verification_key_signature| {
                            key_encode_hex(verification_key_signature).expect(
                                "key_encode_hex of verification_key_signature should not fail",
                            )
                        }),
                    operational_certificate
                        .as_ref()
                        .map(|operational_certificate| {
                            key_encode_hex(operational_certificate)
                                .expect("key_encode_hex of operational_certificate should not fail")
                        }),
                    operational_certificate.as_ref().map(|_| kes_period),
                    stake,
                ),
                protocol_initializer
                    .clone()
                    .new_signer(closed_key_registration.clone())
                    .expect("creating a new protocol signer should not fail"),
                protocol_initializer,
            )
        })
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
        .into_iter()
        .map(|i| match certificates_per_epoch {
            0 => panic!("expected at least 1 certificate per epoch"),
            1 => Epoch(i),
            _ => Epoch(i / certificates_per_epoch + 1),
        })
        .collect::<Vec<_>>();
    let signers_by_epoch = epochs
        .clone()
        .into_iter()
        .map(|epoch| {
            (
                epoch,
                setup_signers(min(2 + epoch.0, 5), &protocol_parameters),
            )
        })
        .collect::<HashMap<_, _>>();
    let clerk_for_signers = |signers: &[(_, ProtocolSigner, _)]| -> ProtocolClerk {
        let first_signer = &signers.first().unwrap().1;
        ProtocolClerk::from_signer(first_signer)
    };
    let avk_for_signers = |signers: &[(_, ProtocolSigner, _)]| -> ProtocolAggregateVerificationKey {
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
            let digest = format!("digest{}", i);
            let certificate_hash = format!("certificate_hash-{}", i);
            let signers = signers_by_epoch.get(&epoch).unwrap();
            let next_signers = signers_by_epoch.get(&(epoch + 1)).unwrap();
            let avk = avk_for_signers(signers);
            let next_avk = avk_for_signers(next_signers);
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
                        genesis_signature,
                    )
                    .unwrap()
                }
                _ => {
                    let mut single_signatures = Vec::new();
                    signers.iter().for_each(|(_, protocol_signer, _)| {
                        if let Some(signature) =
                            protocol_signer.sign(fake_certificate.signed_message.as_bytes())
                        {
                            single_signatures.push(signature);
                        }
                    });
                    let clerk = clerk_for_signers(signers);
                    let multi_signature = clerk
                        .aggregate(
                            &single_signatures,
                            fake_certificate.signed_message.as_bytes(),
                        )
                        .unwrap();
                    fake_certificate.multi_signature = key_encode_hex(&multi_signature).unwrap();
                    fake_certificate.genesis_signature = "".to_string()
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
