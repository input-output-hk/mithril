use super::types::*;

use hex::FromHex;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

pub fn setup_message() -> Bytes {
    Vec::from_hex("7724e03fb8d84a376a43b8f41518a11c").unwrap()
}

pub fn setup_protocol_parameters() -> ProtocolParameters {
    ProtocolParameters {
        m: 10,
        k: 5,
        phi_f: 0.65,
    }
}

pub fn setup_signers(
    total: u64,
) -> Vec<(
    ProtocolPartyId,
    ProtocolStake,
    ProtocolSignerVerificationKey,
    ProtocolSigner,
)> {
    let seed = [0u8; 32];
    let mut rng = ChaCha20Rng::from_seed(seed);
    let protocol_parameters = setup_protocol_parameters();
    let signers = (0..total)
        .into_iter()
        .map(|party_id| {
            let stake = 1 + rng.next_u64() % 999;
            let protocol_initializer: ProtocolInitializer = ProtocolInitializer::setup(
                protocol_parameters,
                party_id as ProtocolPartyId,
                stake,
                &mut rng,
            );
            (
                party_id as ProtocolPartyId,
                stake as ProtocolStake,
                protocol_initializer,
            )
        })
        .collect::<Vec<(ProtocolPartyId, ProtocolStake, ProtocolInitializer)>>();

    let mut key_registration = ProtocolKeyRegistration::new(
        &signers
            .iter()
            .map(|(party_id, stake, _)| (*party_id, *stake))
            .collect::<Vec<_>>(),
    );
    signers
        .iter()
        .for_each(|(party_id, _, protocol_initializer)| {
            key_registration
                .register(*party_id, protocol_initializer.verification_key())
                .expect("key registration should have succeeded");
        });
    let closed_key_registration = key_registration.close();
    signers
        .into_iter()
        .map(|(party_id, stake, protocol_initializer)| {
            (
                party_id,
                stake,
                protocol_initializer.verification_key(),
                protocol_initializer.new_signer(closed_key_registration.clone()),
            )
        })
        .collect::<_>()
}
