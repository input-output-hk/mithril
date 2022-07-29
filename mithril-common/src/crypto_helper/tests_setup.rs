//! Fake data builders for Mithril Core types, for testing purpose.

use super::types::*;
use crate::entities::{ProtocolMessage, ProtocolMessagePartKey};

use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

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

/// Instantiate a [ProtocolParameters] using fake data, use this for tests only.
pub fn setup_protocol_parameters() -> ProtocolParameters {
    ProtocolParameters {
        m: 10,
        k: 5,
        phi_f: 0.65,
    }
}

/// Instantiate a list of signers using fake data, use this for tests only.
pub fn setup_signers(
    total: u64,
) -> Vec<(
    ProtocolPartyId,
    ProtocolStake,
    ProtocolSignerVerificationKey,
    ProtocolSigner,
    ProtocolInitializer,
)> {
    let protocol_parameters = setup_protocol_parameters();
    let signers = (0..total)
        .into_iter()
        .map(|party_id| {
            let seed = [0u8; 32];
            let mut rng = ChaCha20Rng::from_seed(seed);
            let stake = 1 + rng.next_u64() % 999;
            let party_id = format!("{:<032}", party_id);
            let seed: [u8; 32] = party_id.as_bytes()[..32].try_into().unwrap();
            let mut rng = ChaCha20Rng::from_seed(seed);
            let protocol_initializer: ProtocolInitializer =
                ProtocolInitializer::setup(protocol_parameters, stake, &mut rng);
            (
                party_id as ProtocolPartyId,
                stake as ProtocolStake,
                protocol_initializer,
            )
        })
        .collect::<Vec<(ProtocolPartyId, ProtocolStake, ProtocolInitializer)>>();

    let mut key_registration = ProtocolKeyRegistration::init();
    signers.iter().for_each(|(_, stake, protocol_initializer)| {
        key_registration
            .register(*stake, protocol_initializer.verification_key())
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
                protocol_initializer
                    .clone()
                    .new_signer(closed_key_registration.clone()),
                protocol_initializer,
            )
        })
        .collect::<_>()
}
