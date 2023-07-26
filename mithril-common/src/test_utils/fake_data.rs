//! Fake data builders for testing.

use chrono::{DateTime, Utc};

use crate::{
    crypto_helper, entities,
    entities::{
        CertificateMetadata, Epoch, LotteryIndex, ProtocolMessage, ProtocolMessagePartKey,
        SignedEntityType, SingleSignatures,
    },
    test_utils::MithrilFixtureBuilder,
};

/// Fake Beacon
pub fn beacon() -> entities::Beacon {
    let network = "testnet".to_string();
    let immutable_file_number = 100;
    let epoch = 10;
    entities::Beacon::new(network, epoch, immutable_file_number)
}

/// Fake Digest
pub fn digest(beacon: &entities::Beacon) -> Vec<u8> {
    format!(
        "digest-{}-{}-{}",
        beacon.network, beacon.epoch, beacon.immutable_file_number
    )
    .as_bytes()
    .to_vec()
}

/// Fake ProtocolParameters
pub fn protocol_parameters() -> entities::ProtocolParameters {
    let k = 5;
    let m = 100;
    let phi_f = 0.65;
    entities::ProtocolParameters::new(k, m, phi_f)
}

/// Fake EpochSettings
pub fn epoch_settings() -> entities::EpochSettings {
    // Beacon
    let beacon = beacon();

    // Protocol parameters
    let protocol_parameters = protocol_parameters();
    let next_protocol_parameters = protocol_parameters.clone();

    // Epoch settings
    entities::EpochSettings {
        epoch: beacon.epoch,
        protocol_parameters,
        next_protocol_parameters,
    }
}

/// Fake CertificatePending
pub fn certificate_pending() -> entities::CertificatePending {
    // Beacon
    let beacon = beacon();

    // Signed entity type
    let signed_entity_type = SignedEntityType::dummy();

    // Protocol parameters
    let next_protocol_parameters = protocol_parameters();
    let protocol_parameters = protocol_parameters();

    // Signers
    let signers = signers(5);
    let current_signers = signers[1..3].to_vec();
    let next_signers = signers[2..5].to_vec();

    // Certificate pending
    entities::CertificatePending::new(
        beacon,
        signed_entity_type,
        protocol_parameters,
        next_protocol_parameters,
        current_signers,
        next_signers,
    )
}

/// Fake Genesis Certificate
pub fn genesis_certificate(certificate_hash: &str) -> entities::Certificate {
    let mut certificate = certificate(certificate_hash.to_string());
    certificate.previous_hash = String::new();
    certificate.genesis_signature = certificate.multi_signature;
    certificate.multi_signature = String::new();

    certificate
}

/// Fake Certificate
pub fn certificate(certificate_hash: String) -> entities::Certificate {
    // Beacon
    let beacon = beacon();

    // Protocol parameters
    let protocol_parameters = protocol_parameters();

    // Signers with stakes
    let signers = signers_with_stakes(5);

    // Certificate metadata
    let protocol_version = crypto_helper::PROTOCOL_VERSION.to_string();
    let initiated_at = DateTime::parse_from_rfc3339("2006-01-02T15:04:05Z")
        .unwrap()
        .with_timezone(&Utc);
    let sealed_at = DateTime::parse_from_rfc3339("2006-01-02T15:04:05Z")
        .unwrap()
        .with_timezone(&Utc);
    let metadata = CertificateMetadata::new(
        protocol_version,
        protocol_parameters,
        initiated_at,
        sealed_at,
        signers,
    );

    // Protocol message
    let next_aggregate_verification_key = "next-avk-123".to_string();
    let mut protocol_message = ProtocolMessage::new();
    let snapshot_digest = format!("1{}", beacon.immutable_file_number).repeat(20);
    protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, snapshot_digest);
    protocol_message.set_message_part(
        ProtocolMessagePartKey::NextAggregateVerificationKey,
        next_aggregate_verification_key,
    );

    // Certificate
    let previous_hash = format!("{certificate_hash}0");
    let aggregate_verification_key = format!("AVK{}", beacon.immutable_file_number).repeat(5);
    let multi_signature = format!("MSIG{}", beacon.immutable_file_number).repeat(200);
    let genesis_signature = String::new();
    let mut certificate = entities::Certificate::new(
        previous_hash,
        beacon,
        metadata,
        protocol_message,
        aggregate_verification_key,
        multi_signature,
        genesis_signature,
    );
    certificate.hash = certificate_hash;
    certificate
}

/// Fake SignersWithStake
pub fn signers_with_stakes(total: usize) -> Vec<entities::SignerWithStake> {
    MithrilFixtureBuilder::default()
        .with_signers(total)
        .build()
        .signers_with_stake()
}

/// Fake Signers
pub fn signers(total: usize) -> Vec<entities::Signer> {
    signers_with_stakes(total)
        .into_iter()
        .map(|signer| signer.into())
        .collect::<Vec<entities::Signer>>()
}

/// Fake SingleSignatures
pub fn single_signatures(won_indexes: Vec<LotteryIndex>) -> SingleSignatures {
    let party_id = "party_id".to_string();
    let signature = "7b227369676d61223a5b3133302c3137372c31352c3232392c32342c3235312c3234372c3137312c3139362c3231302c3134332c3131332c38362c3138392c39322c35362c3131322c33332c3139332c3231322c35342c3231342c32382c3231362c3232372c3137332c3130302c3132372c3137382c34302c39382c38372c32392c3138312c3235352c3131312c3135372c3232342c3233352c34362c3130302c3136392c3233322c3138392c3235322c38322c3133392c33365d2c22696e6465786573223a5b302c312c332c342c362c382c392c31302c31312c31322c31342c31382c32312c32322c32332c32352c32362c32372c33302c33332c33342c33382c34312c34332c35302c35382c35392c36302c36312c36322c36372c36392c37312c37332c37352c37362c37372c38312c38322c38332c38342c39302c39312c39322c39332c39372c39385d2c227369676e65725f696e646578223a327d"
        .to_string()
        .try_into()
        .unwrap();

    SingleSignatures::new(party_id, signature, won_indexes)
}

/// Fake Snapshots
pub fn snapshots(total: u64) -> Vec<entities::Snapshot> {
    (1..total + 1)
        .map(|snapshot_id| {
            let digest = format!("1{snapshot_id}").repeat(20);
            let beacon = beacon();
            let certificate_hash = "123".to_string();
            let size = snapshot_id * 100000;
            let mut locations = Vec::new();
            locations.push(format!("http://{certificate_hash}"));
            locations.push(format!("http2://{certificate_hash}"));
            entities::Snapshot::new(digest, beacon, size, locations)
        })
        .collect::<Vec<entities::Snapshot>>()
}

/// Fake Mithril Stake Distribution
pub fn mithril_stake_distributions(total: u64) -> Vec<entities::MithrilStakeDistribution> {
    let signers = signers_with_stakes(5);

    (1..total + 1)
        .map(|epoch_idx| entities::MithrilStakeDistribution {
            epoch: Epoch(epoch_idx),
            signers_with_stake: signers.clone(),
            hash: format!("hash-epoch-{epoch_idx}"),
            protocol_parameters: self::protocol_parameters(),
        })
        .collect::<Vec<entities::MithrilStakeDistribution>>()
}
