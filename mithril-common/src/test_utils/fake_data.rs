//! Fake data builders for testing.

use chrono::{DateTime, Utc};
use semver::Version;

use crate::crypto_helper::ProtocolMultiSignature;
use crate::{
    crypto_helper,
    entities::{
        self, CertificateMetadata, CertificateSignature, CompressionAlgorithm, Epoch, LotteryIndex,
        ProtocolMessage, ProtocolMessagePartKey, SignedEntityType, SingleSignatures,
        StakeDistributionParty,
    },
    test_utils::MithrilFixtureBuilder,
};

use super::fake_keys;

/// Fake network
pub fn network() -> crate::CardanoNetwork {
    crate::CardanoNetwork::DevNet(10)
}

/// Fake Beacon
pub fn beacon() -> entities::CardanoDbBeacon {
    let network = network().to_string();
    let time_point = entities::TimePoint::dummy();
    entities::CardanoDbBeacon::new(network, *time_point.epoch, time_point.immutable_file_number)
}

/// Fake ChainPoint
pub fn chain_point() -> entities::ChainPoint {
    entities::ChainPoint {
        slot_number: 500,
        block_number: 42,
        block_hash: "1b69b3202fbe500".to_string(),
    }
}

/// Fake Digest
pub fn digest(beacon: &entities::CardanoDbBeacon) -> Vec<u8> {
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
    // Epoch
    let epoch = beacon().epoch;

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
        epoch,
        signed_entity_type,
        protocol_parameters,
        next_protocol_parameters,
        current_signers,
        next_signers,
    )
}

/// Fake Genesis Certificate
pub fn genesis_certificate(certificate_hash: &str) -> entities::Certificate {
    let multi_signature = fake_keys::genesis_signature()[1].to_string();

    entities::Certificate {
        previous_hash: String::new(),
        signature: CertificateSignature::GenesisSignature(multi_signature.try_into().unwrap()),
        ..certificate(certificate_hash.to_string())
    }
}

/// Fake Certificate
pub fn certificate(certificate_hash: String) -> entities::Certificate {
    // Beacon
    let beacon = beacon();

    // Protocol parameters
    let protocol_parameters = protocol_parameters();

    // Signers with stakes
    let signers: Vec<StakeDistributionParty> = signers_with_stakes(5)
        .into_iter()
        .map(|s| s.into())
        .collect();

    // Certificate metadata
    let protocol_version = crypto_helper::PROTOCOL_VERSION.to_string();
    let initiated_at = DateTime::parse_from_rfc3339("2006-01-02T15:04:05Z")
        .unwrap()
        .with_timezone(&Utc);
    let sealed_at = DateTime::parse_from_rfc3339("2006-01-02T15:04:05Z")
        .unwrap()
        .with_timezone(&Utc);
    let metadata = CertificateMetadata::new(
        &beacon.network,
        beacon.immutable_file_number,
        protocol_version,
        protocol_parameters,
        initiated_at,
        sealed_at,
        signers,
    );

    // Protocol message
    let next_aggregate_verification_key = fake_keys::aggregate_verification_key()[2].to_owned();
    let mut protocol_message = ProtocolMessage::new();
    let snapshot_digest = format!("1{}", beacon.immutable_file_number).repeat(20);
    protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, snapshot_digest);
    protocol_message.set_message_part(
        ProtocolMessagePartKey::NextAggregateVerificationKey,
        next_aggregate_verification_key,
    );

    // Certificate
    let previous_hash = format!("{certificate_hash}0");
    let aggregate_verification_key = fake_keys::aggregate_verification_key()[1]
        .try_into()
        .unwrap();
    let multi_signature: ProtocolMultiSignature =
        fake_keys::multi_signature()[0].try_into().unwrap();

    entities::Certificate {
        hash: certificate_hash,
        previous_hash,
        epoch: beacon.epoch,
        metadata,
        protocol_message,
        signed_message: "".to_string(),
        aggregate_verification_key,
        signature: CertificateSignature::MultiSignature(
            SignedEntityType::CardanoImmutableFilesFull(beacon),
            multi_signature,
        ),
    }
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
    let signature = fake_keys::single_signature()[0].try_into().unwrap();

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
            let cardano_node_version = Version::parse("1.0.0").unwrap();
            let mut locations = Vec::new();
            locations.push(format!("http://{certificate_hash}"));
            locations.push(format!("http2://{certificate_hash}"));

            entities::Snapshot::new(
                digest,
                beacon,
                size,
                locations,
                CompressionAlgorithm::Gzip,
                &cardano_node_version,
            )
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
            protocol_parameters: protocol_parameters(),
        })
        .collect::<Vec<entities::MithrilStakeDistribution>>()
}

/// Fake Cardano Transactions
pub fn cardano_transactions_snapshot(total: u64) -> Vec<entities::CardanoTransactionsSnapshot> {
    (1..total + 1)
        .map(|idx| entities::CardanoTransactionsSnapshot::new(format!("merkleroot-{idx}"), idx))
        .collect()
}

/// Fake transaction hashes that have valid length & characters
pub const fn transaction_hashes<'a>() -> [&'a str; 5] {
    [
        "c96809e2cecd9e27499a4379094c4e1f7b59d918c96327bd8daf1bf909dae332",
        "5b8788784af9c414f18fc1e6161005b13b839fd91130b7c109aeba1792feb843",
        "8b6ae44edf877ff2ac80cf067809d575ab2bad234b668f91e90decde837b154a",
        "3f6f3c981c89097f62c9b43632875db7a52183ad3061c822d98259d18cd63dcf",
        "f4fd91dccc25fd63f2caebab3d3452bc4b2944fcc11652214a3e8f1d32b09713",
    ]
}
