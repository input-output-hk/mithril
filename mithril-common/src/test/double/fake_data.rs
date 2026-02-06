//! Fake data builders for testing.

use chrono::{DateTime, Utc};
use semver::Version;

use crate::CardanoNetwork;
use crate::crypto_helper::{self, KesPeriod, ProtocolMultiSignature};
use crate::entities::{
    self, AncillaryLocations, BlockNumber, CardanoDatabaseSnapshotArtifactData,
    CertificateMetadata, CertificateSignature, CompressionAlgorithm, DigestsLocations, Epoch,
    ImmutablesLocations, LotteryIndex, ProtocolMessage, ProtocolMessagePartKey, SignedEntityType,
    SingleSignature, SlotNumber, StakeDistribution, StakeDistributionParty,
};
use crate::test::{builder::MithrilFixtureBuilder, double::Dummy};

use super::fake_keys;

/// Fake network
pub fn network() -> CardanoNetwork {
    CardanoNetwork::TestNet(CardanoNetwork::DEVNET_MAGIC_ID)
}

/// Fake Beacon
pub fn beacon() -> entities::CardanoDbBeacon {
    let time_point = entities::TimePoint::dummy();
    entities::CardanoDbBeacon::new(*time_point.epoch, time_point.immutable_file_number)
}

/// Fake Epoch
pub fn epoch() -> Epoch {
    let time_point = entities::TimePoint::dummy();
    time_point.epoch
}

/// Fake ChainPoint
pub fn chain_point() -> entities::ChainPoint {
    entities::ChainPoint {
        slot_number: SlotNumber(500),
        block_number: BlockNumber(42),
        block_hash: "1b69b3202fbe500".to_string(),
    }
}

/// Fake ProtocolParameters
pub fn protocol_parameters() -> entities::ProtocolParameters {
    let k = 5;
    let m = 100;
    let phi_f = 0.65;
    entities::ProtocolParameters::new(k, m, phi_f)
}

/// Fake ProtocolInitializer
pub fn protocol_initializer<S: Into<String>>(
    seed: S,
    stake: entities::Stake,
) -> crypto_helper::ProtocolInitializer {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    let protocol_parameters = protocol_parameters();
    let seed: [u8; 32] = format!("{:<032}", seed.into()).as_bytes()[..32].try_into().unwrap();
    let mut rng = ChaCha20Rng::from_seed(seed);
    let kes_period = Some(KesPeriod(0));
    crypto_helper::ProtocolInitializer::setup(
        protocol_parameters.into(),
        None,
        kes_period,
        stake,
        &mut rng,
    )
    .unwrap()
}

/// Fake Genesis Certificate
pub fn genesis_certificate<T: Into<String>>(certificate_hash: T) -> entities::Certificate {
    let multi_signature = fake_keys::genesis_signature()[1].to_string();

    entities::Certificate {
        previous_hash: String::new(),
        signature: CertificateSignature::GenesisSignature(multi_signature.try_into().unwrap()),
        ..certificate(certificate_hash)
    }
}

/// Fake Certificate
pub fn certificate<T: Into<String>>(certificate_hash: T) -> entities::Certificate {
    let hash = certificate_hash.into();

    // Beacon
    let beacon = beacon();

    // Protocol parameters
    let protocol_parameters = protocol_parameters();

    // Signers with stakes
    let signers: Vec<StakeDistributionParty> =
        signers_with_stakes(5).into_iter().map(|s| s.into()).collect();

    // Certificate metadata
    let protocol_version = crypto_helper::PROTOCOL_VERSION.to_string();
    let initiated_at = DateTime::parse_from_rfc3339("2006-01-02T15:04:05Z")
        .unwrap()
        .with_timezone(&Utc);
    let sealed_at = DateTime::parse_from_rfc3339("2006-01-02T15:04:05Z")
        .unwrap()
        .with_timezone(&Utc);
    let metadata = CertificateMetadata::new(
        network(),
        protocol_version,
        protocol_parameters,
        initiated_at,
        sealed_at,
        signers,
    );

    // Protocol message
    let next_aggregate_verification_key =
        fake_keys::aggregate_verification_key_for_concatenation()[2].to_owned();
    let mut protocol_message = ProtocolMessage::new();
    let snapshot_digest = format!("{:0>20}", beacon.immutable_file_number);
    protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, snapshot_digest);
    protocol_message.set_message_part(
        ProtocolMessagePartKey::NextAggregateVerificationKey,
        next_aggregate_verification_key,
    );

    // Certificate
    let previous_hash = format!("{hash}0");
    let aggregate_verification_key = fake_keys::aggregate_verification_key_for_concatenation()[1]
        .try_into()
        .unwrap();
    let multi_signature: ProtocolMultiSignature =
        fake_keys::multi_signature()[0].try_into().unwrap();

    entities::Certificate {
        hash,
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

/// Fake SingleSignature
pub fn single_signature(won_indexes: Vec<LotteryIndex>) -> SingleSignature {
    let party_id = "party_id".to_string();
    let signature = fake_keys::single_signature()[0].try_into().unwrap();

    SingleSignature::new(party_id, signature, won_indexes)
}

/// Fake Snapshot
pub fn snapshot(snapshot_id: u64) -> entities::Snapshot {
    let digest = format!("snapshot-{snapshot_id:0>11}");
    let mut beacon = beacon();
    beacon.immutable_file_number += snapshot_id;
    let certificate_hash = "123".to_string();
    let size = snapshot_id * 100_000;
    let ancillary_size = snapshot_id * 10_000;
    let cardano_node_version = Version::parse("1.0.0").unwrap();
    let locations = vec![
        format!("http://{certificate_hash}"),
        format!("http2://{certificate_hash}"),
    ];
    let ancillary_locations = vec![
        format!("http://ancillary-{certificate_hash}"),
        format!("http2://ancillary-{certificate_hash}"),
    ];

    entities::Snapshot {
        digest,
        network: network().into(),
        beacon,
        size,
        ancillary_size: Some(ancillary_size),
        locations,
        ancillary_locations: Some(ancillary_locations),
        compression_algorithm: CompressionAlgorithm::Gzip,
        cardano_node_version: cardano_node_version.to_string(),
    }
}

/// Fake Snapshots list
pub fn snapshots(total: u64) -> Vec<entities::Snapshot> {
    (1..total + 1).map(snapshot).collect()
}

/// Fake Mithril Stake Distribution
pub fn mithril_stake_distribution(
    epoch: Epoch,
    signers_with_stake: Vec<entities::SignerWithStake>,
) -> entities::MithrilStakeDistribution {
    entities::MithrilStakeDistribution {
        epoch,
        signers_with_stake,
        hash: format!("msd-{epoch}"),
        protocol_parameters: protocol_parameters(),
    }
}

/// Fake Mithril Stake Distribution list
pub fn mithril_stake_distributions(total: u64) -> Vec<entities::MithrilStakeDistribution> {
    let signers = signers_with_stakes(5);

    (1..total + 1)
        .map(|epoch_idx| mithril_stake_distribution(Epoch(epoch_idx), signers.clone()))
        .collect()
}

/// Fake Cardano Transaction
pub fn cardano_transactions_snapshot(
    block_number: BlockNumber,
) -> entities::CardanoTransactionsSnapshot {
    entities::CardanoTransactionsSnapshot::new(format!("merkleroot-{block_number}"), block_number)
}

/// Fake Cardano Transactions list
pub fn cardano_transactions_snapshots(total: u64) -> Vec<entities::CardanoTransactionsSnapshot> {
    (1..total + 1)
        .map(|idx| cardano_transactions_snapshot(BlockNumber(idx)))
        .collect()
}

/// Fake Cardano Blocks Transactions
pub fn cardano_blocks_transactions_snapshot(
    block_number_signed: BlockNumber,
    offset_security_parameter: BlockNumber,
) -> entities::CardanoBlocksTransactionsSnapshot {
    entities::CardanoBlocksTransactionsSnapshot::new(
        format!("merkleroot-{block_number_signed}"),
        block_number_signed,
        offset_security_parameter,
    )
}

/// Fake Cardano Blocks Transactions list
pub fn cardano_blocks_transactions_snapshots(
    total: u64,
) -> Vec<entities::CardanoBlocksTransactionsSnapshot> {
    (1..total + 1)
        .map(|idx| cardano_blocks_transactions_snapshot(BlockNumber(idx), BlockNumber(15)))
        .collect()
}

/// Fake Cardano Stake Distribution
pub fn cardano_stake_distribution(epoch: Epoch) -> entities::CardanoStakeDistribution {
    let stake_distribution = StakeDistribution::from([("pool-1".to_string(), 100)]);
    entities::CardanoStakeDistribution {
        hash: format!("csd-{epoch}"),
        epoch,
        stake_distribution,
    }
}

/// Fake Cardano Stake Distributions list
pub fn cardano_stake_distributions(total: u64) -> Vec<entities::CardanoStakeDistribution> {
    (1..total + 1)
        .map(|epoch_idx| cardano_stake_distribution(Epoch(epoch_idx)))
        .collect()
}

/// Fake Cardano Database snapshots
pub fn cardano_database_snapshot(
    immutable_file_number: entities::ImmutableFileNumber,
) -> entities::CardanoDatabaseSnapshot {
    let merkle_root = format!("cdb-{immutable_file_number:0>16}");
    let mut beacon = beacon();
    beacon.immutable_file_number += immutable_file_number;
    let total_db_size_uncompressed = immutable_file_number * 100000;
    let cardano_node_version = Version::parse("1.0.0").unwrap();

    entities::CardanoDatabaseSnapshot::new(
        merkle_root,
        CardanoNetwork::TestNet(63),
        beacon,
        CardanoDatabaseSnapshotArtifactData {
            total_db_size_uncompressed,
            digests: DigestsLocations::default(),
            immutables: ImmutablesLocations::default(),
            ancillary: AncillaryLocations::default(),
        },
        &cardano_node_version,
    )
}

/// Fake Cardano Database snapshots list
pub fn cardano_database_snapshots(total: u64) -> Vec<entities::CardanoDatabaseSnapshot> {
    (1..total + 1).map(cardano_database_snapshot).collect()
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
