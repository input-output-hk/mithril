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
    let signature = "7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a332c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d".to_string();
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
