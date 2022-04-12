#![allow(dead_code)]

use crate::entities;

/// Fake Beacon
pub fn beacon() -> entities::Beacon {
    let network = "testnet".to_string();
    let epoch = 196;
    let block = 3443000;
    entities::Beacon::new(network, epoch, block)
}

/// Fake ProtocolParameters
pub fn protocol_parameters() -> entities::ProtocolParameters {
    let k = 5;
    let m = 100;
    let phi_f = 0.2;
    entities::ProtocolParameters::new(k, m, phi_f)
}

/// Fake CertificatePending
pub fn certificate_pending() -> entities::CertificatePending {
    // Beacon
    let beacon = beacon();

    // Protocol parameters
    let protocol_parameters = protocol_parameters();

    // Signers with stakes
    let signers = signers_with_stakes(5);

    // Certificate pending
    entities::CertificatePending::new(beacon, protocol_parameters, signers)
}

/// Fake Certificate
pub fn certificate(certificate_hash: String) -> entities::Certificate {
    // Beacon
    let beacon = beacon();

    // Protocol parameters
    let protocol_parameters = protocol_parameters();

    // Signers with stakes
    let signers = signers_with_stakes(5);

    // Certificate
    let previous_hash = format!("{}0", certificate_hash);
    let block = beacon.block;
    let digest = format!("1{}", block).repeat(20);
    let started_at = "2006-01-02T15:04:05Z".to_string();
    let completed_at = "2006-01-02T15:04:05Z".to_string();
    let multisignature = format!("ABC{}", block).repeat(200);
    entities::Certificate::new(
        certificate_hash,
        previous_hash,
        block,
        protocol_parameters,
        digest,
        started_at,
        completed_at,
        signers,
        multisignature,
    )
}

/// Fake SignersWithStake
pub fn signers_with_stakes(total: u64) -> Vec<entities::SignerWithStake> {
    (1..total + 1)
        .map(|signer_id| {
            let party_id = signer_id;
            let verification_key = format!("{}", signer_id).repeat(15);
            let stake = signer_id * 10;
            entities::SignerWithStake::new(party_id, verification_key, stake)
        })
        .collect::<Vec<entities::SignerWithStake>>()
}

/// Fake Signers
pub fn signers(total: u64) -> Vec<entities::Signer> {
    signers_with_stakes(total)
        .iter()
        .map(|signer| entities::Signer::new(signer.party_id, signer.verification_key.clone()))
        .collect::<Vec<entities::Signer>>()
}

/// Fake SingleSignatures
pub fn single_signatures(total: u64) -> Vec<entities::SingleSignature> {
    (1..total + 1)
        .map(|signature_id| {
            let party_id = signature_id;
            let index = signature_id + 100;
            let signature = format!("SIG{}", signature_id).repeat(15);
            entities::SingleSignature::new(party_id, index, signature)
        })
        .collect::<Vec<entities::SingleSignature>>()
}

/// Fake Snapshots
pub fn snapshots(total: u64) -> Vec<entities::Snapshot> {
    (1..total + 1)
        .map(|snapshot_id| {
            let digest = format!("1{}", snapshot_id).repeat(20);
            let certificate_hash = "123".to_string();
            let size = snapshot_id * 100000;
            let created_at = "2006-01-02T15:04:05Z".to_string();
            let mut locations = Vec::new();
            locations.push(format!("http://{}", certificate_hash));
            locations.push(format!("http2://{}", certificate_hash));
            entities::Snapshot::new(digest, certificate_hash, size, created_at, locations)
        })
        .collect::<Vec<entities::Snapshot>>()
}
