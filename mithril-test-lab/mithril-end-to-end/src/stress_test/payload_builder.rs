use std::time::Duration;

use mithril_common::{
    digesters::{CardanoImmutableDigester, DummyImmutableDb, ImmutableDigester},
    entities::{
        Beacon, Epoch, ProtocolMessage, ProtocolMessagePartKey, ProtocolParameters,
        SignedEntityType, Signer, SingleSignatures,
    },
    messages::{RegisterSignatureMessage, RegisterSignerMessage},
    test_utils::{MithrilFixture, MithrilFixtureBuilder},
    StdResult,
};

/// Generate signer data
pub fn generate_signer_data(
    number_of_signers: usize,
    protocol_parameters: ProtocolParameters,
) -> MithrilFixture {
    MithrilFixtureBuilder::default()
        .with_signers(number_of_signers)
        .with_protocol_parameters(protocol_parameters)
        .build()
}

/// Generate signer registration message
pub fn generate_register_signer_message(
    signers: &[Signer],
    epoch: Epoch,
) -> Vec<RegisterSignerMessage> {
    signers
        .iter()
        .cloned()
        .map(|signer| RegisterSignerMessage {
            epoch: Some(epoch),
            party_id: signer.party_id,
            verification_key: signer.verification_key.to_json_hex().unwrap(),
            verification_key_signature: signer
                .verification_key_signature
                .map(|k| k.to_json_hex().unwrap()),
            operational_certificate: signer
                .operational_certificate
                .map(|o| o.to_json_hex().unwrap()),
            kes_period: signer.kes_period,
        })
        .collect::<Vec<_>>()
}

/// Generate register signature message
pub fn generate_register_signature_message(
    signatures: &[SingleSignatures],
    signed_entity_type: SignedEntityType,
) -> Vec<RegisterSignatureMessage> {
    signatures
        .iter()
        .map(|s| RegisterSignatureMessage {
            signed_entity_type: Some(signed_entity_type.clone()),
            party_id: s.party_id.clone(),
            signature: s.signature.clone().to_json_hex().unwrap(),
            won_indexes: s.won_indexes.clone(),
        })
        .collect::<Vec<_>>()
}

/// Precompute all signers single signatures for the given fixture
pub async fn precompute_mithril_stake_distribution_signatures(
    signers_fixture: &MithrilFixture,
    timeout: Duration,
) -> StdResult<Vec<SingleSignatures>> {
    spin_while_waiting!(
        {
            let signers_fixture = signers_fixture.clone();
            let signatures = tokio::task::spawn_blocking(move || -> Vec<SingleSignatures> {
                let mithril_stake_distribution_message = {
                    let mut message = ProtocolMessage::new();
                    message.set_message_part(
                    mithril_common::entities::ProtocolMessagePartKey::NextAggregateVerificationKey,
                    signers_fixture.compute_and_encode_avk(),
                );

                    message
                };

                signers_fixture.sign_all(&mithril_stake_distribution_message)
            })
            .await?;

            Ok(signatures)
        },
        timeout,
        format!("Precompute signatures for MithrilStakeDistribution signed entity"),
        format!("Precomputing signatures timeout after {timeout:?}")
    )
}

/// Compute all signers single signatures for the given fixture
pub async fn compute_immutable_files_signatures(
    immutable_db: &DummyImmutableDb,
    epoch: Epoch,
    signers_fixture: &MithrilFixture,
    timeout: Duration,
) -> StdResult<(Beacon, Vec<SingleSignatures>)> {
    spin_while_waiting!(
        {
            let beacon = Beacon::new(
                "devnet".to_string(),
                *epoch,
                // Minus one because the last immutable isn't "finished"
                immutable_db.last_immutable_number().unwrap() - 1,
            );
            let digester = CardanoImmutableDigester::new(None, slog_scope::logger());
            let digest = digester.compute_digest(&immutable_db.dir, &beacon).await?;
            let signers_fixture = signers_fixture.clone();

            let signatures = tokio::task::spawn_blocking(move || -> Vec<SingleSignatures> {
                let mithril_stake_distribution_message = {
                    let mut message = ProtocolMessage::new();
                    message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);
                    message.set_message_part(
                        ProtocolMessagePartKey::NextAggregateVerificationKey,
                        signers_fixture.compute_and_encode_avk(),
                    );

                    message
                };

                signers_fixture.sign_all(&mithril_stake_distribution_message)
            })
            .await?;

            Ok((beacon, signatures))
        },
        timeout,
        format!("Precompute signatures for CardanoImmutableFiles signed entity"),
        format!("Precomputing signatures timeout after {timeout:?}")
    )
}
