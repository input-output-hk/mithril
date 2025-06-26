use anyhow::{anyhow, Context};
use mithril_stm::StmParameters;

use crate::{
    crypto_helper::{
        ProtocolAggregateVerificationKey, ProtocolAggregationError, ProtocolClerk,
        ProtocolMultiSignature,
    },
    entities::SingleSignature,
    protocol::ToMessage,
    StdResult,
};

/// MultiSigner is the cryptographic engine in charge of producing multi-signatures from individual signatures
pub struct MultiSigner {
    protocol_clerk: ProtocolClerk,
    protocol_parameters: StmParameters,
}

impl MultiSigner {
    pub(super) fn new(protocol_clerk: ProtocolClerk, protocol_parameters: StmParameters) -> Self {
        Self {
            protocol_clerk,
            protocol_parameters,
        }
    }

    /// Aggregate the given single signatures into a multi-signature
    pub fn aggregate_single_signatures<T: ToMessage>(
        &self,
        single_signatures: &[SingleSignature],
        message: &T,
    ) -> Result<ProtocolMultiSignature, ProtocolAggregationError> {
        let protocol_signatures: Vec<_> = single_signatures
            .iter()
            .map(|single_signature| single_signature.to_protocol_signature())
            .collect();

        self.protocol_clerk
            .aggregate(&protocol_signatures, message.to_message().as_bytes())
            .map(|multi_sig| multi_sig.into())
    }

    /// Compute aggregate verification key from stake distribution
    pub fn compute_aggregate_verification_key(&self) -> ProtocolAggregateVerificationKey {
        self.protocol_clerk.compute_avk().into()
    }

    /// Verify a single signature
    pub fn verify_single_signature<T: ToMessage>(
        &self,
        message: &T,
        single_signature: &SingleSignature,
    ) -> StdResult<()> {
        let protocol_signature = single_signature.to_protocol_signature();

        let avk = self.compute_aggregate_verification_key();

        // If there is no reg_party, then we simply received a signature from a non-registered
        // party, and we can ignore the request.
        let (vk, stake) = self
            .protocol_clerk
            .get_reg_party(&protocol_signature.signer_index)
            .ok_or_else(|| {
                anyhow!(format!(
                    "Unregistered party: '{}'",
                    single_signature.party_id
                ))
            })?;

        protocol_signature
            .verify(
                &self.protocol_parameters,
                &vk,
                &stake,
                &avk,
                message.to_message().as_bytes(),
            )
            .with_context(|| {
                format!(
                    "Invalid signature for party: '{}'",
                    single_signature.party_id
                )
            })?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use mithril_stm::StmSignatureError;

    use crate::{
        entities::{ProtocolMessage, ProtocolMessagePartKey, ProtocolParameters},
        protocol::SignerBuilder,
        test_utils::fake_keys,
        test_utils::{MithrilFixture, MithrilFixtureBuilder, StakeDistributionGenerationMethod},
    };

    use super::*;

    fn build_multi_signer(fixture: &MithrilFixture) -> MultiSigner {
        SignerBuilder::new(
            &fixture.signers_with_stake(),
            &fixture.protocol_parameters(),
        )
        .unwrap()
        .build_multi_signer()
    }

    #[test]
    fn cant_aggregate_if_signatures_list_empty() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let multi_signer = build_multi_signer(&fixture);
        let message = ProtocolMessage::default();

        let error = multi_signer.aggregate_single_signatures(&[], &message).expect_err(
            "Multi-signature should not be created with an empty single signatures list",
        );

        assert!(
            matches!(error, ProtocolAggregationError::NotEnoughSignatures(_, _)),
            "Expected ProtocolAggregationError::NotEnoughSignatures, got: {error:?}"
        )
    }

    #[test]
    fn can_aggregate_if_valid_signatures_and_quorum_reached() {
        let fixture = MithrilFixtureBuilder::default().with_signers(10).build();
        let multi_signer = build_multi_signer(&fixture);
        let message = ProtocolMessage::default();
        let signatures: Vec<SingleSignature> = fixture
            .signers_fixture()
            .iter()
            .map(|s| s.sign(&message).unwrap())
            .collect();

        multi_signer
            .aggregate_single_signatures(&signatures, &message)
            .expect("Multi-signature should be created");
    }

    #[test]
    fn can_aggregate_even_with_one_invalid_signature_if_the_other_are_enough_for_the_quorum() {
        let fixture = MithrilFixtureBuilder::default()
            .with_signers(10)
            .with_stake_distribution(StakeDistributionGenerationMethod::Uniform(20))
            .with_protocol_parameters(ProtocolParameters::new(6, 200, 1.0))
            .build();
        let multi_signer = build_multi_signer(&fixture);
        let message = ProtocolMessage::default();
        let mut signatures: Vec<SingleSignature> = fixture
            .signers_fixture()
            .iter()
            .map(|s| s.sign(&message).unwrap())
            .collect();
        signatures[4].signature = fake_keys::single_signature()[3].try_into().unwrap();

        multi_signer
            .aggregate_single_signatures(&signatures, &message)
            .expect("Multi-signature should be created even with one invalid signature");
    }

    #[test]
    fn verify_single_signature_fail_if_signature_signer_isnt_in_the_registered_parties() {
        let multi_signer = build_multi_signer(
            &MithrilFixtureBuilder::default()
                .with_signers(1)
                .with_stake_distribution(StakeDistributionGenerationMethod::RandomDistribution {
                    seed: [3u8; 32],
                    min_stake: 1,
                })
                .build(),
        );
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let message = ProtocolMessage::default();
        let single_signature = fixture.signers_fixture().last().unwrap().sign(&message).unwrap();

        // Will fail because the single signature was issued by a signer from a stake distribution
        // that is not the one used by the multi-signer.
        let error = multi_signer
            .verify_single_signature(&message, &single_signature)
            .expect_err(
                "Verify single signature should fail if the signer isn't in the registered parties",
            );

        match error.downcast_ref::<StmSignatureError>() {
            Some(StmSignatureError::SignatureInvalid(_)) => (),
            _ => panic!("Expected an SignatureInvalid error, got: {error:?}"),
        }
    }

    #[test]
    fn verify_single_signature_fail_if_signature_signed_message_isnt_the_given_one() {
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let multi_signer = build_multi_signer(&fixture);
        let mut signed_message = ProtocolMessage::default();
        signed_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "a_digest".to_string(),
        );
        let single_signature = fixture
            .signers_fixture()
            .first()
            .unwrap()
            .sign(&signed_message)
            .unwrap();

        let error = multi_signer
            .verify_single_signature(&ProtocolMessage::default(), &single_signature)
            .expect_err("Verify single signature should fail");

        match error.downcast_ref::<StmSignatureError>() {
            Some(StmSignatureError::SignatureInvalid(_)) => (),
            _ => panic!("Expected an SignatureInvalid error, got: {error:?}"),
        }
    }

    #[test]
    fn can_verify_valid_single_signature() {
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let multi_signer = build_multi_signer(&fixture);
        let message = ProtocolMessage::default();
        let single_signature = fixture.signers_fixture().first().unwrap().sign(&message).unwrap();

        multi_signer
            .verify_single_signature(&message, &single_signature)
            .expect("Verify single signature should succeed");
    }
}
