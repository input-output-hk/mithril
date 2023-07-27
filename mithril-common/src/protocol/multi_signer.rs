use anyhow::{anyhow, Context, Result};
use mithril_stm::stm::StmParameters;

use crate::{
    crypto_helper::{
        ProtocolAggregateVerificationKey, ProtocolAggregationError, ProtocolClerk,
        ProtocolMultiSignature,
    },
    entities::{ProtocolMessage, SingleSignatures},
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
    pub fn aggregate_single_signatures(
        &self,
        single_signatures: &[SingleSignatures],
        protocol_message: &ProtocolMessage,
    ) -> Result<ProtocolMultiSignature, ProtocolAggregationError> {
        let protocol_signatures: Vec<_> = single_signatures
            .iter()
            .map(|single_signature| single_signature.to_protocol_signature())
            .collect();

        self.protocol_clerk.aggregate(
            &protocol_signatures,
            protocol_message.compute_hash().as_bytes(),
        )
    }

    /// Compute aggregate verification key from stake distribution
    pub fn compute_aggregate_verification_key(&self) -> ProtocolAggregateVerificationKey {
        self.protocol_clerk.compute_avk()
    }

    /// Verify a single signature
    pub fn verify_single_signature(
        &self,
        message: &ProtocolMessage,
        single_signature: &SingleSignatures,
    ) -> Result<()> {
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
                message.compute_hash().as_bytes(),
            )
            .map_err(|e| anyhow!(e))
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
    use crate::{
        entities::{ProtocolMessagePartKey, ProtocolParameters},
        protocol::SignerBuilder,
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

        let error = multi_signer
            .aggregate_single_signatures(&[], &message)
            .expect_err(
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
        let signatures: Vec<SingleSignatures> = fixture
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
        let mut signatures: Vec<SingleSignatures> = fixture
            .signers_fixture()
            .iter()
            .map(|s| s.sign(&message).unwrap())
            .collect();
        signatures[4].signature = "7b227369676d61223a5b3133302c32382c3134332c31372c38302c31302c3231352c3138382c3230352c3132322c31312c3233392c34362c3234352c32312c3139332c32382c3232312c3133302c34302c3131362c39322c3139362c33352c3235342c34332c3138382c362c38372c3136392c37312c3134352c3130342c3137382c392c3136362c39342c31332c3234372c3139302c3130322c37312c3232362c3230392c312c3230392c3235312c3137305d2c22696e6465786573223a5b302c312c382c31322c31332c31342c31382c31392c32332c32352c32362c32372c32382c33322c33332c33342c33352c33372c33382c33392c34312c34322c34332c34342c34352c34362c34372c34382c34392c35302c35312c35322c35332c35342c35352c35362c35372c35382c35392c36302c36312c36332c36342c36352c36372c36382c37302c37312c37352c37362c37372c37392c38302c38312c38322c38342c38352c38392c39302c39312c39332c39352c39372c39382c39395d2c227369676e65725f696e646578223a337d"
            .to_string()
            .try_into()
            .unwrap();

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
                })
                .build(),
        );
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let message = ProtocolMessage::default();
        let single_signature = fixture
            .signers_fixture()
            .last()
            .unwrap()
            .sign(&message)
            .unwrap();

        // Will fail because the single signature was issued by a signer from a stake distribution
        // that is not the one used by the multi-signer.
        let error = multi_signer
            .verify_single_signature(&message, &single_signature)
            .expect_err(
                "Verify single signature should fail if the signer isn't in the registered parties",
            );

        assert!(
            error.to_string().contains("Invalid signature for party"),
            "Expected invalid signature of party error, got: {error}, cause: {}",
            error.root_cause()
        )
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

        assert!(
            error.to_string().contains("Invalid signature for party"),
            "Expected invalid signature of party error, got: {error}, cause: {}",
            error.root_cause()
        )
    }

    #[test]
    fn can_verify_valid_single_signature() {
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let multi_signer = build_multi_signer(&fixture);
        let message = ProtocolMessage::default();
        let single_signature = fixture
            .signers_fixture()
            .first()
            .unwrap()
            .sign(&message)
            .unwrap();

        multi_signer
            .verify_single_signature(&message, &single_signature)
            .expect("Verify single signature should succeed");
    }
}
