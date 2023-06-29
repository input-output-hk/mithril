use anyhow::{anyhow, Context, Result};
use thiserror::Error;

use crate::{
    crypto_helper::{
        key_decode_hex, OpCert, ProtocolClosedKeyRegistration, ProtocolKeyRegistration,
        ProtocolSignerVerificationKey, ProtocolSignerVerificationKeySignature,
        ProtocolStakeDistribution,
    },
    entities::{ProtocolParameters, SignerWithStake},
};

/// Allow to build Single Or Multi signers to generate a single signature or aggregate them
#[derive(Debug)]
pub struct SignerBuilder {
    protocol_parameters: ProtocolParameters,
    closed_key_registration: ProtocolClosedKeyRegistration,
}

/// [SignerBuilder] specific errors
#[derive(Debug, Error)]
pub enum SignerBuilderError {
    /// Error raised when the list of signers given to the builder is empty
    #[error("The list of signers must not be empty to create a signer builder.")]
    EmptySigners,
}

impl SignerBuilder {
    /// [SignerBuilder] constructor.
    pub fn new(
        registered_signers: &[SignerWithStake],
        protocol_parameters: &ProtocolParameters,
    ) -> Result<Self> {
        if registered_signers.is_empty() {
            return Err(SignerBuilderError::EmptySigners.into());
        }

        let stake_distribution = registered_signers
            .iter()
            .map(|s| s.into())
            .collect::<ProtocolStakeDistribution>();
        let mut key_registration = ProtocolKeyRegistration::init(&stake_distribution);

        for signer in registered_signers {
            let signer_keys =
                SignerCryptographicKeys::decode_from_signer(signer).with_context(|| {
                    format!(
                        "Invalid signers in stake distribution: '{}'",
                        signer.party_id
                    )
                })?;
            key_registration
                .register(
                    Some(signer.party_id.to_owned()),
                    signer_keys.operational_certificate,
                    signer_keys.kes_signature,
                    signer_keys.kes_period,
                    signer_keys.verification_key,
                )
                .with_context(|| {
                    format!("Registration failed for signer: '{}'", signer.party_id)
                })?;
        }

        let closed_registration = key_registration.close();
        Ok(Self {
            protocol_parameters: protocol_parameters.clone(),
            closed_key_registration: closed_registration,
        })
    }
}

struct SignerCryptographicKeys {
    pub operational_certificate: Option<OpCert>,
    pub verification_key: ProtocolSignerVerificationKey,
    pub kes_signature: Option<ProtocolSignerVerificationKeySignature>,
    pub kes_period: Option<u32>,
}

impl SignerCryptographicKeys {
    fn decode_from_signer(signer: &SignerWithStake) -> Result<Self> {
        let operational_certificate = match &signer.operational_certificate {
            Some(operational_certificate) => key_decode_hex(operational_certificate)
                .map_err(|e| anyhow!(e))
                .with_context(|| "Could not decode operational certificate".to_string())?,
            _ => None,
        };
        let verification_key = key_decode_hex(&signer.verification_key)
            .map_err(|e| anyhow!(e))
            .with_context(|| "Could not decode verification key".to_string())?;
        let kes_signature = match &signer.verification_key_signature {
            Some(verification_key_signature) => Some(
                key_decode_hex(verification_key_signature)
                    .map_err(|e| anyhow!(e))
                    .with_context(|| "Could not decode verification key signature".to_string())?,
            ),
            _ => None,
        };

        Ok(Self {
            operational_certificate,
            verification_key,
            kes_signature,
            kes_period: signer.kes_period,
        })
    }
}

#[cfg(test)]
mod test {
    use crate::test_utils::{fake_data, MithrilFixtureBuilder};

    use super::*;

    #[test]
    fn cant_construct_signer_builder_with_an_empty_signers_list() {
        let signers = vec![];
        let protocol_parameters = fake_data::protocol_parameters();

        let error = SignerBuilder::new(&signers, &protocol_parameters).expect_err(
            "We should not be able to construct a signer builder with an empty signers list",
        );

        match error.downcast_ref::<SignerBuilderError>() {
            Some(SignerBuilderError::EmptySigners) => (),
            None => panic!("Expected an EmptySigners error, got: {error}"),
        }
    }

    #[test]
    fn cant_construct_signer_builder_with_a_signer_with_invalid_keys() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let mut signers = fixture.signers_with_stake();
        signers[2].operational_certificate = Some("invalid".to_string());

        let error = SignerBuilder::new(&signers, &fixture.protocol_parameters()).expect_err(
            "We should not be able to construct a signer builder with a signer with invalid keys",
        );

        assert!(
            error
                .to_string()
                .contains("Invalid signers in stake distribution"),
            "Expected Invalid signers error, got: {}, cause: {}",
            error,
            error.root_cause()
        );
    }

    #[test]
    fn cant_construct_signer_builder_if_a_signer_registration_fail() {
        // To make this test fail we try to build a SignerBuilder with signers from two
        // different stake distributions, this will pass the individual check but not the
        // register check.
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let fixture_with_another_stake_distribution = MithrilFixtureBuilder::default()
            .with_signers(1)
            .with_stake_distribution(
                crate::test_utils::StakeDistributionGenerationMethod::RandomDistribution {
                    seed: [4u8; 32],
                },
            )
            .build();
        let mut signers = fixture.signers_with_stake();
        signers.append(&mut fixture_with_another_stake_distribution.signers_with_stake());

        let error = SignerBuilder::new(&signers, &fixture.protocol_parameters()).expect_err(
            "We should not be able to construct a signer builder if a signer registration fail",
        );

        assert!(
            error.to_string().contains("Registration failed for signer"),
            "Expected Registration error, got: {}, cause: {}",
            error,
            error.root_cause()
        );
    }

    #[test]
    fn can_construct_signer_builder_with_valid_signers() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();

        SignerBuilder::new(
            &fixture.signers_with_stake(),
            &fixture.protocol_parameters(),
        )
        .expect("We should be able to construct a signer builder with valid signers");
    }
}
