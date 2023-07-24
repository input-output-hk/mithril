use super::super::entities;
use super::types;
use crate::crypto_helper::{ProtocolPartyId, ProtocolStake};

impl From<types::ProtocolParameters> for entities::ProtocolParameters {
    fn from(other: types::ProtocolParameters) -> Self {
        entities::ProtocolParameters::new(other.k, other.m, other.phi_f)
    }
}

impl From<entities::ProtocolParameters> for types::ProtocolParameters {
    fn from(other: entities::ProtocolParameters) -> Self {
        types::ProtocolParameters {
            k: other.k,
            m: other.m,
            phi_f: other.phi_f,
        }
    }
}

impl From<&entities::SignerWithStake> for (types::ProtocolPartyId, types::ProtocolStake) {
    fn from(other: &entities::SignerWithStake) -> Self {
        (
            other.party_id.clone() as ProtocolPartyId,
            other.stake as ProtocolStake,
        )
    }
}

#[cfg(test)]
pub mod tests {

    use crate::test_utils::MithrilFixtureBuilder;

    use super::*;

    #[test]
    fn test_protocol_parameters_from_into() {
        let protocol_parameters_expected = types::ProtocolParameters {
            k: 100,
            m: 1000,
            phi_f: 1.0,
        };
        let protocol_initializer_entities_expected = entities::ProtocolParameters::new(
            protocol_parameters_expected.k,
            protocol_parameters_expected.m,
            protocol_parameters_expected.phi_f,
        );

        let protocol_initializer_entities_into: entities::ProtocolParameters =
            protocol_parameters_expected.into();
        assert_eq!(
            protocol_initializer_entities_expected,
            protocol_initializer_entities_into
        );

        let protocol_initializer_from: types::ProtocolParameters =
            protocol_initializer_entities_expected.into();
        assert_eq!(protocol_parameters_expected, protocol_initializer_from);
    }

    #[test]
    fn test_stake_distribution_from_into() {
        let stake_expected = (
            "1".to_string() as types::ProtocolPartyId,
            100 as types::ProtocolStake,
        );
        let verification_key = MithrilFixtureBuilder::default()
            .with_signers(1)
            .build()
            .signers_with_stake()[0]
            .verification_key
            .clone();
        let signer_with_stake_expected = &entities::SignerWithStake::new(
            "1".to_string(),
            verification_key,
            None,
            None,
            None,
            100,
        );

        let signer_with_stake_expected_into: (types::ProtocolPartyId, types::ProtocolStake) =
            signer_with_stake_expected.into();
        assert_eq!(stake_expected, signer_with_stake_expected_into);
    }
}
