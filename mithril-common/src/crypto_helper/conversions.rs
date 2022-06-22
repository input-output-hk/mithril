use super::super::entities;
use super::types;

impl From<types::ProtocolParameters> for entities::ProtocolParameters {
    fn from(other: types::ProtocolParameters) -> Self {
        entities::ProtocolParameters::new(other.k, other.m, other.phi_f as f32)
    }
}

impl From<entities::ProtocolParameters> for types::ProtocolParameters {
    fn from(other: entities::ProtocolParameters) -> Self {
        types::ProtocolParameters {
            k: other.k,
            m: other.m,
            phi_f: other.phi_f as f64,
        }
    }
}

impl From<entities::SignerWithStake> for entities::Signer {
    fn from(other: entities::SignerWithStake) -> Self {
        entities::Signer::new(other.party_id, other.verification_key)
    }
}

impl From<entities::SignerWithStake> for (types::ProtocolPartyId, types::ProtocolStake) {
    fn from(other: entities::SignerWithStake) -> Self {
        (
            other.party_id as types::ProtocolPartyId,
            other.stake as types::ProtocolStake,
        )
    }
}

impl From<(types::ProtocolPartyId, types::ProtocolStake)> for entities::SignerWithStake {
    fn from(other: (types::ProtocolPartyId, types::ProtocolStake)) -> Self {
        entities::SignerWithStake::new(other.0, "".to_string(), other.1)
    }
}

#[cfg(test)]
pub mod tests {
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
            protocol_parameters_expected.phi_f as f32,
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
        let signer_with_stake_expected =
            entities::SignerWithStake::new("1".to_string(), "".to_string(), 100);
        ();

        let signer_with_stake_expected_into: (types::ProtocolPartyId, types::ProtocolStake) =
            signer_with_stake_expected.clone().into();
        assert_eq!(stake_expected, signer_with_stake_expected_into);

        let stake_expected_from = stake_expected.into();
        assert_eq!(signer_with_stake_expected, stake_expected_from);
    }

    #[test]
    fn test_stake_signers_from_into() {
        let signer_expected = entities::Signer::new("1".to_string(), "123456".to_string());
        ();
        let signer_with_stake =
            entities::SignerWithStake::new("1".to_string(), "123456".to_string(), 100);
        ();

        let signer_into: entities::Signer = signer_with_stake.clone().into();
        assert_eq!(signer_expected, signer_into);
    }
}
