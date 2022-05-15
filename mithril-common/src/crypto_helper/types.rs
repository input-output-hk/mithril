use mithril::key_reg::KeyReg;
use mithril::stm::{
    Index, PartyId, Stake, StmClerk, StmInitializer, StmMultiSig, StmParameters, StmSig, StmSigner,
    StmVerificationKey,
};

use super::super::entities;

pub type Bytes = Vec<u8>;

// Protocol types alias
type D = blake2::Blake2b;
pub type ProtocolPartyId = PartyId;
pub type ProtocolStake = Stake;
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;
pub type ProtocolParameters = StmParameters;
pub type ProtocolLotteryIndex = Index;
pub type ProtocolSigner = StmSigner<D>;
pub type ProtocolInitializer = StmInitializer;
pub type ProtocolClerk = StmClerk<D>;
pub type ProtocolKeyRegistration = KeyReg;
pub type ProtocolSingleSignature = StmSig<D>;
pub type ProtocolMultiSignature = StmMultiSig<D>;
pub type ProtocolSignerVerificationKey = StmVerificationKey;

impl From<ProtocolParameters> for entities::ProtocolParameters {
    fn from(other: ProtocolParameters) -> Self {
        entities::ProtocolParameters::new(other.k, other.m, other.phi_f as f32)
    }
}

impl From<entities::ProtocolParameters> for ProtocolParameters {
    fn from(other: entities::ProtocolParameters) -> Self {
        ProtocolParameters {
            k: other.k,
            m: other.m,
            phi_f: other.phi_f as f64,
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_protocol_parameters_from_into() {
        let protocol_parameters_expected = ProtocolParameters {
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

        let protocol_initializer_from: ProtocolParameters =
            protocol_initializer_entities_expected.into();
        assert_eq!(protocol_parameters_expected, protocol_initializer_from);
    }
}
