use crate::entities::{Beacon, PartyId, ProtocolParameters, Signer};
use serde::{Deserialize, Serialize};

use super::SignedEntityType;

/// CertificatePending represents a pending certificate in the process of production
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CertificatePending {
    /// Current Beacon
    pub beacon: Beacon,

    /// Signed entity type
    pub signed_entity_type: SignedEntityType,

    /// Current Protocol parameters
    #[serde(rename = "protocol")]
    pub protocol_parameters: ProtocolParameters,

    /// Next Protocol parameters
    #[serde(rename = "next_protocol")]
    pub next_protocol_parameters: ProtocolParameters,

    /// Current Signers
    pub signers: Vec<Signer>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<Signer>,
}

impl CertificatePending {
    /// CertificatePending factory
    pub fn new(
        beacon: Beacon,
        signed_entity_type: SignedEntityType,
        protocol_parameters: ProtocolParameters,
        next_protocol_parameters: ProtocolParameters,
        signers: Vec<Signer>,
        next_signers: Vec<Signer>,
    ) -> CertificatePending {
        CertificatePending {
            beacon,
            signed_entity_type,
            protocol_parameters,
            next_protocol_parameters,
            signers,
            next_signers,
        }
    }

    /// get a signer from the certificate pending if it has registered
    pub fn get_signer(&self, party_id: PartyId) -> Option<&Signer> {
        self.signers.iter().find(|s| s.party_id == party_id)
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::fake_data;

    #[test]
    fn certificate_pending_get_signers() {
        let certificate_pending = fake_data::certificate_pending();
        assert!(certificate_pending.get_signer("1".to_string()).is_some());
        assert!(certificate_pending.get_signer("5".to_string()).is_none());
    }
}
