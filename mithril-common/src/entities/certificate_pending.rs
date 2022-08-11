use crate::entities::{Beacon, PartyId, ProtocolParameters, Signer};
use serde::{Deserialize, Serialize};

/// CertificatePending represents a pending certificate in the process of production
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct CertificatePending {
    /// Current Beacon
    #[serde(rename = "beacon")]
    pub beacon: Beacon,

    /// Current Protocol parameters
    #[serde(rename = "protocol")]
    pub protocol_parameters: ProtocolParameters,

    /// Next Protocol parameters
    #[serde(rename = "next_protocol")]
    pub next_protocol_parameters: ProtocolParameters,

    /// Current Signers
    #[serde(rename = "signers")]
    pub signers: Vec<Signer>,

    /// Signers that will be able to sign on the next epoch
    #[serde(rename = "next_signers")]
    pub next_signers: Vec<Signer>,
}

impl CertificatePending {
    /// CertificatePending factory
    pub fn new(
        beacon: Beacon,
        protocol_parameters: ProtocolParameters,
        next_protocol_parameters: ProtocolParameters,
        signers: Vec<Signer>,
        next_signers: Vec<Signer>,
    ) -> CertificatePending {
        CertificatePending {
            beacon,
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
    use crate::fake_data;

    #[test]
    fn certificate_pending_get_signers() {
        let certificate_pending = fake_data::certificate_pending();
        assert!(certificate_pending.get_signer("1".to_string()).is_some());
        assert!(certificate_pending.get_signer("5".to_string()).is_none());
    }
}
