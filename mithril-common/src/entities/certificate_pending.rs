use serde::{Deserialize, Serialize};

use crate::entities::{Epoch, ProtocolParameters, SignedEntityType, Signer};

/// CertificatePending represents a pending certificate in the process of production
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CertificatePending {
    /// Current Epoch
    pub epoch: Epoch,

    /// Signed entity type
    #[serde(rename = "entity_type")]
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
        epoch: Epoch,
        signed_entity_type: SignedEntityType,
        protocol_parameters: ProtocolParameters,
        next_protocol_parameters: ProtocolParameters,
        signers: Vec<Signer>,
        next_signers: Vec<Signer>,
    ) -> CertificatePending {
        CertificatePending {
            epoch,
            signed_entity_type,
            protocol_parameters,
            next_protocol_parameters,
            signers,
            next_signers,
        }
    }
}
