use chrono::{DateTime, Utc};
use either::Either;
use mithril_common::entities::{
    Beacon, CertificatePending, HexEncodedAgregateVerificationKey, ProtocolMessage,
    ProtocolMessageThales, ProtocolParameters, SignerWithStake,
};

/// Gather information needed to create a new [Certificate].
#[derive(Clone, Debug, PartialEq)]
pub struct WorkingCertificate {
    /// Current Beacon
    pub beacon: Beacon,

    /// Current Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Current Signers
    pub signers: Vec<SignerWithStake>,

    /// Message that is currently signed
    pub message: Either<ProtocolMessage, ProtocolMessageThales>,

    /// Created aggregate verification key
    pub aggregate_verification_key: HexEncodedAgregateVerificationKey,

    /// Signing start datetime of current message
    pub initiated_at: DateTime<Utc>,

    /// Hash of the first certificate of the previous epoch
    pub previous_hash: String,
}

impl WorkingCertificate {
    /// Create a [WorkingCertificate] using what it can copy from a given [PendingCertificate]
    pub fn from_pending_certificate(
        pending_certificate: &CertificatePending,
        signers: &[SignerWithStake],
        protocol_message: &Either<ProtocolMessage, ProtocolMessageThales>,
        aggregate_verification_key: &str,
        initiated_at: &DateTime<Utc>,
        previous_hash: &str,
    ) -> Self {
        Self {
            beacon: pending_certificate.beacon.clone(),
            protocol_parameters: pending_certificate.protocol_parameters.clone(),
            signers: signers.to_vec(),
            message: protocol_message.clone(),
            aggregate_verification_key: aggregate_verification_key.to_string(),
            initiated_at: *initiated_at,
            previous_hash: previous_hash.to_string(),
        }
    }

    #[cfg(test)]
    pub fn fake() -> Self {
        use mithril_common::test_utils::fake_data;

        Self {
            beacon: fake_data::beacon(),
            protocol_parameters: fake_data::protocol_parameters(),
            signers: fake_data::signers_with_stakes(3),
            message: Either::Left(ProtocolMessage::new()),
            aggregate_verification_key: "avk".to_string(),
            initiated_at: Utc::now(),
            previous_hash: "hash".to_string(),
        }
    }
}
