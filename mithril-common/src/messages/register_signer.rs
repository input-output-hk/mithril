use serde::{Deserialize, Serialize};

use crate::{
    crypto_helper::KESPeriod,
    entities::{
        HexEncodedOpCert, HexEncodedVerificationKey, HexEncodedVerificationKeySignature, PartyId,
    },
};

/// Register Signer Message
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct RegisterSignerMessage {
    /// The unique identifier of the signer
    // TODO: Should be removed once the signer certification is fully deployed
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    pub verification_key: HexEncodedVerificationKey,

    /// The encoded signer 'Mithril verification key' signature (signed by the
    /// Cardano node KES secret key).
    // TODO: Option should be removed once the signer certification is fully
    //       deployed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verification_key_signature: Option<HexEncodedVerificationKeySignature>,

    /// The encoded operational certificate of stake pool operator attached to
    /// the signer node.
    // TODO: Option should be removed once the signer certification is fully
    //       deployed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operational_certificate: Option<HexEncodedOpCert>,

    /// The KES period used to compute the verification key signature
    // TODO: This KES period should not be used as is and should probably be
    //       within an allowed range of KES periods for the epoch.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kes_period: Option<KESPeriod>,
}
