use crate::{
    crypto_helper::KESPeriod,
    entities::{
        HexEncodedOpCert, HexEncodedVerificationKey, HexEncodedVerificationKeySignature, PartyId,
        SignerWithStake, Stake,
    },
    test_utils::fake_keys,
    StdResult,
};
use serde::{Deserialize, Serialize};

/// Signer Message
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SignerWithStakeMessagePart {
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

    /// The signer stake
    pub stake: Stake,
}

impl SignerWithStakeMessagePart {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            party_id: "pool1m8crhnqj5k2kyszf5j2scshupystyxc887zdfrpzh6ty6eun4fx".to_string(),
            verification_key: fake_keys::signer_verification_key()[0].to_string(),
            verification_key_signature: Some(
                fake_keys::signer_verification_key_signature()[0].to_string(),
            ),
            operational_certificate: Some(fake_keys::operational_certificate()[0].to_string()),
            kes_period: Some(6),
            stake: 234,
        }
    }

    /// Convert a set of signers into message parts
    pub fn from_signers(signers: Vec<SignerWithStake>) -> Vec<Self> {
        signers.into_iter().map(|signer| signer.into()).collect()
    }

    /// Convert a set of signer message parts into a set of signers with stake
    pub fn try_into_signers(messages: Vec<Self>) -> StdResult<Vec<SignerWithStake>> {
        let mut signers: Vec<SignerWithStake> = Vec::new();

        for message in messages {
            let value = SignerWithStake {
                party_id: message.party_id,
                verification_key: message.verification_key.try_into()?,
                verification_key_signature: message.verification_key_signature,
                kes_period: message.kes_period,
                operational_certificate: message.operational_certificate,
                stake: message.stake,
            };
            signers.push(value);
        }

        Ok(signers)
    }
}

impl From<SignerWithStake> for SignerWithStakeMessagePart {
    fn from(value: SignerWithStake) -> Self {
        Self {
            party_id: value.party_id,
            verification_key: value.verification_key.try_into().unwrap(),
            verification_key_signature: value.verification_key_signature,
            operational_certificate: value.operational_certificate,
            kes_period: value.kes_period,
            stake: value.stake,
        }
    }
}
