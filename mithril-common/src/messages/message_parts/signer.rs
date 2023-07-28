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
            verification_key_signature: Some(fake_keys::signer_verification_key_signature()[0].to_string()),
            operational_certificate: Some("5b5b5b3230332c3130392c34302c32382c3235312c39342c35322c32342c3231322c3131362c3134392c38302c3138332c3136322c312c36322c352c3133332c35372c3230342c31352c3137322c3134372c38362c3132352c35392c31322c3235332c3130312c3138342c32332c31355d2c322c3132382c5b3133382c3131302c3139322c35302c38362c332c3136382c33342c3137322c31392c39312c3133392c3139302c3134302c31382c3137372c33312c34362c3132322c3130362c3233342c3137372c3130382c3232352c3230372c342c302c35392c3233372c3133352c3130342c39382c3133332c3133312c32392c3231322c3137312c3139342c3234342c3139312c3137392c3131392c34322c37352c3135302c36312c3232362c3132312c35342c3232332c3139332c3133382c3139302c32372c3138322c3135322c35362c32312c3136302c3230372c33352c3233372c3130322c31325d5d2c5b3230372c31322c3136382c3139302c34362c3131362c3139362c3133332c3139362c3233312c3132342c3235302c3134372c33372c3137352c3231312c3234372c3139382c3134302c3133392c3234362c3130342c3132342c3232372c34392c352c3235332c3232382c3130372c39332c3133362c3134345d5d".to_string()),
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
