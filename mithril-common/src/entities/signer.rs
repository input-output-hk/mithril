use crate::{
    crypto_helper::{KESPeriod, ProtocolSignerVerificationKey},
    entities::{HexEncodedOpCert, HexEncodedVerificationKeySignature, PartyId, Stake},
};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Signer represents a signing participant in the network
#[derive(Clone, Debug, Eq, Serialize, Deserialize)]
pub struct Signer {
    /// The unique identifier of the signer
    // TODO: Should be removed once the signer certification is fully deployed
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    pub verification_key: ProtocolSignerVerificationKey,

    /// The encoded signer 'Mithril verification key' signature (signed by the Cardano node KES secret key)
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verification_key_signature: Option<HexEncodedVerificationKeySignature>,

    /// The encoded operational certificate of stake pool operator attached to the signer node
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operational_certificate: Option<HexEncodedOpCert>,

    /// The kes period used to compute the verification key signature
    // TODO: This kes period shoud not be used as is and should probably be within an allowed range of kes period for the epoch
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kes_period: Option<KESPeriod>,
}

impl PartialEq for Signer {
    fn eq(&self, other: &Self) -> bool {
        self.party_id.eq(&other.party_id)
    }
}

impl Signer {
    /// Signer factory
    pub fn new(
        party_id: PartyId,
        verification_key: ProtocolSignerVerificationKey,
        verification_key_signature: Option<HexEncodedVerificationKeySignature>,
        operational_certificate: Option<HexEncodedOpCert>,
        kes_period: Option<KESPeriod>,
    ) -> Signer {
        Signer {
            party_id,
            verification_key,
            verification_key_signature,
            operational_certificate,
            kes_period,
        }
    }

    /// Computes the hash of Signer
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.party_id.as_bytes());
        hasher.update(self.verification_key.to_json_hex().unwrap().as_bytes());

        if let Some(verification_key_signature) = &self.verification_key_signature {
            hasher.update(verification_key_signature.as_bytes());
        }
        if let Some(operational_certificate) = &self.operational_certificate {
            hasher.update(operational_certificate.as_bytes());
        }
        hex::encode(hasher.finalize())
    }
}

impl From<SignerWithStake> for Signer {
    fn from(other: SignerWithStake) -> Self {
        Signer::new(
            other.party_id,
            other.verification_key,
            other.verification_key_signature,
            other.operational_certificate,
            other.kes_period,
        )
    }
}

/// Signer represents a signing party in the network (including its stakes)
#[derive(Clone, Debug, Eq, Serialize, Deserialize)]
pub struct SignerWithStake {
    /// The unique identifier of the signer
    // TODO: Should be removed once the signer certification is fully deployed
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    pub verification_key: ProtocolSignerVerificationKey,

    /// The encoded signer 'Mithril verification key' signature (signed by the Cardano node KES secret key)
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verification_key_signature: Option<HexEncodedVerificationKeySignature>,

    /// The encoded operational certificate of stake pool operator attached to the signer node
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operational_certificate: Option<HexEncodedOpCert>,

    /// The kes period used to compute the verification key signature
    // TODO: This kes period shoud not be used as is and should probably be within an allowed range of kes period for the epoch
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kes_period: Option<KESPeriod>,

    /// The signer stake
    pub stake: Stake,
}

impl PartialEq for SignerWithStake {
    fn eq(&self, other: &Self) -> bool {
        self.party_id.eq(&other.party_id)
    }
}

impl PartialOrd for SignerWithStake {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.party_id.partial_cmp(&other.party_id)
    }
}

impl Ord for SignerWithStake {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.party_id.cmp(&other.party_id)
    }
}

impl SignerWithStake {
    /// SignerWithStake factory
    pub fn new(
        party_id: PartyId,
        verification_key: ProtocolSignerVerificationKey,
        verification_key_signature: Option<HexEncodedVerificationKeySignature>,
        operational_certificate: Option<HexEncodedOpCert>,
        kes_period: Option<KESPeriod>,
        stake: Stake,
    ) -> SignerWithStake {
        SignerWithStake {
            party_id,
            verification_key,
            verification_key_signature,
            operational_certificate,
            kes_period,
            stake,
        }
    }

    /// Turn a [Signer] into a [SignerWithStake].
    pub fn from_signer(signer: Signer, stake: Stake) -> Self {
        Self {
            party_id: signer.party_id,
            verification_key: signer.verification_key,
            verification_key_signature: signer.verification_key_signature,
            operational_certificate: signer.operational_certificate,
            kes_period: signer.kes_period,
            stake,
        }
    }

    /// Computes the hash of SignerWithStake
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.party_id.as_bytes());
        hasher.update(self.verification_key.to_json_hex().unwrap().as_bytes());

        if let Some(verification_key_signature) = &self.verification_key_signature {
            hasher.update(verification_key_signature.as_bytes());
        }

        if let Some(operational_certificate) = &self.operational_certificate {
            hasher.update(operational_certificate.as_bytes());
        }
        hasher.update(self.stake.to_be_bytes());
        hex::encode(hasher.finalize())
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::MithrilFixtureBuilder;

    use super::*;

    #[test]
    fn test_stake_signers_from_into() {
        let verification_key = MithrilFixtureBuilder::default()
            .with_signers(1)
            .build()
            .signers_with_stake()[0]
            .verification_key
            .clone();
        let signer_expected =
            Signer::new("1".to_string(), verification_key.clone(), None, None, None);
        let signer_with_stake =
            SignerWithStake::new("1".to_string(), verification_key, None, None, None, 100);

        let signer_into: Signer = signer_with_stake.into();
        assert_eq!(signer_expected, signer_into);
    }

    #[test]
    fn test_signer_compute_hash() {
        const HASH_EXPECTED: &str =
            "02778791113dcd8647b019366e223bfe3aa8a054fa6d9d1918b6b669de485f1c";

        assert_eq!(
            HASH_EXPECTED,
            Signer::new(
                "1".to_string(),
                "7b22766b223a5b3138332c37392c3133302c3132302c38342c3135322c33322c3134342c3138302c3130342c3139322c3134302c3133392c38352c32332c302c3137312c3132312c3136342c38382c31382c3136392c3233332c3137382c39322c342c392c3134302c3230332c37352c39362c3232352c31302c31382c33302c38322c3132312c3230302c36382c33362c34352c3132302c3231322c35332c3232322c3135332c31322c3138382c31312c3131332c3231352c31382c3132352c36302c3231302c3130302c35322c3138392c34372c31312c3135322c33382c35332c38362c32342c3231322c34322c3131302c35352c3139372c36322c3137362c33392c35322c39372c3137322c31322c322c3234302c31342c322c3131332c3138362c3131382c39372c37312c34342c3233342c3133392c3234302c362c3232392c3136372c3231312c38302c3136305d2c22706f70223a5b3135312c3138312c3231332c3137382c3235342c3137312c3235352c3234332c3131342c34352c34342c3137352c3234332c3137302c3135372c3133382c3139382c38362c31362c36322c31392c3132342c3131392c37332c3230302c35332c3231372c3230322c382c3139332c3134352c3132312c3231362c34362c3134392c3233382c3232332c3233342c35382c32362c322c3131342c32302c3131372c38332c3234382c3138332c36302c3134372c38322c3230352c3139332c3136322c34352c3136312c3231302c31312c342c3230302c37352c382c3137382c3135342c3235332c37312c3234302c3132362c38332c32322c33342c3231372c3138362c3232312c3131302c3138362c3133372c352c3139372c3231382c35352c3132352c3137372c3136302c36332c32342c39382c3134322c3232362c3133322c3231312c3230392c3136372c32372c3137322c37362c3137375d7d".try_into().unwrap(),
                None,
                None,
                None,
            )
            .compute_hash()
        );
        assert_ne!(
            HASH_EXPECTED,
            Signer::new(
                "0".to_string(),
                "7b22766b223a5b3138332c37392c3133302c3132302c38342c3135322c33322c3134342c3138302c3130342c3139322c3134302c3133392c38352c32332c302c3137312c3132312c3136342c38382c31382c3136392c3233332c3137382c39322c342c392c3134302c3230332c37352c39362c3232352c31302c31382c33302c38322c3132312c3230302c36382c33362c34352c3132302c3231322c35332c3232322c3135332c31322c3138382c31312c3131332c3231352c31382c3132352c36302c3231302c3130302c35322c3138392c34372c31312c3135322c33382c35332c38362c32342c3231322c34322c3131302c35352c3139372c36322c3137362c33392c35322c39372c3137322c31322c322c3234302c31342c322c3131332c3138362c3131382c39372c37312c34342c3233342c3133392c3234302c362c3232392c3136372c3231312c38302c3136305d2c22706f70223a5b3135312c3138312c3231332c3137382c3235342c3137312c3235352c3234332c3131342c34352c34342c3137352c3234332c3137302c3135372c3133382c3139382c38362c31362c36322c31392c3132342c3131392c37332c3230302c35332c3231372c3230322c382c3139332c3134352c3132312c3231362c34362c3134392c3233382c3232332c3233342c35382c32362c322c3131342c32302c3131372c38332c3234382c3138332c36302c3134372c38322c3230352c3139332c3136322c34352c3136312c3231302c31312c342c3230302c37352c382c3137382c3135342c3235332c37312c3234302c3132362c38332c32322c33342c3231372c3138362c3232312c3131302c3138362c3133372c352c3139372c3231382c35352c3132352c3137372c3136302c36332c32342c39382c3134322c3232362c3133322c3231312c3230392c3136372c32372c3137322c37362c3137375d7d".try_into().unwrap(),
                None,
                None,
                None
            )
            .compute_hash()
        );
        assert_ne!(
            HASH_EXPECTED,
            Signer::new(
                "1".to_string(),
                "7b22766b223a5b3136392c3139332c39392c3233362c3131302c36342c3132302c3232362c3138322c35312c3233392c39302c3133332c3134352c3137322c3132382c3137302c35302c3130352c35352c3130372c3230372c36302c3231362c3234302c3137342c3139302c3235322c3232342c31392c37332c3134302c32392c35322c3230312c32362c3135322c3131322c3135342c36352c3232332c392c3139342c372c36392c3134362c3135362c3138302c362c38332c3234302c3137362c3232372c3138302c31322c352c3230342c32392c3138362c3233352c36342c3232362c3234342c3138322c3137372c3135302c3130372c3132332c3138382c3234332c39322c3135372c3136332c3135352c3137362c3231332c32342c3130362c36392c34332c31392c3136302c3130382c31352c38312c3136322c3133392c3234322c3139332c3235312c3131372c39392c302c3135352c3139312c3132355d2c22706f70223a5b3134392c3134372c3135362c3137322c3132372c3230372c3231322c38362c3135362c322c33392c3131332c3139362c3235322c3233352c39382c3130302c3130342c31332c3137302c34312c3235302c3139352c36352c3232332c34382c3232302c3234332c38382c3136352c3139332c34332c34342c31362c35362c3230352c3132332c34392c3133382c38372c33352c32322c3132372c37322c32342c3133332c3136332c3139352c3138342c3232382c3136372c3131312c3132342c3138372c3132322c3231332c3134352c3136342c35352c34342c36322c37302c3130372c3132362c35352c35352c3233322c3230322c38362c39392c33392c34382c3135382c31322c3132342c32332c39382c3135382c3231352c3137372c35342c3233382c3131382c3134362c33302c39352c3235302c3133362c3136362c39352c38362c37372c3138332c3138342c37352c3133325d7d".try_into().unwrap(),
                None,
                None,
                None
            )
            .compute_hash()
        );
    }

    #[test]
    fn test_signer_with_stake_compute_hash() {
        const EXPECTED_HASH: &str =
            "9a832baccd04aabfc419f57319e3831a1655a95bf3bf5ed96a1167d1e81b5085";
        let signers = MithrilFixtureBuilder::default()
            .with_signers(2)
            .build()
            .signers_with_stake();
        let signer = signers[0].clone();

        assert_eq!(EXPECTED_HASH, signer.compute_hash());

        {
            let mut signer_different_party_id = signer.clone();
            signer_different_party_id.party_id = "whatever".to_string();

            assert_ne!(EXPECTED_HASH, signer_different_party_id.compute_hash());
        }
        {
            let mut signer_different_verification_key = signer.clone();
            signer_different_verification_key.verification_key =
                signers[1].verification_key.clone();

            assert_ne!(
                EXPECTED_HASH,
                signer_different_verification_key.compute_hash()
            );
        }
        {
            let mut signer_different_stake = signer.clone();
            signer_different_stake.stake += 20;

            assert_ne!(EXPECTED_HASH, signer_different_stake.compute_hash());
        }
    }
}
