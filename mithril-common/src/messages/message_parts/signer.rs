use anyhow::Context;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Formatter};

use crate::{
    StdError, StdResult,
    crypto_helper::{KesPeriod, ProtocolOpCert, ProtocolSignerVerificationKeySignature},
    entities::{
        HexEncodedOpCert, HexEncodedVerificationKey, HexEncodedVerificationKeySignature, PartyId,
        Signer, SignerWithStake, Stake,
    },
};

/// Signer with Stake Message
#[derive(Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SignerWithStakeMessagePart {
    /// The unique identifier of the signer
    ///
    /// Used only for testing when SPO pool id is not certified
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    pub verification_key: HexEncodedVerificationKey,

    /// The encoded signer 'Mithril verification key' signature (signed by the
    /// Cardano node KES secret key).
    ///
    /// None is used only for testing when SPO pool id is not certified
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verification_key_signature: Option<HexEncodedVerificationKeySignature>,

    /// The encoded operational certificate of stake pool operator attached to
    /// the signer node.
    ///
    /// None is used only for testing when SPO pool id is not certified
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operational_certificate: Option<HexEncodedOpCert>,

    /// The KES period used to compute the verification key signature
    // TODO: This KES period should not be used as is and should probably be
    //       within an allowed range of KES periods for the epoch.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kes_period: Option<KesPeriod>,

    /// The signer stake
    pub stake: Stake,
}

impl SignerWithStakeMessagePart {
    /// Convert a set of signers into message parts
    pub fn from_signers(signers: Vec<SignerWithStake>) -> Vec<Self> {
        signers.into_iter().map(|signer| signer.into()).collect()
    }

    /// Convert a set of signer message parts into a set of signers with stake
    pub fn try_into_signers(messages: Vec<Self>) -> StdResult<Vec<SignerWithStake>> {
        messages
            .into_iter()
            .map(SignerWithStakeMessagePart::try_into)
            .collect()
    }
}

impl TryInto<SignerWithStake> for SignerWithStakeMessagePart {
    type Error = StdError;

    fn try_into(self) -> Result<SignerWithStake, Self::Error> {
        let verification_key_signature: Option<ProtocolSignerVerificationKeySignature> = self
            .verification_key_signature
            .map(|f| f.try_into())
            .transpose()
            .with_context(|| {
                format!(
                    "Error while parsing verification key signature message, party_id = '{}'",
                    self.party_id
                )
            })?;
        let operational_certificate: Option<ProtocolOpCert> = self
            .operational_certificate
            .map(|f| f.try_into())
            .transpose()
            .with_context(|| {
                format!(
                    "Error while parsing operational certificate message, party_id = '{}'.",
                    self.party_id
                )
            })?;
        let value = SignerWithStake {
            party_id: self.party_id,
            verification_key: self.verification_key.try_into()?,
            verification_key_signature,
            kes_period: self.kes_period,
            operational_certificate,
            stake: self.stake,
        };
        Ok(value)
    }
}

impl From<SignerWithStake> for SignerWithStakeMessagePart {
    fn from(value: SignerWithStake) -> Self {
        Self {
            party_id: value.party_id,
            verification_key: value.verification_key.try_into().unwrap(),
            verification_key_signature: value
                .verification_key_signature
                .map(|k| k.try_into().unwrap()),
            operational_certificate: value
                .operational_certificate
                .map(|op_cert| (op_cert.try_into().unwrap())),
            kes_period: value.kes_period,
            stake: value.stake,
        }
    }
}

impl Debug for SignerWithStakeMessagePart {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let should_be_exhaustive = f.alternate();
        let mut debug = f.debug_struct("Signer");
        debug.field("party_id", &self.party_id).field("stake", &self.stake);

        match should_be_exhaustive {
            true => debug
                .field(
                    "verification_key",
                    &format_args!("{:?}", self.verification_key),
                )
                .field(
                    "verification_key_signature",
                    &format_args!("{:?}", self.verification_key_signature),
                )
                .field(
                    "operational_certificate",
                    &format_args!("{:?}", self.operational_certificate),
                )
                .field("kes_period", &format_args!("{:?}", self.kes_period))
                .finish(),
            false => debug.finish_non_exhaustive(),
        }
    }
}

/// Signer Message
#[derive(Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SignerMessagePart {
    /// The unique identifier of the signer
    ///
    /// Used only for testing when SPO pool id is not certified
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    pub verification_key: HexEncodedVerificationKey,

    /// The encoded signer 'Mithril verification key' signature (signed by the
    /// Cardano node KES secret key).
    ///
    /// None is used only for testing when SPO pool id is not certified
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verification_key_signature: Option<HexEncodedVerificationKeySignature>,

    /// The encoded operational certificate of stake pool operator attached to
    /// the signer node.
    ///
    /// None is used only for testing when SPO pool id is not certified
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operational_certificate: Option<HexEncodedOpCert>,

    /// The KES period used to compute the verification key signature
    // TODO: This KES period should not be used as is and should probably be
    //       within an allowed range of KES periods for the epoch.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kes_period: Option<KesPeriod>,
}

impl SignerMessagePart {
    /// Convert a set of signer message parts into a set of signers
    pub fn try_into_signers(messages: Vec<Self>) -> StdResult<Vec<Signer>> {
        messages.into_iter().map(SignerMessagePart::try_into).collect()
    }

    /// Convert a set of signers into message parts
    pub fn from_signers(signers: Vec<Signer>) -> Vec<Self> {
        signers.into_iter().map(|signer| signer.into()).collect()
    }
}

impl TryInto<Signer> for SignerMessagePart {
    type Error = StdError;

    fn try_into(self) -> Result<Signer, Self::Error> {
        let verification_key_signature: Option<ProtocolSignerVerificationKeySignature> = self
            .verification_key_signature
            .map(|f| f.try_into())
            .transpose()
            .with_context(|| {
                format!(
                    "Error while parsing verification key signature message, party_id = '{}'",
                    self.party_id
                )
            })?;
        let operational_certificate: Option<ProtocolOpCert> = self
            .operational_certificate
            .map(|f| f.try_into())
            .transpose()
            .with_context(|| {
                format!(
                    "Error while parsing operational certificate message, party_id = '{}'.",
                    self.party_id
                )
            })?;
        let value = Signer {
            party_id: self.party_id,
            verification_key: self.verification_key.try_into()?,
            verification_key_signature,
            kes_period: self.kes_period,
            operational_certificate,
        };
        Ok(value)
    }
}

impl From<Signer> for SignerMessagePart {
    fn from(value: Signer) -> Self {
        Self {
            party_id: value.party_id,
            verification_key: value.verification_key.try_into().unwrap(),
            verification_key_signature: value
                .verification_key_signature
                .map(|k| k.try_into().unwrap()),
            operational_certificate: value
                .operational_certificate
                .map(|op_cert| (op_cert.try_into().unwrap())),
            kes_period: value.kes_period,
        }
    }
}

impl Debug for SignerMessagePart {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let should_be_exhaustive = f.alternate();
        let mut debug = f.debug_struct("Signer");
        debug.field("party_id", &self.party_id);

        match should_be_exhaustive {
            true => debug
                .field(
                    "verification_key",
                    &format_args!("{:?}", self.verification_key),
                )
                .field(
                    "verification_key_signature",
                    &format_args!("{:?}", self.verification_key_signature),
                )
                .field(
                    "operational_certificate",
                    &format_args!("{:?}", self.operational_certificate),
                )
                .field("kes_period", &format_args!("{:?}", self.kes_period))
                .finish(),
            false => debug.finish_non_exhaustive(),
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    mod golden_protocol_key_encodings {
        use super::*;

        fn golden_signer_message_part_with_json_hex_encoding() -> SignerMessagePart {
            SignerMessagePart {
                    party_id: "pool1m8crhnqj5k2kyszf5j2scshupystyxc887zdfrpzh6ty6eun4fx"
                        .to_string(),
                    verification_key: "7b22766b223a5b3138342c3134352c3230382c3138382c31342c342c3230342c3135392c33322c3234332c37352c3137392c38322c3133352c3235342c3135372c33312c35392c33382c3131302c3133362c3232352c3233342c3132332c34372c3130322c34322c3132352c3138392c31372c3136322c3234342c37352c3234382c3139352c3232372c3131362c3139322c3135322c39302c34312c32372c3235312c3137322c35332c3137382c3231342c35362c32302c3232372c3139372c31392c3234322c3138362c3130312c322c37332c3234332c31342c3230342c3136342c3133342c3136322c3233332c32392c3131342c33302c3136372c3230372c3137332c36382c362c37362c38302c3233342c36352c36342c3137332c3231372c3232392c34382c3133342c31322c39352c3138362c3233382c3135302c3139322c3138302c3139322c31302c312c3136322c3131372c3131322c3132325d2c22706f70223a5b3137382c3231342c3231342c3139332c3137382c3134372c34332c3132362c31362c3231362c3231352c3133322c3136342c3134362c382c3233382c3234352c38322c3232342c3233372c3234392c3134322c3135372c3232392c32372c3135392c3132312c3233312c3234382c3131312c38352c38352c35302c3139382c3233362c39312c3133302c3133352c36322c3133302c3132352c3134372c3136392c3134352c39352c33352c37382c3235332c3135302c3232352c3232352c3232372c38332c3132352c32382c3137332c3130382c3234312c3230302c37332c3134342c36322c31322c3232392c3134332c3134332c37352c37342c3133352c3135382c3139362c3139362c3232342c3232382c38382c3130352c3132342c34372c37362c3234382c3234342c38362c3136332c3232312c3134372c3134382c36352c3232382c31352c37392c3138352c39302c39362c3139322c3138392c3233365d7d".to_string(),
                    verification_key_signature: Some(
                        "7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a5b32312c3131392c302c3232382c3130322c3231362c3137372c3133302c34362c3131362c3132302c3235322c3232342c31322c34332c3139302c32322c3131372c3231362c3132342c3234302c3137332c31392c3234312c3138342c3230392c36342c3130352c39392c3233342c36312c31302c37312c36302c33392c3133392c39382c3133342c3133322c3135372c3133382c33372c3232322c3130362c3131312c37382c3132302c37382c39332c39382c32382c35332c3231322c3138332c3135322c37312c3135312c36332c3136382c3138372c33372c3232372c3133352c31345d2c226c68735f706b223a5b3135372c32312c33362c32392c32302c33342c31372c3233392c31332c38392c33372c3132302c32312c3135352c3233362c3234302c39352c3138392c3136362c3131332c3231332c39302c3138382c34312c3137312c33372c3131312c3138302c3234332c33312c35322c36385d2c227268735f706b223a5b3136302c3234332c39382c34352c342c3232312c36342c3131372c34382c3230332c3138362c34312c39352c31342c3135382c3232382c3137352c3232322c3131362c34342c38342c39372c3136372c3132372c38302c3139372c3234352c3136372c3139332c3139362c3135372c3137375d7d2c226c68735f706b223a5b3138332c3135352c36352c3234322c3233342c34332c3136372c3139322c31362c38322c3130322c39382c3133372c3139362c32382c38312c3232312c38312c36352c322c39332c37302c38342c3136382c3137392c3138372c3130322c32382c3234342c3138322c3132332c3134385d2c227268735f706b223a5b3135352c36392c3134302c32382c3137342c33382c3134392c36302c3137382c3232372c35342c3231312c3130362c3133312c36382c3135362c35392c3138302c35332c382c3138382c3233312c3137332c312c3137312c3131392c33322c3134372c3233342c34322c3136332c3131305d7d2c226c68735f706b223a5b3134312c3137322c3137372c38352c39302c3131312c3135332c3234362c3233332c3139362c3130352c3138342c35342c3234372c3133342c3135382c3232362c3230372c32392c3136332c31392c33342c312c3230372c3232322c37322c3136362c32332c31382c3137342c31362c39365d2c227268735f706b223a5b31322c3231322c3139372c3130382c3233302c38332c33372c3139342c3135382c3135362c38362c3138362c33322c3234352c3234342c34302c3234342c3138302c3136382c3233302c362c3137382c3138362c32322c36382c33392c3230322c35352c3130302c3232392c3138392c3232315d7d2c226c68735f706b223a5b33362c3235302c322c3232392c3233342c3136352c32372c36332c3132302c3137382c342c3138302c3235312c3134372c35302c3134382c3233372c3135342c33382c3134352c3131342c37342c3231312c34382c3133362c3231332c31382c3138322c3230332c3233302c31352c3133345d2c227268735f706b223a5b3139332c3134342c3231332c362c3235352c3137372c3131302c3131302c3231332c3137352c332c32382c3135382c3231362c3137332c32362c3232352c38392c34362c3133382c34392c37342c36332c34332c3134342c34382c34392c39352c35372c31392c3132392c34335d7d2c226c68735f706b223a5b3135362c38312c3130352c3134312c3230392c322c3137352c38332c38332c37342c3138332c3130312c3131362c3137362c31382c3233362c31382c3232332c3233372c3233332c3139332c35342c32382c3231332c302c3133352c3135372c33342c37352c3230352c34382c3133385d2c227268735f706b223a5b33382c3234382c35322c32302c35352c3131382c3138372c33392c3137362c3231332c33342c332c3231342c322c33312c3131382c3232342c3132392c34322c3136332c39342c3130382c3131362c35352c3231332c36372c3133322c39362c3232392c3132322c39362c3136385d7d2c226c68735f706b223a5b3132312c3134342c33312c3131302c3234392c3234342c3139382c3235342c3139312c36322c39312c31372c3135322c3135312c3233322c3130302c3130392c39362c3230322c3234392c3139382c3230342c39332c372c3131372c3233362c3132382c36362c38392c3231342c3133392c3134375d2c227268735f706b223a5b3234362c3136362c3230342c3135302c3139362c39312c3135382c3133312c3133382c3130332c3234352c34392c3134352c3133302c32322c3132362c3134372c39352c32332c39332c31332c3230392c3133312c34392c3138322c34362c3135332c35372c33372c3130332c3235332c3234325d7d".to_string(),
                    ),
                    operational_certificate: Some(
                        "5b5b5b35312c36322c392c3230302c3230392c34312c3234352c3230372c3135392c3139392c31342c372c38322c3230332c3234302c312c3132392c3138372c3131392c3232312c3133362c3234372c38392c3132382c3232382c3133332c302c39382c31322c3232382c3137382c3233345d2c31362c313139302c5b3231302c3134382c37332c3136332c3232322c3233332c3138302c33372c3133312c3235342c392c3230352c3135382c3134392c31342c37302c39322c372c3233352c3231342c3131312c35322c3131362c34312c3131382c362c3132392c312c3130362c312c39342c3233332c3131352c3137332c3130302c3133392c3131342c3130392c31352c31342c3233332c34332c3137392c3137342c35302c31302c3135302c39372c3132372c3138322c31362c372c3131322c3234352c34382c3134312c38342c3130322c342c32352c3231312c3134342c3230322c345d5d2c5b3133312c3135352c37322c35372c3134372c3231382c3137332c36382c3139312c3234322c3138392c3234372c32372c3235342c3134382c3232352c35332c31312c36392c3135372c3138322c38302c3233342c3133312c3233342c33392c3130322c32312c322c332c36352c3139305d5d".to_string(),
                    ),
                    kes_period: Some(6)
                }
        }

        fn golden_signer_message_part_with_bytes_hex_encoding() -> SignerMessagePart {
            SignerMessagePart {
                    party_id: "pool1m8crhnqj5k2kyszf5j2scshupystyxc887zdfrpzh6ty6eun4fx"
                        .to_string(),
                    verification_key: "b891d0bc0e04cc9f20f34bb35287fe9d1f3b266e88e1ea7b2f662a7dbd11a2f44bf8c3e374c0985a291bfbac35b2d63814e3c513f2ba650249f30ecca486a2e91d721ea7cfad44064c50ea4140add9e530860c5fbaee96c0b4c00a01a275707ab2d6d6c1b2932b7e10d8d784a49208eef552e0edf98e9de51b9f79e7f86f555532c6ec5b82873e827d93a9915f234efd96e1e1e3537d1cad6cf1c849903e0ce58f8f4b4a879ec4c4e0e458697c2f4cf8f456a3dd939441e40f4fb95a60c0bdec".to_string(),
                    verification_key_signature: Some(
                        "157700e466d8b1822e7478fce00c2bbe1675d87cf0ad13f1b8d1406963ea3d0a473c278b6286849d8a25de6a6f4e784e5d621c35d4b79847973fa8bb25e3870e9d15241d142211ef0d592578159becf05fbda671d55abc29ab256fb4f31f3444a0f3622d04dd407530cbba295f0e9ee4afde742c5461a77f50c5f5a7c1c49db1b79b41f2ea2ba7c01052666289c41c51dd5141025d4654a8b3bb661cf4b67b949b458c1cae26953cb2e336d36a83449c3bb43508bce7ad01ab772093ea2aa36e8dacb1555a6f99f6e9c469b836f7869ee2cf1da3132201cfde48a61712ae10600cd4c56ce65325c29e9c56ba20f5f428f4b4a8e606b2ba164427ca3764e5bddd24fa02e5eaa51b3f78b204b4fb933294ed9a2691724ad33088d512b6cbe60f86c190d506ffb16e6ed5af031c9ed8ad1ae1592e8a314a3f2b9030315f3913812b9c51698dd102af53534ab76574b012ec12dfede9c1361cd500879d224bcd308a26f834143776bb27b0d52203d6021f76e0812aa35e6c7437d5438460e57a60a879901f6ef9f4c6febf3e5b119897e8646d60caf9c6cc5d0775ec804259d68b93f6a6cc96c45b9e838a67f5319182167e935f175d0dd18331b62e99392567fdf2".to_string(),
                    ),
                    operational_certificate: Some(
                        "82845820333e09c8d129f5cf9fc70e0752cbf00181bb77dd88f75980e48500620ce4b2ea101904a65840d29449a3dee9b42583fe09cd9e950e465c07ebd66f347429760681016a015ee973ad648b726d0f0ee92bb3ae320a96617fb6100770f5308d54660419d390ca045820839b483993daad44bff2bdf71bfe94e1350b459db650ea83ea276615020341be".to_string(),
                    ),
                    kes_period: Some(6)
                }
        }

        mod signer {
            use super::*;

            fn golden_message_with_json_hex_encoding() -> SignerMessagePart {
                golden_signer_message_part_with_json_hex_encoding()
            }

            fn golden_message_with_bytes_hex_encoding() -> SignerMessagePart {
                golden_signer_message_part_with_bytes_hex_encoding()
            }

            #[test]
            fn restorations_from_json_hex_and_bytes_hex_give_same_signer() {
                let signer_from_json_hex: Signer =
                    golden_message_with_json_hex_encoding().try_into().unwrap();

                let signer_from_bytes_hex: Signer =
                    golden_message_with_bytes_hex_encoding().try_into().unwrap();

                assert_eq!(signer_from_json_hex, signer_from_bytes_hex);
            }
        }

        mod signer_with_stake {
            use super::*;

            fn golden_message_with_json_hex_encoding() -> SignerWithStakeMessagePart {
                let signer_message_part = golden_signer_message_part_with_json_hex_encoding();

                SignerWithStakeMessagePart {
                    party_id: signer_message_part.party_id,
                    verification_key: signer_message_part.verification_key,
                    verification_key_signature: signer_message_part.verification_key_signature,
                    operational_certificate: signer_message_part.operational_certificate,
                    kes_period: signer_message_part.kes_period,
                    stake: 123,
                }
            }

            fn golden_message_with_bytes_hex_encoding() -> SignerWithStakeMessagePart {
                let signer_message_part = golden_signer_message_part_with_bytes_hex_encoding();

                SignerWithStakeMessagePart {
                    party_id: signer_message_part.party_id,
                    verification_key: signer_message_part.verification_key,
                    verification_key_signature: signer_message_part.verification_key_signature,
                    operational_certificate: signer_message_part.operational_certificate,
                    kes_period: signer_message_part.kes_period,
                    stake: 123,
                }
            }

            #[test]
            fn restorations_from_json_hex_and_bytes_hex_give_same_signer() {
                let signer_from_json_hex: SignerWithStake =
                    golden_message_with_json_hex_encoding().try_into().unwrap();

                let signer_from_bytes_hex: SignerWithStake =
                    golden_message_with_bytes_hex_encoding().try_into().unwrap();

                assert_eq!(signer_from_json_hex, signer_from_bytes_hex);
            }
        }
    }
}
