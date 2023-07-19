use crate::entities::{
    Beacon, CertificateMetadata, HexEncodedAgregateVerificationKey, HexEncodedGenesisSignature,
    HexEncodedMultiSignature, ProtocolMessage,
};
use std::ops::Not;

use sha2::{Digest, Sha256};

/// Certificate represents a Mithril certificate embedding a Mithril STM multisignature
#[derive(Clone, Debug, PartialEq, Default)]
pub struct Certificate {
    /// Hash of the current certificate
    /// Computed from the other fields of the certificate
    /// aka H(Cp,n))
    pub hash: String,

    /// Hash of the previous certificate in the chain
    /// This is either the hash of the first certificate of the epoch in the chain
    /// Or the first certificate of the previous epoch in the chain (if the certificate is the first of its epoch)
    /// aka H(FC(n))
    pub previous_hash: String,

    /// Mithril beacon on the Cardano chain
    /// aka BEACON(p,n)
    pub beacon: Beacon,

    /// Certificate metadata
    /// aka METADATA(p,n)
    pub metadata: CertificateMetadata,

    /// Structured message that is used to created the signed message
    /// aka MSG(p,n) U AVK(n-1)
    pub protocol_message: ProtocolMessage,

    /// Message that is signed by the signers
    /// aka H(MSG(p,n) || AVK(n-1))
    pub signed_message: String,

    /// Aggregate verification key
    /// The AVK used to sign during the current epoch
    /// aka AVK(n-2)
    pub aggregate_verification_key: HexEncodedAgregateVerificationKey,

    /// STM multi signature created from a quorum of single signatures from the signers
    /// aka MULTI_SIG(H(MSG(p,n) || AVK(n-1)))
    pub multi_signature: HexEncodedMultiSignature,

    // @todo: Should we change this to an option since it's only filled for genesis certificates ?
    /// Genesis signature created from the original stake distribution
    /// aka GENESIS_SIG(AVK(-1))
    pub genesis_signature: HexEncodedGenesisSignature,
}

impl Certificate {
    /// Certificate factory
    pub fn new(
        previous_hash: String,
        beacon: Beacon,
        metadata: CertificateMetadata,
        protocol_message: ProtocolMessage,
        aggregate_verification_key: HexEncodedAgregateVerificationKey,
        multi_signature: HexEncodedMultiSignature,
        genesis_signature: HexEncodedGenesisSignature,
    ) -> Certificate {
        let signed_message = protocol_message.compute_hash();
        let mut certificate = Certificate {
            hash: "".to_string(),
            previous_hash,
            beacon,
            metadata,
            protocol_message,
            signed_message,
            aggregate_verification_key,
            multi_signature,
            genesis_signature,
        };
        certificate.hash = certificate.compute_hash();
        certificate
    }

    /// Computes the hash of a Certificate
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.previous_hash.as_bytes());
        hasher.update(self.beacon.compute_hash().as_bytes());
        hasher.update(self.metadata.compute_hash().as_bytes());
        hasher.update(self.protocol_message.compute_hash().as_bytes());
        hasher.update(self.signed_message.as_bytes());
        hasher.update(self.aggregate_verification_key.as_bytes());
        hasher.update(self.multi_signature.as_bytes());
        hasher.update(self.genesis_signature.as_bytes());
        hex::encode(hasher.finalize())
    }

    /// Tell if the certificate is a genesis certificate
    pub fn is_genesis(&self) -> bool {
        self.genesis_signature.is_empty().not()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entities::{ProtocolMessagePartKey, ProtocolParameters, SignerWithStake};
    use chrono::{Duration, TimeZone, Timelike, Utc};

    fn get_signers_with_stake() -> Vec<SignerWithStake> {
        vec![
            SignerWithStake::new(
                "1".to_string(),
                "7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d".try_into().unwrap(),
                None,
                None,
                None,
                10,
            ),
            SignerWithStake::new(
                "2".to_string(),
                "7b22766b223a5b3134352c35362c3137352c33322c3132322c3138372c3231342c3232362c3235312c3134382c38382c392c312c3130332c3135392c3134362c38302c3136362c3130372c3234332c3235312c3233362c34312c32382c3131312c3132382c3230372c3136342c3133322c3134372c3232382c38332c3234362c3232382c3137302c36382c38392c37382c36302c32382c3132332c3133302c38382c3233342c33382c39372c34322c36352c312c3130302c35332c31382c37382c3133312c382c36312c3132322c3133312c3233382c38342c3233332c3232332c3135342c3131382c3131382c37332c32382c32372c3130312c37382c38302c3233332c3132332c3230362c3232302c3137342c3133342c3230352c37312c3131302c3131322c3138302c39372c39382c302c3131332c36392c3134352c3233312c3136382c34332c3137332c3137322c35362c3130342c3230385d2c22706f70223a5b3133372c3231342c37352c37352c3134342c3136312c3133372c37392c39342c3134302c3138312c34372c33312c38312c3231332c33312c3137312c3231362c32342c3137342c37382c3234382c3133302c37352c3235352c31312c3134352c3132342c36312c38302c3139302c32372c3231362c3130352c3130362c3234382c39312c3134332c3230342c3130322c3230332c3136322c37362c3130372c31352c35322c36312c38322c3134362c3133302c3132342c37342c382c33342c3136342c3138372c3230332c38322c36342c3130382c3139312c3138352c3138382c37372c3132322c352c3234362c3235352c3130322c3131392c3234372c3139392c3131372c36372c3234312c3134332c32392c3136382c36372c39342c3135312c37382c3132392c3133312c33302c3130312c3137332c31302c36392c36382c3137352c39382c33372c3233392c3139342c32395d7d".try_into().unwrap(),
                None,
                None,
                None,
                20,
            ),
        ]
    }

    #[test]
    fn test_certificate_compute_hash() {
        const HASH_EXPECTED: &str =
            "3b5cabc1ddc73c5dfd30064f6b9aef55c464b8359acc1649cb82fc4492be6a15";

        let initiated_at = Utc
            .with_ymd_and_hms(2024, 2, 12, 13, 11, 47)
            .unwrap()
            .with_nanosecond(123043)
            .unwrap();
        let sealed_at = initiated_at + Duration::seconds(100);
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-123".to_string(),
        );
        assert_eq!(
            HASH_EXPECTED,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    initiated_at,
                    sealed_at,
                    get_signers_with_stake(),
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate::new(
                "previous_hash-modified".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    initiated_at,
                    sealed_at,
                    get_signers_with_stake(),
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet-modified".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    initiated_at,
                    sealed_at,
                    get_signers_with_stake(),
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0-modified".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    initiated_at,
                    sealed_at,
                    get_signers_with_stake(),
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        let mut protocol_message_modified = protocol_message.clone();
        protocol_message_modified.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-456".to_string(),
        );
        assert_ne!(
            HASH_EXPECTED,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    initiated_at,
                    sealed_at,
                    get_signers_with_stake(),
                ),
                protocol_message_modified.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    initiated_at,
                    sealed_at,
                    get_signers_with_stake(),
                ),
                protocol_message.clone(),
                "aggregate_verification_key-modified".to_string(),
                "multi_signature".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    initiated_at,
                    sealed_at,
                    get_signers_with_stake(),
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature-modified".to_string(),
                "genesis_signature".to_string(),
            )
            .compute_hash()
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate::new(
                "previous_hash".to_string(),
                Beacon::new("testnet".to_string(), 10, 100),
                CertificateMetadata::new(
                    "0.1.0".to_string(),
                    ProtocolParameters::new(1000, 100, 0.123),
                    initiated_at,
                    sealed_at,
                    get_signers_with_stake(),
                ),
                protocol_message.clone(),
                "aggregate_verification_key".to_string(),
                "multi_signature".to_string(),
                "genesis_signature-modified".to_string(),
            )
            .compute_hash()
        );
    }
}
