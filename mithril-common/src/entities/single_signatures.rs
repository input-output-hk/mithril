use anyhow::{anyhow, Context, Result as StdResult};
use serde::{Deserialize, Serialize};

use crate::{
    crypto_helper::{key_decode_hex, key_encode_hex, ProtocolSingleSignature},
    entities::{LotteryIndex, PartyId},
};

/// Single Signature
#[derive(Debug, Clone, Eq)]
pub struct SingleSignature {
    signature: ProtocolSingleSignature,
}

impl SingleSignature {
    /// Create a signature from a JSON Hex representation
    pub fn from_json_hex(hex_string: &str) -> StdResult<Self> {
        let signature = key_decode_hex::<ProtocolSingleSignature>(&hex_string.to_owned())
            .map_err(|e| anyhow!(e))
            .with_context(|| "Could not deserialize a SingleSignature from JSON hex string.")?;

        Ok(Self { signature })
    }

    /// Dump a JSON Hex representation of the signature
    pub fn to_json_hex(&self) -> StdResult<String> {
        key_encode_hex(&self.signature)
            .map_err(|e| anyhow!(e))
            .with_context(|| "Could not serialize a SingleSignature to JSON hex key string.")
    }
}

impl From<SingleSignature> for ProtocolSingleSignature {
    fn from(value: SingleSignature) -> Self {
        value.signature
    }
}

impl From<ProtocolSingleSignature> for SingleSignature {
    fn from(value: ProtocolSingleSignature) -> Self {
        SingleSignature { signature: value }
    }
}

impl TryFrom<String> for SingleSignature {
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        SingleSignature::from_json_hex(&value)
    }
}

impl TryInto<String> for SingleSignature {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<String, Self::Error> {
        self.to_json_hex()
    }
}

impl Serialize for SingleSignature {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::Error;
        let hex = self.to_json_hex().map_err(Error::custom)?;

        hex.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SingleSignature {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let string = String::deserialize(deserializer)?;

        Self::from_json_hex(&string).map_err(Error::custom)
    }
}

impl PartialEq for SingleSignature {
    fn eq(&self, other: &Self) -> bool {
        self.to_json_hex().unwrap() == other.to_json_hex().unwrap()
    }
}

/// SingleSignatures represent single signatures originating from a participant in the network
/// for a digest at won lottery indexes
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SingleSignatures {
    /// The unique identifier of the signer
    pub party_id: PartyId,

    /// The single signature of the digest
    pub signature: SingleSignature,

    /// The indexes of the won lotteries that lead to the single signatures
    #[serde(rename = "indexes")]
    pub won_indexes: Vec<LotteryIndex>,
}

impl SingleSignatures {
    /// SingleSignature factory
    pub fn new(
        party_id: PartyId,
        signature: SingleSignature,
        won_indexes: Vec<LotteryIndex>,
    ) -> SingleSignatures {
        SingleSignatures {
            party_id,
            signature,
            won_indexes,
        }
    }

    /// Convert this [SingleSignatures] to its corresponding [MithrilStm Signature][ProtocolSingleSignature].
    pub fn to_protocol_signature(&self) -> Result<ProtocolSingleSignature, String> {
        Ok(self.signature.clone().into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{crypto_helper::tests_setup::setup_message, test_utils::MithrilFixtureBuilder};

    #[test]
    fn single_signatures_should_convert_to_protocol_signatures() {
        let message = setup_message();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let signer = &fixture.signers_fixture()[0];
        let protocol_sigs = signer
            .protocol_signer
            .sign(message.compute_hash().as_bytes())
            .unwrap();

        let signature = SingleSignatures::new(
            signer.signer_with_stake.party_id.to_owned(),
            protocol_sigs.clone().into(),
            protocol_sigs.indexes.clone(),
        );

        assert_eq!(protocol_sigs, signature.to_protocol_signature().unwrap());
    }
}
