use crate::entities;
use crate::entities::LotteryIndex;
use std::num::TryFromIntError;

pub static SINGLE_SIGNATURE_MESSAGE_SCHEMA: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/avro-schemas/single_signature_message.json"
));

#[derive(Debug, PartialEq, Eq, Clone, serde::Deserialize, serde::Serialize, Default)]
pub struct SingleSignatureMessage {
    #[serde(rename = "PartyId")]
    pub party_id: String,
    #[serde(rename = "Indexes")]
    pub indexes: Vec<i64>,
    #[serde(rename = "Signature")]
    pub signature: String,
}

impl TryFrom<entities::SingleSignatures> for SingleSignatureMessage {
    type Error = TryFromIntError;

    fn try_from(value: entities::SingleSignatures) -> Result<Self, Self::Error> {
        // Note: should do a safe cast and raise an error if it fails
        let mut indexes = Vec::new();
        for index in value.won_indexes {
            indexes.push(i64::try_from(index)?);
        }

        Ok(SingleSignatureMessage {
            party_id: value.party_id,
            indexes,
            signature: value.signature,
        })
    }
}

impl TryFrom<SingleSignatureMessage> for entities::SingleSignatures {
    type Error = TryFromIntError;

    fn try_from(value: SingleSignatureMessage) -> Result<Self, Self::Error> {
        let mut indexes = Vec::new();
        for index in value.indexes {
            indexes.push(LotteryIndex::try_from(index)?);
        }

        Ok(entities::SingleSignatures::new(
            value.party_id,
            value.signature,
            indexes,
        ))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use apache_avro::{Codec, Reader, Schema, Writer};

    #[test]
    fn schema_is_valid() {
        Schema::parse_str(SINGLE_SIGNATURE_MESSAGE_SCHEMA).expect("Schema should be valid");
    }

    #[test]
    fn can_serialize_deserialize() {
        let schema =
            Schema::parse_str(SINGLE_SIGNATURE_MESSAGE_SCHEMA).expect("Schema should be valid");
        let mut writer = Writer::new(&schema, Vec::new());
        // let mut writer = Writer::with_codec(&schema, Vec::new(), Codec::Deflate);
        let signature = SingleSignatureMessage {
            party_id: "party_id".to_string(),
            indexes: vec![1, 2, 3],
            signature: "signature".to_string(),
        };

        writer
            .append_ser(&signature)
            .expect("writing data should not fail");

        let encoded = writer.into_inner().expect("encoding data should not fail");
        println!(
            "encoded signature [size: {}]: {:?}",
            &encoded.len(),
            &encoded
        );
        println!("utf8 bytes decode: {}", String::from_utf8_lossy(&encoded));

        let reader =
            Reader::with_schema(&schema, &encoded[..]).expect("reader creation should not fail");

        for record in reader {
            let value = record.expect("record read should not fail");
            let signature = apache_avro::from_value::<SingleSignatureMessage>(&value)
                .expect("signature parsing should not fail");
            println!("Avro value: {:?}", &value);
            println!("{:?}", signature);
        }
    }
}
