#![allow(missing_docs)]

use apache_avro::{Reader, Schema};
use serde::de::DeserializeOwned;
use thiserror::Error;

pub struct MessageDecoder {
    schema: Option<Schema>,
}

#[derive(Error, Debug)]
pub enum MessageDecodeError {
    #[error("Could not parse given schema: {0}")]
    InvalidSchema(apache_avro::Error),

    #[error("Could not read data: {0}")]
    Read(apache_avro::Error),

    #[error("Could not convert read data: {0}")]
    Conversion(apache_avro::Error),

    #[error("The given message was empty")]
    MessageEmpty(),
}

impl MessageDecoder {
    pub fn with_schema(schema_str: &str) -> Result<Self, MessageDecodeError> {
        let schema =
            Schema::parse_str(schema_str).map_err(|e| MessageDecodeError::InvalidSchema(e))?;
        Ok(Self {
            schema: Some(schema),
        })
    }

    pub fn decode<T>(&self, data: Vec<u8>) -> Result<Vec<T>, MessageDecodeError>
    where
        T: DeserializeOwned,
    {
        let reader = match self.schema.as_ref() {
            Some(schema) => {
                Reader::with_schema(schema, &data[..]).map_err(|e| MessageDecodeError::Read(e))?
            }
            None => Reader::new(&data[..]).map_err(|e| MessageDecodeError::Read(e))?,
        };

        let mut result: Vec<T> = Vec::new();

        for record in reader {
            let value = record.map_err(|e| MessageDecodeError::Read(e))?;
            result.push(
                apache_avro::from_value::<T>(&value)
                    .map_err(|e| MessageDecodeError::Conversion(e))?,
            );
        }
        Ok(result)
    }

    pub fn decode_one<T>(&self, data: Vec<u8>) -> Result<T, MessageDecodeError>
    where
        T: DeserializeOwned,
    {
        let mut decoded: Vec<T> = self.decode(data)?;

        if decoded.is_empty() {
            Err(MessageDecodeError::MessageEmpty())
        } else {
            Ok(decoded.swap_remove(0))
        }
    }
}

mod test {}
