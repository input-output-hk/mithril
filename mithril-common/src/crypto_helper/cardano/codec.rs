//! Module to provide functions to (de)serialise JSON data structures as used in Shelley,
//! which have the following format:
//! ```json
//! {
//!      "type": <NAME OF SERIALISED STRUCTURE>,
//!      "description": <DESCRIPTION OF SERIALISED STRUCTURE>,
//!      "cborHex": <CBOR HEX REPRESENTATION OF SERIALISED STRUCTURE>
//!  }
//! ```
//!
//! The trait `SerDeShelleyFileFormat` can be implemented for any structure that implements
//! `Serialize` and `Deserialize`.

use hex::FromHex;
use kes_summed_ed25519::kes::Sum6Kes;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::Write;
use std::path::Path;
use thiserror::Error;

/// Parse error
#[derive(Error, Debug)]
pub enum ParseError {
    #[error("io error: `{0}`")]
    IO(#[from] std::io::Error),

    #[error("JSON parse error: `{0}`")]
    JsonFormat(#[from] serde_json::Error),

    #[error("CBOR hex codec error: `{0}`")]
    CborHex(#[from] hex::FromHexError),

    #[error("CBOR parse error: `{0}`")]
    CborFormat(#[from] serde_cbor::Error),
}

/// Fields for a shelley formatted file (holds for vkeys, skeys or certs)
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
struct ShelleyFileFormat {
    #[serde(rename = "type")]
    file_type: String,
    description: String,
    #[serde(rename = "cborHex")]
    cbor_hex: String,
}

/// Trait that allows any structure that implements Serialize and DeserializeOwned to
/// be serialized and deserialized following the Shelly json format.
pub trait SerDeShelleyFileFormat: Serialize + DeserializeOwned {
    /// The type of Cardano key
    const TYPE: &'static str;

    /// The description of the Cardano key
    const DESCRIPTION: &'static str;

    /// Deserialize a Cardano key from file
    fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, ParseError> {
        let data = fs::read_to_string(path)?;
        let file: ShelleyFileFormat = serde_json::from_str(&data)?;
        let hex_vector = Vec::from_hex(file.cbor_hex)?;

        let a: Self = serde_cbor::from_slice(&hex_vector)?;
        Ok(a)
    }

    /// Serialize a Cardano Key to file
    fn to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), ParseError> {
        let cbor_string = hex::encode(&serde_cbor::to_vec(&self)?);

        let file_format = ShelleyFileFormat {
            file_type: Self::TYPE.to_string(),
            description: Self::DESCRIPTION.to_string(),
            cbor_hex: cbor_string,
        };

        let mut file = fs::File::create(path)?;
        let json_str = serde_json::to_string(&file_format)?;

        write!(file, "{}", json_str)?;
        Ok(())
    }
}

impl SerDeShelleyFileFormat for Sum6Kes {
    const TYPE: &'static str = "KesSigningKey_ed25519_kes_2^6";
    const DESCRIPTION: &'static str = "KES Signing Key";
}
