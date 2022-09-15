use hex::FromHex;
use kes_summed_ed25519::kes::Sum6Kes;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

/// Parse error
#[derive(Debug)]
pub enum ParseError {
    Path(std::io::Error),
    JsonFormat(serde_json::Error),
    CborData,
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

pub(crate) trait FromShelleyFile {
    fn from_file<R: DeserializeOwned, P: AsRef<Path>>(path: P) -> Result<R, ParseError> {
        let data = fs::read_to_string(path).map_err(ParseError::Path)?;

        let file: ShelleyFileFormat =
            serde_json::from_str(&data).map_err(ParseError::JsonFormat)?;

        let hex_vector = Vec::from_hex(file.cbor_hex).map_err(|_| ParseError::CborData)?;

        let a: R = serde_cbor::from_slice(&hex_vector).map_err(|_| ParseError::CborData)?;
        Ok(a)
    }
}

impl FromShelleyFile for Sum6Kes {}
