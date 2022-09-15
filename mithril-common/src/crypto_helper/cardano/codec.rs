use hex::FromHex;
use kes_summed_ed25519::kes::Sum6Kes;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::Write;
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

/// Trait that allows any structure that implements serialize to be formatted following
/// the Shelly json format.
pub(crate) trait FromShelleyFile: serde::Serialize {
    const TYPE: &'static str;
    const DESCRIPTION: &'static str;
    fn from_file<R: DeserializeOwned, P: AsRef<Path>>(path: P) -> Result<R, ParseError> {
        let data = fs::read_to_string(path).map_err(ParseError::Path)?;

        let file: ShelleyFileFormat =
            serde_json::from_str(&data).map_err(ParseError::JsonFormat)?;

        let hex_vector = Vec::from_hex(file.cbor_hex).map_err(|_| ParseError::CborData)?;

        let a: R = serde_cbor::from_slice(&hex_vector).map_err(|_| ParseError::CborData)?;
        Ok(a)
    }

    fn to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), ParseError> {
        let cbor_string =
            hex::encode(&serde_cbor::to_vec(&self).map_err(|_| ParseError::CborData)?);

        let file_format = ShelleyFileFormat {
            file_type: Self::TYPE.to_string(),
            description: Self::DESCRIPTION.to_string(),
            cbor_hex: cbor_string,
        };

        let mut file = fs::File::create(path).map_err(ParseError::Path)?;
        let json_str = serde_json::to_string(&file_format).map_err(ParseError::JsonFormat)?;

        write!(file, "{}", json_str).expect("Unable to write bytes to file");
        Ok(())
    }
}

impl FromShelleyFile for Sum6Kes {
    const TYPE: &'static str = "VrfSigningKey_PraosVRF";
    const DESCRIPTION: &'static str = "VRF Signing Key";
}
