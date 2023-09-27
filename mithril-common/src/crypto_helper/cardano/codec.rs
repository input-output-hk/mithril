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

use anyhow::{anyhow, Context};
use hex::FromHex;
use kes_summed_ed25519::kes::Sum6Kes;
use kes_summed_ed25519::traits::KesSk;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_with::{As, Bytes};
use std::fs;
use std::io::Write;
use std::path::Path;
use thiserror::Error;

use crate::StdError;

/// We need to create this struct because the design of Sum6Kes takes
/// a reference to a mutable pointer. It is therefore not possible to
/// implement Ser/Deser using serde.
// We need this helper structure, because we are currently getting the key
// from a file, instead of directly consuming a buffer.
// todo: create the KES key directly from a buffer instead of deserialising from disk
#[derive(Clone, Serialize, Deserialize)]
pub struct Sum6KesBytes(#[serde(with = "As::<Bytes>")] pub [u8; 612]);

/// Parse error
#[derive(Error, Debug)]
#[error("Codec parse error")]
pub struct CodecParseError(#[source] StdError);

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

    /// Deserialize a type `T: Serialize + DeserializeOwned` from file following Cardano
    /// Shelley file format.
    fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, CodecParseError> {
        let data = fs::read_to_string(path)
            .with_context(|| "SerDeShelleyFileFormat can not read data from file {}")
            .map_err(|e| CodecParseError(anyhow!(e)))?;
        let file: ShelleyFileFormat = serde_json::from_str(&data)
            .with_context(|| "SerDeShelleyFileFormat can not unserialize json data")
            .map_err(|e| CodecParseError(anyhow!(e)))?;
        let hex_vector = Vec::from_hex(file.cbor_hex)
            .with_context(|| "SerDeShelleyFileFormat can not unserialize hex data")
            .map_err(|e| CodecParseError(anyhow!(e)))?;
        let a: Self = serde_cbor::from_slice(&hex_vector)
            .with_context(|| "SerDeShelleyFileFormat can not unserialize cbor data")
            .map_err(|e| CodecParseError(anyhow!(e)))?;

        Ok(a)
    }

    /// Serialize a type `T: Serialize + DeserializeOwned` to file following Cardano
    /// Shelley file format.
    fn to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), CodecParseError> {
        let cbor_string = hex::encode(
            serde_cbor::to_vec(&self)
                .with_context(|| "SerDeShelleyFileFormat can not serialize data to cbor")
                .map_err(|e| CodecParseError(anyhow!(e)))?,
        );

        let file_format = ShelleyFileFormat {
            file_type: Self::TYPE.to_string(),
            description: Self::DESCRIPTION.to_string(),
            cbor_hex: cbor_string,
        };

        let mut file = fs::File::create(path)
            .with_context(|| "SerDeShelleyFileFormat can not create file")
            .map_err(|e| CodecParseError(anyhow!(e)))?;
        let json_str = serde_json::to_string(&file_format)
            .with_context(|| "SerDeShelleyFileFormat can not serialize data to json")
            .map_err(|e| CodecParseError(anyhow!(e)))?;

        write!(file, "{json_str}")
            .with_context(|| "SerDeShelleyFileFormat can not write data to file")
            .map_err(|e| CodecParseError(anyhow!(e)))?;
        Ok(())
    }
}

impl SerDeShelleyFileFormat for Sum6KesBytes {
    const TYPE: &'static str = "KesSigningKey_ed25519_kes_2^6";
    const DESCRIPTION: &'static str = "KES Signing Key";

    /// Deserialize a Cardano key from file. Cardano KES key Shelley format does not
    /// contain the period (it is always zero). Therefore we need to include it in the
    /// deserialisation.
    fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, CodecParseError> {
        let data = fs::read_to_string(path)
            .with_context(|| "Sum6KesBytes can not read data from file")
            .map_err(|e| CodecParseError(anyhow!(e)))?;
        let file: ShelleyFileFormat = serde_json::from_str(&data)
            .with_context(|| "Sum6KesBytes can not unserialize json data")
            .map_err(|e| CodecParseError(anyhow!(e)))?;
        let mut hex_vector = Vec::from_hex(file.cbor_hex)
            .with_context(|| "Sum6KesBytes can not unserialize hex data")
            .map_err(|e| CodecParseError(anyhow!(e)))?;

        // We check whether the serialisation was performed by the haskell library or the rust library
        if (hex_vector[2] & 4u8) == 0 {
            // First we need to change the cbor format to notify about the extra 4 bytes:
            hex_vector[2] |= 4u8;
            // Then we append the bytes representing the period = 0
            hex_vector.extend_from_slice(&[0u8; 4]);
        }

        let a: Self = serde_cbor::from_slice(&hex_vector)
            .with_context(|| "Sum6KesBytes can not unserialize cbor data")
            .map_err(|e| CodecParseError(anyhow!(e)))?;
        Ok(a)
    }
}

impl<'a> TryFrom<&'a mut Sum6KesBytes> for Sum6Kes<'a> {
    type Error = CodecParseError;

    fn try_from(value: &'a mut Sum6KesBytes) -> Result<Self, Self::Error> {
        Self::from_bytes(&mut value.0).map_err(|e| CodecParseError(anyhow!(format!("{e:?}"))))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn compat_with_shelly_format() {
        let temp_dir = std::env::temp_dir().join("testing");
        fs::create_dir_all(&temp_dir).expect("temp dir creation should not fail");
        let sk_dir = temp_dir.join("dummy.skey");
        let cbor_string = "590260fe77acdfa56281e4b05198f5136018057a65f425411f0990cac4aca0f2917aa00a3d51e191f6f425d870aca3c6a2a41833621f5729d7bc0e3dfc3ae77d057e5e1253b71def7a54157b9f98973ca3c49edd9f311e5f4b23ac268b56a6ac040c14c6d2217925492e42f00dc89a2a01ff363571df0ca0db5ba37001cee56790cc01cd69c6aa760fca55a65a110305ea3c11da0a27be345a589329a584ebfc499c43c55e8c6db5d9c0b014692533ee78abd7ac1e79f7ec9335c7551d31668369b4d5111db78072f010043e35e5ca7f11acc3c05b26b9c7fe56f02aa41544f00cb7685e87f34c73b617260ade3c7b8d8c4df46693694998f85ad80d2cbab0b575b6ccd65d90574e84368169578bff57f751bc94f7eec5c0d7055ec88891a69545eedbfbd3c5f1b1c1fe09c14099f6b052aa215efdc5cb6cdc84aa810db41dbe8cb7d28f7c4beb75cc53915d3ac75fc9d0bf1c734a46e401e15150c147d013a938b7e07cc4f25a582b914e94783d15896530409b8acbe31ef471de8a1988ac78dfb7510729eff008084885f07df870b65e4f382ca15908e1dcda77384b5c724350de90cec22b1dcbb1cdaed88da08bb4772a82266ec154f5887f89860d0920dba705c45957ef6d93e42f6c9509c966277d368dd0eefa67c8147aa15d40a222f7953a4f34616500b310d00aa1b5b73eb237dc4f76c0c16813d321b2fc5ac97039be25b22509d1201d61f4ccc11cd4ff40fffe39f0e937b4722074d8e073a775d7283b715d46f79ce128e3f1362f35615fa72364d20b6db841193d96e58d9d8e86b516bbd1f05e45b39823a93f6e9f29d9e01acf2c12c072d1c64e0afbbabf6903ef542e".to_string();

        let file_format = ShelleyFileFormat {
            file_type: Sum6KesBytes::TYPE.to_string(),
            description: Sum6KesBytes::DESCRIPTION.to_string(),
            cbor_hex: cbor_string,
        };

        let mut file =
            fs::File::create(sk_dir.clone()).expect("Unexpected error with file creation.");
        let json_str =
            serde_json::to_string(&file_format).expect("Unexpected error with serialisation.");

        write!(file, "{json_str}").expect("Unexpected error writing to file.");

        let mut kes_sk_bytes =
            Sum6KesBytes::from_file(&sk_dir).expect("Failure parsing Shelley file format.");

        assert!(Sum6Kes::try_from(&mut kes_sk_bytes).is_ok());
    }
}
