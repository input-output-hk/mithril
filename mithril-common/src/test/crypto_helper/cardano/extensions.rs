use std::path::Path;

use crate::crypto_helper::{CodecParseError, ProtocolParameters, SerDeShelleyFileFormat};

/// Extension trait adding test utilities to [ProtocolInitializer][crate::crypto_helper::ProtocolInitializer]
pub trait ProtocolInitializerTestExtension {
    /// `TEST ONLY` - Override the protocol parameters of the `Initializer`
    fn override_protocol_parameters(&mut self, protocol_parameters: &ProtocolParameters);
}

/// Extension trait adding test-only file export utilities to any type implementing [SerDeShelleyFileFormat].
pub trait SerDeShelleyFileFormatTestExtension: SerDeShelleyFileFormat {
    /// `TEST ONLY` - Serialize the structure to a Shelley-formatted file.
    fn to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), CodecParseError>;
}
